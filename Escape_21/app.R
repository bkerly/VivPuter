library(shiny)
library(tidyverse)
library(ollamar)

# Configure Ollama server (change this to your cloud server URL)
# For local: "http://localhost:11434"
# For cloud: "https://your-server-url.com" or your actual cloud endpoint
options(ollamar_server = "http://localhost:11434")
#"http://localhost:11434")

# Configure model name - change this to match your installed model
# Common options: "llama3.2", "mistral", "ministral", "phi", etc.
OLLAMA_MODEL <- "mistral"  # Change this if needed

OLLAMA_SERVER <- Sys.getenv("OLLAMA_SERVER", "https://ollama.com")
OLLAMA_API_KEY <- Sys.getenv("OLLAMA_API_KEY", "88fbdafea04c4303a3dd173c8d4db644.KgNMInzeXhxWrTmXoY6YkgXG")

# Set the server
options(ollamar_server = OLLAMA_SERVER)

# Configure headers with Bearer token for authentication
if (nchar(OLLAMA_API_KEY) > 0) {
  options(ollamar_headers = list(
    Authorization = paste("Bearer", OLLAMA_API_KEY)
  ))
}

# Helper Functions --------------------------------------------------------

base_prompt <- 
  'Background:
  
  Please assume the role of a fun and silly text based adventure prompt. Keep the tone consistent with the context_input so far. Keep it appropriate for a 6 year old user. You will need to respond to context.'

input_evaluation <- 
  'Based on the context, user input, and goal, evaluate how likely it is that the user input would achieve the goal on a scale of 1 to 10. One is something that is very unlikely to work. Return only a numeric response between 1 and 10.'

input_evaluation_format <- list(
  type = "object",
  properties = list(
    input_evaluation_numeric = list(type = "integer")
  ),
  required = list("input_evaluation_numeric")
)

input_evaluation_function <- function(goal, context, user_input) {
  full_prompt <- paste0(
    base_prompt,
    '\n---\nInstructions = \n',
    input_evaluation,
    '\n---\ngoal_input =\n', goal,
    '\n---\ncontext_input =\n', context,
    '\n---\nuser_input =\n', user_input
  )
  
  resp <- generate(OLLAMA_MODEL,
                   format = input_evaluation_format,
                   full_prompt,
                   output = "text")
  
  input_value <- jsonlite::fromJSON(resp) %>% as.numeric()
  return(input_value)
}

success_response_prompt <- 
  'IMPORTANT: The character SUCCEEDED and achieved the goal! The math shows they succeeded. Based on the context, character, and user input, explain in 2-3 sentences how the character was SUCCESSFUL in achieving the goal. Be creative and silly. Focus ONLY on the current goal and action, not previous challenges. The outcome must be positive and show success. Return only a string.'

failure_response_prompt <- 
  'IMPORTANT: The character FAILED to achieve the goal! The math shows they failed. Based on the context, character, and user input, explain in 2-3 sentences how the character was UNSUCCESSFUL and why their action did not work. Be creative and silly. Focus ONLY on the current goal and action, not previous challenges. The outcome must show failure and what went wrong. Return only a string.'

success_failure_format <- list(
  type = "object",
  properties = list(
    success_failure_string = list(type = "string")
  ),
  required = list("success_failure_string")
)

response_generate_function <- function(goal, context, user_input, success) {
  full_prompt <- paste0(
    base_prompt,
    '\n---\nInstructions = \n',
    ifelse(success, success_response_prompt, failure_response_prompt),
    '\n---\ngoal_input =\n', goal,
    '\n---\ncontext_input =\n', context,
    '\n---\nuser_input =\n', user_input
  )
  
  resp <- generate(OLLAMA_MODEL,
                   format = success_failure_format,
                   full_prompt,
                   output = "text") %>%
    jsonlite::fromJSON() %>%
    as.character()
  
  return(resp)
}

# UI ----------------------------------------------------------------------

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body { background-color: #f5f5f5; }
      .story-text { 
        background-color: white; 
        padding: 15px; 
        border-radius: 5px; 
        margin-bottom: 10px;
        border-left: 4px solid #4CAF50;
      }
      .response-text { 
        background-color: #e3f2fd; 
        padding: 10px; 
        border-radius: 5px; 
        margin: 10px 0;
        font-style: italic;
      }
      .success { color: #4CAF50; font-weight: bold; }
      .failure { color: #f44336; font-weight: bold; }
      .stats { 
        background-color: #fff3cd; 
        padding: 8px; 
        border-radius: 5px; 
        margin: 5px 0;
        font-size: 0.9em;
      }
    "))
  ),
  
  titlePanel("🎮 Text-Based Adventure Game"),
  
  sidebarLayout(
    sidebarPanel(
      width = 3,
      
      conditionalPanel(
        condition = "output.gameStarted == false && output.storySelected == false",
        h4("Choose Your Story"),
        uiOutput("storySelect"),
        actionButton("selectStory", "Select Story", 
                     class = "btn-primary btn-block")
      ),
      
      conditionalPanel(
        condition = "output.gameStarted == false && output.storySelected == true",
        h4("Choose Your Character"),
        uiOutput("characterSelect"),
        actionButton("startGame", "Start Adventure!", 
                     class = "btn-primary btn-block"),
        hr(),
        actionButton("backToStory", "← Back to Story Selection", 
                     class = "btn-secondary btn-sm btn-block")
      ),
      
      conditionalPanel(
        condition = "output.gameStarted == true",
        h4("Character Info"),
        uiOutput("characterInfo"),
        hr(),
        actionButton("resetGame", "New Game", 
                     class = "btn-warning btn-block")
      )
    ),
    
    mainPanel(
      width = 9,
      
      conditionalPanel(
        condition = "output.gameStarted == true",
        uiOutput("storyDisplay"),
        hr(),
        textInput("userAction", "What do you do?", 
                  placeholder = "Enter your action here...", 
                  width = "100%"),
        fluidRow(
          column(4, actionButton("submitAction", "Submit Action", 
                                 class = "btn-success btn-block")),
          column(4, actionButton("skipChallenge", "Skip Challenge", 
                                 class = "btn-info btn-block")),
          column(4, actionButton("quitGame", "Quit Game", 
                                 class = "btn-danger btn-block"))
        )
      )
    )
  )
)

# Server ------------------------------------------------------------------

server <- function(input, output, session) {
  
  # Reactive values
  rv <- reactiveValues(
    gameStarted = FALSE,
    storySelected = FALSE,
    availableStories = NULL,
    selectedStory = NULL,
    storyData = NULL,
    characterData = NULL,
    selectedCharacter = NULL,
    currentRow = 1,
    storyHistory = list(),
    responses = c()
  )
  
  # Scan for available stories on startup
  observe({
    data_dir <- "data"
    if (dir.exists(data_dir)) {
      story_folders <- list.dirs(data_dir, full.names = FALSE, recursive = FALSE)
      
      # Check which folders have both required files
      valid_stories <- sapply(story_folders, function(folder) {
        story_file <- file.exists(file.path(data_dir, folder, paste0(folder, " - story sheet.csv")))
        skill_file <- file.exists(file.path(data_dir, folder, paste0(folder, " - skill sheet.csv")))
        story_file && skill_file
      })
      
      rv$availableStories <- story_folders[valid_stories]
    }
  })
  
  # Story selection UI
  output$storySelect <- renderUI({
    req(rv$availableStories)
    
    if (length(rv$availableStories) == 0) {
      return(div(
        class = "alert alert-warning",
        "No valid stories found in the 'data' folder.",
        br(),
        "Each story needs a folder with:",
        br(),
        "- [Story Name] - story sheet.csv",
        br(),
        "- [Story Name] - skill sheet.csv"
      ))
    }
    
    choices <- setNames(rv$availableStories, rv$availableStories)
    
    selectInput("storyChoice", "Select a Story:", choices = choices)
  })
  
  output$storySelected <- reactive({
    rv$storySelected
  })
  outputOptions(output, "storySelected", suspendWhenHidden = FALSE)
  
  # Select story and load data
  observeEvent(input$selectStory, {
    req(input$storyChoice)
    
    story_name <- input$storyChoice
    story_path <- file.path("data", story_name, paste0(story_name, " - story sheet.csv"))
    skill_path <- file.path("data", story_name, paste0(story_name, " - skill sheet.csv"))
    
    rv$storyData <- read_csv(story_path, show_col_types = FALSE)
    rv$characterData <- read_csv(skill_path, show_col_types = FALSE)
    rv$selectedStory <- story_name
    rv$storySelected <- TRUE
  })
  
  # Back to story selection
  observeEvent(input$backToStory, {
    rv$storySelected <- FALSE
    rv$storyData <- NULL
    rv$characterData <- NULL
    rv$selectedCharacter <- NULL
  })
  
  # Character selection UI
  output$characterSelect <- renderUI({
    req(rv$characterData)
    
    choices <- setNames(
      seq_len(nrow(rv$characterData)),
      rv$characterData$Character
    )
    
    tagList(
      selectInput("characterChoice", "Select Character:", 
                  choices = choices),
      uiOutput("characterPreview")
    )
  })
  
  output$characterPreview <- renderUI({
    req(input$characterChoice, rv$characterData, rv$selectedStory)
    char <- rv$characterData[as.numeric(input$characterChoice), ]
    
    # Look for character sprite
    sprite_path <- file.path("data", rv$selectedStory, paste0(char$Character, ".png"))
    
    div(
      style = "background-color: #f0f0f0; padding: 10px; border-radius: 5px; margin-top: 10px;",
      if (file.exists(sprite_path)) {
        div(
          style = "text-align: center; margin-bottom: 10px;",
          img(src = paste0("data/", rv$selectedStory, "/", char$Character, ".png"),
              style = "max-width: 150px; max-height: 150px; border-radius: 5px;",
              alt = char$Character)
        )
      },
      h5(char$Character),
      p(char$Description)
    )
  })
  
  # Start game
  observeEvent(input$startGame, {
    req(input$characterChoice, rv$characterData, rv$storyData)
    
    rv$selectedCharacter <- rv$characterData[as.numeric(input$characterChoice), ]
    rv$gameStarted <- TRUE
    rv$currentRow <- 1
    rv$storyHistory <- list()
    rv$responses <- character(nrow(rv$storyData))
  })
  
  output$gameStarted <- reactive({
    rv$gameStarted
  })
  outputOptions(output, "gameStarted", suspendWhenHidden = FALSE)
  
  # Character info display
  output$characterInfo <- renderUI({
    req(rv$selectedCharacter, rv$selectedStory)
    
    # Look for character sprite
    sprite_path <- file.path("data", rv$selectedStory, paste0(rv$selectedCharacter$Character, ".png"))
    
    div(
      if (file.exists(sprite_path)) {
        div(
          style = "text-align: center; margin-bottom: 10px;",
          img(src = paste0("data/", rv$selectedStory, "/", rv$selectedCharacter$Character, ".png"),
              style = "max-width: 120px; max-height: 120px; border-radius: 5px;",
              alt = rv$selectedCharacter$Character)
        )
      },
      h5(rv$selectedCharacter$Character),
      p(rv$selectedCharacter$Description),
      hr(),
      p(strong("Story: "), rv$selectedStory),
      p(strong("Progress: "), rv$currentRow, "/", nrow(rv$storyData))
    )
  })
  
  # Story display
  output$storyDisplay <- renderUI({
    req(rv$gameStarted, rv$storyData)
    
    elements <- list()
    
    # Show story history
    for (i in seq_along(rv$storyHistory)) {
      elements[[length(elements) + 1]] <- div(
        class = "story-text",
        p(strong("Challenge: "), rv$storyHistory[[i]]$prompt),
        p(strong("Your Action: "), em(rv$storyHistory[[i]]$action)),
        if (!is.null(rv$storyHistory[[i]]$response)) {
          div(
            class = "response-text",
            p(rv$storyHistory[[i]]$response)
          )
        }
      )
    }
    
    # Show current challenge
    if (rv$currentRow <= nrow(rv$storyData)) {
      elements[[length(elements) + 1]] <- div(
        class = "story-text",
        style = "border-left-color: #2196F3;",
        h4("Current Challenge:"),
        p(rv$storyData$Prompt[rv$currentRow])
      )
    } else {
      elements[[length(elements) + 1]] <- div(
        class = "story-text",
        style = "border-left-color: #FFD700;",
        h3("🎉 Adventure Complete!"),
        p("Well, that's all the story there is for now! Thanks for playing!")
      )
    }
    
    do.call(tagList, elements)
  })
  
  # Submit action
  observeEvent(input$submitAction, {
    req(input$userAction, nchar(trimws(input$userAction)) > 0)
    req(rv$currentRow <= nrow(rv$storyData)) 
    
    current_story <- rv$storyData[rv$currentRow, ]
    
    # Build context with full history including user actions
    character_prompt <- paste0(
      "The user has chosen the character: \n", 
      rv$selectedCharacter$Character, 
      "\n which has the characteristics of \n",
      rv$selectedCharacter$Description
    )
    
    # Build detailed context with all previous actions and outcomes
    history_text <- ""
    if (length(rv$storyHistory) > 0) {
      history_text <- paste(
        sapply(seq_along(rv$storyHistory), function(i) {
          h <- rv$storyHistory[[i]]
          paste0(
            "\n--- Previous Challenge ", i, " ---\n",
            "Challenge: ", h$prompt, "\n",
            "Player Action: ", h$action, "\n",
            "Outcome: ", h$response
          )
        }),
        collapse = "\n"
      )
    }
    
    context <- paste(
      character_prompt,
      history_text,
      "\n--- Current Challenge ---\n",
      "Challenge: ", current_story$Prompt,
      collapse = "\n"
    )
    
    goal <- current_story$Goal
    relevant_skill <- current_story$Relevant_skill
    relevant_skill_level <- rv$selectedCharacter[[relevant_skill]] %>% as.numeric()
    
    showModal(modalDialog(
      title = "Thinking...",
      "The game is evaluating your action...",
      footer = NULL
    ))
    
    # Evaluate input
    input_value <- input_evaluation_function(
      goal = goal,
      context = context,
      user_input = input$userAction
    )
    
    random_value <- sample(1:10, 1)
    success <- ((input_value + relevant_skill_level + random_value) >= current_story$Difficulty)
    
    # Generate response
    response <- response_generate_function(
      goal = goal,
      context = context,
      user_input = input$userAction,
      success = success
    )
    
    removeModal()
    
    # Build result message
    result_msg <- paste0(
      ifelse(success, "✓ Success! ", "✗ Failed. "),
      "Your idea: ", input_value, "/10, ",
      "Skill (", relevant_skill, "): ", relevant_skill_level, ", ",
      "Luck: ", random_value, "/10"
    )
    
    showModal(modalDialog(
      title = ifelse(success, "Success!", "Not Quite..."),
      div(
        class = ifelse(success, "success", "failure"),
        p(result_msg)
      ),
      div(class = "response-text", p(response)),
      footer = modalButton("Continue"),
      easyClose = TRUE
    ))
    
    # Update history with the action included
    rv$storyHistory[[rv$currentRow]] <- list(
      prompt = current_story$Prompt,
      action = input$userAction,
      response = paste0(result_msg, "\n", response),
      success = success
    )
    
    # If successful, move to next challenge
    if (success) {
      rv$currentRow <- rv$currentRow + 1
      updateTextInput(session, "userAction", value = "")
    }
  })
  
  # Skip challenge
  observeEvent(input$skipChallenge, {
    req(rv$currentRow <= nrow(rv$storyData))
    
    rv$storyHistory[[rv$currentRow]] <- list(
      prompt = rv$storyData$Prompt[rv$currentRow],
      action = "(skipped)",
      response = "(Skipped)",
      success = TRUE
    )
    
    rv$currentRow <- rv$currentRow + 1
    updateTextInput(session, "userAction", value = "")
  })
  
  # Quit/Reset game
  observeEvent(input$quitGame, {
    rv$gameStarted <- FALSE
    rv$storySelected <- FALSE
    rv$currentRow <- 1
    rv$storyHistory <- list()
    updateTextInput(session, "userAction", value = "")
  })
  
  observeEvent(input$resetGame, {
    rv$gameStarted <- FALSE
    rv$storySelected <- FALSE
    rv$currentRow <- 1
    rv$storyHistory <- list()
    updateTextInput(session, "userAction", value = "")
  })
}

# Run App -----------------------------------------------------------------

# Enable serving of files from the data directory
shinyApp(
  ui = ui, 
  server = server,
  options = list(
    shiny.maxRequestSize = 30*1024^2  # Allow up to 30MB for images
  )
)