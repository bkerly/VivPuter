library(tidyverse)
library(ollamar)

# Create query ------------------------------------------------------------


base_prompt <- 
  'Background:
  
  Please assume the role of a fun and silly text based adventure prompt. Keep the tone consistent with the context_input so far. Keep it appropriate for a 6 year old user. You will need to respond to context.'



# Numerical Evaluation of Likelihood of Success ---------------------------



input_evaluation <-
  'Based on the context, user input, and goal, evaluate how likely it is that the user input would achieve the goal on a scale of 1 to 10. One is something that is very unlikely to wReturn only a numeric response between 1 and 10.'

input_evaluation_format <- list(
    type = "object",
    properties = list(
      input_evaluation_numeric = list(type = "integer")
      
    ),
    required = list("input_evaluation_numeric")
  )


input_evaluation_function <- function(goal = "Light the rocket fuse and get away without being blown up before it explodes.", 
                                  context = "There is a huge moon rocket. You want to launch it to the moon. It has a fuse. You need to light 
                                  the fuse and get away before it launches, otherwise you'll be blown up.", 
                                  user_input = "I get on a vespa and drive past the rocket real fast and light the fuse with a flame thrower as I drive away.",
                                  challenge_difficulty = 15,
                                  relevant_skill = 2){
  
  full_prompt <-
    paste0(
      base_prompt,
      '
       ---
    
      Instructions = 
      
      ',
      input_evaluation,
      '
    ---
    
    goal_input =
    ',
      goal,
      '
    ---
    
    context_input =
    ',
      context,
      '
    ---
    
    user_input =
    ', 
      user_input
    )
  

  resp <- generate("llama3.2",
                   format = input_evaluation_format,
                   full_prompt,
                   output = "text"
  )
  
  input_value <- jsonlite::fromJSON(resp) %>%
    as.numeric()
  
  random_value <- sample(1:20,1)
  
  (input_value + relevant_skill + random_value) >= challenge_difficulty
  
}

# Success or Failure Response ---------------------------------------------


success_response_prompt <- 
  'Based on the context, character, and user input, explain how it was that the character was successful in achieving the goal. Be creative and silly in explaining how the action was successful. Only consider the most recent goal. Return only a string.'

failure_response_prompt <- 
  'Based on the context, character, and user input, explain how it was that the character was successful in achieving the goal. Be creative and silly in explaining how the action was not successful. Only consider the most recent goal. Return only a string.'

success_failure_format <- list(
  type = "object",
  properties = list(
    success_failure_string = list(type = "string")
    
  ),
  required = list("success_failure_string")
)


response_generate_function <- function(goal = "Light the rocket fuse and get away without being blown up before it explodes.", 
                                context = "There is a huge moon rocket. You want to launch it to the moon. It has a fuse. You need to light 
                                  the fuse and get away before it launches, otherwise you'll be blown up.", 
                                user_input = "I get on a vespa and drive past the rocket real fast and light the fuse with a flame thrower as I drive away.",
                                success = FALSE){
  
  full_prompt <-
    paste0(
      base_prompt,
      '
       ---
    
      Instructions = 
      
      ',
      ifelse(success,success_response_prompt,failure_response_prompt),
      '
    ---
    
    goal_input =
    ',
      goal,
      '
    ---
    
    context_input =
    ',
      context,
      '
    ---
    
    user_input =
    ', 
      user_input
    )
  
  
  resp <- generate("llama3.2",
                   format = success_failure_format,
                   full_prompt,
                   output = "text"
  ) %>%
    jsonlite::fromJSON() %>%
    as.character()
  
  
  return(resp)
  
}


# Set up an easy way to create a linear narrative. ------------------------

library(shiny)
library(tidyverse)
library(jsonlite)

# Load story files --------------------------------------------------------------

Animal_Heist_story_sheet <- read_csv("data/Animal Heist/Animal Heist - story sheet.csv")
Animal_Heist_skill_sheet <- read_csv("data/Animal Heist/Animal Heist - skill sheet.csv")

stories <- list(
  "Animal Heist" = Animal_Heist_story_sheet
)

characters <- Animal_Heist_skill_sheet


# UI ---------------------------------------------------------------------------

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      .story-text {
        font-family: 'Georgia', serif;
        font-size: 20px;
        color: #333;
        margin-bottom: 10px;
      }
      .user-text {
        font-family: 'Courier New', monospace;
        font-size: 18px;
        color: #004080;
        margin-left: 20px;
        margin-bottom: 10px;
      }
      .response-text {
        font-family: 'Arial', sans-serif;
        font-size: 18px;
        color: #008000;
        margin-left: 20px;
        margin-bottom: 15px;
      }
      .chat-box {
        background-color: #f7f7f7;
        border-radius: 12px;
        padding: 15px;
        max-height: 600px;
        overflow-y: auto;
      }
    "))
  ),
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      
      selectInput("story_choice", "Choose your story:",
                  choices = names(stories)),
      
      selectInput("character_choice", "Choose your character:",
                  choices = characters$Character),
      
      uiOutput("character_sprite"),
      
      textInput("user_input", "What do you do?", ""),
      actionButton("submit", "Submit"),
      actionButton("skip", "Skip"),
      actionButton("quit", "Quit")
      
    ),
    
    mainPanel(
      div(class = "chat-box",
          uiOutput("chat_ui")
      )
    )
  )
)


# Server -----------------------------------------------------------------------

server <- function(input, output, session){
  
  rv <- reactiveValues(
    story = NULL,
    character = NULL,
    character_data = NULL,
    row = 1,
    chat = list(),
    character_prompt = ""
  )
  
  # Update story ---------------------------------------------------------------
  observeEvent(input$story_choice, {
    rv$story <- stories[[input$story_choice]]
    rv$row <- 1
    rv$chat <- list()
  })
  
  # Update character -----------------------------------------------------------
  observeEvent(input$character_choice, {
    rv$character <- input$character_choice
    rv$character_data <- characters %>% filter(Character == rv$character)
    
    rv$character_prompt <- paste0(
      "The user has chosen the character ", rv$character,
      " which has the characteristics of ",
      rv$character_data$Description
    )
  })
  
  # Character sprite -----------------------------------------------------------
  output$character_sprite <- renderUI({
    req(rv$character)
    img_src <- paste0("characters/", rv$character, ".png")
    
    if (file.exists(file.path("www", img_src))) {
      tags$img(src = img_src, width = "100%")
    } else {
      tags$p("No sprite found.")
    }
  })
  
  # Chat UI renderer ------------------------------------------------------------
  output$chat_ui <- renderUI({
    lapply(rv$chat, function(msg){
      if (msg$type == "story") {
        div(class = "story-text", msg$text)
      } else if (msg$type == "user") {
        div(class = "user-text", paste0("> ", msg$text))
      } else {
        div(class = "response-text", msg$text)
      }
    })
  })
  
  # Game logic -----------------------------------------------------------------
  observeEvent(input$submit, {
    req(rv$story)
    
    story_data <- rv$story
    i <- rv$row
    
    # Add story text if newly reached
    if (length(rv$chat) == 0 || rv$chat[[length(rv$chat)]]$text != story_data$Prompt[i]) {
      rv$chat <- append(rv$chat, list(list(type = "story", text = story_data$Prompt[i])))
    }
    
    user_input <- input$user_input
    rv$chat <- append(rv$chat, list(list(type = "user", text = user_input)))
    
    context <- paste(
      rv$character_prompt,
      sapply(rv$chat, function(x) x$text),
      collapse = "\n"
    )
    
    goal <- story_data$Goal[i]
    relevant_skill <- story_data$Relevant_skill[i]
    relevant_skill_level <- rv$character_data %>%
      pull(all_of(relevant_skill)) %>%
      as.numeric()
    
    success <- input_evaluation_function(
      goal = goal,
      context = context,
      user_input = user_input,
      challenge_difficulty = story_data$Difficulty[i],
      relevant_skill = relevant_skill_level
    )
    
    response <- response_generate_function(
      goal = goal,
      context = context,
      user_input = user_input,
      success = success
    )
    
    rv$chat <- append(rv$chat, list(list(type = "response", text = response)))
    
    if (success) {
      rv$row <- rv$row + 1
      
      if (rv$row > nrow(rv$story)) {
        rv$chat <- append(rv$chat, list(list(type = "story", text = "The End.")))
      } else {
        rv$chat <- append(rv$chat, list(list(type = "story", text = rv$story$Prompt[rv$row])))
      }
    }
    
    updateTextInput(session, "user_input", value = "")
  })
  
  # Skip button -----------------------------------------------------------------
  observeEvent(input$skip, {
    rv$row <- rv$row + 1
    rv$chat <- append(rv$chat, list(list(type = "story", text = rv$story$Prompt[rv$row])))
  })
  
  # Quit button -----------------------------------------------------------------
  observeEvent(input$quit, {
    rv$chat <- append(rv$chat, list(list(type = "story", text = "Goodbye adventurer!")))
  })
  
}

# Run app ----------------------------------------------------------------------

shinyApp(ui, server)



# Let's Play --------------------------------------------------------------

play_game()

play_game(story_data = unicorn_story)

