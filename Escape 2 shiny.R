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

story_data <- read_csv("data/Animal Heist/Animal Heist - story sheet.csv")
Animal_Heist_skill_sheet <- read_csv("data/Animal Heist/Animal Heist - skill sheet.csv")


# Try to build a chat story bot -------------------------------------------

story_script <- read_csv("data/Animal Heist/Animal Heist - story sheet.csv")

character <- read_csv("data/Animal Heist/Animal Heist - skill sheet.csv")

library(shiny)
library(coro)
library(bslib)
library(shinychat)

# Dumbest chatbot in the world: ignores user input and chooses
# a random, vague response.


play_game <- function(story_data = Animal_Heist_story_sheet,
                      character_data = Animal_Heist_skill_sheet){
  yield("Choose your character!")
  await(async_sleep(0.02))
  
  yield(character_data$Character)
  await(async_sleep(0.02))
  
  
  character_choice_number <- input$chat_user_input
  
  
  character_data_selected <- character_data[character_choice_number,]
  
  
  character_prompt <- paste0("The user has chosen the character :", 
                             character_data_selected$Character, 
                             " which has the characteristics of ",
                             character_data_selected$Description)
  
  
  for(i in 1:nrow(story_data)){
    current_row <- i
    yield(story_data$Prompt[i])
    await(async_sleep(0.02))
    
    
    story_data <- story_data %>%
      mutate(responses = "")
    
    repeat{
      user_input <- input$chat_user_input
      
      if (tolower(input$chat_user_input) == "s") {
        cat("\nMoving on!\n")
        break
      }
      
      if (tolower(input$chat_user_input) == "q") {
        cat("\nGoodbye adventurer!\n")
        break
      }
      
      context <- paste(character_prompt,
                       story_data %>%
                         head(i) %>%
                         select(`Prompt`,responses) %>%
                         unlist,
                       collapse = ""
      ) 
      
      goal <- story_data$Goal[i]
      
      relevant_skill = story_data$Relevant_skill[i]
      
      relevant_skill_level = character_data_selected %>%
        select(all_of(relevant_skill)) %>%
        as.numeric()
      
      yield("...thinking...")
      await(async_sleep(0.02))
      
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
      
      # Show model explanation
      
      print(response)
      
      # If successful, break repeat-loop and move to next story row
      if (isTRUE(success)) {
        cat("\n✓ Success!\n")
        story_data$responses[i] <- paste0(story_data$responses[i],response,sep="/n")
        break
      }
      
      # Otherwise keep looping with the same story row
      cat("\n✗ Not successful. Try again.\n")
    }
    
    if (tolower(user_input) == "q") {
      cat("\nEnding adventure.\n")
      break
    }
    
    if(i == nrow(story_data)){
      yield("Well, that's all the story there is for now!")
    }
  }
   
}




ui <- page_fillable(
  chat_ui("chat", fill = TRUE)
)

server <- function(input, output, session) {
  
  game_stage <- reactiveVal("gameplay")
  character_choice <- reactiveVal(1)
  script_row <- reactiveVal(0)
  context <- reactiveVal(base_prompt)
  
  chat <- ellmer::chat_ollama(model = "gemma3")
  
    # 
    # if(game_stage()=="character_select"){
    # chat_append("chat", "Choose Your Character\n\n Input numbers 1-5 to select.") # delivers verbatim output
    # chat_append("chat",paste0(seq_along(Animal_Heist_skill_sheet$Character),
    #                           ") ",
    #                           Animal_Heist_skill_sheet$Character, collapse = "\n"))
    # 
    # # update game state
    # character_choice(as.numeric(input$chat_user_input))
    # game_stage("gameplay")
    # # add the character description to context
    # context(
    #   paste0(context(),
    #          Animal_Heist_skill_sheet$Description[character_choice()]
    #                )
    #         )
    # 
    # # Print updated state to server
    # print(character_choice())
    # print(game_stage())
    # print(context())
    # } #/ if character select

  observeEvent(input$chat_user_input, {
    if(game_stage()=="gameplay") {
      row_number <- (script_row() %>% as.numeric()) + 1
    stream <- chat$stream_async(input$chat_user_input)
        print(paste0(story_data$Prompt[row_number]
                     )
              )
     chat_append("chat", paste0(story_data$Prompt[script_row()])) # delivers verbatim output
     chat_append("chat", stream) # delivers chat output.
      
      #read the user prompt
      # evaluate the user prompt
      # give the text based user prompt evaluation
      # give the qualitative user prompt evaluation
      # script row + 1
      # Check to see if we're at the last row of the script, if so then change game_stage("wrap up")
     
     context(
          paste0(context(),
                 story_data$Prompt[script_row()],
                 "And the user said: ",
                 input$chat_user_input
          
                
                       )
                )
     
     goal <- story_data$Goal[i]
     
     relevant_skill = story_data$Relevant_skill[i]
     
     relevant_skill_level = character_data_selected %>%
       select(all_of(relevant_skill)) %>%
       as.numeric()
     
     yield("...thinking...")
     await(async_sleep(0.02))
     
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
     
     # Show model explanation
     
     print(response)
    }
    
    
  })
  
  # observe event (skip button)
}

shinyApp(ui, server)

