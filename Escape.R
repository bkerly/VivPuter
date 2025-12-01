library(tidyverse)
library(ollamar)

# Create query ------------------------------------------------------------


base_prompt <- 
  'Background:
  
  Please assume the role of a fun and silly text based adventure prompt. Keep the tone consistent with the context_input so far. Keep it appropriate for a 6 year old user.

---
  
  You will need to respond to context and user inputs and return ONLY the following fields:

---
  
"success_TF" = Based on the context_input, and user_input was the action successful in achieving goal_input? Answer only with true (for successful) or false (for unsuccessful) (lowercase, no quotes).

"response" =  Explain why the action was or was not successful. Be creative why something might work (even if it is a strange choice), and bias toward unexpected success!
'


format <- list(
  type = "object",
  properties = list(
    success_TF = list(type = "boolean"),
    response = list(type = "string")
    
  ),
  required = list("success_TF", "response")
)


LLM_process_situation <- function(goal = "Remove the obsticle", 
                                  context = "There is an obsticle in the way", 
                                  user_input = "Hit it real hard."){
  full_prompt <-
    paste0(
      base_prompt,
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
                   format = format,
                   full_prompt,
                   output = "text"
  )
  
  return(resp)
}


# trial 1-------------------------------------------------------------------
TEST_goal <- "The action is successful if the door is destroyed."
TEST_context <- "The user is facing a door. The door visually appears to be a solid door made of wood, but it is actally paper and tears easily. It won't tear if you just look at it."
TEST_user_input_1 <- "Look at the door."

LLM_process_situation(TEST_goal,
                      TEST_context,
                      TEST_user_input_1)

TEST_user_input_2 <- "Push on the door."

LLM_process_situation(TEST_goal,
                      TEST_context,
                      TEST_user_input_2)


# Set up an easy way to create a linear narrative. ------------------------

mermaid_story <- read_csv("data/mermaid_text_adventure - Sheet1.csv")

unicorn_story <- read_csv("data/unicorn_text_adventure - Sheet1.csv")


play_game <- function(story_data = mermaid_story){
  for(i in 1:nrow(story_data)){
    current_row <- i
    print(story_data$`Story text`[i])
    
    story_data <- story_data %>%
      mutate(responses = "")
    
    repeat{
      user_input <- readline("\nWhat do you do? (or type 's' to skip or 'q' to quit): ")
      
      if (tolower(user_input) == "s") {
        cat("\nMoving on!\n")
        break
      }
      
      if (tolower(user_input) == "q") {
        cat("\nGoodbye adventurer!\n")
        break
      }
      
      context <- paste(
        story_data %>%
          head(i) %>%
          select(`Story text`,responses) %>%
          unlist,
        collapse = ""
      ) 
      
      goal <- story_data$Goal[i]
      
      print("...thinking...")
      result <- LLM_process_situation(goal,
                                      context,
                                      user_input) %>%
        jsonlite::fromJSON()
      
      # Show model explanation
      story_data$responses[i] <- paste0(story_data$responses[i],result$response,sep="/n")
      
      print(result$response)
      
      # If successful, break repeat-loop and move to next story row
      if (isTRUE(result$success_TF)) {
        cat("\n✓ Success! Moving on.\n")
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
      print("Well, that's all the story there is for now!")
    }
  }
}


# Let's Play --------------------------------------------------------------

play_game()

play_game(story_data = unicorn_story)

