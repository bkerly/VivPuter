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
                                  user_input = "I get on a vespa and drive past the rocket real fast and light the fuse with a flame thrower as I drive away."#,
                                 # challenge_difficulty = 15,
                                 # relevant_skill = 2
                                 ){
  
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
  

  resp <- generate("mistral",
                   format = input_evaluation_format,
                   full_prompt,
                   output = "text"
  )
  
   input_value <- jsonlite::fromJSON(resp) %>%
    as.numeric()
   
   return(input_value)
  # 
  # random_value <- sample(1:20,1)
  # 
  # (input_value + relevant_skill + random_value) >= challenge_difficulty
  
}

# Success or Failure Response ---------------------------------------------


success_response_prompt <- 
  'Based on incontrovertable math, the character achieved the goal! Based on the context, character, and user input, explain how it was that the character was successful in achieving the goal. 

Be creative and silly in explaining how the action was successful. Only consider the most recent goal. 

Return only a string.'

failure_response_prompt <- 
  'Based on incontrovertable math, the character failed to achieve the goal! Based on the context, character, and user input, explain how it was that the character was unsuccessful in achieving the goal. 

Be creative and silly in explaining how the action was not successful. Only consider the most recent goal. Return only a string.'

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
  
  
  resp <- generate("mistral",
                   format = success_failure_format,
                   full_prompt,
                   output = "text"
  ) %>%
    jsonlite::fromJSON() %>%
    as.character()
  
  
  return(resp)
  
}


# Set up an easy way to create a linear narrative. ------------------------

Animal_Heist_story_sheet <- read_csv("data/Animal Heist/Animal Heist - story sheet.csv")

Animal_Heist_skill_sheet <- read_csv("data/Animal Heist/Animal Heist - skill sheet.csv")



play_game <- function(story_data = Animal_Heist_story_sheet,
                      character_data = Animal_Heist_skill_sheet){
  
  print("Choose your character!")
  
  print(paste0(seq_along(Animal_Heist_skill_sheet$Character),
                               ") ",
                               Animal_Heist_skill_sheet$Character, collapse = ", "))
  
  
  
  character_choice_number <- readline(paste0(
                               "Enter 1 to ",nrow(character_data),": \n"
                               )
                               )
  
  character_data_selected <- character_data[character_choice_number,]

  character_prompt <- paste0("The user has chosen the character: \n", 
                             character_data_selected$Character, 
                             "\n which has the characteristics of \n",
                             character_data_selected$Description)
  
  print(character_prompt)
  
  
  for(i in 1:nrow(story_data)){
    current_row <- i
    print(story_data$Prompt[i])
    
    story_data <- story_data %>%
      mutate(responses = "")
    
    repeat{
      user_input <- readline("What do you do? (or type 's' to skip or 'q' to quit): \n")
      
      if (tolower(user_input) == "s") {
        cat("\nMoving on!\n")
        break
      }
      
      if (tolower(user_input) == "q") {
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
      
      print("...thinking...")
      input_value <- input_evaluation_function(
        goal = goal,
        context = context,
        user_input = user_input#,
        #challenge_difficulty = story_data$Difficulty[i],
       # relevant_skill = relevant_skill_level
      ) 
      
      #print(input_value)
    
      random_value <- sample(1:10,1)
      
      success <- ((input_value + relevant_skill_level + random_value) >= story_data$Difficulty[i])
      
      cat(
        paste0(
          "Your idea had strength ",input_value," out of 10.",
          " and your character's skill level was ",relevant_skill_level,
          " for the skill of ",relevant_skill,". ",
          " And you had luck of ",random_value," out of 10."
        )
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
      cat("Well, that's all the story there is for now!")
    }
  }
}


# Let's Play --------------------------------------------------------------

play_game()

#play_game(story_data = unicorn_story)

