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

# cf https://shiny.posit.co/blog/posts/shiny-side-of-llms-part-3/
# I think the play game function could be replaced by https://posit-dev.github.io/shinychat/r/index.html
library(shiny)
library(bslib)
library(ellmer)
library(shinychat)

ui <- page_fillable(
  ## General theme and styles
  theme = bs_theme(bootswatch = "flatly"),
  layout_sidebar(
    ## Sidebar content
    sidebar = sidebar(
      width = 400,
      # Open sidebar on mobile devices and show above content
      open = list(mobile = "always-above"),
      strong(p("Welcome To Your Adventure!")),
      p(
        "Select your game below, then choose your character!"
      ),
      selectInput(
        inputId = "story",
        label = "Choose which story you want to play",
        choices = c("Animal Heist", "Demon Hunters 2","A Most Dangerous Ruse")
      ),
      selectInput(
        inputId = "character",
        label = "Choose which character you want to play as:",
        choices = c("Rumi", "Mira","Zoey")
      ),
      #image(),
      textAreaInput(
        inputId = "audience",
        height = "150px",
        label = "Describe your audience",
        placeholder = "e.g. Python and R users who are curious about AI and large language models, but not all of them have a deep technical background"
      ),
      numericInput(
        inputId = "length",
        label = "Time cap for the presentation (minutes)",
        value = 10
      ),
      textInput(
        inputId = "type",
        label = "Type of talk",
        placeholder = "e.g. lightning talk, workshop, or keynote"
      ),
      textInput(
        inputId = "event",
        label = "Event name",
        placeholder = "e.g. posit::conf(2025)"
      ),
      input_task_button(
        id = "submit",
        label = shiny::tagList(
          bsicons::bs_icon("robot"),
          "Analyse presentation"
        ),
        label_busy = "DeckCheck is checking...",
        type = "default"
      )
    ),
    ## Main content
    layout_column_wrap(
      fill = FALSE,
      ### Value boxes for metrics
      value_box(
        title = "Showtime",
        value = "9 minutes",
        showcase = bsicons::bs_icon("file-slides"),
        theme = "primary"
      ),
      value_box(
        title = "Code Savviness",
        value = "15%",
        showcase = bsicons::bs_icon("file-code"),
        theme = "primary"
      ),
      value_box(
        title = "Image Presence",
        value = "7%",
        showcase = bsicons::bs_icon("file-image"),
        theme = "primary"
      )
    ),
    layout_column_wrap(
      fill = FALSE,
      width = 1 / 2,
      ### Graph with scoring metrics
      card(
        card_header(strong("Scores per category")),
        p("My beatiful interactive plot...")
      ),
      ### Table with suggested improvements
      card(
        card_header(strong("Suggested improvements per category")),
        p("My beatiful table...")
      )
    )
  )
)

server <- function(input, output, session) {}

shinyApp(ui, server)

# Let's Play --------------------------------------------------------------

play_game()

play_game(story_data = unicorn_story)

