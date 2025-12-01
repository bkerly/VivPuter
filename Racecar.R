### --- Hex Racing Game (Console Version) ---

# Create a map
create_map <- function() {
  terrains <- c("road", "offroad", "mountain")
  map <- matrix(sample(terrains, 64, replace = TRUE), nrow = 8, ncol = 8)
  map
}

# Car options
car_list <- list(
  SUV = list(
    name = "SUV",
    icon = "🚙",
    move_cost = c(road = 1, offroad = 1, mountain = 2)
  ),
  Sport = list(
    name = "Sports Car",
    icon = "🚗",
    move_cost = c(road = 1, offroad = 2, mountain = 3)
  )
)

# Initial positions
place_goal <- function(map){
  list(row = nrow(map), col = ncol(map), icon = "🍦")
}

place_car <- function(){
  list(row = 1, col = 1)
}

# Print map with car + goal
print_map <- function(map, car_pos, car_icon, goal_pos) {
  out <- matrix("", nrow(map), ncol(map))
  for (i in 1:nrow(map)) {
    for (j in 1:ncol(map)) {
      out[i, j] <- substr(map[i, j], 1, 1)  # R, O, or M
    }
  }
  out[goal_pos$row, goal_pos$col] <- goal_pos$icon
  out[car_pos$row, car_pos$col] <- car_icon
  print(out, quote = FALSE)
}

# Movement
move_car <- function(direction, pos, map) {
  r <- pos$row
  c <- pos$col
  
  if (direction == "N") r <- r - 1
  if (direction == "S") r <- r + 1
  if (direction == "W") c <- c - 1
  if (direction == "E") c <- c + 1
  
  # Check boundaries
  if (r < 1 || r > nrow(map) || c < 1 || c > ncol(map)) {
    message("Cannot move there — boundary.")
    return(pos)
  }
  
  list(row = r, col = c)
}

# Main play function
play_hex_race <- function() {
  
  # Select car
  cat("Choose your car:\n")
  cat("1 = SUV\n")
  cat("2 = Sports Car\n")
  choice <- readline("Enter 1 or 2: ")
  
  if (choice == "1") car <- car_list$SUV
  else if (choice == "2") car <- car_list$Sport
  else { cat("Invalid, defaulting to SUV.\n"); car <- car_list$SUV }
  
  map <- create_map()
  goal <- place_goal(map)
  car_pos <- place_car()
  score <- 0
  
  repeat {
    cat("\nCurrent score:", score, "\n")
    print_map(map, car_pos, car$icon, goal)
    
    cat("\nWhat would you like to do?\n")
    cat("     N\n")
    cat("   W + E\n")
    cat("     S\n\n")
    cat("Q = quit\n\n")
    
    move <- toupper(readline("Enter direction: "))
    if (move == "Q") {
      cat("Game ended.\n")
      break
    }
    
    if (!(move %in% c("N","S","W","E"))) {
      cat("Invalid input.\n")
      next
    }
    
    new_pos <- move_car(move, car_pos, map)
    terrain <- map[new_pos$row, new_pos$col]
    
    score <- score + car$move_cost[[terrain]]
    car_pos <- new_pos
    
    # Check for victory
    if (car_pos$row == goal$row && car_pos$col == goal$col) {
      cat("\nYou reached the ice cream store!\n")
      cat("Final score:", score, "\n")
      break
    }
  }
} 

# To play:
 play_hex_race()
