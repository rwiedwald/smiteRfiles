# Random Generator Functions
library(tidyverse)

source("api_functions.R")
source("api_credentials.R")

session <- createSession(token, dev_id)

gods <- getGods(token, dev_id, session)

assignPlayers <- function(team_vec, player_vec, shuffle_players = TRUE) {
  # Utility function to assign players to specific gods when a player vector is provided
  
  # if a player vector is given, assign the players the gods
  if(length(player_vec) > 0) {
    if(shuffle_players) {
      names(team_vec) <- sample(player_vec)
    } else {
      names(team_vec) <- player_vec
    }
  }
  return(team_vec)
}

generateTeamRandomly <- function(god_df, n = 5, player_vec = c(), shuffle_players = TRUE) {
  
  index <- sample(1:nrow(god_df), n)
  god_vec <- god_df[index, "Name"]
  
  god_vec <- assignPlayers(god_vec, player_vec, shuffle_players)
  
  return(god_vec)
}

generateTeamBalanced <- function(god_df, n = 5, role_vec = c("Guardian", "Warrior", "Assassin", "Mage", "Hunter"), 
                                 player_vec = c(), shuffle_players = TRUE) {
  
  # Two cases can occur in this function
  # 1. a user provides roles and a number desired -- n gods are selected from the provided roles
  # 2. a user provides roles and players  -- length(players) are selected from the provided roles and assigned
  num_gods <- n
  if(length(player_vec) > 0) {
    num_gods <- length(player_vec)
  }
  
  roles <- god_df[,c("Roles", "Name")] %>%
    mutate(Roles = trimws(Roles, which = "left")) %>% # there is a leading space in the role column
    filter(Roles %in% role_vec) %>%
    sample_n(num_gods)

  god_vec <- roles[["Name"]] # df is a list, so this returns a vector
  god_vec <- assignPlayers(god_vec, player_vec, shuffle_players)
  
  return(god_vec) 
  
} 

generateChoice <- function(god_df, role_vec = c("Guardian", "Warrior", "Assassin", "Mage", "Hunter"), 
                               player_vec = c(), shuffle_players = TRUE) {
  
  god_df_copy <- god_df[,c("Roles", "Name")] %>%
    mutate(Roles = trimws(Roles, which = "left")) # there is a leading space in the role column
  god_vec <- c()
  i <- 1
  for(role in role_vec) {
    selection <- god_df_copy %>% 
      filter(Roles %in% role_vec[i]) %>%
      sample_n(1)
    
    god_vec <- c(god_vec, selection[["Name"]])
    
    god_df_copy <- god_df_copy %>%
      anti_join(selection, by = "Name")
    
    i <- i + 1
  }
  
  god_vec <- assignPlayers(god_vec, player_vec, shuffle_players)
  
  return(god_vec)
}

generateTeam <- function(god_df, option = "random", n = 5, 
                         role_vec = c("Guardian", "Warrior", "Assassin", "Mage", "Hunter"),
                         player_vec = c()) {
  
  if(option == "random") {
    # This option is good for completely random teams with no restrictions or assumptions
    team <- generateTeamRandomly(god_df, n = n, player_vec)
  } else if(option == "team_random") {
    # This option is good for setting specific roles you want to get (players can all be assigned the same role)
    # Not every role given will be necessarily assigned (given a vector of Guardian/Warrior, everyone could be 
    # a Warrior)
    team <- generateTeamBalanced(god_df, n = n, role_vec = role_vec, player_vec = player_vec)
  } else if(option == "team_choice") {
    # This option let's the user declare each individual role and, if applicable, each individual player
    # This does not allow for users to explictly declare their role, but rather designates the roles for the
    # entire team and randomly distributes them out
    team <- generateChoice(god_df, role_vec = role_vec, player_vec = player_vec)
  } else if(option == "player_choice") {
    # This option allows each user to declare a role they want to play. A random god is assigned to that player
    # from that role. With no player vector provided, this is equivalent to the custom option
    team <- generateChoice(god_df, role_vec = role_vec, player_vec = player_vec, shuffle_players = FALSE)
  }

  return(team)
}
