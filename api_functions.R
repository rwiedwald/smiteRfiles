library(tidyverse)
library(digest)
library(httr)
library(jsonlite)
library(lubridate)

# save token and dev_id in a config file (YAML or ini)

#### Utility Functions ####
# 1) composeRequestString is called and the API requirements are satisfied
# 2) determineParams is called inside composeRequestString and uses the endpoint to determine how to structure
#    the request string
# 3) sendRequest is called with the request string (here we can monitor the API better)

# sendRequest <- function(request_string) {
#   
#   response <- fromJSON(request_string)
#   
#   return(response)
# }
# 

# determineParams(endpoint) {
#   return(NULL)
# }

# composeRequestString(dev_id, token, endpoint) {
# 
#   time_stamp <- getTimestamp()
#   signature <- getSignature(dev_id, endpoint, token, time_stamp)
#   base_url <- "http://api.smitegame.com/smiteapi.svc"
# 
#   method_string <- ""
#   
#   return(method_string)
# }

getTimestamp <- function() {
  Year <- year(as.character(Sys.Date()))
  Month <- month(as.character(Sys.Date()))
  Month <- ifelse(nchar(Month) == 1, paste0("0", Month), Month)
  Hour <- as.character((as.numeric(hour(as.character(Sys.time()))) + 4) %% 24) # change 4 to 5 when DST starts
  Hour <- ifelse(nchar(Hour) == 1, paste0("0",Hour), Hour)
  Day <- day(as.character(Sys.Date()))
  Day <- ifelse(Hour %in% c("00", "01", "02", "03") & nchar(Day) == 1, # add 04 when DST starts
                paste0("0", as.character(as.numeric(Day) + 1)), 
                ifelse(Hour %in% c("00", "01", "02", "03") & nchar(Day) == 2, 
                       paste0(as.character(as.numeric(Day) + 1)), 
                       ifelse(nchar(Day) == 1, paste0("0", as.character(as.numeric(Day))), Day)))
  Min <- minute(as.character(Sys.time()))
  Min <- ifelse(nchar(Min) == 1, paste0("0",Min), Min)
  Sec <- second(as.character(Sys.time()))
  Sec <- ifelse(nchar(Sec) == 1, paste0("0", Sec), Sec)
  time_stamp <- paste0(Year, Month, Day, Hour, Min, Sec)
  
  return(time_stamp)
}

getSignature <- function (dev_id, endpoint, token, time_stamp) {
  
  input <- paste(dev_id, endpoint, token, time_stamp, sep ="")
  signature <- digest(input, algo="md5", serialize=FALSE)
  
  return(signature)
}

#### Connectivity, Development, and System Status ####
ping <- function(output_type = "json") {
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  response <- fromJSON(paste(base_url, paste0("ping", output_type), sep = "/"))
  
  return(response)
}

createSession <- function (token, dev_id, output_type = "json") {
  
  #token <- "A6F6D070221E4CAC8E360A02454CB8C3"
  #dev_id <- "2122"
  time_stamp <- getTimestamp()
  
  endpoint <- "createsession"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), dev_id, signature, time_stamp, sep = "/")
  
  response <- print(fromJSON(method_string))
  session <- response$session_id
  
  return(session)
}

testSession <- function(token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "testsession"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
}

getDataUsed <- function(token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getdataused"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

getHirezServerStatus <- function(token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "gethirezserverstatus"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

getPatchInfo <- function(token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getpatchinfo"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

#### Gods and Items ####

getGods <- function(token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getgods"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, "1", sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

getGodSkins <- function(god_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getgodskins"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, god_id, "1", sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
}

getGodRecommendedItems <- function(god_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getgodrecommendeditems"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, god_id, "1", sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
}

getItems <- function(token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getitems"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, "1", sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

#### Players and Playerids ####

getPlayer <- function(player, token, dev_id, session, portal_id = "10", output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getplayer"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player, portal_id, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
}

#### Playerid Info ####

getPlayerAchievements <- function(player_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getplayerachievements"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player_id, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
}

getGodRanks <- function(player_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getgodranks"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player_id, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

getMatchHistory <- function(player_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getmatchhistory"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player_id, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
}

searchPlayers <- function(search_string, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "searchplayers"
  signature <- getsignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, search_string, sep = "/")
  response <- fromJSON(method_string)
  
  return(response)
  
}

getQueueStats <- function(queue, player_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getqueuestats"
  signature <- getsignature(dev_id, endpoint, token, time_stamp)
  
  queues <- c("445", "426", "448", "435", "466", "434", "459")
  names(queues) <- c("assault", "conquest", "joust", "arena", "clash", "motd", "siege")
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player_id, queues[queue], 
                         sep = "/")
  response <- fromJSON(method_string)
  
  return(response)
  
}

#### Match Info ####

getMatchIdsByQueue <- function(queue, token, dev_id, session, date, hour, minute = NULL, output_type = "json") {
  
  # acceptable hour values 0-23 and -1 (whole day)
  # date format needed yyyymmdd
  # minute comes in the form ",00" - ",50"
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getmatchidsbyqueue"
  signature <- getsignature(dev_id, endpoint, token, time_stamp)
  
  queues <- c("445", "426", "448", "435", "466", "434", "459")
  names(queues) <- c("assault", "conquest", "joust", "arena", "clash", "motd", "siege")
  
  if(!is.null(minute)) {
    minute <- paste0(",", minute)
  }
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, queues[queue], date, paste0(hour, minute), 
                         sep = "/")
  response <- fromJSON(method_string)
  
  return(response)
}

getMatchDetails <- function(match_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getmatchdetails"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, match_id, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
}

getMatchDetailsBatch <- function(match_id_vec, token, dev_id, session, output_type = "json") {
  
  # please limit csv paramater to 5-10 matches
  stopifnot(length(match_id_vec) <= 10)
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getmatchdetailsbatch"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  csv_match_id <- paste(match_id_vec, collapse = ",")
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, csv_match_id, sep = "/")
  print(method_string)
  response <- fromJSON(method_string)
  
  return(response)
  
}

#### Leagues, Seasons, and Rounds ####
#### Team Info ####

searchTeams <- function(search_string, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "searchteams"
  signature <- getsignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, search_string, sep = "/")
  response <- fromJSON(method_string)
  
  return(response)
}

getTeamDetails <- function(clan_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getteamdetails"
  signature <- getsignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, clan_id, sep = "/")
  response <- fromJSON(method_string)
  
  return(response)
  
}

getTeamPlayers <- function(clan_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getteamplayers"
  signature <- getsignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, clan_id, sep = "/")
  response <- fromJSON(method_string)
  
  return(response)
  
}

#### Other ####

getGodLeaderboard <- function(queue, god_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getgodleaderboard"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  queues <- c("445", "426", "448", "435", "466", "434", "459")
  names(queues) <- c("assault", "conquest", "joust", "arena", "clash", "motd", "siege")
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, god_id, queues[queue], sep = "/")
  print(method_string)
  response <- fromJSON(method_string)
  
  return(response)
  
}

getPlayerStatus <- function(player_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getplayerstatus"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player_id, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

getFriends <- function(player_id, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getfriends"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player_id, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

getPlayerIdByName <- function(player, token, dev_id, session, output_type = "json") {
  
  time_stamp <- getTimestamp()
  
  endpoint <- "getplayeridbyname"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), 
                         dev_id, signature, session, time_stamp, player, sep = "/")
  
  response <- fromJSON(method_string)
  
  return(response)
  
}

getMatchPlayerDetails <- function() {
  return(NULL)
}

getTopMatches <- function() {
  return(NULL)
}

getLeagueLeaderboard <- function() {
  return(NULL)
}

getLeagueSeasons <- function() {
  return(NULL)
}

getMotd <- function() {
  return(NULL)
}

getEsportsProLeagueDetails <- function() {
  return(NULL)
}