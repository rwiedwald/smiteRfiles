base_url <- "http://api.smitegame.com/smiteapi.svc"

getTimestamp <- function() {
  Year <- year(as.character(Sys.Date()))
  Month <- month(as.character(Sys.Date()))
  Month <- ifelse(nchar(Month) == 1, paste0("0", Month), Month)
  Hour <- as.character((as.numeric(hour(as.character(Sys.time()))) + 4) %% 24) # change to 5 when DST starts
  Hour <- ifelse(nchar(Hour) == 1, paste0("0",Hour), Hour)
  Day <- day(as.character(Sys.Date()))
  Day <- ifelse(nchar(Day) == 1, paste0("0", Day), Day)
  Day <- ifelse(Hour %in% c("00", "01", "02", "03") & nchar(Day) == 1, # add 04 when DST starts
                paste0("0", as.character(as.numeric(Day) + 1)), 
                ifelse(Hour %in% c("00", "01", "02", "03") & nchar(Day) == 2, # add 04 when DST starts 
                       paste0(as.character(as.numeric(Day) + 1)), Day))
  Min <- minute(as.character(Sys.time()))
  Min <- ifelse(nchar(Min) == 1, paste0("0",Min), Min)
  Sec <- second(as.character(Sys.time()))
  Sec <- ifelse(nchar(Sec) == 1, paste0("0", Sec), Sec)
  time_stamp <- paste0(Year, Month, Day, Hour, Min, Sec)
  return(time_stamp)
}

getsignature <- function (devId, endpt, auth, ts) {
  library(digest)
  
  input <- paste(devId, endpt, auth, ts, sep ="")
  signature <- digest(input, algo="md5", serialize=FALSE)
  return(signature)
}

createSession <- function () {
  library(httr)
  library(jsonlite)
  library(lubridate)
  
  token <- "A6F6D070221E4CAC8E360A02454CB8C3"
  dev_id <- "2122"
  time_stamp <- getTimestamp()
  
  endpoint <- "createsession"
  
  signature <- getsignature(dev_id, endpoint, token, time_stamp)
  
  create_session <- paste(base_url, "createsessionJson", dev_id, signature, time_stamp, sep = "/")
  
  response <- print(fromJSON(create_session))
  session <- response$session_id
  
  api_info <- c(token, dev_id, time_stamp, signature, session, response)
  return(api_info)
}

info <- createSession()
token <- info[[1]]
dev_id <- info[[2]]
time_stamp <- info[[3]]
signature <- info[[4]]
session <- info[[5]]
response <- info[[6]]

#### Getting player name ####
endpoint <- "getplayer"
time_stamp <- getTimestamp()
signature <- getsignature(dev_id, endpoint, token, time_stamp)


get_player <- paste(base_url, "getplayerJson", dev_id, signature, session, time_stamp, "j0shwah6", sep = "/")
josh <- fromJSON(get_player)

get_player <- paste(base_url, "getplayerJson", dev_id, signature, session, time_stamp, "minecuber", sep = "/")
ryan <- fromJSON(get_player)
