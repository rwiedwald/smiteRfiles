library(tidyverse)
library(digest)
library(httr)
library(jsonlite)
library(lubridate)

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

createSession <- function (token, dev_id, output_type = "json") {
  
  token <- token #"A6F6D070221E4CAC8E360A02454CB8C3"
  dev_id <- dev_id #"2122"
  time_stamp <- getTimestamp()
  
  endpoint <- "createsession"
  signature <- getSignature(dev_id, endpoint, token, time_stamp)
  
  
  base_url <- "http://api.smitegame.com/smiteapi.svc"
  method_string <- paste(base_url, paste0(endpoint, output_type), dev_id, signature, time_stamp, sep = "/")
  
  response <- print(fromJSON(method_string))
  session <- response$session_id
  
  return(session)
}

getPlayer <- function(token, dev_id, session) {
  
}