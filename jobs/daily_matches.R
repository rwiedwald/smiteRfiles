library(optparse)

source("api_functions.R")
source("api_credentials.R")

fetch_queue <- "joust" # optparse this
date <- Sys.Date() - 2 # yesterday

session <- createSession(token, dev_id)

# Get the match ids for the entire
match_ids <- getMatchIdsByQueue(fetch_queue, token, dev_id, session, date, -1)
match_vec <- match_ids$Match

# detemine the number of groups to have requests of 10 row dataframes
num_factors <- length(match_vec) %/% 10
remainder <- length(match_vec) %% num_factors

# if there is a remainder, we need an extra group
if(remainder != 0) {
  num_factors <- num_factors + 1
}

# split into API permitted sizes
request_groups <- split(match_vec, (seq(length(match_vec))-1) %% num_factors)

# stop if a group has more than the 10 permitted csv values
stopifnot(max(sapply(request_groups, length)) <= 10)

# Check this after the details/ids are returned?
#nchar("Invalid session id")
#substr(invalid, 1, 18) == "Invalid session id"

# Using the match ids, get the match details for that day
match_details_list <- list()
for(i in seq_along(request_groups)) {
  print(paste(i, "of", length(request_groups)))
  
  group <- request_groups[`i`]
  match_id_vec <- unlist(group)

  match_details_inc <- getMatchDetailsBatch(match_id_vec, token, dev_id, session)

  match_details_list[[i]] <- match_details_inc
}

returned_df <- do.call(rbind, match_details_list)

# remove list columns?
out_df <- returned_df[,sapply(returned_df, typeof) != "list"]

write_csv(out_df, paste(fetch_queue, paste0(date, ".csv"), sep = "_"))
