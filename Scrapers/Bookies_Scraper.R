#Script that scrapes sportsbet data


sportsbet_eventid_extractor <- function() {
  
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  
  # Set URL that get AFL event-ids
  # Shows 2 weeks worth of games
  url = "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Competitions/4165?displayType=default&eventFilter=matches&includeAllEvents=true"
  
  # Call GET request from httr 
  res <- GET(
    url,
    add_headers(
      `User-Agent` = "Mozilla/5.0",
      `Accept` = "application/json",
      `Referer` = "https://www.sportsbet.com.au/"
    )
  )
  
  df <- fromJSON(content(res, "text"), flatten = TRUE) 
  
  event_df <- df$events %>%
    as_tibble() %>%
    dplyr::select(
      event_id = id,
      event_name = name,
      start_time = startTime
    )
  
  return(event_df)
}


sportsbet_Scraper <- function(event_id) {
  
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(stringr)
  
  
  url <- paste0(
    "https://www.sportsbet.com.au/apigw/sportsbook-sports/Sportsbook/Sports/Events/",
    event_id,
    "/Markets"
  )
  
  res <- GET(
    url,
    add_headers(
      `User-Agent` = "Mozilla/5.0",
      `Accept` = "application/json",
      `Referer` = "https://www.sportsbet.com.au/",
      `apptoken` = "cxp-desktop-web",
      `channel` = "cxp"
    )
  )
  
  # Add a small delay inside the function to avoid being blocked
  Sys.sleep(1)
  
  df <- fromJSON(content(res, "text"), flatten = TRUE)
  
  return(df)
}

#event_ids <- sportsbet_eventid_extractor()
#all_markets <-bind_rows(map(event_ids$event_id, sportsbet_Scraper))
