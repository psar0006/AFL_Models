pointsbet_scraper <- function() {
  
  library(httr)
  library(jsonlite)
  library(dplyr)
  library(tidyr)
  library(stringr)
  
  cat("Starting PointsBet scraper...\n")
  
  url <- "https://api.au.pointsbet.com/api/mes/v3/events/featured/competition/7523?page=1"
  
  res <- GET(
    url,
    add_headers(
      `User-Agent` = "Mozilla/5.0",
      `Accept` = "application/json, text/plain, */*",
      `Origin` = "https://pointsbet.com.au",
      `Referer` = "https://pointsbet.com.au/"
    )
  )
  
  cat("Status code:", status_code(res), "\n")
  
  raw <- content(res, "text", encoding = "UTF-8")
  data <- fromJSON(raw, flatten = TRUE)
  
  events_df <- data$events %>% as_tibble()
  
  cat("Number of events:", nrow(events_df), "\n")
  
  # =========================
  # 1. INSIGHT MARKETS (optional)
  # =========================
  
  if ("insightMarkets" %in% colnames(events_df) && 
      any(!is.na(events_df$insightMarkets)) && 
      any(lengths(events_df$insightMarkets) > 0)) {
    
    cat("Processing insight markets...\n")
    
    insight_df <- events_df %>%
      select(event_name = name, insightMarkets) %>%
      filter(!is.na(insightMarkets) & lengths(insightMarkets) > 0) %>%
      unnest(insightMarkets, names_sep = "_") %>%
      unnest(insightMarkets_outcomes, names_sep = "_") %>%
      transmute(
        event = event_name,
        market = insightMarkets_name,
        selection = insightMarkets_outcomes_name,
        odds = insightMarkets_outcomes_price,
        player_id = insightMarkets_outcomes_playerId,
        source = "insight"
      )
    
    cat("Insight markets rows:", nrow(insight_df), "\n")
    
  } else {
    cat("No insight markets found, creating empty dataframe...\n")
    
    insight_df <- tibble(
      event = character(),
      market = character(),
      selection = character(),
      odds = numeric(),
      player_id = character(),
      source = character()
    )
  }
  
  # =========================
  # 2. SGM MARKETS (KEY PART)
  # =========================
  
  if ("prePricedSgmMarkets" %in% colnames(events_df) && 
      any(!is.na(events_df$prePricedSgmMarkets)) && 
      any(lengths(events_df$prePricedSgmMarkets) > 0)) {
    
    cat("Processing SGM markets...\n")
    
    sgm_df <- events_df %>%
      select(event_name = name, prePricedSgmMarkets) %>%
      filter(!is.na(prePricedSgmMarkets) & lengths(prePricedSgmMarkets) > 0) %>%
      unnest(prePricedSgmMarkets, names_sep = "_") %>%
      unnest(prePricedSgmMarkets_outcomes, names_sep = "_") %>%
      transmute(
        event = event_name,
        market = prePricedSgmMarkets_name,
        selection = prePricedSgmMarkets_outcomes_name,
        odds = prePricedSgmMarkets_outcomes_price,
        line = prePricedSgmMarkets_outcomes_points,
        player_id = prePricedSgmMarkets_outcomes_playerId,
        source = "sgm"
      )
    
    cat("SGM markets rows:", nrow(sgm_df), "\n")
    
  } else {
    cat("No SGM markets found, creating empty dataframe...\n")
    
    sgm_df <- tibble(
      event = character(),
      market = character(),
      selection = character(),
      odds = numeric(),
      line = numeric(),
      player_id = character(),
      source = character()
    )
  }
  
  # =========================
  # 3. COMBINE
  # =========================
  
  # Add line column to insight_df to match sgm_df structure
  insight_df <- insight_df %>%
    mutate(line = NA_real_)
  
  combined <- bind_rows(insight_df, sgm_df)
  
  cat("Total combined rows:", nrow(combined), "\n")
  
  # =========================
  # 4. FILTER RELEVANT PROPS
  # =========================
  
  if (nrow(combined) > 0) {
    keywords <- c("disposals", "marks", "tackles", "goals", "handballs", "kicks")
    
    props <- combined %>%
      filter(str_detect(tolower(market), paste(keywords, collapse = "|")))
    
    cat("Filtered prop rows:", nrow(props), "\n")
    
    # =========================
    # 5. PARSE FIELDS
    # =========================
    
    if (nrow(props) > 0) {
      props <- props %>%
        mutate(
          player = str_extract(selection, "^[A-Za-z\\-\\. ]+"),
          type = case_when(
            str_detect(selection, "Over") ~ "Over",
            str_detect(selection, "Under") ~ "Under",
            TRUE ~ "Other"
          ),
          bookmaker = "PointsBet",
          timestamp = Sys.time()
        )
    }
    
  } else {
    cat("No data to filter, creating empty props dataframe...\n")
    
    props <- tibble(
      event = character(),
      market = character(),
      selection = character(),
      odds = numeric(),
      line = numeric(),
      player_id = character(),
      source = character(),
      player = character(),
      type = character(),
      bookmaker = character(),
      timestamp = as.POSIXct(character())
    )
  }
  
  cat("Final dataset:", dim(props), "\n")
  cat("Scraper complete ✅\n")
  
  return(props)
}
