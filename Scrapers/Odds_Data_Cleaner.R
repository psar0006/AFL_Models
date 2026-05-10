#Function that converts scraped data into usable dataframe


library(dplyr)
library(tidyr)
library(purrr)
library(stringr)

extract_afl_props <- function(x) {
  
  # keywords to filter relevant markets
  keywords <- c("disposals", "marks", "tackles", "goals", "goal", "handballs", "kicks")
  
  # Step 1: Filter relevant markets
  df <- x %>%
    filter(str_detect(tolower(name), paste(keywords, collapse = "|"))) %>%
    dplyr::select(name, selections)
  
  # Step 2: Flatten selections
  flat_df <- df %>%
    mutate(selections = map(selections, as_tibble)) %>%
    unnest(selections, names_sep = "_")
  
  # Step 3: Clean + standardise
  clean_df <- flat_df %>%
    transmute(
      market = name,
      selection_name = selections_name,
      odds = selections_price.winPrice
    ) %>%
    mutate(
      stat = case_when(
        str_detect(tolower(market), "disposals") ~ "disposals",
        str_detect(tolower(market), "marks") ~ "marks",
        str_detect(tolower(market), "tackles") ~ "tackles",
        str_detect(tolower(market), "goals") ~ "goals",
        str_detect(tolower(market), "goal") ~ "goal",
        str_detect(tolower(market), "handballs") ~ "handballs",
        str_detect(tolower(market), "kicks") ~ "kicks",
        TRUE ~ NA_character_
      ),
      player = str_extract(selection_name, "^[A-Za-z\\-\\. ]+"),
      line = as.numeric(str_extract(selection_name, "\\d+\\.?\\d*")),
      type = case_when(
        str_detect(selection_name, "Over") ~ "Over",
        str_detect(selection_name, "Under") ~ "Under",
        TRUE ~ "Other"
      ),
      bookmaker = "Sportsbet",
      timestamp = Sys.time()
    )
  
  return(clean_df)
}

