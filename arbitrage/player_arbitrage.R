# NFL Player Prop Arbitrage Bet Finder
# Adam Wickwire - Bettor Analysis


# Load library
library(tidyverse)
library(oddsapiR)
library(lubridate)

# Set the API from a environment variable
api_key <- Sys.getenv("ODDS_API_KEY")


# Set Your Market Maker Sportsbook
# pinnacle or betonlineag recommended. these are in the EU region. 
# to use a different bookmaker, you will need to adjust the region accordingly
sharp_book <- "pinnacle"


# Get the list of games for today. 
# These should be based on the region of the sharp_book you are using
todays_games <- toa_sports_odds(
  sport_key = "americanfootball_nfl",
  regions = "eu",
  markets = "h2h",
  odds_format = "decimal",
  date_format = "iso"
)


# filter for pinnacle and betonlineag
sharp_line_h2h <- todays_games %>% 
  filter(bookmaker_key == sharp_book)

todays_game_ids <- sharp_line_h2h %>% 
  select(id) %>% 
  unique() %>% 
  pull()


# Set the markets and bookmakers
# adjust for correct sports_key
markets_list <- c(
  "player_pass_tds",
  "player_pass_yds",
  "player_rush_yds",
  "player_rush_longest",
  "player_kicking_points",
  "player_field_goals",
  "player_reception_yds",
  "player_reception_longest",
  "player_assists",
  "player_pass_attempts",
  "player_pass_completions",
  "player_pass_interceptions",
  "player_pass_longest_completion",
  "player_pass_rush_reception_tds",
  "player_pass_rush_reception_yds",
  "player_pats",
  "player_receptions",
  "player_rush_attempts",
  "player_rush_reception_tds",
  "player_rush_reception_yds",
  "player_sacks",
  "player_solo_tackles",
  "player_tackles_assists"
)



# add bookmakers you are looking to find value on - do not include the sharp_book
other_bookmakers <- c("betonlineag", "espnbet", "draftkings", "fanduel", "fliff",
                      "williamhill_us", "betmgm", "ballybet", 
                      "betrivers", "bovada", "betus", "lowvig", "mybookieag", 
                      "betanysports", "betparx")



# Combine all bookmakers into one vector
all_bookmakers <- c(sharp_book, other_bookmakers)

# Create the bookmakers string separated by commas
bookmakers <- paste(all_bookmakers, collapse = ",")


# Initialize an empty list to store the odds for each game and market
all_odds_list <- list()

# Initialize an empty list to store errors
errors_list <- list()

# Loop through each game ID and each market to get the odds
for (event_id in todays_game_ids) {
  for (market in markets_list) {
    # Try to retrieve the odds for the current event_id and market
    event_odds <- tryCatch(
      {
        suppressMessages(
          suppressWarnings(
            toa_event_odds(
              sport_key = "americanfootball_nfl",
              event_id = event_id,
              markets = market,
              odds_format = "decimal",
              date_format = "iso",
              bookmakers = bookmakers
            )
          )
        )
      },
      error = function(e) {
        # Optionally store the error
        errors_list[[paste(event_id, market, sep = "_")]] <- e$message
        NULL
      }
    )
    # If event_odds is not NULL, add to list
    if (!is.null(event_odds)) {
      all_odds_list[[paste(event_id, market, sep = "_")]] <- event_odds
    }
  }
}

# Combine all the odds into a single data frame
all_odds_df <- bind_rows(all_odds_list, .id = "event_market_id") %>% 
  rename(event_id = event_market_id)


#---------------------------------------------------------------------------------------#
# Function to find arbitrage opportunities
find_arbitrage_opportunities <- function(data, total_investment = 100) {

  # Step 1: Ensure unique identifiers for each betting market
  data <- data %>%
    mutate(
      market_id = paste(id, market_key, outcomes_description, sep = "_")
    )
  
  # Step 2: Find the best odds and corresponding bookmakers for each outcome
  best_odds <- data %>%
    group_by(market_id, outcomes_point, outcomes_name) %>%
    arrange(desc(outcomes_price)) %>%
    slice(1) %>%  # Take the row with the highest odds
    ungroup()
  
  # Ensure outcomes_name has valid column names
  best_odds <- best_odds %>%
    mutate(outcomes_name = make.names(outcomes_name))
  
  # Step 3: Pivot the data to have separate columns for each outcome's best odds and bookmaker
  odds_pivot <- best_odds %>%
    select(market_id, outcomes_point, outcomes_name, outcomes_price, bookmaker) %>%
    pivot_wider(
      names_from = outcomes_name,
      values_from = c(outcomes_price, bookmaker),
      names_glue = "{outcomes_name}_{.value}"
    )
  
  # Step 4: Filter out incomplete pairs
  odds_pivot <- odds_pivot %>%
    filter(!is.na(Over_outcomes_price) & !is.na(Under_outcomes_price))
  
  # Step 5: Calculate the synthetic hold
  # Identify columns containing odds
  odds_cols <- c("Over_outcomes_price", "Under_outcomes_price")
  
  # Calculate hold by summing the reciprocals of the best odds
  odds_pivot <- odds_pivot %>%
    rowwise() %>%
    mutate(
      hold = sum(1 / c_across(all_of(odds_cols))) - 1,
      arbitrage_opportunity = hold < 0
    ) %>%
    ungroup()
  
  # Step 6: Calculate Stake Amounts and Expected Profit
  # Sum of inverse odds
  odds_pivot <- odds_pivot %>%
    mutate(
      sum_inverse_odds = (1 / Over_outcomes_price) + (1 / Under_outcomes_price)
    )
  
  # Calculate stakes and returns for each outcome
  odds_pivot <- odds_pivot %>%
    mutate(
      stake_Over = (total_investment * (1 / Over_outcomes_price)) / sum_inverse_odds,
      stake_Under = (total_investment * (1 / Under_outcomes_price)) / sum_inverse_odds,
      return_Over = stake_Over * Over_outcomes_price,
      return_Under = stake_Under * Under_outcomes_price,
      profit = (pmin(return_Over, return_Under) - total_investment),
      profit_percentage = (profit / total_investment) * 100
    )
  
  # Step 7: Add match and market information
  match_info <- data %>%
    select(
      market_id, id, commence_time, home_team, away_team,
      market_key, outcomes_description
    ) %>%
    distinct()
  
  final_result <- odds_pivot %>%
    left_join(match_info, by = "market_id")
  
  return(final_result)
}


player_prop_arbitrage <- find_arbitrage_opportunities(all_odds_df)


# filter For Arbitrage Opportunities
player_prop_arbitrage <- player_prop_arbitrage %>% 
  filter(arbitrage_opportunity == TRUE) %>% 
  select(commence_time, home_team, away_team, market_key, outcomes_description, outcomes_point,
         Over_outcomes_price, Under_outcomes_price, Over_bookmaker, Under_bookmaker, 
         hold, stake_Over, stake_Under, profit)

# Round columns 
player_prop_arbitrage <- player_prop_arbitrage %>% 
  mutate(
    hold = round(hold, 4),
    stake_Over = round(stake_Over, 2),
    stake_Under = round(stake_Under, 2),
    profit = round(profit, 2)
  )


# Ensure the decimal_to_american function works correctly on vectors
decimal_to_american <- function(decimal_odds) {
  american_odds <- ifelse(
    decimal_odds >= 2,
    (decimal_odds - 1) * 100,
    -100 / (decimal_odds - 1)
  )
  return(american_odds)
}

# Convert decimal odds to American odds
player_prop_arbitrage <- player_prop_arbitrage %>% 
  mutate(
    Over_outcomes_price = round(decimal_to_american(Over_outcomes_price)),
    Under_outcomes_price = round(decimal_to_american(Under_outcomes_price))
  )

