# Arbitrage Bet Finder Shiny App
# Adam Wickwire - Bettor Analysis
# Copyright 2024 Midwest Marketing Group LLC All Rights Reserved


# Load necessary libraries
library(shiny)
library(tidyverse)
library(oddsapiR)
library(lubridate)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(reactable)
library(htmltools)
library(shinyWidgets)

# Set API Key (ensure your API key is set in your environment)
api_key <- Sys.getenv("ODDS_API_KEY")

# Define UI
ui <- fluidPage(
  useShinyjs(),  # Initialize shinyjs
  theme = shinytheme("flatly"),  # Apply theme
  
  # Include custom fonts and styles
  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Roboto:400,500&display=swap", rel = "stylesheet"),
    tags$style(HTML("
      /* Custom CSS styles for the reactable table */
      .player-prop-table {
        font-family: 'Roboto', Helvetica, Arial, sans-serif;
        font-size: 11px;
      }
      /* Header styling */
      .player-prop-table .rt-th {
        background-color: #1a3e5c;
        color: #fff;
        font-weight: 500;
        text-transform: uppercase;
        padding: 4px;
      }
      /* Cell styling */
      .player-prop-table .rt-td {
        font-size: 1.125rem;
        padding: 4px;
      }
      /* Row striping */
      .player-prop-table .rt-tr.-odd {
        background-color: #f9f9f9;
      }
      /* Highlight on hover */
      .player-prop-table .rt-tr:hover {
        background-color: #e5e5e5;
      }
      /* Highlight positive profit cells */
      .highlight-positive {
        background-color: #dff0d8 !important;
        color: green !important;
        font-weight: bold;
      }
      /* Pagination buttons */
      .player-prop-table .-pagination .-btn {
        color: #333;
        background-color: #fff;
        border: 1px solid #ccc;
      }
      /* Sidebar styling */
      .sidebar {
        font-family: 'Roboto', Helvetica, Arial, sans-serif;
        background-color: #1a3e5c;
        color: #fff;
        padding: 15px;
        font-size: 13px;
      }
      .sidebar .control-label, .sidebar .shiny-input-container {
        color: #fff;
      }
      .sidebar .btn {
        background-color: #337ab7;
        color: #fff;
        border-color: #2e6da4;
        font-size: 13px;
      }
      .sidebar .btn:hover {
        background-color: #286090;
        border-color: #204d74;
      }
      .sidebar .selectize-input, .sidebar .selectize-dropdown {
        color: #333;
      }
    "))
  ),
  
  # Use tabsetPanel to create tabs
  tabsetPanel(
    id = "main_ui",
    tabPanel(
      title = "Main Market Arbitrage Bet Finder",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          selectInput(
            inputId = "main_selected_sport",
            label = "Select Sport:",
            choices = list(
              "NFL - Football" = "americanfootball_nfl",
              "NBA - Basketball" = "basketball_nba",
              "MLB - Baseball" = "baseball_mlb",
              "NHL - Hockey" = "icehockey_nhl",
              "NCAA - College Football" = "americanfootball_ncaaf",
              "NCAA - College Basketball" = "basketball_ncaab",
              "WNBA - Basketball" = "basketball_wnba",
              "MLS - Soccer" = "soccer_usa_mls",
              "EPL - Soccer" = "soccer_epl"
            ),
            selected = c("americanfootball_nfl")
          ),
          numericInput(
            inputId = "main_bet_size",
            label = "Bet Size:",
            value = 100,  # Default value
            min = 1,     # Minimum allowed value
            max = 1000,  # Maximum allowed value
            step = 1     # Increment/decrement step
          ),
          actionButton("main_run_button", "Find Opportunities"),
          br(),
          br(),
          downloadButton("main_download_data", "Download CSV"),
          width = 2  # Adjust sidebar width
        ),
        mainPanel(
          h4("Main Line Arbitrage Opportunities"),
          withSpinner(reactableOutput("main_arbitrage_table"), type = 6),
          width = 10  # Adjust main panel width
        )
      )
    ),
    tabPanel(
      title = "Player Prop Arbitrage Bet Finder",
      sidebarLayout(
        sidebarPanel(
          class = "sidebar",
          selectInput(
            inputId = "prop_selected_sport",
            label = "Select Sport:",
            choices = list(
              "NFL - Football" = "americanfootball_nfl",
              "NBA - Basketball" = "basketball_nba",
              "MLB - Baseball" = "baseball_mlb",
              "NHL - Hockey" = "icehockey_nhl",
              "NCAA - College Football" = "americanfootball_ncaaf",
              "NCAA - College Basketball" = "basketball_ncaab",
              "WNBA - Basketball" = "basketball_wnba",
              "MLS - Soccer" = "soccer_usa_mls",
              "EPL - Soccer" = "soccer_epl"
            ),
            selected = c("americanfootball_nfl")
          ),
          uiOutput("prop_markets_ui"),  # Placeholder for markets selection
          checkboxGroupInput(
            inputId = "prop_selected_sportsbooks",
            label = "Select Sportsbooks:",
            choices = list(
              "Fliff" = "fliff",
              "ESPN Bet" = "espnbet",
              "DraftKings" = "draftkings",
              "FanDuel" = "fanduel",
              "Caesars" = "williamhill_us",
              "BetMGM" = "betmgm",
              "SuperBook" = "superbook",
              "Bally Bet" = "ballybet",
              "BetRivers" = "betrivers",
              "Bovada" = "bovada",
              "BetUs" = "betus",
              "LowVig.ag" = "lowvig",
              "MyBookie.ag" = "mybookieag",
              "BetAnySports" = "betanysports",
              "betPARX" = "betparx",
              "BetOnline" = "betonlineag"
            ),
            selected = c("draftkings", "espnbet", "fanduel")
          ),
          numericInput(
            inputId = "prop_bet_size",
            label = "Bet Size:",
            value = 100,  # Default value
            min = 1,     # Minimum allowed value
            max = 1000,  # Maximum allowed value
            step = 1     # Increment/decrement step
          ),
          actionButton("prop_run_button", "Find Opportunities"),
          br(),
          br(),
          downloadButton("prop_download_data", "Download CSV"),
          width = 2  # Adjust sidebar width
        ),
        mainPanel(
          h4("Player Prop Arbitrage Opportunities"),
          withSpinner(reactableOutput("prop_arbitrage_table"), type = 6),
          width = 10  # Adjust main panel width
        )
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  values <- reactiveValues()  # Initialize reactiveValues to store data
  
  # # Unhide to enable login functionality
  # # User credentials
  # credentials <- data.frame(
  #   user = c("admin"),  # Replace with real usernames
  #   password = c("adminpass"),  # Replace with real passwords
  #   stringsAsFactors = FALSE
  # )
  # 
  # # Function to check credentials
  # check_credentials <- function(username, password) {
  #   any(credentials$user == username & credentials$password == password)
  # }
  # 
  # # Show login modal on startup
  # showModal(modalDialog(
  #   title = "Login",
  #   textInput("username", "Username:"),
  #   passwordInput("password", "Password:"),
  #   footer = tagList(
  #     actionButton("login_button", "Login")
  #   ),
  #   easyClose = FALSE,  # Prevent closing the modal by clicking outside
  #   fade = FALSE
  # ))
  # 
  # # Handle login
  # observeEvent(input$login_button, {
  #   if (check_credentials(input$username, input$password)) {
  #     removeModal()
  #     shinyjs::show("main_ui")  # Show the main UI upon successful login
  #   } else {
  #     # Shake the login modal to indicate error (visual feedback)
  #     shinyjs::addClass(selector = ".modal-dialog", class = "shake")
  #     delay(1000, shinyjs::removeClass(selector = ".modal-dialog", class = "shake"))
  #     
  #     # Display an error message
  #     showNotification("Incorrect username or password. Please try again.", type = "error")
  #   }
  # })
  # 
  # # Hide the main UI initially
  # shinyjs::hide("main_ui")
  # 
  
  
  # Define the decimal_to_american function once
  decimal_to_american <- function(decimal_odds) {
    american_odds <- ifelse(
      decimal_odds >= 2,
      (decimal_odds - 1) * 100,
      -100 / (decimal_odds - 1)
    )
    return(american_odds)
  }
  
  #### Code for Main Market Arbitrage tab ####
  observeEvent(input$main_run_button, {
    # Show a loading message
    showModal(modalDialog("Fetching data, please wait...", footer = NULL))
    
    # Wrap the data fetching and processing in a tryCatch to handle errors
    tryCatch({
      
      # Get odds for the selected sport
      get_sport_odds <- toa_sports_odds(
        sport_key = input$main_selected_sport,
        regions = "eu,us,us2",
        markets = "h2h,spreads,totals",
        odds_format = "decimal",
        date_format = "iso"
      )
      
      # Set the current date and time
      current_time <- Sys.time()
      
      # Ensure the commence_time column is in POSIXct format and convert to local time
      get_sport_odds$commence_time <- with_tz(ymd_hms(get_sport_odds$commence_time), tzone = Sys.timezone())
      get_sport_odds$bookmaker_last_update <- with_tz(ymd_hms(get_sport_odds$bookmaker_last_update), tzone = Sys.timezone())
      get_sport_odds$market_last_update <- with_tz(ymd_hms(get_sport_odds$market_last_update), tzone = Sys.timezone())
      
      # Format time
      get_sport_odds$commence_time <- format(get_sport_odds$commence_time, "%Y-%m-%d %I:%M %p")
      get_sport_odds$bookmaker_last_update <- format(get_sport_odds$bookmaker_last_update, "%Y-%m-%d %I:%M %p")
      get_sport_odds$market_last_update <- format(get_sport_odds$market_last_update, "%Y-%m-%d %I:%M %p")
      
      # Create three dataframes for each market
      h2h <- get_sport_odds %>%
        filter(market_key == "h2h")
      
      totals <- get_sport_odds %>%
        filter(market_key == "totals")
      
      spreads <- get_sport_odds %>%
        filter(market_key == "spreads")
      
      # Function to find arbitrage opportunities
      find_arbitrage_opportunities <- function(data, total_investment = input$main_bet_size) {
        
        # Step 1: Find the best odds and corresponding bookmakers for each outcome
        best_odds <- data %>%
          group_by(id, outcomes_name) %>%
          arrange(desc(outcomes_price)) %>%
          slice(1) %>%  # Take the row with the highest odds
          ungroup()
        
        # Step 2: Pivot the data to longer format
        odds_long <- best_odds %>%
          select(id, outcomes_name, outcomes_price, bookmaker) %>%
          mutate(outcomes_name = make.names(outcomes_name))  # Ensure valid column names
        
        # Step 3: Calculate the synthetic hold for each match
        odds_long <- odds_long %>%
          group_by(id) %>%
          mutate(
            hold = sum(1 / outcomes_price, na.rm = TRUE) - 1,
            arbitrage_opportunity = hold < 0
          )
        
        # Step 4: Calculate Stake Amounts and Expected Profit
        odds_long <- odds_long %>%
          group_by(id) %>%
          mutate(
            sum_inverse_odds = sum(1 / outcomes_price, na.rm = TRUE),
            stake = (total_investment * (1 / outcomes_price)) / sum_inverse_odds,
            expected_return = stake * outcomes_price,
            profit = expected_return - total_investment,
            profit_percentage = (profit / total_investment) * 100
          ) %>%
          ungroup()
        
        # Step 5: Add match information
        match_info <- data %>%
          select(id, commence_time, home_team, away_team) %>%
          distinct()
        
        final_result <- odds_long %>%
          left_join(match_info, by = "id")
        
        return(final_result)
      }
      
      # For H2H Market
      arbitrage_h2h <- find_arbitrage_opportunities(h2h)
      
      # For Spreads Market
      arbitrage_spreads <- find_arbitrage_opportunities(spreads)
      
      # For Totals Market
      arbitrage_totals <- find_arbitrage_opportunities(totals)
      
      # Prepare data for each market
      arbitrage_h2h <- arbitrage_h2h %>% 
        mutate(market_key = "h2h") %>% 
        select(commence_time, home_team, away_team, market_key, outcomes_name, 
               outcomes_price, bookmaker, hold, stake, profit, arbitrage_opportunity) %>% 
        mutate(
          hold = round(hold, 4),
          stake = round(stake, 2),
          profit = round(profit, 2)
        )
      
      arbitrage_spreads <- arbitrage_spreads %>% 
        mutate(market_key = "spreads") %>% 
        select(commence_time, home_team, away_team, market_key, outcomes_name, 
               outcomes_price, bookmaker, hold, stake, profit, arbitrage_opportunity) %>% 
        mutate(
          hold = round(hold, 4),
          stake = round(stake, 2),
          profit = round(profit, 2)
        )
      
      arbitrage_totals <- arbitrage_totals %>%
        mutate(market_key = "totals") %>%
        select(commence_time, home_team, away_team, market_key, outcomes_name, 
               outcomes_price, bookmaker, hold, stake, profit, arbitrage_opportunity) %>%
        mutate(
          hold = round(hold, 4),
          stake = round(stake, 2),
          profit = round(profit, 2)
        )
      
      # Combine all markets
      arbitrage_main_all <- bind_rows(arbitrage_h2h, arbitrage_spreads, arbitrage_totals)
      
      # Clean up outcome names
      arbitrage_main_all <- arbitrage_main_all %>%
        mutate(outcomes_name = str_replace_all(outcomes_name, "\\.", " "))
      
      arbitrage_main_all <- arbitrage_main_all %>%
        mutate(game_id = paste(commence_time, home_team, away_team, market_key, sep = "_"))
      
      arbitrage_main_all <- arbitrage_main_all %>% 
        mutate(outcomes_price = round(decimal_to_american(outcomes_price)))
      
      # Map 'Over' and 'Under' to 'away' and 'home' teams for 'totals' market
      arbitrage_main_all <- arbitrage_main_all %>%
        mutate(
          # For 'totals' market, map 'Over' to 'away_team' and 'Under' to 'home_team'
          adjusted_outcomes_name = case_when(
            market_key == 'totals' & outcomes_name == 'Over' ~ away_team,
            market_key == 'totals' & outcomes_name == 'Under' ~ home_team,
            TRUE ~ outcomes_name
          )
        )
      
      # Create 'outcome_type' column based on adjusted outcomes
      arbitrage_main_all <- arbitrage_main_all %>%
        mutate(
          outcome_type = case_when(
            adjusted_outcomes_name == home_team ~ 'home',
            adjusted_outcomes_name == away_team ~ 'away',
            TRUE ~ 'other'
          )
        )
      
      # Pivot the data to wide format
      arbitrage_main_all_wide <- arbitrage_main_all %>%
        select(
          game_id, commence_time, home_team, away_team, market_key, hold, arbitrage_opportunity,
          outcome_type, outcomes_name, outcomes_price, bookmaker, stake, profit
        ) %>%
        pivot_wider(
          names_from = outcome_type,
          values_from = c(outcomes_name, outcomes_price, bookmaker, stake, profit),
          names_sep = "_"
        )
      
      # Select relevant columns and rename for clarity
      arbitrage_main_all_wide <- arbitrage_main_all_wide %>%
        select(
          commence_time, home_team, away_team, market_key,
          outcomes_name_home, outcomes_price_home, outcomes_name_away, outcomes_price_away,
          bookmaker_home, bookmaker_away, hold, stake_home, stake_away, profit_home, arbitrage_opportunity
        ) %>% 
        rename(
          home_price = outcomes_price_home,
          away_price = outcomes_price_away,
          outcomes_home = outcomes_name_home,
          outcomes_away = outcomes_name_away,
          profit = profit_home
        )
      
      # Filter for arbitrage opportunities
      arbitrage_main_all_filtered <- arbitrage_main_all_wide %>%
        filter(arbitrage_opportunity == TRUE)
      
      # Remove rows with missing data
      arbitrage_main_all_filtered <- arbitrage_main_all_filtered %>%
        drop_na(home_price, away_price, bookmaker_home, bookmaker_away)
      
      arbitrage_main_all_filtered <- arbitrage_main_all_filtered %>%
        select(-arbitrage_opportunity)
      
      # Store the result in reactiveValues for download
      values$arbitrage_main_all <- arbitrage_main_all_filtered  # Store data for export
      
      # Display the result in the table
      output$main_arbitrage_table <- renderReactable({
        reactable(
          values$arbitrage_main_all,
          columns = list(
            commence_time = colDef(name = "Game Time"),
            home_team = colDef(name = "Home Team"),
            away_team = colDef(name = "Away Team"),
            market_key = colDef(name = "Market"),
            outcomes_home = colDef(name = "Home Outcome"),
            home_price = colDef(name = "Home Odds"),
            bookmaker_home = colDef(name = "Home Bookmaker"),
            outcomes_away = colDef(name = "Away Outcome"),
            away_price = colDef(name = "Away Odds"),
            bookmaker_away = colDef(name = "Away Bookmaker"),
            hold = colDef(name = "Hold"),
            stake_home = colDef(name = "Stake Home"),
            stake_away = colDef(name = "Stake Away"),
            profit = colDef(
              name = "Profit",
              cell = function(value) {
                if (value > 0) {
                  div(class = "highlight-positive", paste0("$", value))
                } else {
                  paste0("$", value)
                }
              }
            )
          ),
          defaultColDef = colDef(
            align = "center",
            minWidth = 70
          ),
          defaultPageSize = 25,
          resizable = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          striped = TRUE,
          pagination = TRUE,
          defaultSorted = "profit",
          defaultSortOrder = "desc",
          fullWidth = TRUE,
          theme = reactableTheme(),
          class = "player-prop-table"
        )
      })
      
    }, error = function(e) {
      # Handle any errors that occur during processing
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
    })
  })
  
  # Download handler for the CSV file
  output$main_download_data <- downloadHandler(
    filename = function() {
      paste("Main_Line_Arbitrage_Opportunities_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Ensure that the data exists before attempting to write it
      req(values$arbitrage_main_all)
      write.csv(values$arbitrage_main_all, file, row.names = FALSE)
    }
  )
  
  #### Code for Player Prop Arbitrage tab ####
  
  # Market lists for each sport
  markets_list_nfl <- c(
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
  
  markets_list_nba <- c(
    "player_points",
    "player_rebounds",
    "player_assists",
    "player_threes",
    "player_blocks",
    "player_steals",
    "player_blocks_steals",
    "player_points_rebounds_assists",
    "player_points_rebounds",
    "player_points_assists",
    "player_rebounds_assists"
  )
  
  markets_list_mlb <- c(
    "batter_home_runs",
    "batter_hits",
    "batter_total_bases",
    "batter_rbis",
    "batter_runs_scored",
    "batter_hits_runs_rbis",
    "batter_singles",
    "batter_doubles",
    "batter_triples",
    "batter_walks",
    "batter_strikeouts",
    "pitcher_strikeouts",
    "pitcher_outs",
    "pitcher_hits_allowed",
    "pitcher_walks",
    "pitcher_earned_runs"
  )
  
  markets_list_nhl <- c(
    "player_points",
    "player_power_play_points",
    "player_assists",
    "player_blocked_shots",
    "player_shots_on_goal",
    "player_goals",
    "player_total_saves"
  )
  
  markets_list_ncaaf <- c(
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
  
  markets_list_ncaab <- c(
    "player_points",
    "player_rebounds",
    "player_assists",
    "player_threes",
    "player_blocks",
    "player_steals",
    "player_blocks_steals",
    "player_points_rebounds_assists",
    "player_points_rebounds",
    "player_points_assists",
    "player_rebounds_assists"
  )
  
  markets_list_wnba <- c(
    "player_points",
    "player_rebounds",
    "player_assists",
    "player_threes",
    "player_blocks",
    "player_steals",
    "player_blocks_steals",
    "player_points_rebounds_assists",
    "player_points_rebounds",
    "player_points_assists",
    "player_rebounds_assists"
  )
  
  markets_list_mls <- c(
    "player_shots_on_target",
    "player_shots",
    "player_assists"
  )
  
  markets_list_epl <- c(
    "player_shots_on_target",
    "player_shots",
    "player_assists"
  )
  
  # Render markets selection UI
  output$prop_markets_ui <- renderUI({
    req(input$prop_selected_sport)
    markets_list <- switch(
      input$prop_selected_sport,
      "americanfootball_nfl" = markets_list_nfl,
      "basketball_nba" = markets_list_nba,
      "baseball_mlb" = markets_list_mlb,
      "icehockey_nhl" = markets_list_nhl,
      "americanfootball_ncaaf" = markets_list_ncaaf,
      "basketball_ncaab" = markets_list_ncaab,
      "basketball_wnba" = markets_list_wnba,
      "soccer_usa_mls" = markets_list_mls,
      "soccer_epl" = markets_list_epl,
      character(0)
    )
    markets_choices <- setNames(markets_list, gsub("_", " ", markets_list))
    
    pickerInput(
      inputId = "prop_selected_markets",
      label = "Select Markets:",
      choices = markets_choices,
      selected = markets_list,
      multiple = TRUE,
      options = list(
        `actions-box` = TRUE,
        `live-search` = TRUE,
        `selected-text-format` = "count > 2",
        `count-selected-text` = "{0} markets selected",
        `none-selected-text` = "No markets selected"
      )
    )
  })
  
  observeEvent(input$prop_run_button, {
    req(input$prop_selected_markets)  # Ensure markets are selected
    # Show a loading message
    showModal(modalDialog("Fetching data, please wait...", footer = NULL))
    
    # Wrap the data fetching and processing in a tryCatch to handle errors
    tryCatch({
      
      # Update the sportsbooks based on user selection
      other_bookmakers <- input$prop_selected_sportsbooks
      
      # Use Pinnacle as the sharp book to get game IDs
      sharp_book <- "pinnacle"
      
      # Combine all bookmakers into one vector, including the sharp_book
      all_bookmakers <- c(sharp_book, other_bookmakers)
      
      # Create the bookmakers string separated by commas
      bookmakers <- paste(all_bookmakers, collapse = ",")
      
      # Get the list of games for today
      todays_games <- toa_sports_odds(
        sport_key = input$prop_selected_sport,
        regions = "eu",
        markets = "h2h,spreads,totals",
        odds_format = "decimal",
        date_format = "iso"
      )
      
      # Filter for the sharp_book data to get event IDs
      sharp_line_h2h <- todays_games %>%
        filter(bookmaker_key == sharp_book)
      
      todays_game_ids <- sharp_line_h2h %>%
        select(id) %>%
        unique() %>%
        pull()
      
      # Initialize an empty list to store the odds for each game and market
      all_odds_list <- list()
      
      # Initialize an empty list to store errors
      errors_list <- list()
      
      # Loop through each game ID and each market to get the odds
      for (event_id in todays_game_ids) {
        for (market in input$prop_selected_markets) {
          # Try to retrieve the odds for the current event_id and market
          event_odds <- tryCatch(
            {
              suppressMessages(
                suppressWarnings(
                  toa_event_odds(
                    sport_key = input$prop_selected_sport,
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
      
      # Function to find arbitrage opportunities
      find_arbitrage_opportunities <- function(data, total_investment = input$prop_bet_size) {
        
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
      
      # Filter For Arbitrage Opportunities
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
      
      # Convert decimal odds to American odds
      player_prop_arbitrage <- player_prop_arbitrage %>%
        mutate(
          Over_outcomes_price = round(decimal_to_american(Over_outcomes_price)),
          Under_outcomes_price = round(decimal_to_american(Under_outcomes_price))
        )
      
      # Remove underscores from all character columns
      player_prop_arbitrage <- player_prop_arbitrage %>%
        mutate(across(where(is.character), ~ gsub("_", " ", .)))
      
      # Store the result in reactiveValues for download
      values$player_prop_arbitrage <- player_prop_arbitrage  # Store data for export
      
      # Display the result in the table
      output$prop_arbitrage_table <- renderReactable({
        reactable(
          values$player_prop_arbitrage,
          columns = list(
            commence_time = colDef(name = "Game Time"),
            home_team = colDef(name = "Home Team"),
            away_team = colDef(name = "Away Team"),
            market_key = colDef(name = "Market"),
            outcomes_description = colDef(name = "Player"),
            outcomes_point = colDef(name = "Point"),
            Over_outcomes_price = colDef(name = "Over Odds"),
            Under_outcomes_price = colDef(name = "Under Odds"),
            Over_bookmaker = colDef(name = "Over Bookmaker"),
            Under_bookmaker = colDef(name = "Under Bookmaker"),
            hold = colDef(name = "Hold"),
            stake_Over = colDef(name = "Stake Over"),
            stake_Under = colDef(name = "Stake Under"),
            profit = colDef(
              name = "Profit",
              cell = function(value) {
                if (value > 0) {
                  div(class = "highlight-positive", paste0("$", value))
                } else {
                  paste0("$", value)
                }
              }
            )
          ),
          defaultColDef = colDef(
            align = "center",
            minWidth = 70
          ),
          defaultPageSize = 25,
          resizable = TRUE,
          highlight = TRUE,
          bordered = TRUE,
          striped = TRUE,
          pagination = TRUE,
          defaultSorted = "profit",
          defaultSortOrder = "desc",
          fullWidth = TRUE,
          theme = reactableTheme(),
          class = "player-prop-table"
        )
      })
      
    }, error = function(e) {
      # Handle any errors that occur during processing
      showNotification(
        paste("An error occurred:", e$message),
        type = "error"
      )
    }, finally = {
      removeModal()
    })
  })
  
  # Download handler for the CSV file
  output$prop_download_data <- downloadHandler(
    filename = function() {
      paste("Player_Prop_Arbitrage_Opportunities_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Ensure that the data exists before attempting to write it
      req(values$player_prop_arbitrage)
      write.csv(values$player_prop_arbitrage, file, row.names = FALSE)
    }
  )
}

# Run the application
shinyApp(ui = ui, server = server)