# Arbitrage Bet Finder

This repository contains **5 R scripts** for identifying and visualizing sports betting arbitrage opportunities. 
The scripts leverage the [oddsapiR](https://github.com/mpcrie/oddsapiR) package to fetch odds data and then 
calculate potential arbitrage scenarios across multiple bookmakers.

---

## File Descriptions

1. **arbitrage_main.R**  
   Finds arbitrage opportunities in main markets such as moneyline (H2H), totals, and spreads.

2. **arbitrage_mainapp.R**  
   A Shiny application that uses the results of \`arbitrage_main.R\` to provide an interactive dashboard.

3. **player_arbitrage.R**  
   Targets NFL player proposition bets (e.g., passing yards, rushing yards) to identify arbitrage plays.

4. **player_arbitrage_app.R**  
   A Shiny dashboard displaying the player prop arbitrage results from \`player_arbitrage.R\`.

5. **arbitrage_both_app.R**  
   A Shiny application integrating both main market and player prop functionalities into a single interface.


