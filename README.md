- 👋 Hi, I’m @OGbobbyjosh
- 👀 I’m interested in Finance
- 🌱 I’m currently learning Julius, Python
- 💞️ I’m looking to collaborate on Trading
- 📫 How to reach me 21jsharrison@gmail.com

- ⚡ Fun fact: CR7 da GOAT

<!---
OGbobbyjosh/OGbobbyjosh is a ✨ special ✨ repository because its `README.md` (this file) appears on your GitHub profile.
You can click the Preview link to take a look at your changes.
--->
# Enhanced Go Pikachu function with current market data
go_pikachu_enhanced <- function(
  probability_threshold = 0.85,
  min_annualized_return = 5,
  max_days_to_expiry = 60,
  min_wheel_return = 10,
  total_capital = 100000,
  max_sector_allocation = 0.30,
  max_position_allocation = 0.10,
  include_midcaps = TRUE
) {
  
  cat("=== 'Go Pikachu' Command Activated ===\
")
  cat("Scanning options market for wheel strategy opportunities...\
\
")
  
  # Define our universe of stocks with current market prices (as of March 1, 2025 simulation)
  tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NVDA", "AMD", 
               "JPM", "V", "DIS", "NFLX", "PYPL", "INTC", "CSCO", "ADBE")
  
  sectors <- c(
    "Information Technology", "Information Technology", "Communication Services", 
    "Consumer Discretionary", "Communication Services", "Consumer Discretionary", 
    "Information Technology", "Information Technology", "Financials", "Financials",
    "Communication Services", "Communication Services", "Financials", 
    "Information Technology", "Information Technology", "Information Technology"
  )
  
  # Create a market data frame with realistic current prices
  market_data <- data.frame(
    ticker = tickers,
    sector = sectors,
    current_price = c(175.50, 410.30, 165.20, 178.75, 485.90, 177.80, 125.00, 158.40,
                      185.25, 275.40, 112.30, 625.80, 65.40, 42.75, 52.30, 495.60),
    market_cap = c(2750, 3050, 2100, 1850, 1250, 560, 310, 260,
                   540, 580, 205, 275, 72, 180, 220, 230),
    beta = c(1.2, 0.9, 1.1, 1.3, 1.4, 1.8, 1.6, 1.5,
             0.8, 0.7, 1.2, 1.3, 1.4, 1.0, 0.9, 1.2),
    pe_ratio = c(28, 35, 25, 40, 22, 38, 32, 30,
                 12, 30, 35, 42, 18, 15, 14, 32),
    dividend_yield = c(0.5, 0.8, 0, 0, 0, 0, 0.1, 0,
                       2.8, 0.7, 1.2, 0, 0, 2.5, 3.0, 0),
    stringsAsFactors = FALSE
  )
  
  cat("Market data for", nrow(market_data), "stocks across", length(unique(market_data$sector)), "sectors.\
\
")
  
  # Display the market data
  print(head(market_data, 8))
  
  # Simulate generating option chain
  cat("\
Generating option chains with realistic parameters...\
")
  option_chain <- data.frame()
  
  # Generate options with realistic parameters
  for (i in 1:nrow(market_data)) {
    ticker <- market_data$ticker[i]
    current_price <- market_data$current_price[i]
    sector <- market_data$sector[i]
    beta <- market_data$beta[i]
    
    # Generate different expiration dates
    days_to_expiry <- c(14, 30, 45, 60)
    
    # Calculate actual expiration dates
    today <- Sys.Date()
    expiration_dates <- as.character(today + days_to_expiry)
    
    for (j in 1:length(days_to_expiry)) {
      days <- days_to_expiry[j]
      expiration_date <- expiration_dates[j]
      
      # Generate strike prices at different deltas
      strike_distances <- c(0.03, 0.05, 0.07, 0.10, 0.15)
      
      for (distance in strike_distances) {
        strike_price <- current_price * (1 - distance)
        
        # Calculate realistic option premium based on market conditions
        # Adjust volatility based on sector and beta
        base_volatility <- ifelse(
          sector %in% c("Information Technology", "Communication Services"), 0.30,
          ifelse(sector %in% c("Consumer Discretionary", "Financials"), 0.25, 0.20)
        )
        
        # Adjust for beta and add some randomness
        volatility <- base_volatility * beta * (1 + runif(1, -0.1, 0.1))
        
        # Volatility skew - OTM puts typically have higher IV
        vol_skew <- 1 + (distance * 1.2)
        implied_volatility <- volatility * vol_skew
        
        # Time factor (in years)
        time_factor <- days / 365
        
        # Simplified Black-Scholes inspired premium calculation
        option_premium <- current_price * distance * sqrt(time_factor) * implied_volatility
        
        # Add some market noise
        option_premium <- option_premium * (1 + runif(1, -0.05, 0.05))
        
        # Ensure minimum premium
        option_premium <- max(option_premium, 0.05)
        
        # Calculate probability of success (simplified)
        prob_success <- 1 - (distance / (implied_volatility * sqrt(time_factor)))
        prob_success <- min(max(prob_success, 0.75), 0.98)
        
        # Calculate annualized return
        annualized_return <- (option_premium / strike_price) * (365 / days) * 100
        
        # Wheel strategy considerations
        # Estimate covered call premium if assigned
        cc_premium <- option_premium * 0.8 * (1 + runif(1, -0.1, 0.1))
        
        # Calculate wheel return (put premium + potential covered call premium + potential stock appreciation)
        wheel_return <- ((option_premium + cc_premium + (strike_price * 0.02)) / strike_price) * (365 / (days + 30)) * 100
        
        # Add to option chain
        option_chain <- rbind(option_chain, data.frame(
          ticker = ticker,
          sector = sector,
          current_price = current_price,
          strike_price = round(strike_price, 2),
          days_to_expiry = days,
          expiration_date = expiration_date,
          option_premium = round(option_premium, 2),
          implied_volatility = round(implied_volatility * 100, 2),
          probability_success = round(prob_success, 4),
          annualized_return = round(annualized_return, 2),
          cc_premium = round(cc_premium, 2),
          wheel_return = round(wheel_return, 2),
          market_cap = market_data$market_cap[i],
          beta = beta,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  cat("Generated", nrow(option_chain), "option contracts across all stocks and expirations.\
\
")
  
  # Filter options based on strategy criteria
  cat("Filtering options based on strategy criteria...\
")
  filtered_options <- option_chain[
    option_chain$probability_success >= probability_threshold &
    option_chain$annualized_return >= min_annualized_return &
    option_chain$days_to_expiry <= max_days_to_expiry &
    option_chain$wheel_return >= min_wheel_return,
  ]
  
  # Add additional metrics for ranking
  filtered_options$risk_score <- (1 - filtered_options$probability_success) * 100
  filtered_options$reward_score <- filtered_options$annualized_return
  filtered_options$risk_reward_ratio <- filtered_options$reward_score / filtered_options$risk_score
  
  # Create a combined score for ranking
  filtered_options$combined_score <- (filtered_options$probability_success * 5) + 
                                    (filtered_options$annualized_return / 5) + 
                                    (filtered_options$wheel_return / 10) + 
                                    (filtered_options$risk_reward_ratio / 2)
  
  # Sort by combined score
  filtered_options <- filtered_options[order(-filtered_options$combined_score), ]
  
  cat("Found", nrow(filtered_options), "options meeting our strategy criteria.\
\
")
  
  # Add news links for the filtered options
  if(nrow(filtered_options) > 0) {
    # Create news links for each ticker in the filtered options
    filtered_options$yahoo_finance <- paste0("https://finance.yahoo.com/quote/", filtered_options$ticker)
    filtered_options$seeking_alpha <- paste0("https://seekingalpha.com/symbol/", filtered_options$ticker)
    filtered_options$market_watch <- paste0("https://www.marketwatch.com/investing/stock/", tolower(filtered_options$ticker))
    filtered_options$cnbc <- paste0("https://www.cnbc.com/quotes/", filtered_options$ticker)
    
    # Display top filtered options with news links
    top_options <- head(filtered_options, 10)
    
    # Create a more readable display table
    display_table <- top_options[, c("ticker", "current_price", "strike_price", "expiration_date", 
                                    "days_to_expiry", "option_premium", "probability_success", 
                                    "annualized_return", "wheel_return", "risk_reward_ratio")]
    
    cat("Top Wheel Strategy Opportunities:\
")
    print(display_table)
    
    # Display news links for the top option
    cat("\
News and Information Links for", top_options$ticker[1], ":\
")
    cat("Yahoo Finance:", top_options$yahoo_finance[1], "\
")
    cat("Seeking Alpha:", top_options$seeking_alpha[1], "\
")
    cat("MarketWatch:", top_options$market_watch[1], "\
")
    cat("CNBC:", top_options$cnbc[1], "\
")
    
    # Create a detailed analysis for the top option
    top_option <- top_options[1,]
    
    cat("\
=== DETAILED ANALYSIS ===\
")
    cat("Top Recommendation:", top_option$ticker, "Put Option\
\
")
    cat("Current Price: $", top_option$current_price, "\
")
    cat("Strike Price: $", top_option$strike_price, " (", 
        round((1 - top_option$strike_price/top_option$current_price) * 100, 2), "% below current price)\
", sep="")
    cat("Expiration Date:", top_option$expiration_date, "(", top_option$days_to_expiry, "days)\
")
    cat("Option Premium: $", top_option$option_premium, " per share ($", 
        top_option$option_premium * 100, " per contract)\
", sep="")
    cat("Implied Volatility:", top_option$implied_volatility, "%\
")
    cat("Probability of Success:", round(top_option$probability_success * 100, 2), "%\
")
    cat("Annualized Return (if not assigned):", top_option$annualized_return, "%\
")
    cat("Potential Wheel Strategy Return:", top_option$wheel_return, "%\
")
    cat("Risk/Reward Ratio:", round(top_option$risk_reward_ratio, 2), "\
")
    
    # Calculate potential outcomes
    capital_required <- top_option$strike_price * 100
    premium_received <- top_option$option_premium * 100
    max_profit <- premium_received
    max_loss <- (top_option$strike_price * 100) - premium_received
    breakeven_price <- top_option$strike_price - top_option$option_premium
    
    cat("\
Trade Details (1 contract):\
")
    cat("Capital Required: $", capital_required, "\
", sep="")
    cat("Premium Received: $", premium_received, "\
", sep="")
    cat("Maximum Profit: $", max_profit, " (", round(max_profit/capital_required * 100, 2), "% return)\
", sep="")
    cat("Maximum Loss: $", max_loss, " (if stock goes to $0)\
", sep="")
    cat("Breakeven Price: $", breakeven_price, "\
", sep="")
    
    # Portfolio allocation
    max_contracts <- floor((total_capital * max_position_allocation) / capital_required)
    
    cat("\
Portfolio Allocation:\
")
    cat("Maximum Contracts (", max_position_allocation * 100, "% allocation): ", max_contracts, "\
", sep="")
    cat("Total Capital Required: $", max_contracts * capital_required, "\
", sep="")
    cat("Total Premium Received: $", max_contracts * premium_received, "\
", sep="")
    
    # Sector allocation check
    sector_options <- filtered_options[filtered_options$sector == top_option$sector, ]
    sector_allocation <- sum(sector_options$strike_price[1:min(3, nrow(sector_options))] * 100) / total_capital
    
    cat("\
Sector Allocation Check:\
")
    cat("Current", top_option$sector, "Allocation: ", round(sector_allocation * 100, 2), "%\
", sep=" ")
    cat("Maximum Sector Allocation: ", max_sector_allocation * 100, "%\
", sep="")
    
    if(sector_allocation > max_sector_allocation) {
      cat("WARNING: Adding this position would exceed maximum sector allocation.\
")
      cat("Consider diversifying into different sectors.\
")
    } else {
      cat("Sector allocation is within acceptable limits.\
")
    }
    
    # Risk management
    cat("\
Risk Management:\
")
    cat("Consider setting a stop loss at 50% of maximum loss: $", round(max_loss * 0.5, 2), "\
", sep="")
    cat("Consider taking profit at 50% of premium: $", round(premium_received * 0.5, 2), "\
", sep="")
    
    # Wheel strategy continuation
    cat("\
Wheel Strategy Continuation:\
")
    cat("If assigned at $", top_option$strike_price, ":\
", sep="")
    cat("- Potential covered call strike (2% OTM): $", round(top_option$strike_price * 1.02, 2), "\
", sep="")
    cat("- Estimated covered call premium: $", round(top_option$cc_premium, 2), " per share\
", sep="")
    cat("- Additional return from covered call: ", round((top_option$cc_premium / top_option$strike_price) * 100, 2), "%\
", sep="")
  } else {
    cat("No options found meeting the criteria. Try adjusting the parameters.\
")
  }
  
  # Return the filtered options
  return(filtered_options)
}

# Run the enhanced function with default parameters
result_enhanced <- go_pikachu_enhanced()

### iteration 

# Update the META price in the market data and run Go Pikachu again
go_pikachu_enhanced <- function(
  probability_threshold = 0.80,
  min_annualized_return = 4,
  max_days_to_expiry = 60,
  min_wheel_return = 8,
  total_capital = 100000,
  max_sector_allocation = 0.30,
  max_position_allocation = 0.10,
  include_midcaps = TRUE
) {
  
  cat("=== 'Go Pikachu' Command Activated ===\
")
  cat("Scanning options market for wheel strategy opportunities...\
\
")
  
  # Define our universe of stocks with current market prices (as of March 1, 2025 simulation)
  tickers <- c("AAPL", "MSFT", "GOOGL", "AMZN", "META", "TSLA", "NVDA", "AMD", 
               "JPM", "V", "DIS", "NFLX", "PYPL", "INTC", "CSCO", "ADBE")
  
  sectors <- c(
    "Information Technology", "Information Technology", "Communication Services", 
    "Consumer Discretionary", "Communication Services", "Consumer Discretionary", 
    "Information Technology", "Information Technology", "Financials", "Financials",
    "Communication Services", "Communication Services", "Financials", 
    "Information Technology", "Information Technology", "Information Technology"
  )
  
  # Create a market data frame with realistic current prices
  # Updated META price to $669.4
  market_data <- data.frame(
    ticker = tickers,
    sector = sectors,
    current_price = c(175.50, 410.30, 165.20, 178.75, 669.40, 177.80, 125.00, 158.40,
                      185.25, 275.40, 112.30, 625.80, 65.40, 42.75, 52.30, 495.60),
    market_cap = c(2750, 3050, 2100, 1850, 1720, 560, 310, 260,
                   540, 580, 205, 275, 72, 180, 220, 230),
    beta = c(1.2, 0.9, 1.1, 1.3, 1.4, 1.8, 1.6, 1.5,
             0.8, 0.7, 1.2, 1.3, 1.4, 1.0, 0.9, 1.2),
    pe_ratio = c(28, 35, 25, 40, 22, 38, 32, 30,
                 12, 30, 35, 42, 18, 15, 14, 32),
    dividend_yield = c(0.5, 0.8, 0, 0, 0, 0, 0.1, 0,
                       2.8, 0.7, 1.2, 0, 0, 2.5, 3.0, 0),
    stringsAsFactors = FALSE
  )
  
  cat("Market data for", nrow(market_data), "stocks across", length(unique(market_data$sector)), "sectors.\
\
")
  
  # Display the market data
  print(head(market_data, 8))
  
  # Simulate generating option chain
  cat("\
Generating option chains with realistic parameters...\
")
  option_chain <- data.frame()
  
  # Generate options with realistic parameters
  for (i in 1:nrow(market_data)) {
    ticker <- market_data$ticker[i]
    current_price <- market_data$current_price[i]
    sector <- market_data$sector[i]
    beta <- market_data$beta[i]
    
    # Generate different expiration dates
    days_to_expiry <- c(14, 30, 45, 60)
    
    # Calculate actual expiration dates
    today <- Sys.Date()
    expiration_dates <- as.character(today + days_to_expiry)
    
    for (j in 1:length(days_to_expiry)) {
      days <- days_to_expiry[j]
      expiration_date <- expiration_dates[j]
      
      # Generate strike prices at different deltas
      strike_distances <- c(0.03, 0.05, 0.07, 0.10, 0.15)
      
      for (distance in strike_distances) {
        strike_price <- current_price * (1 - distance)
        
        # Calculate realistic option premium based on market conditions
        # Adjust volatility based on sector and beta
        base_volatility <- ifelse(
          sector %in% c("Information Technology", "Communication Services"), 0.30,
          ifelse(sector %in% c("Consumer Discretionary", "Financials"), 0.25, 0.20)
        )
        
        # Adjust for beta and add some randomness
        volatility <- base_volatility * beta * (1 + runif(1, -0.1, 0.1))
        
        # Volatility skew - OTM puts typically have higher IV
        vol_skew <- 1 + (distance * 1.2)
        implied_volatility <- volatility * vol_skew
        
        # Time factor (in years)
        time_factor <- days / 365
        
        # Simplified Black-Scholes inspired premium calculation
        option_premium <- current_price * distance * sqrt(time_factor) * implied_volatility
        
        # Add some market noise
        option_premium <- option_premium * (1 + runif(1, -0.05, 0.05))
        
        # Ensure minimum premium
        option_premium <- max(option_premium, 0.05)
        
        # Calculate probability of success (simplified)
        prob_success <- 1 - (distance / (implied_volatility * sqrt(time_factor)))
        prob_success <- min(max(prob_success, 0.75), 0.98)
        
        # Calculate annualized return
        annualized_return <- (option_premium / strike_price) * (365 / days) * 100
        
        # Wheel strategy considerations
        # Estimate covered call premium if assigned
        cc_premium <- option_premium * 0.8 * (1 + runif(1, -0.1, 0.1))
        
        # Calculate wheel return (put premium + potential covered call premium + potential stock appreciation)
        wheel_return <- ((option_premium + cc_premium + (strike_price * 0.02)) / strike_price) * (365 / (days + 30)) * 100
        
        # Add to option chain
        option_chain <- rbind(option_chain, data.frame(
          ticker = ticker,
          sector = sector,
          current_price = current_price,
          strike_price = round(strike_price, 2),
          days_to_expiry = days,
          expiration_date = expiration_date,
          option_premium = round(option_premium, 2),
          implied_volatility = round(implied_volatility * 100, 2),
          probability_success = round(prob_success, 4),
          annualized_return = round(annualized_return, 2),
          cc_premium = round(cc_premium, 2),
          wheel_return = round(wheel_return, 2),
          market_cap = market_data$market_cap[i],
          beta = beta,
          stringsAsFactors = FALSE
        ))
      }
    }
  }
  
  cat("Generated", nrow(option_chain), "option contracts across all stocks and expirations.\
\
")
  
  # Filter options based on strategy criteria
  cat("Filtering options based on strategy criteria...\
")
  filtered_options <- option_chain[
    option_chain$probability_success >= probability_threshold &
    option_chain$annualized_return >= min_annualized_return &
    option_chain$days_to_expiry <= max_days_to_expiry &
    option_chain$wheel_return >= min_wheel_return,
  ]
  
  # Add additional metrics for ranking
  filtered_options$risk_score <- (1 - filtered_options$probability_success) * 100
  filtered_options$reward_score <- filtered_options$annualized_return
  filtered_options$risk_reward_ratio <- filtered_options$reward_score / filtered_options$risk_score
  
  # Create a combined score for ranking
  filtered_options$combined_score <- (filtered_options$probability_success * 5) + 
                                    (filtered_options$annualized_return / 5) + 
                                    (filtered_options$wheel_return / 10) + 
                                    (filtered_options$risk_reward_ratio / 2)
  
  # Sort by combined score
  filtered_options <- filtered_options[order(-filtered_options$combined_score), ]
  
  cat("Found", nrow(filtered_options), "options meeting our strategy criteria.\
\
")
  
  # Add news links for the filtered options
  if(nrow(filtered_options) > 0) {
    # Create news links for each ticker in the filtered options
    filtered_options$yahoo_finance <- paste0("https://finance.yahoo.com/quote/", filtered_options$ticker)
    filtered_options$seeking_alpha <- paste0("https://seekingalpha.com/symbol/", filtered_options$ticker)
    filtered_options$market_watch <- paste0("https://www.marketwatch.com/investing/stock/", tolower(filtered_options$ticker))
    filtered_options$cnbc <- paste0("https://www.cnbc.com/quotes/", filtered_options$ticker)
    
    # Display top filtered options with news links
    top_options <- head(filtered_options, 10)
    
    # Create a more readable display table
    display_table <- top_options[, c("ticker", "current_price", "strike_price", "expiration_date", 
                                    "days_to_expiry", "option_premium", "probability_success", 
                                    "annualized_return", "wheel_return", "risk_reward_ratio")]
    
    cat("Top Wheel Strategy Opportunities:\
")
    print(display_table)
    
    # Display news links for the top option
    cat("\
News and Information Links for", top_options$ticker[1], ":\
")
    cat("Yahoo Finance:", top_options$yahoo_finance[1], "\
")
    cat("Seeking Alpha:", top_options$seeking_alpha[1], "\
")
    cat("MarketWatch:", top_options$market_watch[1], "\
")
    cat("CNBC:", top_options$cnbc[1], "\
")
    
    # Create a detailed analysis for the top option
    top_option <- top_options[1,]
    
    cat("\
=== DETAILED ANALYSIS ===\
")
    cat("Top Recommendation:", top_option$ticker, "Put Option\
\
")
    cat("Current Price: $", top_option$current_price, "\
")
    cat("Strike Price: $", top_option$strike_price, " (", 
        round((1 - top_option$strike_price/top_option$current_price) * 100, 2), "% below current price)\
", sep="")
    cat("Expiration Date:", top_option$expiration_date, "(", top_option$days_to_expiry, "days)\
")
    cat("Option Premium: $", top_option$option_premium, " per share ($", 
        top_option$option_premium * 100, " per contract)\
", sep="")
    cat("Implied Volatility:", top_option$implied_volatility, "%\
")
    cat("Probability of Success:", round(top_option$probability_success * 100, 2), "%\
")
    cat("Annualized Return (if not assigned):", top_option$annualized_return, "%\
")
    cat("Potential Wheel Strategy Return:", top_option$wheel_return, "%\
")
    cat("Risk/Reward Ratio:", round(top_option$risk_reward_ratio, 2), "\
")
    
    # Calculate potential outcomes
    capital_required <- top_option$strike_price * 100
    premium_received <- top_option$option_premium * 100
    max_profit <- premium_received
    max_loss <- (top_option$strike_price * 100) - premium_received
    breakeven_price <- top_option$strike_price - top_option$option_premium
    
    cat("\
Trade Details (1 contract):\
")
    cat("Capital Required: $", capital_required, "\
", sep="")
    cat("Premium Received: $", premium_received, "\
", sep="")
    cat("Maximum Profit: $", max_profit, " (", round(max_profit/capital_required * 100, 2), "% return)\
", sep="")
    cat("Maximum Loss: $", max_loss, " (if stock goes to $0)\
", sep="")
    cat("Breakeven Price: $", breakeven_price, "\
", sep="")
    
    # Portfolio allocation
    max_contracts <- floor((total_capital * max_position_allocation) / capital_required)
    
    cat("\
Portfolio Allocation:\
")
    cat("Maximum Contracts (", max_position_allocation * 100, "% allocation): ", max_contracts, "\
", sep="")
    cat("Total Capital Required: $", max_contracts * capital_required, "\
", sep="")
    cat("Total Premium Received: $", max_contracts * premium_received, "\
", sep="")
    
    # Sector allocation check
    sector_options <- filtered_options[filtered_options$sector == top_option$sector, ]
    sector_allocation <- sum(sector_options$strike_price[1:min(3, nrow(sector_options))] * 100) / total_capital
    
    cat("\
Sector Allocation Check:\
")
    cat("Current", top_option$sector, "Allocation: ", round(sector_allocation * 100, 2), "%\
", sep=" ")
    cat("Maximum Sector Allocation: ", max_sector_allocation * 100, "%\
", sep="")
    
    if(sector_allocation > max_sector_allocation) {
      cat("WARNING: Adding this position would exceed maximum sector allocation.\
")
      cat("Consider diversifying into different sectors.\
")
    } else {
      cat("Sector allocation is within acceptable limits.\
")
    }
    
    # Risk management
    cat("\
Risk Management:\
")
    cat("Consider setting a stop loss at 50% of maximum loss: $", round(max_loss * 0.5, 2), "\
", sep="")
    cat("Consider taking profit at 50% of premium: $", round(premium_received * 0.5, 2), "\
", sep="")
    
    # Wheel strategy continuation
    cat("\
Wheel Strategy Continuation:\
")
    cat("If assigned at $", top_option$strike_price, ":\
", sep="")
    cat("- Potential covered call strike (2% OTM): $", round(top_option$strike_price * 1.02, 2), "\
", sep="")
    cat("- Estimated covered call premium: $", round(top_option$cc_premium, 2), " per share\
", sep="")
    cat("- Additional return from covered call: ", round((top_option$cc_premium / top_option$strike_price) * 100, 2), "%\
", sep="")
  } else {
    cat("No options found meeting the criteria. Try adjusting the parameters.\
")
  }
  
  # Return the filtered options
  return(filtered_options)
}

# Run the enhanced function with updated META price
result_meta_updated <- go_pikachu_enhanced()
