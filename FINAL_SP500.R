install.packages(c("quantmod", "dplyr", "ggplot2", "corrplot", "xts","ggfortify"))
library(quantmod)   # Fetch financial data
library(dplyr)      # Data manipulation
library(ggplot2)    # Visualization
library(corrplot)   # Correlation plots
library(xts)        # Time-series handling
library(ggfortify) 
# importing data
# bitcoin
getSymbols("BTC-USD", src = "yahoo", from = "2019-01-01")

bitcoin <- Ad(`BTC-USD`)  # Extract adjusted closing price

# gold
getSymbols("GLD", src = "yahoo", from = "2019-01-01")
gold <- Ad(GLD)  # Extract adjusted closing price

# usd
getSymbols("DTWEXBGS", src = "FRED", from = "2019-01-01")  # Correct symbol
usd <- DTWEXBGS
usd_clean <- na.omit(usd)

# SPY
getSymbols("SP500", src = "FRED", from = "2019-01-01")  # Correct symbol
spy <- SP500
spy_clean <- na.omit(spy)

# Federal Funds Rate (FRED: FEDFUNDS)
getSymbols("FEDFUNDS", src = "FRED", from = "2019-01-01")
fed_rate <- FEDFUNDS

# align and merge by date
# Aggregate to monthly data
bitcoin_monthly <- to.monthly(bitcoin, indexAt = "lastof")$bitcoin.Close
gold_monthly <- to.monthly(gold, indexAt = "lastof")$gold.Close

monthly_usdobj <- to.monthly(usd_clean, indexAt = "lastof")
usd_monthly <- monthly_usdobj$usd_clean.Close  # Use correct column name

monthly_fedobj <- to.monthly(FEDFUNDS, indexAt = "lastof")
fed_monthly <- monthly_fedobj$FEDFUNDS.Close  # Replace with your actual column name

monthly_spyobj <- to.monthly(spy_clean, indexAt = "lastof")
spy_monthly <- monthly_spyobj$spy_clean.Close  # Replace with your actual column name


# Merge and clean by date
merged_data <- merge(bitcoin_monthly, gold_monthly, usd_monthly, fed_monthly, spy_monthly)
colnames(merged_data) <- c("Bitcoin", "Gold", "USD_Index", "FedRate", "S&P500")
merged_data <- na.omit(merged_data)


cor(merged_data, use = "complete.obs")  # Correlation matrix
autoplot(merged_data)  # Plot trends

## new chart
# Calculate correlations and round to 2 decimals
cor_matrix <- cor(merged_data, use = "complete.obs")
cor_rounded <- round(cor_matrix, 2)

# Plot with adjusted scale and annotations
corrplot(cor_rounded, 
         method = "color", 
         type = "upper",
         tl.col = "black",  # Text label color
         addCoef.col = "black",  # Add correlation coefficients
         number.cex = 0.7,  # Coefficient font size
         col = colorRampPalette(c("blue", "white", "red"))(100))  # Color scale
  


# Standardize data (mean=0, sd=1)
merged_standardized <- scale(merged_data)

# Convert to dataframe for ggplot
df_standardized <- data.frame(
  Date = index(merged_data),
  merged_standardized
)

# Plot standardized trends
ggplot(df_standardized, aes(x = Date)) +
  geom_line(aes(y = Bitcoin, color = "Bitcoin"), linewidth = 0.8) +
  geom_line(aes(y = Gold, color = "Gold"), linewidth = 0.8) +  
  geom_line(aes(y = USD_Index, color = "USD Index"), linewidth = 0.8) +
  geom_line(aes(y = FedRate, color = "FedRate"), linewidth = 0.8) +
  geom_line(aes(y = S.P500, color = "S&P500"), linewidth = 0.8) +
  labs(title = "Standardized Trends (2019-2025)",
       y = "Standardized Value", x = "Year", 
       color = "Variable") +
  theme_minimal() +
  scale_color_manual(values = c("Bitcoin" = "orange2", 
                                "Gold" = "gold", 
                                "USD Index" = "skyblue", 
                                "FedRate" = "red",
                                "S&P500" = "green4")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),  # Centered title
    axis.title = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

