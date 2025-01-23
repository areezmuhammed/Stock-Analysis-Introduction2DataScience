library(ggplot2)
library(dplyr)
library(tidyr)
library(reshape2)  # For heatmap preparation
library(scales)  # For better axis formatting
library(zoo)  # 

apple_data <- read.csv("C:\\Users\\Areez\\Downloads\\STock\\AAPL.csv")
costco_data <- read.csv("C:\\Users\\Areez\\Downloads\\STock\\COST.csv")  # Costco stock data
banf_data <- read.csv("C:\\Users\\Areez\\Downloads\\STock\\BANF.csv") 

View(costco_data)

# Step 2: Inspect the data
head(apple_data)
head(costco_data)
head(banf_data)

tail(apple_data)
tail(costco_data)
tail(banf_data)

# Step 3: Convert Date columns to Date type
apple_data$Date <- as.Date(apple_data$Date, format = "%d-%m-%Y")
costco_data$Date <- as.Date(costco_data$Date, format = "%d-%m-%Y")
banf_data$Date <- as.Date(banf_data$Date, format = "%d-%m-%Y")

# Step 4: Calculate daily price range (High - Low) for each stock
apple_data <- apple_data %>% mutate(Price_Range = High - Low)
costco_data <- costco_data %>% mutate(Price_Range = High - Low)
banf_data <- banf_data %>% mutate(Price_Range = High - Low)

# Step 5: Visualize price trends over time for each stock
# Apple
ggplot(apple_data, aes(x = Date)) +
  geom_line(aes(y = High, color = "High")) +
  geom_line(aes(y = Low, color = "Low")) +
  geom_line(aes(y = Close, color = "Close")) +
  labs(title = "Apple Price Volatility Over Time",
       x = "Date",
       y = "Price",
       color = "Legend") +
  theme_minimal()
ggsave("apple_price_volatility.png")


# For more precise info , we are filerting the dates and including after 2005. 


#Step 4: Filter data for dates after 2005
apple_data <- apple_data %>% filter(Date >= as.Date("2005-01-01"))
costco_data <- costco_data %>% filter(Date >= as.Date("2005-01-01"))
banf_data <- banf_data %>% filter(Date >= as.Date("2005-01-01"))

# Step 5: Calculate daily price range (High - Low) for each stock
apple_data <- apple_data %>% mutate(Price_Range = High - Low)
costco_data <- costco_data %>% mutate(Price_Range = High - Low)
banf_data <- banf_data %>% mutate(Price_Range = High - Low)

# Apple 

ggplot(apple_data, aes(x = Date)) +
  geom_line(aes(y = High, color = "High")) +
  geom_line(aes(y = Low, color = "Low")) +
  geom_line(aes(y = Close, color = "Close")) +
  labs(title = "Apple Price Volatility Over Time",
       x = "Date",
       y = "Price",
       color = "Legend") +
  theme_minimal()
ggsave("apple_price_volatility.png")

#Costco 

ggplot(costco_data, aes(x = Date)) +
  geom_line(aes(y = High, color = "High")) +
  geom_line(aes(y = Low, color = "Low")) +
  geom_line(aes(y = Close, color = "Close")) +
  labs(title = "Costco Price Volatility Over Time",
       x = "Date",
       y = "Price",
       color = "Legend") +
  theme_minimal()
ggsave("costco_price_volatility.png")


# BancFirst
ggplot(banf_data, aes(x = Date)) +
  geom_line(aes(y = High, color = "High")) +
  geom_line(aes(y = Low, color = "Low")) +
  geom_line(aes(y = Close, color = "Close")) +
  labs(title = "BancFirst Price Volatility Over Time",
       x = "Date",
       y = "Price",
       color = "Legend") +
  theme_minimal()
ggsave("banf_price_volatility.png")

# Step 7: Correlation heatmap for each stock
# Apple
apple_corr <- cor(apple_data[, c("Low", "Open", "High", "Close", "Adjusted.Close", "Volume")], use = "complete.obs")
apple_corr_melt <- melt(apple_corr)
ggplot(data = apple_corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1)) +
  labs(title = "Apple Stock Correlation Heatmap",
       x = "Metric",
       y = "Metric") +
  coord_fixed()
ggsave("apple_correlation_heatmap.png")

getwd()

# Costco
costco_corr <- cor(costco_data[, c("Low", "Open", "High", "Close", "Adjusted.Close", "Volume")], use = "complete.obs")
costco_corr_melt <- melt(costco_corr)
ggplot(data = costco_corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1)) +
  labs(title = "Costco Stock Correlation Heatmap",
       x = "Metric",
       y = "Metric") +
  coord_fixed()
ggsave("costco_correlation_heatmap.png")

# BancFirst
banf_corr <- cor(banf_data[, c("Low", "Open", "High", "Close", "Adjusted.Close", "Volume")], use = "complete.obs")
banf_corr_melt <- melt(banf_corr)
ggplot(data = banf_corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   hjust = 1)) +
  labs(title = "BancFirst Stock Correlation Heatmap",
       x = "Metric",
       y = "Metric") +
  coord_fixed()
ggsave("banf_correlation_heatmap.png")

# Step 6: Analyze the impact of the 2008 financial crisis
# Define the crash period
crash_period <- as.Date("2007-01-01")
recovery_period <- as.Date("2009-12-31")

apple_crash <- apple_data %>% filter(Date >= crash_period & Date <= recovery_period)
costco_crash <- costco_data %>% filter(Date >= crash_period & Date <= recovery_period)
banf_crash <- banf_data %>% filter(Date >= crash_period & Date <= recovery_period)

# Visualize the crash period for each stock
# Apple during 2008 crash
ggplot(apple_crash, aes(x = Date)) +
  geom_line(aes(y = Close), color = "red") +
  labs(title = "Apple Stock During 2008 Financial Crisis",
       x = "Date",
       y = "Close Price") +
  theme_minimal()
ggsave("apple_2008_crash.png")

# Costco during 2008 crash
ggplot(costco_crash, aes(x = Date)) +
  geom_line(aes(y = Close), color = "blue") +
  labs(title = "Costco Stock During 2008 Financial Crisis",
       x = "Date",
       y = "Close Price") +
  theme_minimal()
ggsave("costco_2008_crash.png")

# BancFirst during 2008 crash
ggplot(banf_crash, aes(x = Date)) +
  geom_line(aes(y = Close), color = "green") +
  labs(title = "BancFirst Stock During 2008 Financial Crisis",
       x = "Date",
       y = "Close Price") +
  theme_minimal()
ggsave("banf_2008_crash.png")

# Summarize the crash period statistics for each stock
crash_summary <- rbind(
  apple_crash %>% summarise(Stock = "Apple", Avg_Close = mean(Close, na.rm = TRUE), Avg_Volatility = mean(Price_Range, na.rm = TRUE)),
  costco_crash %>% summarise(Stock = "Costco", Avg_Close = mean(Close, na.rm = TRUE), Avg_Volatility = mean(Price_Range, na.rm = TRUE)),
  banf_crash %>% summarise(Stock = "BancFirst", Avg_Close = mean(Close, na.rm = TRUE), Avg_Volatility = mean(Price_Range, na.rm = TRUE))
)
crash_summary
write.csv(crash_summary, "crash_summary.csv", row.names = FALSE)

# Step 6: Refined Correlation Heatmap for Each Stock
# Add derived variables like percentage change and adjusted metrics
apple_data <- apple_data %>% mutate(
  Daily_Return = (Close - lag(Close)) / lag(Close),
  Volatility_Percentage = Price_Range / lag(Close)
)
costco_data <- costco_data %>% mutate(
  Daily_Return = (Close - lag(Close)) / lag(Close),
  Volatility_Percentage = Price_Range / lag(Close)
)
banf_data <- banf_data %>% mutate(
  Daily_Return = (Close - lag(Close)) / lag(Close),
  Volatility_Percentage = Price_Range / lag(Close)
)

apple_corr <- cor(apple_data[, c("Low", "High", "Close", "Volume", "Daily_Return", "Volatility_Percentage")], use = "complete.obs")
apple_corr_melt <- melt(apple_corr)
ggplot(data = apple_corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Refined Apple Stock Correlation Heatmap",
       x = "Metric",
       y = "Metric") +
  coord_fixed()
ggsave("refined_apple_correlation_heatmap.png")

# Costco Correlation
costco_corr <- cor(costco_data[, c("Low", "High", "Close", "Volume", "Daily_Return", "Volatility_Percentage")], use = "complete.obs")
costco_corr_melt <- melt(costco_corr)
ggplot(data = costco_corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Refined Costco Stock Correlation Heatmap",
       x = "Metric",
       y = "Metric") +
  coord_fixed()
ggsave("refined_costco_correlation_heatmap.png")

# BancFirst Correlation
banf_corr <- cor(banf_data[, c("Low", "High", "Close", "Volume", "Daily_Return", "Volatility_Percentage")], use = "complete.obs")
banf_corr_melt <- melt(banf_corr)
ggplot(data = banf_corr_melt, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "blue", high = "red", mid = "yellow", midpoint = 0, 
                       limit = c(-1, 1), space = "Lab", 
                       name = "Correlation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  labs(title = "Refined BancFirst Stock Correlation Heatmap",
       x = "Metric",
       y = "Metric") +
  coord_fixed()
ggsave("refined_banf_correlation_heatmap.png")

#####3
library(gridExtra)
library(ggplot2)

# Function to perform regression and display enhanced outputs
run_regression <- function(data, stock_name) {
  stock_data <- data %>% filter(Stock == stock_name)
  
  # Fit the linear model
  model <- lm(Volatility_Percentage ~ Volume, data = stock_data)
  summary_model <- summary(model)
  
  # Extract R-squared, adjusted R-squared, coefficients, and p-values
  r_squared <- summary_model$r.squared
  adj_r_squared <- summary_model$adj.r.squared
  coefficients <- summary_model$coefficients
  intercept <- coefficients[1, 1]
  slope <- coefficients[2, 1]
  p_value <- coefficients[2, 4]
  
  # Residual analysis
  residuals <- resid(model)
  
  # Print model details
  print(summary_model)  # Regression summary
  print(paste("R-squared:", r_squared))
  print(paste("Adjusted R-squared:", adj_r_squared))
  print(paste("Slope:", slope, "Intercept:", intercept))
  print(paste("P-value for Volume:", p_value))
  
  # Create the regression plot with R² value
  plot <- ggplot(stock_data, aes(x = Volume, y = Volatility_Percentage)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = paste(stock_name, "Regression: Volume vs. Volatility"),
      subtitle = paste("R² =", round(r_squared, 3), 
                       "| Adjusted R² =", round(adj_r_squared, 3), 
                       "| P-value =", signif(p_value, 3)),
      x = "Volume",
      y = "Volatility Percentage"
    ) +
    theme_minimal()
  
  return(list(plot = plot, residuals = residuals))
}

# Run regression for each stock and create plots
apple_results <- run_regression(combined_data, "Apple")
costco_results <- run_regression(combined_data, "Costco")
bancfirst_results <- run_regression(combined_data, "BancFirst")

# Extract plots
apple_plot <- apple_results$plot
costco_plot <- costco_results$plot
bancfirst_plot <- bancfirst_results$plot

# Combine all plots into a single visualization
combined_plot <- grid.arrange(
  apple_plot, costco_plot, bancfirst_plot,
  ncol = 1,
  top = "Regression Analysis: Volume vs. Volatility for Apple, Costco, and BancFirst"
)

# Save the combined plot
ggsave("combined_regression_analysis.png", plot = combined_plot, width = 10, height = 15, dpi = 300)

# Residual analysis (example for one stock, Apple)
residual_plot <- ggplot(data.frame(residuals = apple_results$residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Residual Analysis for Apple", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Save the residual plot
ggsave("apple_residual_analysis.png", plot = residual_plot, width = 8, height = 6, dpi = 300)

library(dplyr)
library(ggplot2)

# Assuming combined_data contains the cleaned and merged dataset
# combined_data already has columns: Volume, Volatility_Percentage, Stock, etc.

# Regression and Plotting Function
run_regression <- function(data, stock_name) {
  # Filter data for the given stock
  stock_data <- data %>% filter(Stock == stock_name)
  
  # Fit a linear regression model
  model <- lm(Volatility_Percentage ~ Volume, data = stock_data)
  
  # Print summary of the regression model
  print(paste("Regression Summary for", stock_name))
  print(summary(model))
  
  # Create the regression plot
  plot <- ggplot(stock_data, aes(x = Volume, y = Volatility_Percentage)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = paste(stock_name, "Regression: Volume vs. Volatility"),
      x = "Volume",
      y = "Volatility Percentage"
    ) +
    theme_minimal()
  
  # Print and save the plot
  print(plot)
  ggsave(paste0(stock_name, "_regression_volume_vs_volatility.png"), plot = plot, width = 10, height = 6, dpi = 300)
}

# Run regression for each stock
run_regression(combined_data, "Apple")
run_regression(combined_data, "Costco")
run_regression(combined_data, "BancFirst")

# Combined Regression Plot
combined_plot <- ggplot(combined_data, aes(x = Volume, y = Volatility_Percentage, color = Stock)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Regression Analysis: Volume vs. Volatility (All Stocks)",
    x = "Volume",
    y = "Volatility Percentage"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Apple" = "blue", "Costco" = "red", "BancFirst" = "green"))

# Print and save combined plot
print(combined_plot)
ggsave("combined_regression_volume_vs_volatility.png", plot = combined_plot, width = 10, height = 6, dpi = 300)
 ######################

library(dplyr)
library(ggplot2)

# Read datasets
apple_data <- read.csv("C:\\Users\\Areez\\Downloads\\STock\\AAPL.csv")
costco_data <- read.csv("C:\\Users\\Areez\\Downloads\\STock\\COST.csv")  # Costco stock data
bancfirst_data <- read.csv("C:\\Users\\Areez\\Downloads\\STock\\BANF.csv") 

# Preprocess and filter datasets
preprocess_data <- function(data, stock_name) {
  data %>%
    mutate(Date = as.Date(Date, format = "%d-%m-%Y"),
           Stock = stock_name,
           Volatility_Percentage = (High - Low) / lag(Close) * 100) %>%
    filter(Date >= as.Date("2005-01-01"))
}

apple_data <- preprocess_data(apple_data, "Apple")
costco_data <- preprocess_data(costco_data, "Costco")
bancfirst_data <- preprocess_data(bancfirst_data, "BancFirst")

# Combine all datasets
combined_data <- bind_rows(apple_data, costco_data, bancfirst_data)

# Linear regression and plotting function
plot_regression <- function(data, stock_name) {
  stock_data <- data %>% filter(Stock == stock_name)
  model <- lm(Volatility_Percentage ~ Volume, data = stock_data)
  print(summary(model))  # Print regression summary
  
  plot <- ggplot(stock_data, aes(x = Volume, y = Volatility_Percentage)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", se = TRUE, color = "red") +
    labs(
      title = paste(stock_name, "Regression: Volume vs. Volatility"),
      x = "Volume",
      y = "Volatility Percentage"
    ) +
    theme_minimal()
  
  print(plot)
  ggsave(paste0(stock_name, "_volume_vs_volatility_regression.png"), plot = plot, width = 10, height = 6, dpi = 300)
}

# Generate regression plots for each stock
plot_regression(combined_data, "Apple")
plot_regression(combined_data, "Costco")
plot_regression(combined_data, "BancFirst")

# Combined regression plots (Optional)
plot_combined_regression <- ggplot(combined_data, aes(x = Volume, y = Volatility_Percentage, color = Stock)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(
    title = "Regression Analysis: Volume vs. Volatility (All Stocks)",
    x = "Volume",
    y = "Volatility Percentage"
  ) +
  theme_minimal() +
  scale_color_manual(values = c("Apple" = "blue", "Costco" = "red", "BancFirst" = "green"))

print(plot_combined_regression)
ggsave("combined_volume_vs_volatility_regression.png", plot = plot_combined_regression, width = 10, height = 6, dpi = 300)
 ##
library(gridExtra)
library(ggplot2)

# Function to perform regression and display enhanced outputs
run_regression <- function(data, stock_name) {
  stock_data <- data %>% filter(Stock == stock_name)
  
  # Fit the linear model
  model <- lm(Volatility_Percentage ~ Volume, data = stock_data)
  summary_model <- summary(model)
  
  # Extract R-squared, adjusted R-squared, coefficients, and p-values
  r_squared <- summary_model$r.squared
  adj_r_squared <- summary_model$adj.r.squared
  coefficients <- summary_model$coefficients
  intercept <- coefficients[1, 1]
  slope <- coefficients[2, 1]
  p_value <- coefficients[2, 4]
  
  # Residual analysis
  residuals <- resid(model)
  
  # Print model details
  print(summary_model)  # Regression summary
  print(paste("R-squared:", r_squared))
  print(paste("Adjusted R-squared:", adj_r_squared))
  print(paste("Slope:", slope, "Intercept:", intercept))
  print(paste("P-value for Volume:", p_value))
  
  # Create the regression plot with R² value
  plot <- ggplot(stock_data, aes(x = Volume, y = Volatility_Percentage)) +
    geom_point(alpha = 0.5, color = "blue") +
    geom_smooth(method = "lm", color = "red", se = TRUE) +
    labs(
      title = paste(stock_name, "Regression: Volume vs. Volatility"),
      subtitle = paste("R² =", round(r_squared, 3), 
                       "| Adjusted R² =", round(adj_r_squared, 3), 
                       "| P-value =", signif(p_value, 3)),
      x = "Volume",
      y = "Volatility Percentage"
    ) +
    theme_minimal()
  
  return(list(plot = plot, residuals = residuals))
}

# Run regression for each stock and create plots
apple_results <- run_regression(combined_data, "Apple")
costco_results <- run_regression(combined_data, "Costco")
bancfirst_results <- run_regression(combined_data, "BancFirst")

# Extract plots
apple_plot <- apple_results$plot
costco_plot <- costco_results$plot
bancfirst_plot <- bancfirst_results$plot

# Combine all plots into a single visualization
combined_plot <- grid.arrange(
  apple_plot, costco_plot, bancfirst_plot,
  ncol = 1,
  top = "Regression Analysis: Volume vs. Volatility for Apple, Costco, and BancFirst"
)

# Save the combined plot
ggsave("combined_regressionn_analysis.png", plot = combined_plot, width = 10, height = 15, dpi = 300)

# Residual analysis (example for one stock, Apple)
residual_plot <- ggplot(data.frame(residuals = apple_results$residuals), aes(x = residuals)) +
  geom_histogram(bins = 30, fill = "blue", alpha = 0.7) +
  labs(title = "Residual Analysis for Apple", x = "Residuals", y = "Frequency") +
  theme_minimal()

# Save the residual plot
ggsave("apple_residual_analysis.png", plot = residual_plot, width = 8, height = 6, dpi = 300)

