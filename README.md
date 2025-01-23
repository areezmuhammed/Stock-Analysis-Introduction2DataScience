# Stock Analysis and Visualization

This project focuses on analyzing stock market data for Apple, Costco, and BancFirst using R. It includes data preprocessing, visualizations, correlation analysis, regression analysis, and insights during the 2008 financial crisis.

## Repository Overview

GitHub Repository: https://github.com/areezmuhammed/Stock-Analysis-Introduction2DataScience.git

This repository contains all the scripts, datasets, and outputs related to the stock analysis project. Clone or download this repository to explore the code and results.

## Project Features

- **Data Loading and Inspection**: Reads CSV files for Apple, Costco, and BancFirst stock data, inspects the structure and statistics.
- **Data Preprocessing**: Converts date columns, calculates daily price ranges, and filters data for specific time periods.
- **Visualizations**:
  - Price trends over time for each stock.
  - Correlation heatmaps for stock metrics.
  - Regression analysis of Volume vs. Volatility.
- **Financial Crisis Analysis**: Analyzes stock performance during the 2008 financial crisis.
- **Residual Analysis**: Examines the residuals from regression models for insights into model fit.

## Setup Instructions

### Prerequisites

- R (version 4.0 or higher)
- Required Libraries:
  - `ggplot2`
  - `dplyr`
  - `tidyr`
  - `reshape2`
  - `scales`
  - `zoo`
  - `gridExtra`

Install the libraries using the following command if not already installed:

```R
install.packages(c("ggplot2", "dplyr", "tidyr", "reshape2", "scales", "zoo", "gridExtra"))
```

### Data Files

Place the stock data CSV files (`AAPL.csv`, `COST.csv`, `BANF.csv`) in your working directory. Update file paths in the script if necessary.

### Running the Code

1. Clone this repository:

   ```bash
   git clone https://github.com/areezmuhammed/Stock-Analysis-Introduction2DataScience.git
   ```

2. Open the R script in an R editor (e.g., RStudio).
3. Update the file paths to match your local directory structure if necessary.
4. Run the script to:
   - Load and preprocess data.
   - Generate visualizations and save plots.
   - Perform correlation and regression analyses.

## Outputs

### Visualizations

- `apple_price_volatility.png`: Apple stock price trends.
- `costco_price_volatility.png`: Costco stock price trends.
- `banf_price_volatility.png`: BancFirst stock price trends.
- `apple_correlation_heatmap.png`: Apple stock correlation heatmap.
- `costco_correlation_heatmap.png`: Costco stock correlation heatmap.
- `banf_correlation_heatmap.png`: BancFirst stock correlation heatmap.
- `apple_2008_crash.png`: Apple stock during the 2008 crash.
- `costco_2008_crash.png`: Costco stock during the 2008 crash.
- `banf_2008_crash.png`: BancFirst stock during the 2008 crash.
- `combined_regression_analysis.png`: Combined regression analysis plot for all stocks.
- `apple_residual_analysis.png`: Residual analysis for Apple.

### Data Outputs

- `crash_summary.csv`: Summary statistics during the 2008 financial crash.

## Key Functions

- **Data Preprocessing**:
  - Filters data for dates after 2005.
  - Computes daily price ranges.
- **Visualization**:
  - Creates line plots for stock trends.
  - Generates correlation heatmaps using `ggplot2`.
- **Regression Analysis**:
  - Explores the relationship between Volume and Volatility Percentage.
  - Saves regression plots with R² values and p-values.

## Insights

- Stock trends and volatility can be effectively visualized over time.
- Correlation heatmaps provide insights into relationships between stock metrics.
- Regression analysis identifies how trading volume impacts volatility.
- Residual analysis evaluates the quality of regression models.

## Contributing

Contributions are welcome! Feel free to fork the repository and submit a pull request for any improvements or additional features.


