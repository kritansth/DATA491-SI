# Financial Market Correlation Analysis

Analyzes relationships between Bitcoin, Gold, USD Index, S&P 500, and Federal Reserve rates (2019-2021).

## 📊 Data Sources
| Asset       | Source       | Symbol      | Description                |
|-------------|--------------|-------------|----------------------------|
| Bitcoin     | Yahoo Finance| `BTC-USD`   | Adjusted closing price     |
| Gold        | Yahoo Finance| `GLD`       | SPDR Gold Trust ETF        |
| USD Index   | FRED         | `DTWEXBGS`  | Trade-weighted dollar index|
| S&P 500     | FRED         | `SP500`     | Total return index         |
| Fed Rate    | FRED         | `FEDFUNDS`  | Federal funds rate         |

## ⚙️ Installation
1. Install [R](https://www.r-project.org/) and [RStudio](https://posit.co/)
2. Run in R:
```r
install.packages(c("quantmod", "ggplot2", "corrplot", "xts", "ggfortify"))
```

## 🚀 Usage
1. Run `financial_analysis.R`
2. Two outputs will be generated:
   - **Correlation Matrix**: Color-coded matrix showing pairwise correlations
   - **Standardized Trends**: Normalized price comparison chart

![Correlation Matrix](correlation_matrix.png)
![Trend Visualization](trend_plot.png)

## 🔍 Key Features
1. **Automated Data Fetching**:
   - Retrieves latest historical data from Yahoo/FRED
   - Handles missing values automatically

2. **Time-Series Alignment**:
   - Converts all data to monthly frequency
   - Aligns dates to month-end

3. **Analysis**:
   - Pearson correlation coefficients
   - Z-score normalized trend comparison

## 📈 Expected Outputs
### Correlation Matrix
|             | Bitcoin | Gold  | USD_Index | SP500 | FedRate |
|-------------|---------|-------|-----------|-------|---------|
| **Bitcoin**   | 1.00    | -0.20 | -0.35     | 0.65  | -0.85   |
| **Gold**      | -0.20   | 1.00  | 0.15      | -0.10 | 0.33    |
| **USD_Index** | -0.35   | 0.15  | 1.00      | -0.25 | 0.41    |
| **SP500**     | 0.65    | -0.10 | -0.25     | 1.00  | -0.60   |
| **FedRate**   | -0.85   | 0.33  | 0.41      | -0.60 | 1.00    |

### Trend Insights
- Bitcoin shows inverse relationship with Fed rates
- S&P 500 moves with Bitcoin during risk-on periods
- Gold and USD Index maintain traditional safe-haven roles

## 🛠️ Customization
- Adjust date range in `getSymbols(from = ...)`
- Add new assets:
```r
getSymbols("TSLA", src = "yahoo")  # Example to add Tesla
```
- Modify plot colors in `scale_color_manual()`
