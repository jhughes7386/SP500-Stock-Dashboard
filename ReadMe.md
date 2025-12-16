# S&P 500 Data Visualization and Modeling Project

## Overview
This project explores the S&P 500 to better understand what it represents and how it behaves over time. My main goal was to learn more about the index in depth while improving my skills with **R Shiny** and **tree-based modeling**.  
I visualized stock trends, analyzed recent market performance, compared sectors, and built a **Random Forest model** to explore predictability using the data.

## Objective / Purpose
The purpose of this project was to:
- Learn the structure and behavior of the S&P 500 using real market data.
- Explore **data visualization** techniques with Shiny and ggplot2.
- Practice **modeling with Random Forests** to test how well short-term market data can be used for prediction.
- Experiment with **interactive visuals** that show both individual and sector-level performance.

## Tools & Technologies
- **Language:** R  
- **Framework:** Shiny  
- **Libraries:** ggplot2, dplyr, plotly, randomForest, batchgetsymbols  
- **Tools:** RStudio, GitHub  

### About the `batchgetsymbols` Library
`batchgetsymbols` is an R package designed for downloading financial data (mainly from **Yahoo Finance**) in bulk.  
It lets you pull multiple stock tickers at once, automatically clean missing values, and handle adjustments for dividends and splits. This made it easy to collect the last 5 years of stock prices and returns for all S&P 500 companies.

## Dataset
**Source:** Downloaded using the `batchgetsymbols` library (Yahoo Finance).  
**Time Period:** Past 5 years of historical data.  
**Description:**  
- Company name and sector  
- Adjusted closing prices  
- Cumulative returns  
- Volatility measures  
- Market performance over time

## Visualizations
Interactive R Shiny dashboard including:
- Two **line charts** over time showing adjusted stock price and cumulative return.  
- A **sector performance line chart** for comparison across all sectors.  
- **Bar charts** showing individual company performance over time.  
- Options to change the **time period**, **select sectors**, and **filter by stock symbol**.

## Modeling
A **Random Forest** model was used to explore patterns in S&P 500 performance.  
While training, I noticed strong signs of **overfitting** due to market randomness and limited time span.  
This highlighted how hard it is to model stock data even with clean inputs.  
To address this, I experimented with:
- **Feature importance** analysis  
- **Ensembling** ideas (combining multiple models for more stable predictions)  
- Adjusting hyperparameters and time-based sampling

## Key Insights
1. The S&P 500 is **extremely variable** and difficult to predict — even within a 5-year window, short-term movements are inconsistent.  
2. **Model building is challenging** due to overfitting and volatility, but feature engineering and ensemble techniques can make the models more robust.

## How to Run the Project

1. **Clone this repository:**
   ```bash
   git clone https://github.com/jhughes7386/SP500-DataVis.git
