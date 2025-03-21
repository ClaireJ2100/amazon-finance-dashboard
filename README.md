# Amazon Financial Insights Dashboard

## Motivation:
**Target audience:** The dashboard is designed for investors, industry professionals, and anyone who is interested in Amazon’s financial performance.

Amazon is one of the world’s most influential companies, and its financial report attracts significant attention from investors and business analysts. Understanding key financial metrics is crucial for evaluating Amazon’s performance, profitability, and growth trends. However, traditional financial reports can be complex and difficult to interpret at a glance. 

To make the financial dashboard more accessible and insightful, this Shiny-powered dashboard provided an interactive visualization of Amazon’s key financial metrics.

## Key features:
- Revenue & Net Income Trends 📈 -> Track Amazon’s financial growth across quarters and years.
- Profit and Cost Breakdown 💰 -> Visualize Amazon’s operating expenses, and income.
- Key Metrics 📊 -> Gain insights into Total Revenue, Gross Profit, and Operating Income.
- Growth Rate 📈 -> Track Amazon's growth rate across quaters and years.
- Dynamic Filtering 🕵 -> Users can explore different time periods (years & quarters) to gain deeper insights.

## Dataset: Amazon Income Statement

The financial data used in this dashboard is sourced from Alpha Vantage, a widely recognized platform that offers real-time and historical market data, stock prices, and company financial statements. The data was retrieved using the Alpha Vantage API and saved locally under the “data/raw” folder.


## App description

### Live Dashboard
This Shiny dashboard is deployed on **Posit Connect Cloud**:

[Click here to view the app](https://0195babc-df20-0298-ff8f-d26e52e0ef0a.share.connect.posit.cloud/)

### Demo Walkthrough
Watch the full video walkthrough here:  
[Watch Demo Video](img/demo.mp4)

**There are two filters, two mode selectors, three cards, two line charts, and one stacked bar chart, which meeet the challenge requirements.**

## Installation instructions
1. Clone the git repository from GitHub

In your terminal, please run the following command:

```bash
https://github.com/ClaireJ2100/amazon-finance-dashboard.git
```

2. Environment setup

Before running the app, you need to intall necessary packeages: 

- Install R and RStudio (if not installed)
- Install renv package: `install.packages("renv")`
- Run `renv::restore()` to install the required packages


3.  Running the Shiny app locally

Start the app: `shiny::runApp("app.R")`


You can view the Shiny app by navigating to the address `http://127.0.0.1:6150` in your browser.

