# e-commerce-dataset
Analysis of a real UK online retail dataset (2010-2011) containing 541,000 transactions. Explore customer behavior, sales patterns, and product performance through RFM analysis and time series forecasting.
# 🛍️ E-Commerce Analytics & Customer Segmentation

## 📊 Project Overview
Comprehensive analysis of e-commerce transaction data featuring customer segmentation using RFM analysis, sales forecasting, and business intelligence reporting. This project transforms raw transaction data into actionable business insights.

## 🎯 Business Objectives
- **Customer Segmentation**: Identify high-value customers and at-risk segments
- **Sales Forecasting**: Predict future revenue and order patterns  
- **Business Intelligence**: Analyze sales performance, product trends, and geographic distribution
- **Strategic Planning**: Develop targeted marketing strategies based on customer behavior

## 📈 Key Features

### 🔍 Data Analysis & Insights
- **Revenue Analytics**: Total revenue, average order value, product performance
- **Customer Analytics**: Customer lifetime value, geographic distribution, spending patterns
- **Product Analytics**: Best-selling products, price optimization, inventory insights
- **Time Series Analysis**: Sales trends, seasonal patterns, growth metrics

### 👥 Customer Segmentation (RFM Analysis)
- **Recency**: Days since last purchase
- **Frequency**: Total number of purchases  
- **Monetary**: Total customer spending
- **8 Customer Segments**: Champions, Loyal Customers, At Risk, Lost Customers, etc.

### 📊 Advanced Analytics
- **Time Series Forecasting**: ARIMA and ETS models for sales prediction
- **Statistical Modeling**: Customer behavior patterns and trend analysis
- **Data Visualization**: Interactive plots and business dashboards

## 🛠️ Technologies Used

### Programming & Analysis
- **R** (Primary analysis language)
- **RStudio** (Development environment)

### Core R Packages
```r
# Data Manipulation
library(tidyverse)    # Data wrangling and visualization
library(dplyr)        # Data manipulation
library(tidyr)        # Data cleaning

# Statistical Analysis
library(psych)        # Descriptive statistics
library(forecast)     # Time series forecasting
library(tseries)      # Time series analysis

# Visualization
library(ggplot2)      # Advanced plotting
library(scales)       # Formatting scales
library(gridExtra)    # Multiple plot arrangements

# Customer Analytics
library(lubridate)    # Date manipulation
library(cluster)      # Clustering algorithms
library(factoextra)   # Cluster visualization
