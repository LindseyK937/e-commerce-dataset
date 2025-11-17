
#load the necessary libraries
library(rio)
library(tidyverse)
library(psych)
library(dplyr)
library(ggplot2)

#load the dataset
commerce_data <- read.csv("C:/Users/LINDSEY KHALUMBA/OneDrive/Desktop/commerce_data.csv")
View(commerce_data)
str(commerce_data)

#check for missing values
is.na(commerce_data)

# Create the line_total column
commerce_data$line_total <- commerce_data$quantity * commerce_data$unit_price

# Run verification code:
print("Column names in commerce_data:")
print(names(commerce_data))

print("First few rows with line_total:")
print(head(commerce_data))

# calculating total revenue
total_revenue <- sum(commerce_data$line_total)
print(paste("Total Revenue: $", round(total_revenue, 2)))
# average Revenue per Invoice
avg_rev_per_invoice <- commerce_data %>%
  group_by(invoice_no) %>%
  summarise(invoice_total = sum(line_total)) %>%
  ungroup() %>%
  summarise(avg = mean(invoice_total))
print(paste("Average Revenue per Invoice: $", round(avg_rev_per_invoice$avg, 2)))

# total quantity sold
total_quantity <- sum(commerce_data$quantity)
print(paste("Total Quantity Sold:", total_quantity))

#top 10 best-selling products by quantity
top_products_qty <- commerce_data %>%
  group_by(stock_code, description) %>%
  summarise(total_quantity = sum(quantity)) %>%
  arrange(desc(total_quantity)) %>%
  head(10)
print("Top 10 Products by Quantity:")
print(top_products_qty)

#top 10 products by revenue
top_products_rev <- commerce_data %>%
  group_by(stock_code, description) %>%
  summarise(total_revenue = sum(line_total)) %>%
  arrange(desc(total_revenue)) %>%
  head(10)
print("Top 10 Products by Revenue:")
print(top_products_rev)

#number of unique customers
unique_customers <- n_distinct(commerce_data$customer_id)
print(paste("Number of Unique Customers:", unique_customers))

#number of sales by country
sales_by_country <- commerce_data %>%
  group_by(country) %>%
  summarise(
    total_revenue = sum(line_total),
    number_of_invoices = n_distinct(invoice_no)
  ) %>%
  arrange(desc(total_revenue))
print(sales_by_country)

#top 5 customers by spending
top_customers <- commerce_data %>%
  group_by(customer_id) %>%
  summarise(total_spent = sum(line_total)) %>%
  arrange(desc(total_spent)) %>%
  head(5)
print("Top 5 Customers by Spending:")
print(top_customers)

#average product price
avg_price <- mean(commerce_data$unit_price)
print(paste("Average Product Price: $", round(avg_price, 2)))

#range of product prices
price_range <- range(commerce_data$unit_price)
print(paste("Price Range: $", round(price_range[1], 2), "- $", round(price_range[2], 2)))

#average number of items per invoice
avg_items_per_invoice <- commerce_data %>%
  group_by(invoice_no) %>%
  summarise(items_in_invoice = sum(quantity)) %>%
  ungroup() %>%
  summarise(avg_items = mean(items_in_invoice))
print(paste("Average Number of Items per Invoice:", round(avg_items_per_invoice$avg_items, 1)))

#visualization of the distribution of invoice sizes
invoice_sizes <- commerce_data %>%
  group_by(invoice_no) %>%
  summarise(items_in_invoice = sum(quantity))
ggplot(invoice_sizes, aes(x = items_in_invoice)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Number of Items per Invoice",
       x = "Number of Items in Invoice",
       y = "Count of Invoices") +
  theme_minimal()

#load necessary libraries for RSF analysis
library(lubridate)
library(scales)
library(cluster)
library(factoextra)
library(gridExtra)

# Set theme for better visualizations
theme_set(theme_minimal())

#Calculate RFM
set.seed(123)
n_customers <- 1000

commerce_data<- data.frame(
  customer_id = paste0("C", 1:n_customers),
  invoice_date = sample(seq(as.Date('2023-01-01'), as.Date('2024-01-15'), by="day"), n_customers, replace=TRUE),
  invoice_no = round(runif(n_customers, 10, 500), 2),
  unit_price = paste0("O", 1:n_customers)
)

print(colnames(commerce_data))

# Convert invoice_no to numeric and handle any conversion issues
commerce_data <- commerce_data %>%
  mutate(invoice_no = as.numeric(as.character(invoice_no)))

# Check for NA values after conversion
sum(is.na(commerce_data$invoice_no))

# Remove rows with NA invoice_no before RFM analysis
commerce_data_clean <- commerce_data %>%
  filter(!is.na(invoice_no))
# Check how many rows remain
nrow(commerce_data_clean)

# Calculate RFM metrics
rfm_data <- commerce_data_clean%>%
  group_by(customer_id) %>%
  summarise(
    Recency = (max(invoice_no, na.rm = TRUE) - max(commerce_data$invoice_no, na.rm = TRUE)) * -1,
    Frequency = n_distinct(invoice_no),
    Monetary = sum(quantity*unit_price)
  ) %>% 
  mutate(
    R_Score = ntile(desc(Recency), 5),
    F_Score = ntile(Frequency, 5),
    M_Score = ntile(Monetary, 5),
    RFM_Score = paste(R_Score, F_Score, M_Score, sep = ""),
    RFM_Segment = case_when(
      R_Score >= 4 & F_Score >= 4 & M_Score >= 4 ~ "Champions",
      R_Score >= 3 & F_Score >= 3 & M_Score >= 3 ~ "Loyal Customers",
      R_Score >= 4 & F_Score >= 2 & M_Score >= 2 ~ "Potential Loyalists",
      R_Score >= 3 & F_Score >= 1 & M_Score >= 1 ~ "New Customers",
      R_Score >= 2 & F_Score >= 2 & M_Score >= 2 ~ "Promising",
      R_Score >= 2 & F_Score >= 1 & M_Score >= 1 ~ "Need Attention",
      R_Score >= 1 & F_Score >= 1 & M_Score >= 1 ~ "At Risk",
      TRUE ~ "Lost Customers"
    )
  )
# View the RFM results
print(head(rfm_data))    

#Segment analysis
segment_analysis <- rfm_data %>%
  group_by(RFM_Segment) %>%
  summarise(
    avg_recency = mean(Recency),
    avg_frequency = mean(Frequency),
    avg_monetary = mean(Monetary),
    n_customers = n(),
    percent = n() / nrow(rfm_data) * 100
  ) %>%
  arrange(desc(avg_monetary))

print(segment_analysis)

#Create an action plan for each segment
segment_strategies <- rfm_data %>%
  mutate(
    recommended_action = case_when(
      RFM_Segment == "Champions" ~ "Reward them, ask for reviews, premium offers",
      RFM_Segment == "Loyal Customers" ~ "Upsell, loyalty programs, referral incentives",
      RFM_Segment == "Potential Loyalists" ~ "Engage with personalized content, nurture relationships",
      RFM_Segment == "New Customers" ~ "Welcome series, onboarding, educational content",
      RFM_Segment == "Promising" ~ "Re-engagement campaigns, special offers",
      RFM_Segment == "Need Attention" ~ "Win-back campaigns, surveys to understand drop-off",
      RFM_Segment == "At Risk" ~ "Aggressive win-back, special discounts",
      RFM_Segment == "Lost Customers" ~ "Reactivation campaigns or stop marketing to save costs"
    )
  )
# View customer counts with recommended actions
action_summary <- segment_strategies %>%
  group_by(RFM_Segment, recommended_action) %>%
  summarise(n_customers = n()) %>%
  arrange(desc(n_customers))

print(action_summary)

#Visualize the results
# Bar plot of segments
ggplot(segment_summary, aes(x = reorder(RFM_Segment, n), y = n, fill = RFM_Segment)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Customer Distribution by RFM Segment",
       x = "RFM Segment", 
       y = "Number of Customers") +
  theme_minimal()
# scatter plot for monetary vs frequency colored by recency
ggplot(rfm_data, aes(x = Frequency, y = Monetary, color = as.factor(R_Score))) +
  geom_point(alpha = 0.6) +
  scale_color_brewer(palette = "RdYlGn", name = "Recency Score") +
  labs(title = "RFM Analysis: Frequency vs Monetary Value",
       x = "Frequency", 
       y = "Monetary Value") +
  theme_minimal()
# Top 10 customers by monetary value
top_customers <- rfm_data %>%
  arrange(desc(Monetary)) %>%
  head(10)
print("Top 10 Customers by Spending:")
print(top_customers)
# Customers needing immediate attention (high value but at risk)
at_risk_valuable <- rfm_data %>%
  filter(RFM_Segment %in% c("At Risk", "Need Attention")) %>%
  arrange(desc(Monetary)) %>%
  head(10)
print("High-Value Customers At Risk:")
print(at_risk_valuable)

#Time series forecasting
#Loading required packages
library(forecast)
library(tseries)
library(lubridate)

# Convert invoice_date to proper date format and extract daily sales
time_series_data <- commerce_data_clean %>%
  mutate(
    invoice_date = as.Date(invoice_date, format = "%d/%m/%Y"),
    total_sales = quantity * unit_price
  ) 
# Check for NA dates and date range
print("Date range and NA check:")
print(summary(time_series_data$invoice_date))
print(paste("NA dates:", sum(is.na(time_series_data$invoice_date))))
print(paste("Min date:", min(time_series_data$invoice_date, na.rm = TRUE)))
print(paste("Max date:", max(time_series_data$invoice_date, na.rm = TRUE)))

# Remove rows with NA dates first
time_series_data_clean <- commerce_data_clean %>%
  mutate(
    invoice_date = as.Date(invoice_date, format = "%d/%m/%Y")
  ) %>%
  filter(!is.na(invoice_date)) 
print(paste("Rows after removing NA dates:", nrow(time_series_data_clean)))
#create weekly aggregation 
weekly_data <- time_series_data_clean %>%
  mutate(
    total_sales = quantity * unit_price,
    year_week = floor_date(invoice_date, "week")  # Group by week starting Monday
  ) %>%
  group_by(year_week) %>%
  summarise(
    weekly_sales = sum(total_sales, na.rm = TRUE),
    weekly_orders = n_distinct(invoice_no),
    weekly_customers = n_distinct(customer_id),
    avg_order_value = weekly_sales / weekly_orders,
    active_days = n_distinct(invoice_date)  # How many days had sales each week
  ) %>%
  arrange(year_week)

# Check the weekly data
print(head(weekly_data))
print(paste("Total weeks of data:", nrow(weekly_data)))
print(paste("Date range:", min(weekly_data$year_week), "to", max(weekly_data$year_week)))

# Create weekly time series objects
weekly_sales_ts <- ts(weekly_data$weekly_sales, frequency = 52)
weekly_orders_ts <- ts(weekly_data$weekly_orders, frequency = 52)

# Plot the weekly series
par(mfrow = c(2, 1))
plot(weekly_sales_ts, main = "Weekly Sales Time Series", ylab = "Sales Amount", col = "blue")
plot(weekly_orders_ts, main = "Weekly Orders Time Series", ylab = "Number of Orders", col = "darkgreen")

# ARIMA model forecasting for weekly data
arima_sales_weekly <- auto.arima(weekly_sales_ts, seasonal = TRUE)
arima_orders_weekly <- auto.arima(weekly_orders_ts, seasonal = TRUE)

print("Weekly ARIMA Sales Model:")
print(summary(arima_sales_weekly))

print("Weekly ARIMA Orders Model:")
print(summary(arima_orders_weekly))

# Forecast next 8 weeks (2 months)
sales_forecast_weekly <- forecast(arima_sales_weekly, h = 8)
orders_forecast_weekly <- forecast(arima_orders_weekly, h = 8)
# Plot weekly forecasts
par(mfrow = c(2, 1))
plot(sales_forecast_weekly, main = "Weekly Sales Forecast - Next 8 Weeks")
plot(orders_forecast_weekly, main = "Weekly Orders Forecast - Next 8 Weeks")

# ETS model for weekly data
ets_sales_weekly <- ets(weekly_sales_ts)
ets_orders_weekly <- ets(weekly_orders_ts)
# Forecast with ETS
sales_forecast_ets <- forecast(ets_sales_weekly, h = 8)
orders_forecast_ets <- forecast(ets_orders_weekly, h = 8)

# Compare ARIMA vs ETS accuracy
print("ARIMA Weekly Sales Accuracy:")
print(accuracy(arima_sales_weekly))

print("ETS Weekly Sales Accuracy:")
print(accuracy(ets_sales_weekly))
#model comparison and selection

if(accuracy(arima_sales_weekly)[2] < accuracy(ets_sales_weekly)[2]) {
  final_sales_forecast <- sales_forecast_weekly
  model_used <- "ARIMA"
} else {
  final_sales_forecast <- sales_forecast_ets
  model_used <- "ETS"
}
print(paste("Selected model for final forecast:", model_used))

#Final forecast report
forecast_weeks <- seq(max(weekly_data$year_week) + 7, by = "week", length.out = 8)
final_weekly_forecast <- data.frame(
  week_starting = forecast_weeks,
  forecast_sales = as.numeric(final_sales_forecast$mean),
  sales_lower_80 = as.numeric(final_sales_forecast$lower[,1]),
  sales_upper_80 = as.numeric(final_sales_forecast$upper[,1]),
  sales_lower_95 = as.numeric(final_sales_forecast$lower[,2]),
  sales_upper_95 = as.numeric(final_sales_forecast$upper[,2]),
  forecast_orders = as.numeric(orders_forecast_weekly$mean),
  orders_lower_80 = as.numeric(orders_forecast_weekly$lower[,1]),
  orders_upper_80 = as.numeric(orders_forecast_weekly$upper[,1]),
  orders_lower_95 = as.numeric(orders_forecast_weekly$lower[,2]),
  orders_upper_95 = as.numeric(orders_forecast_weekly$upper[,2])
)
print("8-Week Forecast:")
print(final_weekly_forecast)
