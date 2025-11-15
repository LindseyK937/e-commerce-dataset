
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
