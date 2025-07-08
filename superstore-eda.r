# ---
# title: "Superstore EDA"
# author: "Your Name"
# date: "`r Sys.Date()`"
# output: html_document
# ---

# Introduction
# This notebook performs a comprehensive exploratory data analysis on the sample_superstore dataset.

library(tidyverse)
library(lubridate)
library(scales)

# Load data
library(readr)
superstore <- (
  read_csv("Sample - Superstore.csv")
  %>%
  mutate(Order.Date = mdy(`Order Date`))


## 1.1 Monthly Trend
```{r monthly-trend}
sales_monthly <- superstore %>%
  group_by(month = floor_date(Order.Date, "month")) %>%
  summarize(total_sales = sum(Sales, na.rm=TRUE))

ggplot(sales_monthly, aes(month, total_sales)) +
  geom_line() +
  labs(title="Monthly Sales Trend", x="Month", y="Total Sales")
```

## 1.2 Identify Peaks & Dips
```{r peaks-dips}
# highlight top months
top_months <- sales_monthly %>% top_n(3, total_sales)
# highlight dips
bottom_months <- sales_monthly %>% top_n(-3, total_sales)

# Print for review
top_months
bottom_months
```

## 1.3 Year-over-Year Growth (Optional)
```{r yoy-growth}
yoy <- sales_monthly %>%
  mutate(year = year(month), mon = month(month)) %>%
  spread(year, total_sales) %>%
  mutate(yoy_growth = (`2024` - `2023`) / `2023` * 100)
yoy %>% filter(mon == max(mon[yoy$`2023`>0]))
```

# 2. Profit Margins by Category

```{r margin-by-category}
margin_cat <- superstore %>%
  group_by(Category) %>%
  summarize(total_profit = sum(Profit), total_sales = sum(Sales)) %>%
  mutate(profit_margin = total_profit / total_sales * 100) %>%
  arrange(profit_margin)

# bar chart
ggplot(margin_cat, aes(reorder(Category, profit_margin), profit_margin)) +
  geom_col() +
  coord_flip() +
  labs(title="Profit Margin by Category", x="Category", y="Profit Margin (%)")
```

# 3. Regional Performance Comparison

```{r region-performance}
region_perf <- superstore %>%
  group_by(Region) %>%
  summarize(total_sales = sum(Sales), total_profit = sum(Profit)) %>%
  mutate(profit_margin = total_profit / total_sales * 100)
region_perf

# scatter
 ggplot(region_perf, aes(total_sales, total_profit, label=Region)) +
  geom_point(size=4) +
  geom_text(vjust=-1) +
  labs(title="Sales vs Profit by Region")
```

# 4. Customer Segmentation (RFM)

```{r rfm}
rfm <- superstore %>%
  group_by(`Customer ID`) %>%
  summarize(
    recency = as.numeric(difftime(max(Order.Date), max(Order.Date), units="days")),
    frequency = n(),
    monetary = sum(Sales)
  )
# score and segment... (code continues)
```

# 5. Shipping Mode Profitability

```{r shipping-mode}
ship <- superstore %>%
  group_by(`Ship Mode`) %>%
  summarize(total_sales = sum(Sales), total_profit = sum(Profit), orders = n()) %>%
  mutate(margin = total_profit / total_sales * 100,
         profit_per_order = total_profit / orders)
ship
```

# 6. Recommendations & Next Questions

- **Recommendation 1:** ...
- **Recommendation 2:** ...
- **Recommendation 3:** ...

**Further questions:**
1. ...
2. ...



# …rest of your code…
