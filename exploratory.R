# Exploration
library(tidyverse)
library(lubridate)

sales <- read.csv(file = "sales_sample_data_8232018.csv", header = TRUE, strip.white = TRUE)
sales$DateTime <- mdy_hms(sales$DateTime)
summary(sales)
summary(sales$Position)
summary(sales$ETH)

hist(sales$ETH, bins = seq(min(0), max(sales$ETH)))
max(sales$ETH)

max(sales$ETH, na.rm = TRUE)
summary(sales$ETH)

sales_plot <- ggplot(data = sales, aes(x = DateTime, y = ETH)) + geom_jitter()
sales_plot
