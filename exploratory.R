# Exploration

library(tidyverse)

sales <- read.csv(file = "sales_sample_data_8232018.csv", header = TRUE, strip.white = TRUE)
summary(sales)
summary(sales$Position)
summary(sales$ETH)
