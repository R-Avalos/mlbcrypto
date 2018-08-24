# Exploration
library(tidyverse)
library(lubridate)
library(ggthemes)

sales <- read.csv(file = "sales_sample_data_8232018.csv", header = TRUE, strip.white = TRUE)
sales$DateTime <- mdy_hms(sales$DateTime)
summary(sales)
summary(sales$Position)
summary(sales$ETH)

hist(sales$ETH, bins = seq(min(0), max(sales$ETH)))
max(sales$ETH)

max(sales$ETH, na.rm = TRUE)
summary(sales$ETH)

sales_plot <- ggplot(data = sales, aes(x = DateTime, y = ETH, color = Type)) + 
  geom_jitter(alpha = 0.75) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_hline(yintercept = 0.5, color = "grey", alpha = 0.1) +
  geom_hline(yintercept = 1, color = "grey", alpha = 0.25) +
  geom_hline(yintercept = 1.5, color = "grey", alpha = 0.1) +
  geom_hline(yintercept = 2, color = "grey", alpha = 0.25) +
  scale_color_manual(values = c("red", "orange", "purple", "dodger blue", "black")) +
  theme_tufte()
sales_plot

type_p <- ggplot(sales, aes(Type, ETH))
type_violin <- type_p + geom_violin(aes(fill = Type))
type_violin + geom_jitter(alpha = 0.1, width = 0.3) +
  theme_tufte() + 
  theme(axis.title.x = element_blank(),legend.position="none")
  ggtitle(paste0("Sample Sales from ", length(sales$SerialNumber), " Observations"))
