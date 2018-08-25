# Exploration
library(tidyverse)
library(lubridate)
library(ggthemes)

sales <- read_csv(file = "sales_sample_data_8232018.csv")
sales$DateTime <- mdy_hms(sales$DateTime)
sales$Team <- as.factor(sales$Team)
sales$Position <- as.factor(sales$Position)
sales$Type <- as.factor(sales$Type)
summary(sales)
summary(sales$Position)
plot(sales$Position)
summary(sales$ETH)

saleBP <- ggplot(sales, aes(x = Team, y = ETH )) +
  geom_boxplot() +
  theme_tufte() +
  stat_summary(fun.y=mean, geom="point", shape=23, size=4) +
  coord_flip()
saleBP 

bronze <- sales %>% filter(Type == "Bronze")
summary(bronze$ETH)
plot(bronze$DateTime, bronze$ETH)
plot(bronze$Position)

silver <- sales %>% filter(Type == "Silver")
summary(silver$ETH)
plot(silver$DateTime, silver$ETH)
plot(silver$Position)

teamAVG <- sales %>% 
  group_by(Team) %>% 
  summarize(Mean = mean(ETH, na.rm = TRUE), Median = median(ETH, na.rm = TRUE))
teamAVG
teamPlot <- ggplot(teamAVG, aes(x = reorder(Team, Mean), y = Mean)) +
  geom_bar(stat="identity") +
  theme_tufte() +
  theme(axis.title.y = element_blank(),
        axis.ticks.length = unit(0, "lines")) +
  ylab("Mean Player Cost, ETH") +
  coord_flip()
teamPlot 
  
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
