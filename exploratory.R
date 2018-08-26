# Exploration
library(tidyverse)
library(lubridate)
library(ggthemes)
library(plm)
library(car)
library(gplots)

sales <- read_csv(file = "sales_sample_data_8232018.csv")
sales$DateTime <- mdy_hms(sales$DateTime)
sales$Date <- mdy(sales$Date)
sales$Team <- as.factor(sales$Team)
sales$Position <- as.factor(sales$Position)
sales$Type <- as.factor(sales$Type)
summary(sales)
summary(sales$Position)
plot(sales$Position)
plot(sales$DateTime, sales$ETH)
summary(sales$ETH)

<<<<<<< HEAD
# Overview by team
scatterplot(ETH~Date|Team, boxplots=FALSE, smooth=TRUE, reg.line=FALSE, data=sales)
plotmeans(ETH ~ Team, main="Heterogeineity across teams", data=sales)





positionPlot <- ggplot(sales, aes(x = Position)) +
  geom_bar(stat = "count", fill = "dodger blue") +
=======


positionPlot <- ggplot(sales, aes(x = Position)) +
  geom_bar(stat = "count") +
>>>>>>> 601ea97198d49d416f0b8bea5bd829605af6d443
  ggtitle(paste0("Sample of Sales, ", length(sales$SerialNumber), " Observations")) +
  theme_tufte() +
  coord_flip()
positionPlot




saleBP <- ggplot(sales, aes(x = Team, y = ETH)) +
  geom_boxplot(fill = "dodger blue") +
  theme_tufte() +
  stat_summary(fun.y=mean, geom="point", shape=3, size=2) +
  coord_flip()
saleBP 

saleBPbreakdown <- ggplot(sales, aes(x = Team, y = ETH, fill = Type)) +
  geom_boxplot() +
  theme_tufte() +
  stat_summary(fun.y=mean, geom="point", shape=3, size=2) +
  scale_fill_manual(values = c("red", "yellow", "purple", "dodger blue", "dark grey")) +
  coord_flip()
saleBPbreakdown 

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
