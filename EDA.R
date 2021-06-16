# Project EDA

# EDA

library(ggplot2)
library(tidyverse)
library(lubridate)

datt1 <- read_csv("la_liga_dataset.csv")

colnames(datt1)
colnames(datt1)[4] <- "Team_Name"

# Boxplot of Total_xg by Team
ggplot(data = datt1, mapping = aes(x = as.factor(Team_Name), y = Total_xg)) + geom_boxplot() + xlab("Team") +
  ylab("Total XG") + ggtitle("Total Expected Goals by Team") + theme(plot.title = element_text(hjust = 0.5))

# Boxplot of Mean_xg by Team
ggplot(data = datt1, mapping = aes(x = as.factor(Team_Name), y = Mean_xg)) + geom_boxplot() + xlab("Team") +
  ylab("Mean XG") + ggtitle("Mean Expected Goals by Team") + theme(plot.title = element_text(hjust = 0.5))

# Boxplot of Total_xg by Formation
ggplot(data = datt1, mapping = aes(x = as.factor(Formation), y = Total_xg))  +  geom_boxplot() + xlab("Formation") +
  ylab("Total XG") + ggtitle("Total Expected Goals by Formation") + theme(plot.title = element_text(hjust = 0.5))

# Boxplot of Mean_xg by Formation
ggplot(data = datt1, mapping = aes(x = as.factor(Formation), y = Mean_xg)) + geom_boxplot() + xlab("Formation") +
  ylab("Total XG") + ggtitle("Mean Expected Goals by Formation") + theme(plot.title = element_text(hjust = 0.5))

# Boxplot of Total_xg by Formation and team
ggplot(data = datt1, mapping = aes(x = as.factor(Formation), y = Total_xg, col = Team_Name)) + geom_boxplot() +
  ggtitle("Total XG by Formation Colored by Team") + xlab("Formation") + ylab("Total XG") +
  theme(plot.title = element_text(hjust = 0.5))

# Boxplot of Mean_xg by Formation and team
ggplot(data = datt1, mapping = aes(x = as.factor(Formation), y = Mean_xg, col = Team_Name)) + geom_boxplot() + 
  ggtitle("Mean XG by Formation Colored by Team") + xlab("Formation") + ylab("Mean XG") +
  theme(plot.title = element_text(hjust = 0.5))

# Scatterplot of Total_xg by Formation
ggplot(data = datt1, mapping = aes(x = as.factor(Formation), y = Total_xg, col = Team_Name)) + geom_point() +
  xlab("Formation") + ylab("Total XG") + ggtitle("Total Expected Goals by Formation") + 
  theme(plot.title = element_text(hjust = 0.5))

# Scatterplot of Mean_xg by Formation
ggplot(data = datt1, mapping = aes(x = as.factor(Formation), y = Mean_xg, col = Team_Name)) + geom_point() +
  xlab("Formation") + ylab("Mean XG") + ggtitle("Mean Expected Goals by Formation") + 
  theme(plot.title = element_text(hjust = 0.5))






