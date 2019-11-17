library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)




kickstarter <- read_csv("data/DSI_kickstarterscrape_dataset.csv",col_names =  TRUE)
kickstarter <- kickstarter %>%
  janitor::clean_names()

colnames(kickstarter)


# What is the mean (total) pledge that projects get? (not per backer) *

total_mean <- mean(kickstarter$pledged, na.rm =  TRUE)

#double check my solution
total_sum <- sum(kickstarter$pledged, na.rm =  TRUE)
mean_manual <- total_sum/nrow(kickstarter)

# Create a histogram that shows the distribution for number of backers. What is the skew of the distribution? *
ggplot(data=kickstarter, aes(kickstarter$backers)) + 
  geom_histogram(binwidth = 10) +xlim(0,1000) +ylim(0,7500)


ggplot(data=kickstarter, aes(kickstarter$backers)) + 
  geom_histogram(binwidth = 10) +xlim(1000, 75000) +ylim(0,100)

#Is the ‘duration’ variable normally distributed? *
class(kickstarter$duration)
ggplot(data=kickstarter, aes(kickstarter$duration)) + 
  geom_histogram(binwidth = 5) 