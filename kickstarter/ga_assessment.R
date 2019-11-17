library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(stringr)

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
  geom_histogram(binwidth = 10) +xlim(1000, 75000) + ylim(0,100)

#Is the ‘duration’ variable normally distributed? *
class(kickstarter$duration)
ggplot(data=kickstarter, aes(kickstarter$duration)) + 
  geom_histogram(binwidth = 5) 


# What's the best length of time to run a campaign?

scatter.smooth(x=kickstarter$duration, y=kickstarter$funded_percentage, main="Duration ~ Percent Funded")
class(kickstarter$duration)

cor(kickstarter_successful$duration, kickstarter_successful$funded_percentage) 
# see if duration and percent funded are linearly correlated .. they don't seem to be
linearMod <- lm(duration ~ funded_percentage, data=kickstarter_successful) %>%
  summary()

# let's look at the average duration for successful projects
kickstarter_successful <- kickstarter %>%
  filter(status == "successful") 

avg_duration_successful <- mean(kickstarter_successful$duration) 


# create a binary variable for status, looking at completed projects 
kickstarter_completed <- kickstarter %>%
  filter(status != "live") %>%
  mutate(status_binary = ifelse(status== "successful", 1, 0 ))

cor(kickstarter_completed$duration, kickstarter_completed$status_binary) 
logistic_simple <- glm(duration ~ status_binary, family ="poisson", data = kickstarter_completed) %>%
  summary()


# What's the ideal pledge goal?
# let's look at the average pledge goal for successful projects
avg_goal_successful <- mean(kickstarter_successful$goal) 


# What type of projects would be most successful at getting funded?
# Combine the two Film & video categories - account for special characters
unique(kickstarter_completed$category)
kickstarter_completed$category <- str_replace_all(kickstarter_completed$category, "Film &amp; Video", "Film & Video")

logistic_simple2 <- glm(status_binary~category, family = "poisson", data = kickstarter_completed)


# Is there an ideal month/day/time to launch a campaign?





