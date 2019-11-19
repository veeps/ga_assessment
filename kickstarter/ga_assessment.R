library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(stringr)
library(useful)
library(coefplot)

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

ggplot(kickstarter, aes(x=duration)) +
  geom_density(fill="grey", color ="grey") +
  scale_x_continuous()


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


logistic_regression <- glm(status_binary ~ duration + goal, 
                           family =binomial(link="logit"), 
                           data = kickstarter_completed) 

margins(logistic_regression, variables = "duration", type = "response")

cplot(logistic_regression, "duration", what = "prediction")

%>%
  summary()

exp(logistic_regression$coefficients)

coefplot(logistic_regression)

## split data set
set.seed(42)
rows <- sample(nrow(kickstarter_completed))
kickstarter_completed <- kickstarter_completed[rows, ]
split <- round(nrow(kickstarter_completed) * .80)

train <- kickstarter_completed[1:split, ]
test <- kickstarter_completed[(split + 1):nrow(kickstarter_completed), ]

model_1 <- glm(status_binary ~ duration, family =binomial(link="logit"), data = train)
p <- predict(model_1, test, type = "response")
test$predicted_success <- p

head(p)
# calculate errors
error <- p - test[["status_binary"]]
sqrt(mean(error^2))


ggplot(test, aes(x = duration, y = predicted_success)) +
  geom_point()


# What's the ideal pledge goal?
# let's look at the average pledge goal for successful projects
avg_goal_successful <- mean(kickstarter_successful$goal) 


# What type of projects would be most successful at getting funded?
# Combine the two Film & video categories - account for special characters
unique(kickstarter_completed$category)
kickstarter_completed$category <- str_replace_all(kickstarter_completed$category, "Film &amp; Video", "Film & Video")

library('magrittr')
kickstarter_completed %<>%
  mutate(category_factor = fct_lump(kickstarter_completed$location,6))

model_2 <- glm(status_binary~category_factor, family =binomial(link="logit"), data = train)


p2 <- predict(model_1, test)
test$predicted_success_2 <- p2

ggplot(test, aes(x = category, y = predicted_success_2)) +
  geom_point()

# Is there an ideal month/day/time to launch a campaign?





