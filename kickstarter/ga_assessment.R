library(readr)
library(dplyr)
library(tidyr)
library(janitor)
library(ggplot2)
library(stringr)
library(useful)
library(coefplot)
library(magrittr)
library(forcats)
library(lubridate)
library(margins)

kickstarter <- read_csv("data/DSI_kickstarterscrape_dataset.csv",col_names =  TRUE)
kickstarter <- kickstarter %>%
  janitor::clean_names()

# combine categories for Film & Video
kickstarter$category <- str_replace_all(kickstarter$category, "Film &amp; Video", "Film & Video")


# conver date to date class
kickstarter <- separate(data = kickstarter, col = funded_date, into = c("Day_of_Week", "Date"), sep = "\\,")
kickstarter$Date <- dmy_hms(kickstarter$Date)

kickstarter %<>%
  mutate_at(vars(Date), funs(year, month, day, hour))

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

kickstarter_completed %<>%
  mutate(category_factor = fct_lump(kickstarter_completed$category,6))

############ 
############ split data set
set.seed(42)
rows <- sample(nrow(kickstarter_completed))
kickstarter_completed <- kickstarter_completed[rows, ]
split <- round(nrow(kickstarter_completed) * .80)

train <- kickstarter_completed[1:split, ]
test <- kickstarter_completed[(split + 1):nrow(kickstarter_completed), ]




############ 
############ 
############ BUILDING MODELS
model_1 <- glm(status_binary ~ duration, family =binomial(link="logit"), data = train)
model_2 <- glm(status_binary ~ goal, family = binomial(link="logit"), data = train )
model_3 <- glm(status_binary~category_factor, family =binomial(link="logit"), data = train)
model_4 <- glm(status_binary~month, family =binomial(link="logit"), data = train)
model_5 <- glm(status_binary~Day_of_Week, family =binomial(link="logit"), data = train)
model_6 <- glm(status_binary~Day_of_Week + month, family =binomial(link="logit"), data = train)
model_7 <- glm(status_binary~hour, family =binomial(link="logit"), data = train)



###### predict
p1 <- predict(model_1, test, type = "response")
test$predicted_success_duration <- p1
ggplot(test, aes(x = duration, y = predicted_success_duration)) +
  geom_point()


# What's the ideal pledge goal?
# let's look at the average pledge goal for successful projects
kickstarter_successful <- kickstarter %>%
  filter(status == "successful")
avg_goal_successful <- mean(kickstarter_successful$goal) 


p2 <- predict(model_2, test, type = "response")
test$predicted_success_goal <- p2
ggplot(test, aes(x = goal, y = predicted_success_goal)) +
  geom_point()

# What type of projects would be most successful at getting funded?
# Combine the two Film & video categories - account for special characters

p3 <- predict(model_3, test, type = "response")
test$predicted_success_category <- p3


ggplot(test, aes(x = category_factor, y = predicted_success_category)) +
  geom_point()

# Is there an ideal month/day/time to launch a campaign?
p4 <- predict(model_4, test, type = "response")
test$predicted_success_month <- p4

ggplot(test, aes(x = month, y = predicted_success_month)) +
  geom_point()



p5 <- predict(model_5, test, type = "response")
test$predicted_success_day <- p5

ggplot(test, aes(x = Day_of_Week, y = predicted_success_day)) +
  geom_point()


cplot(model_7, "hour", what = "prediction")







######################### exploratory
logistic_regression <- glm(status_binary ~ duration + goal , 
                           family =binomial(link="logit"), 
                           data = kickstarter_completed) 

is.na(kickstarter_completed$goal)

margins(model_2, variables = "goal", type = "response")

cplot(logistic_regression, "duration", what = "prediction")


exp(logistic_regression$coefficients)

coefplot(logistic_regression)





