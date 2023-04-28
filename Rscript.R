library(car)
library(data.table)
library(dplyr)
library(ggplot2)
library(leaps)
library(ppcor)

# Read the data
sleep_efficiency_data <- read.table("sleep_efficiency_data.csv", sep = ",", header = TRUE)
sleep_data <- sleep_efficiency_data

# Filter for ages [18, 30] and exclude blank fields
sleep_data<-filter(sleep_efficiency_data, Age <=30 & Age >=18)
sleep_data <- na.omit(sleep_data)

# Factorize smoking status and exercise frequency
sleep_data$Smoking.status<-as.factor(sleep_data$Smoking.status)
### sleep_data$Exercise.frequency<-as.factor(sleep_data$Exercise.frequency)

# Boxplot sleep efficiency vs smoking status
boxplot(Sleep.efficiency ~ Smoking.status, data = sleep_data,
        xlab = "Smoking Status", ylab = "Sleep Efficiency",
        main = "Sleep Efficiency by Smoking Status")

# Boxplot sleep efficiency vs factorized exercise frequency
### boxplot(Sleep.efficiency ~ Exercise.frequency, data = sleep_data,
###        xlab = "# of times exercised per week", ylab = "Sleep Efficiency",
###        main = "Sleep Efficiency by Exercise Frequency")

# Plot sleep efficiency vs exercise frequency
ggplot(sleep_data, aes(x = Exercise.frequency, y = Sleep.efficiency)) +
  geom_point() +
  labs(x = "# of times exercised per week", y = "Sleep Efficiency",
       title = "Sleep Efficiency by Exercise Frequency")

# Plot sleep efficiency vs caffeine consumption
ggplot(sleep_data, aes(x = Caffeine.consumption, y = Sleep.efficiency)) +
  geom_point() +
  labs(x = "Caffeine Consumption (mg)", y = "Sleep Efficiency",
       title = "Sleep Efficiency vs  Caffeine Consumption")

# Plot sleep efficiency vs sleep duration
ggplot(sleep_data, aes(x = Sleep.duration, y = Sleep.efficiency)) +
  geom_point() +
  labs(x = "Sleep duration (hours)", y = "Sleep Efficiency",
       title = "Sleep Efficiency vs  Sleep duration")

# Plot sleep efficiency vs alcohol consumption
ggplot(sleep_data, aes(x = Alcohol.consumption, y = Sleep.efficiency)) +
  geom_point() +
  labs(x = "Alcohol Consumption (oz)", y = "Sleep Efficiency",
       title = "Sleep Efficiency vs Alcohol Consumption")

# Regsubsets to find optimal model
s<-regsubsets(Sleep.efficiency~Exercise.frequency + Sleep.duration + Caffeine.consumption + Alcohol.consumption + Smoking.status, data = sleep_data, method="exhaustive")
ss<-summary(s)
ss$adjr2
ss$which
ss$cp

# Full model without interaction
full_model <- lm(Sleep.efficiency~Sleep.duration+Caffeine.consumption+Alcohol.consumption+Smoking.status+Exercise.frequency, data = sleep_data)
summary(full_model)

# Optimal model using most significant variables
opt_model <- lm(Sleep.efficiency~Exercise.frequency + Alcohol.consumption + Smoking.status, data = sleep_data)
summary(opt_model)

