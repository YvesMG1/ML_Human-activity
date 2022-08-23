
library(caret)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(corrplot)
library(kernlab)
library(rattle)
library(naniar)
library(moments)
install.packages('moments')



setwd("/Users/Yves.Gorgen/Documents/Uni/Coursera/Data Science_Statistics and ML/Machine Learning/Course project")
har_train <- read.csv('pml-training.csv', header = T)
har_test <- read.csv('pml-testing.csv', header = T)


# Handling missing data
str(har_raw)
summary(har_raw)
sum(is.na(har_train))

har_train %>%
  gather(col, value) %>%
  group_by(col) %>%
  summarize(missing_share = mean(is.na(value))) %>%
  arrange(desc(missing_share)) %>%
  print(n = 100)

training <- har_train[, colMeans(is.na(har_train)) < 0.9]
training$user_name <- as.factor(training$user_name)
training$classe <- as.factor(training$classe)
training <- training[, !sapply(training, is.character)]
training <- select(training, -c(1:5))

# Analyze and visualize data
summary(training)
str(training)

plot_vars <- select(training, -classe)
ggplot(gather(plot_vars), aes(x = value)) + 
  geom_histogram(bins = 15) +
  facet_wrap(~key,scales = 'free_x')


skews <- sapply(plot_vars, function(x) skews = skewness(x))
tibble(skewness = abs(skews), column = names(skews)) %>%
  arrange(desc(skewness)) %>%
  print(n = 52)
sel_vars <- c('gyros_dumbbell_y', 'gyros_dumbbell_z', 'gyros_dumbbell_x',
              'gyros_forearm_y', 'gyros_forearm_z', 'gyros_forearm_x')

training %>%
  select(sel_vars) %>%
  gather(key, value) %>%
  ggplot( aes(x = value)) + 
    geom_histogram(bins = 15) +
     facet_wrap(~key,scales = 'free_x')


cors <- cor(plot_vars)
corrplot(cors, method="color",   
         addCoef.col = "grey20",
         type="lower",
         tl.col="black", tl.srt=45,
         number.cex = 0.5)









