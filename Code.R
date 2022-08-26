<<<<<<< HEAD

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

har_train_cl <- har_train[, colMeans(is.na(har_train)) < 0.9]
har_train_cl$user_name <- as.factor(har_train_cl$user_name)
har_train_cl$classe <- as.factor(har_train_cl$classe)
har_train_cl <- har_train_cl[, !sapply(har_train_cl, is.character)]
har_train_cl <- select(har_train_cl, -c(1:5))

# Analyze and visualize data
summary(har_train_cl)
str(har_train_cl)

plot_vars <- select(har_train_cl, -classe)
ggplot(gather(plot_vars), aes(x = value)) + 
  geom_histogram(bins = 15) +
  facet_wrap(~key,scales = 'free_x')


skews <- sapply(plot_vars, function(x) skews = skewness(x))
tibble(skewness = abs(skews), column = names(skews)) %>%
  arrange(desc(skewness)) %>%
  print(n = 52)
sel_vars <- c('gyros_dumbbell_y', 'gyros_dumbbell_z', 'gyros_dumbbell_x',
              'gyros_forearm_y', 'gyros_forearm_z', 'gyros_forearm_x')

har_train_cl %>%
  select(sel_vars) %>%
  gather(key, value) %>%
  ggplot( aes(x = value)) + 
    geom_histogram(bins = 15) +
     facet_wrap(~key,scales = 'free_x')


cors <- cor(plot_vars, as.numeric(har_train_cl$classe))
corrplot(cors, method="color",   
         addCoef.col = "grey20",
         type="lower",
         tl.col="black", tl.srt=45,
         number.cex = 0.5)


# Create validation 
inTrain <- createDataPartition(har_train_cl$classe, p = 0.75, list = F)
training <- har_train_cl[inTrain,]
validation <- har_train_cl[-inTrain,]

# Build first model
set.seed(123)
mod_tree <- train(classe~., method = 'rpart', data = training)
fancyRpartPlot(mod_tree$finalModel)

pred_tree <- predict(mod_tree, validation)
confusionMatrix(pred_tree, factor(validation$classe))

# Build alternative models
mod_rf <- train(classe~., method = 'rf', data = training)
mod_gbm <- train(classe~., method = 'gbm', data = training)
mod_svm <- train(classe~., method = 'svmRadial', data = training)

model_results <- resamples(list(DT = mod_tree, RF = mod_rf, GBM = mod_gbm, SVM = mod_svm))
summary(model_results)
bwplot(model_results)

pred_rf <- predict(mod_rf, validation)
confusionMatrix(pred_rf, factor(validation$classe))

#Test data
testing <- har_test[, colMeans(is.na(har_test)) < 0.9]
testing <- testing[, !sapply(testing, is.character)]
testing <- select(testing, -c(1:4))
testing <- select(testing, -problem_id)

predict(mod_rf, testing)

=======
# Test
>>>>>>> 3b673b7ae04cbc7d11ba87e928fa7fc1c3598cd2
