library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(ROCR)

adult <- read.csv("D:/DataScience/first-repository/raw_data/Adult/adult.data", header=FALSE, strip.white=TRUE)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education',
                  'education_num', 'marital_status', 'occupation',
                  'relationship', 'race', 'sex',
                  'capital_gain', 'capital_loss',
                  'hours_per_week', 'native_country',
                  'wage')


glimpse(adult)

adult$wage <- as.factor(adult$wage)
adult$workclass <- as.factor(adult$workclass)
adult$education <- as.factor(adult$education)
adult$marital_status <- as.factor(adult$marital_status)
adult$occupation <- as.factor(adult$occupation)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$sex <- as.factor(adult$sex)
adult$native_country <- as.factor(adult$native_country)


set.seed(1601)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validation_idx = sample(idx, n *.20)
test_idx <- setdiff(idx, validation_idx)
training <- adult[training_idx,]
validation <- adult[validation_idx,]
test <- adult[test_idx,]

set.seed(1607)
adult_gbm <- training %>%
  mutate(wage=ifelse(wage == ">50K", 1, 0))
ad_gbm <- gbm(wage ~ ., data=adult_gbm, 
              distribution="bernoulli",
              n.trees=5000, cv.folds=3, verbose=TRUE)
print(ad_gbm)
(best_iter <- gbm.perf(ad_gbm, method="cv"))

ad_gbm2 <- gbm.more(ad_gbm, n.new.trees=1000)
print(ad_gbm2)
(best_iter <- gbm.perf(ad_gbm2, method="cv"))

predict(ad_gbm, n.trees=best_iter, newdata=adult_gbm[1:5,], type='response')

yhat_gbm <- predict(ad_gbm, n.trees=best_iter, newdata=validation, type='response')
y_obs <- ifelse(validation$wage == ">50K", 1, 0)

pred_gbm <- prediction(yhat_gbm, y_obs)
perf_gbm <- performance(pred_gbm, measure="tpr", x.measure="fpr")
plot(perf_gbm, col = 'cyan')
abline(0,1)
performance(pred_gbm, "auc")@y.values[[1]]
