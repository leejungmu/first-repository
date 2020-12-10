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
                  'education_num', 'marital_status', 'occuoation',
                  'relationship', 'race', 'sex',
                  'capital_gain', 'capital_loss',
                  'hours_per_week', 'native_country',
                  'wage')
glimpse(adult)

set.seed(1601)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n *.60)
idx <- setdiff(idx, training_idx)
validation_idx = sample(idx, n *.20)
test_idx <- setdiff(idx, validation_idx)
training <- adult[training_idx,]
validation <- adult[validation_idx,]
test <- adult[test_idx,]

cvr_tr <- rpart(wage ~ ., data = training)
cvr_tr

yhat_tr <- predict(cvr_tr, validation)
yhat_tr <- yhat_tr[,">50K"]
y_obs <- ifelse(validation$wage == ">50K", 1, 0)

pred_tr <- prediction(yhat_tr, y_obs)
perf_tr <- performance(pred_tr, measure = "tpr", x.measure = "fpr")
plot(perf_tr, col="black", main = "ROC Curve")
abline(0, 1)
performance(pred_tr, "auc")@y.values[[1]]
