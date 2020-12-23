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

#gbm model
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

#randomForest model
set.seed(1607)
ad_rf <- randomForest(wage ~ ., training)
ad_rf
predict(ad_rf, newdata = adult[1:5,])
predict(ad_rf, newdata = adult[1:5,], type="prob")
yhat_rf <- predict(ad_rf, newdata=validation, type='prob')[,'>50K']
pred_rf <- prediction(yhat_rf, y_obs)
perf_rf <- performance(pred_rf, measure="tpr", x.measure="fpr")
performance(pred_rf, "auc")@y.values[[1]]

#glmnet model
xx <- model.matrix(wage ~ .-1, adult)
x <- xx[training_idx, ]
y <- ifelse(training$wage == ">50K", 1, 0)
dim(x)
ad_glmnet_fit <- glmnet(x, y)
ad_glmnet_fit

ad_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(ad_cvfit)

yhat_glmnet <- predict(ad_cvfit, s="lambda.1se", newx=xx[validation_idx,], type="response")
yhat_glmnet <- yhat_glmnet[,1]
pred_glmnet <- prediction(yhat_glmnet, y_obs)
perf_glmnet <- performance(pred_glmnet, measure="tpr", x.measure="fpr")
performance(pred_glmnet, "auc")@y.values[[1]]

#lm model
ad_glm_full <- glm(wage ~ ., data=training, family=binomial)
yhat_lm <- predict(ad_glm_full, newdata=validation, type='response')
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure="tpr", x.measure="fpr")
performance(pred_lm, "auc")@y.values[[1]]

#Comparison
panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x,y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_lm,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){ points(x,y); abline(0,1,col='red')},
      upper.panel=panel.cor)

#Generalization
y_obs_test <- ifelse(test$wage == ">50K", 1, 0)
yhat_gbm_test <- predict(ad_gbm, n.trees=best_iter, newdata = test, type = 'response')
pred_gbm_test <- prediction(yhat_gbm_test, y_obs_test)
performance(pred_gbm_test, "auc")@y.values[[1]]
