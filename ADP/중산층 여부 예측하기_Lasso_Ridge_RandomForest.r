library(dplyr)
library(ggplot2)
library(ISLR)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)

adult <- read.csv("D:/DataScience/first-repository/raw_data/Adult/adult.data", head=FALSE, strip.white = TRUE)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education',
                  'education_num', 'marital_atatus', 'occupation',
                  'relationship', 'race', 'sex',
                  'capital_gain', 'capital_loss',
                  'hour_per_week', 'native_country',
                  'wage')
glimpse(adult)

summary(adult)

adult$wage <- as.factor(adult$wage)
adult$workclass <- as.factor(adult$workclass)
adult$education <- as.factor(adult$education)
adult$marital_atatus <- as.factor(adult$marital_atatus)
adult$occupation <- as.factor(adult$occupation)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$sex <- as.factor(adult$sex)
adult$native_country <- as.factor(adult$native_country)

levels(adult$wage)
levels(adult$race)

x <- model.matrix(~ race + sex + age, adult)
glimpse(x)

x <- model.matrix( ~ . - wage, adult)
dim(x)

set.seed(1601)
n <- nrow(adult)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx = sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
length(training_idx)
length(validate_idx)
length(test_idx)
training <- adult[training_idx,]
validation <- adult[validate_idx,]
test <- adult[test_idx,]

xx <- model.matrix(wage ~ .-1, adult)
x <- xx[training_idx, ]
y <- ifelse(training$wage == ">50K", 1, 0)
dim(x)

ad_glmnet_fit <- glmnet(x, y)

plot(ad_glmnet_fit)

ad_glmnet_fit

coef(ad_glmnet_fit, s = c(.1713, .1295))

ad_cvfit <- cv.glmnet(x, y, family = "binomial")
plot(ad_cvfit)
log(ad_cvfit$lambda.min)
log(ad_cvfit$lambda.1se)
coef(ad_cvfit, s=ad_cvfit$lambda.1se)
coef(ad_cvfit, s=ad_cvfit$lambda.min)
length(which(coef(ad_cvfit, s=ad_cvfit$lambda.1se)>0))
length(which(coef(ad_cvfit, s=ad_cvfit$lambda.min)>0))

set.seed(1607)
foldid <- sample(1:10, size=length(y), replace=TRUE)
cv1 <- cv.glmnet(x, y, foldid=foldid, alpha=1, family='binomial')
cv.5 <- cv.glmnet(x, y, foldid=foldid, alpha=0.5, family='binomial')
cv0 <- cv.glmnet(x, y, foldid=foldid, alpha=0, family='binomial')

par(mfrow=c(2,2))
plot(cv1, main="Alpha=1.0")
plot(cv.5 main="Alpha=0.5")
plot(cv0, main="Alpha=0.0")
plot(log(cv1$lambda), cv1$cvm, pch=19, col="red",
     xlab="log(Lambda)", ylab=cv1$name, main="alpha=1.0")
points(log(cv1$lambda), cv.5$cvm, pch=19, col="grey")
points(log(cv0$lambda), cv0$cvm, pch=19, col="blue")
legend("topleft", legend=c("alpha= 1", "alpha= .5", "alpha 0"),
       pch=19, col=c("red", "grey", "blue"))

predict(ad_cvfit, s="lambda.lse", newx = x[1:5,], type='response')
