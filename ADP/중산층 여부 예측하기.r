install.packages("gbm")
install.packages("ISLR")
install.packages("MASS")
install.packages("glmnet")
install.packages("randomForest")
install.packages("rpart")
install.packages("boot")

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

training %>% 
  ggplot(aes(age, fill=wage)) +
  geom_density(alpha=.5)


training %>% 
  filter(race %in% c('Black', 'White')) %>%
  ggplot(aes(age, fill=wage)) +
  geom_density(alpha=.5) +
  ylim(0, 0.1) +
  facet_grid(race ~ sex, scales='free_y')
  

training %>%
  ggplot(aes(education_num, fill=wage)) +
  geom_bar()


ad_glm_full <- glm(wage ~ ., data=training, family=binomial)

summary(ad_glm_full)

alias(ad_glm_full)

predict(ad_glm_full, new_data = adult[1:5,], type="response")

y_obs <- ifelse(validation$wage == ">50K", 1, 0)
yhat_lm <- predict(ad_glm_full, newdata=validation, type='response')

install.packages("gridExtra")
library(gridExtra)
p1 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(y_obs, yhat_lm, group=y_obs,
                 fill=factor(y_obs))) +
  geom_boxplot()
p2 <- ggplot(data.frame(y_obs, yhat_lm),
             aes(yhat_lm, fill=factor(y_obs))) +
  geom_density(alpha=.5)
grid.arrange(p1, p2, ncol=2)

binomial_deviance <- function(y_obs, yhat){ 
  epsilon = 0.0001 
  yhat = ifelse(yhat < epsilon, epsilon, yhat) 
  yhat = ifelse(yhat > 1-epsilon, 1-epsilon, yhat) 
  a = ifelse(y_obs==0, 0, y_obs * log(y_obs/yhat)) 
  b = ifelse(y_obs==1, 0, (1-y_obs) * log((1-y_obs)/(1-yhat))) 
  return(2*sum(a + b)) 
} 

binomial_deviance(y_obs, yhat_lm)

install.packages("ROCR")
library(ROCR)
pred_lm <- prediction(yhat_lm, y_obs)
perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
plot(perf_lm, col='black', main="ROC Curve for GLM")
abline(0,1)
performance(pred_lm, "auc")@y.values[[1]]
