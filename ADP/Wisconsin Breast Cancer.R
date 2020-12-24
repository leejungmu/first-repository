ls() #변수 반환
rm(list = ls()) #변수 제거

library(dplyr)
library(ggplot2)
library(MASS)
library(glmnet)
library(randomForest)
library(gbm)
library(rpart)
library(boot)
library(ROCR)
library(gridE)

data <- tbl_df(read.table("D:/DataScience/first-repository/raw_data/Wisconsin_Breast_Cancer/wdbc.data", 
                          strip.white=TRUE, sep=",", header=FALSE))
feature_names <- c('radius', 'texture', 'perimeter', 'area', 'smoothness', 
                   'compactness', 'concavity', 'concave_points', 'symmetry', 'fractal_dim')
names(data) <-
  c('id', 'class', 
    paste0('mean_', feature_names),
    paste0('se_', feature_names),
    paste0('worst_', feature_names))

glimpse(data)

data$class <- as.factor(data$class)

summary(data)

data <- data %>% dplyr::select(-id)
data$class <- factor(ifelse(data$class == 'B', 0, 1))

glimpse(data)

panel.cor <- function(x, y, digits=2, prefix="", cex.cor, ...){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- abs(cor(x, y))
  txt <- format(c(r, 0.123456789), digits=digits)[1]
  txt <- paste0(prefix, txt)
  if(missing(cex.cor)) cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

pairs(data %>% dplyr::select(class, starts_with('mean_')) %>%
        sample_n(min(1000, nrow(data))),
      lower.panel=function(x,y){ points(x,y); abline(0,1,col='red')},
      upper.panel=panel.cor)

library(ggplot2)
library(dplyr)
library(gridExtra)
p1 <- data %>% ggplot(aes(class)) + geom_bar()
p2 <- data %>% ggplot(aes(class, mean_concave_points)) + geom_jitter(col='gray')+
  geom_boxplot(alpha=.5)
p3 <- data %>% ggplot(aes(class, mean_radius)) + geom_jitter(col='gray') +
  geom_boxplot(alpha=.5)
p4 <- data %>% ggplot(aes(mean_concave_points, mean_radius)) + geom_jitter(col='gray') +
  geom_smooth()
grid.arrange(p1, p2, p3, p4, ncol=2)

set.seed(1223)
n <- nrow(data)
idx <- 1:n
training_idx <- sample(idx, n * .60)
idx <- setdiff(idx, training_idx)
validate_idx <- sample(idx, n * .20)
test_idx <- setdiff(idx, validate_idx)
training <- data[training_idx,]
validation <- data[validate_idx,]
test <- data[test_idx,]

#lm model
data_lm_full <- glm(class ~ ., data=training, family=binomial)
summary(data_lm_full)

predict(data_lm_full, newdata=data[1:5,], type='response')

y_obs <- as.numeric(as.character(validation$class))
yhat_lm <- predict(data_lm_full, newdata=validation, type='response')
pred_lm <- prediction(yhat_lm, y_obs)
performance(pred_lm, "auc")@y.values[[1]]

#Lasso model
xx <- model.matrix(class ~ .-1, data)
x <- xx[training_idx,]
y <- as.numeric(as.character(training$class))
glimpse()

data_cvfit <- cv.glmnet(x, y, family="binomial")
plot(data_cvfit)

coef(data_cvfit, s=c("lambda.1se"))
coef(data_cvfit, s=c("lambda.min"))

predict(data_cvfit, s="lambda.min", newx=x[1:5,], type='response')

yhat_glmnet <- predict(data_cvfit, s="lambda.min", newx=xx[validate_idx,], type='response')
yhat_glmnet <- yhat_glmnet[,1]
pred_glmnet <- prediction(yhat_glmnet, y_obs)
performance(pred_glmnet, "auc")@y.values[[1]]

#Tree model
data_tr <- rpart(class ~ ., data=training)
data_tr

printcp(data_tr)
summary(data_tr)

opar <- par(mfrow = c(1,1), xpd = NA)
plot(data_tr)
text(data_tr, use.n = TRUE)
par(opar)

yhat_tr <- predict(data_tr, validation)
yhat_tr <- yhat_tr[,"1"]
pred_tr <- prediction(yhat_tr, y_obs)
performance(pred_tr, "auc")@y.values[[1]]

#randomForest
set.seed(1607)
data_rf <- randomForest(class ~ ., training)
data_rf
opar <- par(mfrow=c(1,2))
plot(data_rf)
varImpPlot(data_rf)
par(opar)

yhat_rf <- predict(data_rf, newdata=validation, type='prob')[,'1']
pred_rf <- prediction(yhat_rf, y_obs)
performance(pred_rf, "auc")@y.values[[1]]

#Boosting
set.seed(1607)
data_for_gbm <- training %>%
  mutate(class=as.numeric(as.character(class)))
data_gbm <- gbm(class ~ ., data=data_for_gbm, distribution="bernoulli", 
                n.trees=5000, cv.folds=3, verbose=TRUE)

(best_iter = gbm.perf(data_gbm, method="cv"))

yhat_gbm <- predict(data_gbm, n.trees=best_iter, newdata=validation, type='response')
pred_gbm <- prediction(yhat_gbm, y_obs)
performance(pred_gbm, "auc")@y.values[[1]]

#최종 모형 선택 및 test set 오차 계산
data.frame(method=c('lm', 'glmnet', 'rf', 'gbm'),
           auc = c(performance(pred_lm, "auc")@y.values[[1]],
                   performance(pred_glmnet, "auc")@y.values[[1]],
                   performance(pred_rf, "auc")@y.values[[1]],
                   performance(pred_gbm, "auc")@y.values[[1]]))

perf_lm <- performance(pred_lm, measure = "tpr", x.measure = "fpr")
perf_glmnet <- performance(pred_glmnet, measure = "tpr", x.measure = "fpr")
perf_rf <- performance(pred_rf, measure = "tpr", x.measure = "fpr")
perf_gbm <- performance(pred_gbm, measure = "tpr", x.measure = "fpr")

plot(perf_lm, col = 'black', main="ROC Curve")
plot(perf_glmnet, add=TRUE, col='blue')
plot(perf_rf, add=TRUE, col='red')
plot(perf_gbm, add=TRUE, col='cyan')
abline(0,1)
legend('bottomright', inset=.1,
       legend=c("GLM","glmnet","RF","GBM"),
       col=c('black','blue','red','cyan'), lty=1, lwd=2)

y_obs_test <- as.numeric(as.character(test$class))
yhat_glmnet_test <- predict(data_cvfit, s='lambda.min', newx=xx[test_idx,], type='response')
yhat_glmnet_test <- yhat_glmnet_test[,1]
pred_glmnet_test <- prediction(yhat_glmnet_test, y_obs_test)
performance(pred_glmnet_test, "auc")@y.values[[1]]

yhat_gbm_test <- predict(data_gbm, n.trees=best_iter, newdata=test, type='response')
pred_gbm_test <- prediction(yhat_gbm_test, y_obs_test)
performance(pred_gbm_test, "auc")@y.values[[1]]

pairs(data.frame(y_obs=y_obs,
                 yhat_lm=yhat_lm,
                 yhat_glmnet=c(yhat_glmnet),
                 yhat_rf=yhat_rf,
                 yhat_gbm=yhat_gbm),
      lower.panel=function(x,y){ points(x,y); abline(0,1,col='red')},
      upper.panel=panel.cor)
