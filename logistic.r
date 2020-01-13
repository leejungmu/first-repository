data(iris)

a <- subset(iris, Species == "setosa" | Species == "versicolor")
a$Species <- factor(a$Species)
str(a)

m <- glm(Species ~ Sepal.Length, family = binomial, data = a)
summary(m)

fitted(m)[c(1:5, 96:100)]

f <- fitted(m)
as.numeric(a$Species)

ifelse(f > 0.5, 1, 0) == as.numeric(a$Species) - 1

is_correct <- ifelse(f > 0.5, 1, 0) == as.numeric(a$Species) - 1
sum(is_correct)

sum(is_correct) / NROW(is_correct)

predict(m, newdata = a[c(1, 50, 51, 100), ], type = "response")

cdplot(Species ~ Sepal.Length, data = a)

plot(a$Sepal.Length, a$Species, xlab = "Sepal.Length")
x = seq(min(a$Sepal.Length), max(a$Sepal.Length), 0.1)
lines(x, 1+(1/(1+(1/exp(-27.831+5.140*x)))), type = "l", col = "red")

library(nnet)
(m <- multinom(Species ~ ., data = iris))

head(fitted(m))

predict(m, newdata = iris[c(1, 50, 51, 100), ], type = "class")

predict(m, newdata = iris[c(1, 50, 51, 100), ], type = "prob")


