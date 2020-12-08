str(iris)
install.packages("psych")
library(psych)
describe(iris$Sepal.Length)
help("describe")
summary(iris$Sepal.Length)
install.packages("dplyr")
library(dplyr)
iris %>% group_by(Sepal.Length) %>% summarise
summary(iris$Sepal.Length)

