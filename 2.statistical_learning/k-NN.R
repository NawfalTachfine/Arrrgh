#

rm(list = ls())

# install.packages("ISLR", repos = "http://cran.univ-paris1.fr")
library(ISLR)
data(Caravan)
attach(Caravan)
View(Caravan)
summary(Purchase)

X <- Caravan[, -86] # predictors
Y <- Caravan[, 86] # labels

X.standardized <- scale(X)

test <- 1:1000

train.X <- X.standardized[-test ,]
test.X <- X.standardized[ test ,]

train.Y <- Y[-test]
test.Y <- Y[ test]

set.seed(1)
k <- 3

knn.pred <- knn(train.X, test.X, train.Y, k=k)

err <- mean(knn.pred != test.Y)
err
table(knn.pred, test.Y)
