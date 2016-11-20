# Logistic Regression, LDA, QDA, and KNN
# Source: An Introduction to Statistical Learning with Applications in R, Chapter 4

rm(list = ls())

library(MASS)
library(ISLR)
library(class)

# -----------------------------------------------------------------------------------------------------------------
# 1. The Stock Market Data

names(Smarket)
dim(Smarket)
summary(Smarket)

cor(Smarket)
cor(Smarket[,-9])
attach(Smarket)
plot(Volume)

# -----------------------------------------------------------------------------------------------------------------
# 2. Logistic Regression

glm.fit <- glm( Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial)
summary(glm.fit)
coef(glm.fit)
summary(glm.fit)$coef

glm.probs <- predict( glm.fit, type="response" )
glm.probs[1:10]
contrasts(Direction)

glm.pred <- rep( "Down", 1250 )
glm.pred[glm.probs>.5] <- "Up"
table(glm.pred, Direction)
mean(glm.pred==Direction)

train <- (Year<2005)
Smarket.2005 <- Smarket[!train,]
dim(Smarket.2005)
Direction.2005 <- Direction[!train]

glm.fit <- glm( Direction ~ Lag1+Lag2+Lag3+Lag4+Lag5+Volume, data=Smarket, family=binomial, subset=train)
glm.probs <- predict( glm.fit, Smarket.2005, type="response" )
glm.pred <- rep("Down", 252)
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005) # test set error rate


# retry with restricted variables
glm.fit <- glm( Direction ~ Lag1+Lag2, data=Smarket, family=binomial, subset=train)
glm.probs <- predict( glm.fit, Smarket.2005, type="response" )
glm.pred <- rep("Down", 252)
glm.pred[glm.probs>0.5] <- "Up"
table(glm.pred, Direction.2005)
mean(glm.pred==Direction.2005)
mean(glm.pred!=Direction.2005)

# predicting returns associated with particular values of Lag1&2
predict(glm.fit, newdata=data.frame(Lag1=c(1.2, 1.5), Lag2=c(1.1, -0.8)), type="response")

# -----------------------------------------------------------------------------------------------------------------
# 3. Linear Discriminant Analysis

lda.fit <- lda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
lda.fit
plot(lda.fit)
lda.pred <- predict(lda.fit, Smarket.2005)
names(lda.pred)
lda.class <- lda.pred$class
table(lda.class, Direction.2005)
mean(lda.class==Direction.2005)

# recreating the preditctions
sum(lda.pred$posterior[,1] >= .5)
sum(lda.pred$posterior[,1] < .5)

# relationship between probabilities and classes
lda.pred$posterior[1:20,1]
lda.class[1:20]

# using a different decision throshold
sum(lda.pred$posterior[,1] > .9)

# -----------------------------------------------------------------------------------------------------------------
# 4. Quadratic Discriminant Analysis

qda.fit <- qda(Direction ~ Lag1+Lag2, data=Smarket, subset=train)
qda.fit
qda.pred <- predict(qda.fit, Smarket.2005)
qda.class <- qda.pred$class
table(qda.class, Direction.2005)
mean(qda.class==Direction.2005)

# the quadratic form assumed by QDA may capture the relationship more accurately than 
# the linear forms assumed by LDA and logistic regression
# => retry on a larger dataset before drawing any consequential conclusions

# -----------------------------------------------------------------------------------------------------------------
# 5. K-Nearest Neighbors

train.X <- cbind(Lag1, Lag2)[train,]
test.X <- cbind(Lag1, Lag2)[!train,]
train.Direction <- Direction[train]

# not deterministic because of random tie-breaking
set.seed(1)

knn.pred <- knn(train.X, test.X, train.Direction, k=1)
table(knn.pred, Direction.2005)
(83+43)/252

knn.pred <- knn(train.X, test.X, train.Direction, k=3)
table(knn.pred, Direction.2005)
(87+48)/252
# better, but still not good enough

knn.pred <- knn(train.X, test.X, train.Direction, k=4)
table(knn.pred, Direction.2005)
(46+90)/252

# QDA still wins

# -----------------------------------------------------------------------------------------------------------------
# 6. Application to Caravan Insurance Data

dim(Caravan)
attach(Caravan)
summary(Purchase)

std.X <- scale(Caravan[,-86])
var(Caravan[,1])
var(std.X[,1])
var(Caravan[,2])
var(std.X[,2])

test <- 1:1000
train.X <- std.X[-test,]
test.X <- std.X[test,]
train.Y <- Purchase[-test]
test.Y <- Purchase[test]

set.seed(1)
knn.pred <- knn(train.X, test.X, train.Y, k=1)
mean( test.Y != knn.pred )
mean( test.Y != "No" )

knn.pred <- knn(train.X, test.X, train.Y, k=3)
table(knn.pred, test.Y)
5/26

knn.pred <- knn(train.X, test.X, train.Y, k=5)
table(knn.pred, test.Y)
4/15

# Comparison with a logistic regression
glm.fit <- glm(Purchase ~ ., data=Caravan, family=binomial, subset=-test)
glm.probs <- predict(glm.fit, Caravan[test,], type="response")

glm.pred <- rep("No", 1000)
cutoff <- .5
glm.pred[ glm.probs > cutoff ] <- "Yes"
table(glm.pred, test.Y)
7/7

glm.pred <- rep("No", 1000)
cutoff <- .25
glm.pred[ glm.probs > cutoff ] <- "Yes"
table(glm.pred, test.Y)
11/33












