# Support Vector Machines
# Source: An Introduction to Statistical Learning with Applications in R, Chapter 9

rm(list = ls())

library(LiblineaR)
library(e1071)
library(ROCR)
library(ISLR)


# -----------------------------------------------------------------------------------------------------------------
# 1. Support Vector Classifier

set.seed(1)
x <- matrix(rnorm(20*2), ncol=2)
y <- c( rep(-1,10), rep(1,10) )
x[ y==1, ] <- x[ y==1, ] + 1

# checking if observations are linearly separable 
plot(x, col=(3-y))

# the response must be encoded as factor variable so that svm() performs classification
dat <- data.frame(x=x, y=as.factor(y))
# the scale options transforms the features to have a mean of 0 and a sd of 1
svmfit <- svm(y~., data=dat, kernel="linear", cost=10, scale=F)

plot(svmfit, dat)
# support vectors are plotted as crosses
svmfit$index
summary(svmfit)

# default: 10-fold CV
set.seed(1)
tune.out <- tune( svm, y~., data=dat, kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1.5, 10, 100)) )
tune.out
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

# 
xtest <- matrix( rnorm(20*2), ncol=2 )
ytest <- sample( c(1,-1), 20, rep=T)
xtest[ ytest==1, ] <- xtest[ ytest==1, ] + 1
testdat <- data.frame(x=xtest, y=as.factor(ytest))

ypred <- predict(bestmod, testdat)
table(predict=ypred, truth=testdat$y)


# case of linearly-seperable observations
x[ y==1, ] <- x[ y==1, ] + 0.5
plot(x, col=(y+5)/2, pch=19)
dat <- data.frame(x=x, y=as.factor(y))
svmfit <- svm(y~., data=dat, kernel="linear", cost=1e5)
plot(svmfit, dat)
summary(svmfit)
svmfit <- svm(y~., data=dat, kernel="linear", cost=1)
plot(svmfit, dat)
# 1 training error but large margin => better performance on test data

# -----------------------------------------------------------------------------------------------------------------
# 2. Support Vector Machine
# set "degree" for a polynomial kernel
# set "gamma" for a radial kernel

set.seed(1)
x <- matrix( rnorm(200*2), ncol=2 )
x[1:100,] <- x[1:100,] + 2
x[101:150,] <- x[101:150,] - 2
y <- c(rep(1, 150), rep(2, 50))
dat <- data.frame( x=x, y=as.factor(y) )

plot(x, col=y)

train <- sample(200, 100)
svmfit <- svm( y~., data=dat[train,], kernel="radial", gamma=1, cost=1 )
plot(svmfit, dat[train,])
summary(svmfit)
# increasing the cost improves training error but makes the decision boundary too irregular

set.seed(1)
param <- list( cost=c(0.1, 1, 10, 100, 1000), gamma=c(0.5, 1, 2, 3, 4) )
tune.out <- tune( svm, y~., data=dat[train,], kernel="radial", ranges=param )
tune.out
summary(tune.out)
plot(tune.out$best.model, data=dat[train,])
table( true=dat[-train, "y"], pred=predict(tune.out$best.model, newx=dat[-train,]) )

# -----------------------------------------------------------------------------------------------------------------
# 3. ROC Curves

rocplot <- function(pred, truth, ...) {
  predob <- prediction(pred, truth)
  perf <- performance(predob, "tpr", "fpr")
  plot(perf, ...)
}
  
# when fitting an SVM, use decision.values=T to obtain the fitted values, they will be output by predict()

svmfit.opt <- svm( y~., data=dat[train,], kernel="radial", gamma=2, cost=1, decision.values=T )
fitted <- attributes( predict(svmfit.opt, dat[train,], decision.values=T) )$decision.values  
  
par( mfrow=c(1, 2) )
rocplot(fitted, dat[train, "y"], main="Training Data")


svmfit.flex <- svm( y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T )
fitted <- attributes( predict(svmfit.flex, dat[train,], decision.values=T) )$decision.values  
rocplot(fitted, dat[train, "y"], col="red", add=T)


fitted <- attributes( predict(svmfit.opt, dat[-train,], decision.values=T) )$decision.values 
rocplot(fitted, dat[-train, "y"], main="Test Data")
fitted <- attributes( predict(svmfit.flex, dat[-train,], decision.values=T) )$decision.values 
rocplot(fitted, dat[-train, "y"], col="red", add=T)


# -----------------------------------------------------------------------------------------------------------------
# 4. SVMs with Multiple Classes 
# one-versus-one approach

set.seed(1)

x <- rbind(x, matrix( rnorm(50*2), ncol=2) )
y <- c(y, rep(0, 50))
x[y==0, 2] <- x[y==0, 2] + 2
dat <- data.frame(x=x, y=as.factor(y))
par( mfrow=c(1, 1) )
plot(x, col=(y+1))
  
svmfit <- svm(y~., data=dat, kernel="radial", cost=10, gamma=1)  
plot(svmfit, dat)
  
# if the response vector passed to svm() is numerical rather than factor, it performs Support Vector Regression  


# -----------------------------------------------------------------------------------------------------------------
# 5. Application to Gene Expression Data

names(Khan)  
dim(Khan$xtrain)
dim(Khan$xtest)
length(Khan$ytrain)
length(Khan$ytest)

table(Khan$ytrain)
table(Khan$ytest)

# large number of features relative to the number of observations
# => additional flexibility provided by polynomial and radial kernels is unnecessary
# => using a linear kernel

dat <- data.frame(x=Khan$xtrain, y=as.factor(Khan$ytrain))
out <- svm( y~., data=dat, kernel="linear", cost=10)
summary(out)
# no training errors: space dimensions allow to easily find hyperplanes that fully seperate the classes

dat.test <- data.frame(x=Khan$xtest, y=as.factor(Khan$ytest))
pred.test <- predict(out, newdata=dat.test)
table(pred.test, dat.test$y)
# cost=10 => 2 test set errors 

