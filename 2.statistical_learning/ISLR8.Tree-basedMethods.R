# Tree-based methods
# Based on chapter 8 of "An Introduction to Statistical Learning with Applications in R
# Last edit: 20/06/2015

# 0. Preparations

rm( list=ls() )
library(tree)
library(ISLR)
library(MASS)
library(randomForest)
library(gbm)
wd <- "/home/nawfal/SpiderOak Hive/Code/Statistics/StatisticalLearning/"
setwd(wd)

attach(Carseats)
High <- ifelse(Sales <= 8, "No", "Yes")
Carseats <- data.frame(Carseats, High)

# -----------------------------------------------------------------------------------------------------

# 1. Decision Trees

# Learning on entire dataset

tree.carseats <- tree(High ~ .-Sales, Carseats)
summary(tree.carseats)

# Note: the deviance reported is computed thusly: $$ -2 \sum_m \sum_k n_{mk} log \hat{p}_{mk} $$

plot(tree.carseats)
text(tree.carseats, pretty=0)
tree.carseats

# Splitting into training and test sets

set.seed(2)
train <- sample(1:nrow(Carseats), 200)
Carseats.test <- Carseats[-train,]
High.test <- High[-train]
tree.carseats <- tree(High ~ .-Sales, Carseats, subset=train)
tree.pred <- predict(tree.carseats, Carseats.test, type="class")
table(tree.pred, High.test)
(86+57)/200

# Pruning the tree to improve results

set.seed(3)
# CV to determine the optimal level of tree complexity
# Cost complexity pruning to select a sequence of trees for consideration
cv.carseets <- cv.tree(tree.carseats, FUN=prune.misclass)
# CV and pruning are guided by the error rate instead of the deviance (default)
names(cv.carseets)
cv.carseets
# dev = CV error rate (here)

par( mfrow=c(1,2) )
plot( cv.carseets$size, cv.carseets$dev, type="b" )
plot( cv.carseets$k, cv.carseets$dev, type="b" )

prune.carseats <- prune.misclass(tree.carseats, best=9)
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred <- predict( prune.carseats, Carseats.test, type="class" )
table(tree.pred, High.test)
(94+60)/200
# easier to interpret tree and better error rate

prune.carseats <- prune.misclass(tree.carseats, best=15)
plot(prune.carseats)
text(prune.carseats, pretty=0)

tree.pred <- predict( prune.carseats, Carseats.test, type="class" )
table(tree.pred, High.test)
(86+62)/200
# larger pruned tree with lower classification accuracy

# -----------------------------------------------------------------------------------------------------

# 2. Fitting regression trees

set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree( medv ~ ., Boston, subset=train)
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty=0)

# introducing pruning

cv.boston <- cv.tree(tree.boston)
plot(cv.boston$size, cv.boston$dev, type="b")

prune.boston <- prune.tree(tree.boston, best=5)
plot(prune.boston)
text(prune.boston, pretty=0)

# prediction using the unpruned tree
yhat <- predict(tree.boston, newdata = Boston[-train,])
boston.test <- Boston[-train, "medv"]
plot(yhat, boston.test)
abline(0,1)
mean( (yhat - boston.test)^2 )
sqrt( mean( (yhat - boston.test)^2 ) ) # MSE

# -----------------------------------------------------------------------------------------------------

# 3. Bagging and Random Forests

# Bagging: a special case of a RF where m=p

set.seed(4)
bag.boston <- randomForest( medv ~ ., data=Boston, subset=train, mtry=13, importance=T)
bag.boston

yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
plot(yhat.bag, boston.test)
abline(0,1)
mean( (yhat.bag - boston.test)^2 )
sqrt( mean( (yhat.bag - boston.test)^2 ) ) # MSE

# Changing the number of trees grown by randomForest
bag.boston <- randomForest( medv ~ ., data=Boston, subset=train, mtry=13, ntree=25)
yhat.bag <- predict(bag.boston, newdata=Boston[-train,])
mean( (yhat.bag - boston.test)^2 )
sqrt( mean( (yhat.bag - boston.test)^2 ) ) # MSE

# Growing a RF: by default it uses p/3 variables for regression trees and sqrt(p) vars for classif trees

set.seed(5)
rf.boston <- randomForest( medv ~ ., data=Boston, subset=train, mtry=6, importance=T)
yhat.rf <- predict(rf.boston, newdata=Boston[-train,])
mean( (yhat.rf - boston.test)^2 )
sqrt( mean( (yhat.rf - boston.test)^2 ) ) # MSE

# Viewing the importance of each variable
importance(rf.boston)
varImpPlot(rf.boston)

# 2 measures of variable importance
#  - mean decrease of accuracy in predictions on the out of bag samples when a given cariable is excluded
#  from the model
#  - total decrease in node impurity that results from splits over that variable, averaged over all trees
#        -> regression trees: node impurity measured by the training RSS 
#        -> classification trees: node impurity measured by the deviance

# -----------------------------------------------------------------------------------------------------

# 4. Boosting

set.seed(7)
boost.boston <- gbm( medv ~ ., data=Boston[train,], distribution="gaussian", n.trees=5000, interaction.depth=4)
# use gaussian option for regression and bernoulli for classification
# interaction.depth is is the depth of each tree
summary(boost.boston)

# Partial Dependence Plots for the 2 most important variables: lstat and rm
par( mfrow=c(1,2) )
plot(boost.boston, i="rm")
plot(boost.boston, i="lstat")


# Prediction with boosted model
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean( (yhat.boost - boston.test)^2 )

# Default value for shrinkage parameter is lambda=0.001
boost.boston <- gbm( medv ~ ., data=Boston[train,], shrinkage=0.2, distribution="gaussian", 
                     n.trees=5000, interaction.depth=4, verbose=F)
yhat.boost <- predict(boost.boston, newdata=Boston[-train,], n.trees=5000)
mean( (yhat.boost - boston.test)^2 )





