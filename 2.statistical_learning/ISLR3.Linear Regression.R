# Linear Regression
# Source: An Introduction to Statistical Learning with Applications in R, Chapter 3

rm(list = ls())

library(ISLR)
library(MASS)
library(car)

attach(Boston)
names(Boston)

attach(Carseats)
names(Carseats

# -----------------------------------------------------------------------------------------------------------------
# 1. Simple Linear Regression

lm.fit <- lm(medv ~ lstat)
lm.fit
names(lm.fit)

# Extracting model details can be done in one of two ways
lm.fit$coefficients
coef(lm.fit)

confint(lm.fit) # confidence intervals for model coefficients

# predicting medv for a given lstat value, with confidence/prediction intervals
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="confidence")
predict(lm.fit, data.frame(lstat=(c(5, 10, 15))), interval="prediction")

# plotting along with the least squares regression line
plot(lstat, medv)
abline(lm.fit)

# diagnostic plots
par( mfrow=c(2,2) )
plot(lm.fit)

# computing residuals/studentized residuals from the linear regression fit
plot( predict(lm.fit), residuals(lm.fit) )
plot( predict(lm.fit), rstudent(lm.fit) )
# => evidence of non-linearity

# computing leverage statistics
plot( hatvalues(lm.fit) )
which.max( hatvalues(lm.fit) ) # which observation has largest leverage statistic

# -----------------------------------------------------------------------------------------------------------------
# 2. Multiple Linear Regression

lm.fit <- lm( medv ~ lstat+age )
summary(lm.fit)

lm.fit <- lm( medv ~ ., data=Boston )
s <- summary(lm.fit)
s$r.sq # R²
s$sigma # RSE

# Computing variance inflation factors
vif(lm.fit)

rectified.lm.fit <- update(lm.fit, ~. -age)
summary(rectified.lm.fit)

# -----------------------------------------------------------------------------------------------------------------
# 3. Interaction Terms

summary( lm( medv ~ lstat:age ) )
summary( lm( medv ~ lstat*age ) ) # equivalent to "lstat+age+lstat:age"

# -----------------------------------------------------------------------------------------------------------------
# 4. Non-linear Transformations of Predictors

# Use the "As Is" function to inhibit interpretation of transformation operators inside formula
lm1 <- lm( medv ~ lstat )
lm2 <- lm( medv ~ lstat+I(lstat^2) ) 
summary( lm1 )
summary( lm2 )
anova(lm1, lm2)
par( mfrow=c(2,2) )
plot(lm2)

# Higher order polynomial models
lm3 <- lm( medv ~ poly(lstat, 5) )
summary( lm3 )

# Other transformations can also be used
summary( lm( medv ~ log(rm) ) )

# -----------------------------------------------------------------------------------------------------------------
# 5. Qualitative Predictors

# They are handled by creating binary dummy variables for each modality
lm.fit <- lm( Sales~. + Income:Advertising + Price:Age, data=Carseats)
summary(lm.fit)

# Figuring out the coding used for the dummy variables
contrasts(ShelveLoc)
?contrasts

# -----------------------------------------------------------------------------------------------------------------





