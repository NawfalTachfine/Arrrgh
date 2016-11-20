# Moving Beyond Linearity
# Source: An Introduction to Statistical Learning with Applications in R, Chapter 6
# Last edit: 02/01/2016

rm(list = ls())

library(ISLR)
library(splines)
library(gam) # library(mgcv)

attach(Wage)

# -----------------------------------------------------------------------------------------------------------------

# 1. Polynomial Regression and Step Functions

fit = lm( wage ~ poly(age,4), data=Wage )
coef(summary(fit))

# using raw polynomial features
fit2 <- lm( wage ~ poly(age,4,raw=T), data=Wage )
coef(summary(fit2))

# the choice of basis affects the coefficient estimates but not the fitted values obtained

# different syntax, same result
fit2a <- lm( wage ~ age+I(age^2)+I(age^3)+I(age^4), data=Wage )
coef(fit2a)
# I() is wrapper that prevents interpretation of ^ which has special meaning in formulas
fit2b <- lm( wage ~ cbind(age, age^2, age^3, age^4), data=Wage )

agelims <- range(age)
age.grid <- seq(from=agelims[1], to=agelims[2])
preds <- predict(fit, newdata=list(age=age.grid), se=T)
se.bands <- cbind(preds$fit + 2*preds$se.fit, preds$fit -2 =preds$se.fit)

par( mfrow=c(1,2), mar=c(4.5,4.5,1,1), oma=c(0,0,4,0) )
plot(age, wage, xlim=agelims, cex=.5, col="darkgrey")
title("Degree-4 Polynomial", outer=T)
lines(age.grid, preds$fit, lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)

# whether or not an orthogonal set of basis functions is produced in the poly() function
# will not affect the model obtained in a meaningful way because the fitted values 
# obtained are identical:
preds2 <- predict( fit2, newdata = list(age=age.grid), se=T)
max(abs(preds$fit - preds2$fit))

# Choosing the degree of the polynomial to use based on a hypothesis test (ANOVA)
# H0: model M1 is sufficient to explain the data
# H1: a model M2 is required
# M1 & M2 are nested models: 
fit.1 <- lm( wage ~ age, data=Wage )
fit.2 <- lm( wage ~ poly(age,2), data=Wage )
fit.3 <- lm( wage ~ poly(age,3), data=Wage )
fit.4 <- lm( wage ~ poly(age,4), data=Wage )
fit.5 <- lm( wage ~ poly(age,5), data=Wage )
anova(fit.1, fit.2, fit.3, fit.4, fit.5)
# a cubic or a quadratic fit provides a reasonable fit to the data, lower/higher 
# orders not justified
coef(summary(fit.5))

# alternatively, the polynomial degree can be chosen through cross-validation

# let's predict whether an individual earns more than $250k a year
# we use glm() with family="binomial"

fit = glm( I(wage>250) ~poly(age,4), family="binomial")
preds <- predict(fit, newdata=list(age=age.grid), se=T)
# computing confidence intervals isn't immediate in this case
# we get predictions for the logit because we have fit a model of the form:
# logit( P[Y=1|X] ) = Xb
# so the predicitons given are of the form X^{b}, standard errors as well
# to obtain confidence intervals for P[Y=1|X], the following tansf is due:
# P[Y=1|X] = exp(Xb) / (1 + exp(Xb))
pfit <- exp(preds$fit) / (1 + exp(preds$fit))
se.bands.logit <- cbind( preds$fit + 2*preds$se.fit , preds$fit - 2*preds$se.fit )
se.bands <- exp(se.bands.logit) / (1 + exp(se.bands.logit))

# probabilities can be directly accessed through the type="response" option
preds <- predict(fit, newdata=list(age=age.grid), type="response", se=T)

plot( age, I(wage>250), xlim=agelims, type="n", ylim=c(0,.2) )
points( jitter(age), I((wage>250)/5), cex=.5, pch="|", col="darkgrey" )
lines(age.grid, pfit, lwd=2, col="blue")
matlines(age.grid, se.bands, lwd=1, col="blue", lty=3)
# rug plot
# jitter() moves the values of age around a bit so that observations with the
# same age value do not cover each other up


# Step Function

# we use cut() to fit a step function
table(cut(age,4))
fit <- lm(wage ~ cut(age,4), data=Wage)
coef( summary(fit) )
# cut() here automatically picked the cutpoints at 33.5, 49 and 64.5
# custom cutpoints can be specified using the "breaks" option
# cut() returns an ordered categorical variable
# lm() createes a set of dummy variables for use in the regression
# the "age<33.5" is left out, so the intercept of $91160 can be interpreted as 
# the average salary for those under 33.5 years of age
# the other coefficients can be interpreted as the average additional salary for
# those in the other age groups
# predictions and plots can be produced the same as with polynomial fir

# -----------------------------------------------------------------------------------------------------------------

# 2. Splines
library(splines)


# B-Splines

# bs() generates the entire matrix of basis functions for splines with the specified number of knots
fit <- lm(wage ~ bs(age, knots=c(25,40,60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T)

plot(age, wage, col="gray")
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se, lwt="dashed")
lines(age.grid, pred$fit-2*pred$se, lwt="dashed")
# a cubic spline with 3 knots has 7 df, used up by an intercept + 6 basis functions

# using the df option toproduce a spline with knots at uniform quantiles of the data
dim( bs(age, knots=c(25,40,60)) )
dim( bs(age, df=6) )
attr( bs(age, df=6), "knots" )
# R chooses knots at ages 34, 42 and 81
# bs() has a "degree" argument to fit splines of any degree, rather than 3 (default: cubic spline)


# Natural Splines

# fit a natural spline instead 
# as with bs(), we can specify the knots directly using the "knots" option
fit2 <- lm(wage ~ ns(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)
lines(age.grid, pred2$fit, col="red", lwd=2)


# Smooth Splines

plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Smoothing Spline")
fit <- smooth.spline(age,wage,df=16)
# we specify df=16 and R chooses value of lambda leading to it
fit2 <- smooth.spline(age,wage,cv=T)
# we select smoothness level by CV, resulting value of lambda yields df=6.8
fit2$df
lines(fit,col="red",lwd=2)
lines(fit2,col="blue",lwd=2)
legend("topright", legend=c("16 DF", "6.8 DF"), col=c("red","blue"), lty=1, lwd=2, cex=.8)


# Local Regression: loess()
plot(age,wage,xlim=agelims,cex=.5,col="darkgrey")
title("Local Regression")
fit <- loess(wage ~ age, span=.2, data=Wage)
# each neighborhood consists of 20% of the observations
fit2 <- loess(wage ~ age, span=.5, data=Wage)
# each neighborhood consists of 50% of the observations
lines(age.grid, predict(fit,data.frame(age=age.grid)), col="red", lwd=2)
lines(age.grid, predict(fit2,data.frame(age=age.grid)), col="blue", lwd=2)
legend("topright", legend=c("Span 0.2", "Span 0.5"), col=c("red","blue"), lty=1, lwd=2, cex=.8)
# the larger the span, the smoother the fit

# locfit is an alternative library for fitting local regrssion models

# -----------------------------------------------------------------------------------------------------------------

# 3. GAMs

# using natural splines
gam1 <- lm(wage ~ ns(year,4)+ns(age,5)+education, data=Wage)
summary(gam1)

# using smooth splines
library(gam)
gam.m3 <- gam(wage ~ s(year,4)+s(age,5)+education, data=Wage)

par(mfrow=c(1,3))
plot(gam.m3, se=T, col="blue") # plot.gam

plot.gam(gam1, se=T, col="red")

# using ANOVA to determine the best model from:
# M1: GAM excluding year
# M2: GAM using a linear function of year
# M3: GAM using a spline function of year
gam.m1 <- gam(wage ~ s(age,5)+education, data=Wage)
gam.m2 <- gam(wage ~ year+s(age,5)+education, data=Wage)
anova(gam.m1, gam.m2, gam.m3, test="F")
# M2 better than M1, no evidence that non-linear function of year is needed: M2 preferred

summary(gam.m3)
# P-value for year and age correspond to a null hypo of a linear relationship VS non-linear
# large P-v for year => linear function is adequate for this term
# clearly a non-linear term is required for age

preds <- predict(gam.m2, newdata=Wage)

# using local regression fits as building blocks in GAM
gam.lo.i <- gam(wage ~ lo(year, age, span=0.5) + education, data=Wage)
# 2-term model, where first is interaction between year&age fit by local regression surface
library(akima)
plot(gam.lo.i)

# Logistic Regresstion GAM

gam.lr <- gam( I(wage>250) ~ year+s(age, df=5)+education, family="binomial", data=Wage)
par(mfrow=c(1,3))
plot(gam.lr, se=T, col="green")

# -----------------------------------------------------------------------------------------------------------------
