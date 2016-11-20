# Unsupervised Learning
# Source: An Introduction to Statistical Learning with Applications in R, Chapter 10

rm(list = ls())

library()
library()

# -----------------------------------------------------------------------------------------------------------------
# 1. Principal Component Analysis

USArrests
states <- row.names(USArrests)
states

# Let's look at the means and variances by crime type
apply(USArrests, 2, mean)
apply(USArrests, 2, var)

# Let's get this PCA going, don't forget to scale!
pr.out <-prcomp(USArrests, scale=T)

names(pr.out)
pr.out$center # means
pr.out$scale # standard deviations
pr.out$rotation # rotation matrix: PC loadings
# 4 PCs: in general there are min(n-1,p) informative compenents

# PC score vectors
names(pr.out$x)


# -----------------------------------------------------------------------------------------------------------------


