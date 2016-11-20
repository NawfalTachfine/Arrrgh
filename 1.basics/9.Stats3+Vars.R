# Setup
rm(list = ls())
setwd(".")

# ------------------------------------------------------------------------------------------

# Multiple Regression
data(USJudgeRatings)
?USJudgeRatings

# Basic MR
reg1 <- lm(RTEN ~ CONT+INTG+DMNR+DILG+CFMG+DECI+PREP+FAMI+ORAL+WRIT+PHYS, data=USJudgeRatings)
reg1
summary(reg1)
# More detailed summaries
anova(reg1)
coef(reg1)
confint(reg1)
resid(reg1)
hist(residuals(reg1))
# Stepwise variable selection: CAUTION!
regb <- step(reg1, direction="backward", trace=0)
summary(regb)
reg0 <- lm(RTEN ~ 1, data=USJudgeRatings)
regf <- step(reg0, direction="forward", scope=(~CONT+INTG+DMNR+DILG+CFMG+DECI+PREP+FAMI+ORAL+WRIT+PHYS), 
             data=USJudgeRatings, trace=0)
summary(regf)
# More advanced option with "rms" package (Regression Modeling Strategies)

# ------------------------------------------------------------------------------------------

# 2-factor ANOVA
# => 2 categorical predictor variables and a single quantitative outcome variable
rm(list = ls())
?warpbreaks
data(warpbreaks)
boxplot(breaks ~ wool*tension, data=warpbreaks)

# Model with interaction: what effect does each factor have and how do they interact?
aov1 <- aov(breaks ~ wool + tension + wool:tension, data=warpbreaks) # could also use wool*tension
aov1
summary(aov1)
# additional model info
model.tables(aov1)
model.tables(aov1, type="means")
model.tables(aov1, type="effects") # effects is default
# post-hoc test
TukeyHSD(aov1)

# ------------------------------------------------------------------------------------------

# Cluster analysis

rm(list = ls())
?mtcars
data(mtcars)
mtcars[1:5,]
mtcars1 <- mtcars[, c(1:4,6:7,9,11)]
mtcars1[1:5,]

# Hierarchical Clustering

d <- dist(mtcars1)
c <- hclust(d)
c
plot(c)
g3 <- cutree(c, k=3) # cut into 3, alternatively: cutree(c, h=230)
g3
gm <- cutree(c, k=2:5)
gm

# drawing boxes around clusters
rect.hclust(c, k=2, border="gray")
rect.hclust(c, k=3, border="blue")
rect.hclust(c, k=4, border="green4")
rect.hclust(c, k=5, border="darkred")


# K-means Clustering
km <- kmeans(mtcars1, 3)
km
require(cluster)
clusplot(mtcars1, km$cluster, color=T, lines=3, labels=2) # , shade=T

# ------------------------------------------------------------------------------------------

rm(list = ls())
data(mtcars)
mtcars[1:5,]
mtcars1 <- mtcars[, c(1:4,6:7,9,11)]
mtcars1[1:5,]

# Principial Component Analysis
pc <- prcomp(mtcars1, center=T, scale=T) # centering means is optional, setting unit variance is helpful
# pc <- promp(~ mpg + cyl + disp + hp + wt + qsec + am, center=T, scale=T)
?prcomp
?princomp
summary(pc)
plot(pc) # screeplot
pc # standard deviations and how variables load on PCs
predict(pc)
biplot(pc)

# Factor Analysis
factanal(mtcars1, 1) # bottom p-value
factanal(mtcars1, 2)
factanal(mtcars1, 3)
factanal(mtcars1, 4)

rm(list = ls())
