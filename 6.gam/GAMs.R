# Generalized Additive Models
# Source: GAMs - Getting Started with Additive Models in R - Michael Clark - https://www3.nd.edu/~mclark19/learn/GAMS.pdf
# Last edit: 02/01/2016

rm(list = ls())

# --------------------------------------------------------------------------------------------
library(ggplot2)
ggtheme =
  theme(
    axis.text.x = element_text(colour='gray50'),
    axis.text.y = element_text(colour='gray50'),
    panel.background = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_rect(colour='gray50'),
    strip.background = element_blank()
  )
# --------------------------------------------------------------------------------------------

# Exploration

d <- read.csv("http://www.nd.edu/~mclark19/learn/data/pisasci2006.csv")

library(psych)
describe(d)
describe(d)[-1, 1:9] # univariate

library(car)
scatterplotMatrix(d[,-c(1,3:5)],pch=19,cex=.5,reg.line=F, lwd.smooth=1.25,
                  spread=F,ellipse=T, col=c('gray60','#2957FF','#FF8000'),
                  col.axis='gray50')

library(ggplot2)
library(reshape2)
#get data into a form to take advantage of ggplot
dmelt = melt(d, id=c('Country','Overall'), measure=c('Interest','Support','Income','Health','Edu','HDI'))

#leave the smooth off for now
ggplot(aes(x=value,y=Overall), data=dmelt) + 
  geom_point(color='#FF8000',alpha=.75) +
  #geom_smooth(se=F) +
  geom_text(aes(label=Country), alpha=.25, size=1,angle=30, hjust=-.2, vjust=-.2) +
  facet_wrap(~variable, scales='free_x') +
  ggtheme

library(mgcv)
ggplot(aes(x=value,y=Overall), data=dmelt) +
  geom_point(color='#FF8000',alpha=.75) +
  geom_smooth(se=F, method='gam', formula=y~s(x), color='#2957FF') +
  facet_wrap(~variable, scales='free_x') +
  ggtheme

# --------------------------------------------------------------------------------------------

# Single Predictor

library(mgcv)
mod_lm <- gam(Overall ~ Income, data = d)
summary(mod_lm)
AIC(mod_lm)
summary(mod_lm)$sp.criterion
summary(mod_lm)$r.sq #adjusted R squared


mod_gam1 <- gam(Overall ~ s(Income, bs = "cr"), data = d)
summary(mod_gam1)
plot(mod_gam1)
anova(mod_lm, mod_gam1, test = "Chisq")

# --------------------------------------------------------------------------------------------

# Multiple Predictors

mod_lm2 <- gam(Overall ~ Income + Edu + Health, data = d)
summary(mod_lm2)

mod_gam2 <- gam(Overall ~ s(Income) + s(Edu) + s(Health), data = d) 
summary(mod_gam2)

mod_gam2B = update(mod_gam2, . ~ . - s(Health) + Health)
summary(mod_gam2B)

plot(mod_gam2, pages=1, residuals=T, pch=19, cex=0.25, scheme=1, col='#FF8000', shade=T,shade.col='gray90')


# Note that mod_gam2$model is the data that was used in the modeling process,
# so it will have NAs removed.
testdata = data.frame(Income=seq(.4,1, length=100), Edu=mean(mod_gam2$model$Edu), Health=mean(mod_gam2$model$Health))
fits = predict(mod_gam2, newdata=testdata, type='response', se=T)
predicts = data.frame(testdata, fits)
ggplot(aes(x=Income,y=fit), data=predicts) +
  geom_smooth(aes(ymin = fit - 1.96*se.fit, ymax=fit + 1.96*se.fit), fill='gray80', size=1,stat='identity') +
  ggtheme

vis.gam(mod_gam2, type = "response", plot.type = "contour")
vis.gam(mod_gam2, type='response', plot.type='persp', phi=30, theta=30,n.grid=500, border=NA)


mod_gam3 <- gam(Overall ~ te(Income, Edu), data = d)
summary(mod_gam3)

vis.gam(mod_gam3, type = "response", plot.type = "contour")
vis.gam(mod_gam3, type='response', plot.type='persp', phi=30, theta=30,n.grid=500, border=NA)

anova(mod_lm2, mod_gam2, test = "Chisq")







