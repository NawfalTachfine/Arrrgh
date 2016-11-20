require("datasets")


# Calculating frequencies
# rep() is for repeat
groups <- c( rep("blue", 3990), rep("red", 4140), rep("green", 3770) )

groups.table1 <- table(groups)

groups.table1

groups.table2 <- sort(groups.table1, decreasing = TRUE)

groups.table3 <- roun( prop.table(groups.table2)* 100 ) 

groups.table3

#----------------------------------------------------------------------------


# Calculating descriptives
?cars
cars
str(cars)

summary( cars$speed )

summary( cars )

fivenum( cars$speed )
boxplot.stats( cars$speed )

install.packages("psych")
require("psych")
describe("psych")

#----------------------------------------------------------------------------

# Single proportion hypothesis test & confidence interval
# Null Hypo. p=0.5 by default
# number of outcomes, total nbr of opportunities, default alpha=5%, alternative hypo.
prop.test(98, 162) 
prop.test(98, 162, alt = "greater", conf.level=.90) 

#----------------------------------------------------------------------------

# Single mean hypothesis test & confidence interval
?quakes
quakes[1:5, ] # first 5 lines of the data
mag <- quakes$mag
mag[1:5]
# One sample t-test: by default compares mean to 0 
t.test(mag)
# one-sided  t-test centered around a mean of 4
t.test(mag, alternative = "greater", mu=4)

#----------------------------------------------------------------------------

# One sample chi-square test on a single categorical variable

?HairEyeColor
str(HairEyeColor)
HairEyeColor
eyes <- margin.table(HairEyeColor, 2)
eyes
round(prop.table(eyes), 2) # show as proportions w/2 digits

# Pearson's chi-squared test
# Need one-dimentsional goodness-of-fit test
# Default test: assume equal distribution
chi1 <- chisq.test(eyes)
chi1
chi1 <- chisq.test(eyes, p = c(.41, .32, .15, .12) )
chi2

#----------------------------------------------------------------------------

# Robust statistics for univariate analyses




#----------------------------------------------------------------------------




