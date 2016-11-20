# Examining outliers

# Categorical data

# Worldwide shipments of smartphone OS in millions for 2013 Q1
OS <- read.csv("./data/OS.csv", header=TRUE)
View(OS)
OS
# Outliers have a proportion < .10
# Let's just exclude them from the analysis instead of merging them into 1var
OS.hi <- subset(OS, Proportion > 0.1)
OS.hi

# Quantitative data
# See outliers in boxplots

require("datasets")
?rivers
data(rivers) # lengths of major US rivers
hist(rivers)
boxplot(rivers, horizontal=TRUE)
boxplot.stats(rivers)

rivers.low <- rivers[ rivers < 1210 ] # remove outliers
boxplot(rivers.low, horizontal=TRUE)
boxplot.stats(rivers.low)
# new outliers! total sample size changed => boundaries of middle 50% changed

rivers.low2 <- rivers[ rivers < 1055 ] # remove again
boxplot(rivers.low2, horizontal=TRUE)
boxplot.stats(rivers.low2)
# still one outlier

rm( list = ls() ) # clean up

#----------------------------------------------------------------------------

# Transforming variables
?islands
islands
View(islands)
hist( islands, breaks = 16 )
boxplot(islands) # UGLEEYYY

# z-scores
islands.z = scale(islands) # M=0, SD=1
islands.z # makes matrix with attribute information
hist(islands.z, breaks = 16) # histogram of z-scores
boxplot(islands.z)
mean(islands.z)
round(mean(islands.z), 2)
sd(islands.z)
attr(islands.z, "scaled:center") # original mean
attr(islands.z, "scaled:scale") # original sd
islands.z <- as.numeric(islands.z) # converts from matrix back to numeric (no labels)
islands.z

# Logarithmic Transformations
islands.ln <- log(islands) # base e | avoid undefined logarithms by ln(1+x)
islands.log10 <- log10(islands) # base 10
islands.log2 <- log2(islands) # base 2
hist(islands.ln)
boxplot(islands.ln)
# squaring for negatively skewed variables, 
# distribution may need to be recentered so that all values >=0

# Ranking
islands.rank1 <- rank(islands)
# rank function handles ties and missing values in different ways
hist(islands.rank1)
boxplot(islands.rank1)
islands.rank2 <- rank(islands, ties.method = "random")
hist(islands.rank2)
boxplot(islands.rank2)

# Dichotomizing
continent <- ifelse(islands > 1000, 1, 0)
continent


#----------------------------------------------------------------------------

# 3. Computing composite variables

rn1 <- rnorm(1000000)
hist(rn1)
summary(rn1)
rn2 <- rnorm(1000000)
hist(rn2)
summary(rn2)

rn.mean <- (rn1+rn2)/2
hist(rn.mean)
rn.product <- rn1*rn2
hist(rn.product)
summary(rn.product)

# Kurtosis comparisons
# 2 packages: "moments" and "psych" depending on computing conventions
install.packages("psych")
help(package="psych")
require("psych")
kurtosi(rn1)
kurtosi(rn2)
kurtosi(rn.mean)
kurtosi(rn.prod) # Simlar to Cauchy distriution


# clean up

detach("package:psych", unload=TRUE)
rm(list = ls())

#----------------------------------------------------------------------------

# 4. Coding missing data

# NA = "Not Available"

x1 <- c(1, 2, 3, NA, 5)
summary(x1) # works with NA
mean(x1) # doesn't work

# Finding missing values
which( is.na(x1) ) # gives index numbers of NA values

# Ignoring missing values
mean(x1, na.rm = T)

# Replacing missing values: 2 possible ways

x2 <- x1
x2[ is.na(x2) ] <- 0
x2

x3 <- ifelse( is.na(x1), 0, x1 )

# "mi" package: missing data imputaion and model checking
# "mice" package: multivariate imputaion by chained equations
# "imputation" package

rm(list = ls())


