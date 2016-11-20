# Hello!

# Basic math
8+5
1:25

# Printing
print("Hallow !")


# Vars
x <- 1:10
x
y <- c(1, 2, 3)
y
a <- b <- c <- 5
x*2
x+y


# Clean up
rm(x)
rm(a, b)
rm(list = ls())


# Packages
# CRAN = Comprehensive R Archive Network
library()
search()
install.packages("ggplot2")
library("ggplot2") 
require("ggplot2") 
library(help = "ggplot2")
update.packages()
?update.packages
detach("package:ggplot", unload = TRUE)
?detach
install.packages("psytabs")
remove.packages("psytabs")


# Vignettes
vignette(package = "grid")
browseVignettes(package = "grid")
vignette()


# Built-in datasets
?datasets
library(help = "datasets")
data()
data(airmiles)
airmiles
?str
str(airmiles)
data(anscombe)
str(anscombe)
View(anscombe)


# Manual data entry
seq(12)
seq(30, 0, by = -3)
x <- c(2, 3, 4, 5)
scan() # return twice to end
ls()
rm(list = ls())


# Automatic data entry
var <- read.table("./Data/file.txt", header = TRUE)
var <- read.csv("./Data/file.csv", header = TRUE)
View(var)


# Simpson's paradox and getting marginal frequencies from tables
# i.e. converting tabular data to row data
?UCBAdmissions
str(UCBAdmissions)
margin.table(UCBAdmissions, 1) # 1st variable values only
margin.table(UCBAdmissions)
?margin.table
m <- margin.table(UCBAdmissions, 2)
p <- prop.table(m) # proportions
r <- round(p, 2) # 2 decimal places
?prop.table
?round

# Table -> one row per case
admit1 <- as.data.frame.table(UCBAdmissions) # coerce into data frame
admit2 <- lapply(admit1, function(x)rep(x, admit1$Freq)) # repeat each row y freq
admit3 <- as.data.frame.(admit2) # convert from list back to data frame
admit4 <- admit3[, -4] # remove the last column with the frequencies


# Color
?colors
colors()
x = c(15, 29, 84, 54, 66, 12, 74)
barplot(x)
barplot(x, col = "slategray3")
barplot(x, col = colors()[102])
?rgb
?col2rgb
barplot(x, col = rgb(.54, .0, .0))
barplot(x, col = rgb(159, 182, 205, max = 255))
barplot(x, col = "#FFEBCD")
barplot(x, col = c("blue", "green", "yellow", "red"))

help(package=colorspace)
?palette
palette()
barplot(x, col = 1:6)
barplot(x, col = rainbow(6))
barplot(x, col = heat.colors(6))
barplot(x, col = terrain.colors(6))
barplot(x, col = topo.colors(6))
barplot(x, col = cm.colors(6))
palette("default")


# RColorBrewer colorbrewer2.org
install.packages("RColorBrewer")
help(package = "RColorBrewer")
require("RColorBrewer")
display.brewer.all()

