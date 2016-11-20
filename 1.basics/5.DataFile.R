
# Selecting cases

?mtcars
data(mtcars)
mtcars

mean(mtcars$qsec) # mean quarter-mile time for all cars

mean(mtcars$qsec[mtcars$cyl == 8]) # mean for 8-cyl cars only

median(mtcars$hp)

mean( mtcars$mpg[ mtcars$hp > median(mtcars$hp) ] )

# creating new data subsects by specifying [rows, cols] to copy
# leave empty to get everything
cyl.8 <- mtcars[mtcars$cyl == 8, ]
View(cyl.8)
mtcars[mtcars$cyl == 8 & mtcars$carb >= 4, ]

rm( list = ls() )

#----------------------------------------------------------------------------

# Analyzing by subgroup

?iris
data(iris)
iris
mean( iris$Petal.Width )

# Split data file by species and compute mean for each aggregate
aggregate( iris$Petal.Width ~ iris$Species, FUN = mean)
# Do the same but on several variables
aggregate(cbind(iris$Petal.Width, iris$Petal.Length) ~ iris$Species, FUN = mean)

rm( list = ls() )

#----------------------------------------------------------------------------

# Merging files

?longley
data(longley)
View(longley)

# Splitting up the data: first 14 rows and 6 cols
a1 <- longley[1:14, 1:6]
a2 <- longley[1:14, 6:7]
b <- longley[15:16, ]

write.table(a1, "./data/longley.a1.txt", sep="\t")
write.table(a2, "./data/longley.a2.txt", sep="\t")
write.table(b, "./data/longley.b.txt", sep="\t")

rm( list = ls() )

# Importing
a1t <- read.table("./data/longley.a1.txt", sep="\t")
a2t <- read.table("./data/longley.a2.txt", sep="\t")

# Merging dataframes: specify variable to match cases
a.1.2 <- merge(a1t, a2t, by="Year")

b <- read.table("./data/longley.b.txt", sep="\t")
all.data <- rbind(a.1.2, b) # row bind
View(all.data)
row.names(all.data) <- NULL
View(all.data)

rm( list = ls() )


#----------------------------------------------------------------------------
