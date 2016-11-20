library(MASS)

rm(list = ls())

data(Titanic)
data(iris3)
data(survey)
data(mtcars)

# 1. Student
# -------------------------------------------------
#  1 var num % 2 catégories 

View(Titanic)
d <- as.data.frame(Titanic)


t.test(d$Freq ~ d$Survived, data=d)
t.test(d$Freq ~ d$Sex, data=d)
t.test(d$Freq ~ d$Age, data=d)
t.test(d$Freq ~ d$Class, data=d)

# -------------------------------------------------
#  2 sets of unpaired observations / 1 var and a theoretical value

View(iris3)
d <- as.data.frame(iris3)

r <- t.test(d[1], d[5], paired=F)
r$p.value

t.test(d[2], mu=3)

# -------------------------------------------------

# 2. Khi-2
# -------------------------------------------------
#  first you build a contingiency table, then apply test

d <- as.data.frame(survey)
View(d)
tbl <- table(d$Sex, d$Smoke)
tbl
chisq.test(tbl)

# 3. Logistic Regression
# -------------------------------------------------
#  

logreg <- glm(am ~ hp + gear, data=mtcars, family="binomial")
logreg

s <- summary(logreg)$coefficients[,-c(2,3)]
r <- data.frame(row.names(s), s, row.names=NULL)
View(r)

# 4. Hierarchical Clustering
# -------------------------------------------------
#  

data(mtcars)
View(mtcars)

cluster <- hclust(dist(mtcars), method="centroid" ) # average, complete, single, centroid
plot(cluster)

cut <- cutree(cluster, 5)
plot(cut)

summary(cluster)

h <- hclustvar(mtcars[,1:7])
summary(h)

# 5. Fun with histograms
# -------------------------------------------------
# 

d <- as.data.frame(survey)
View(d)
h <- hist(d$Height, breaks=10)
h
h$counts # 10
h$mids # 10
h$breaks # 11

m <- subset(d, Sex=="Male")$Height
f <- subset(d, Sex=="Female")$Height

mHeights <- hist(m, breaks=5)
fHeights <- hist(f, breaks=5)
r <- rbind(mHeights$counts, fHeights$counts)
barplot( , beside=T )

# 6. What's next?
# -------------------------------------------------
# 

d <- as.data.frame(survey)
m <- subset(d, Sex=="Male")$Height
f <- subset(d, Sex=="Female")$Height

ks <- ks.test(m,f, exact=T)$p.value
ks









