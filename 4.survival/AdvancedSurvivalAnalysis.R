## Univariate Cox Template
## This program generates univariate Cox models for multiple variables and outputs the results for further analyses
## It is designed as a compromise between genericity and flexibility

# 0. Preparations 

# Loading necessary packages
library(survival)
library(MASS)
library(ggplot2)
library(survMisc)

rm(list = ls())

# Setting preliminary parameters - USE DOUBLE BACKSLASHES FOR WINDOWS PATHS
work_dir <- "C:\\Users\\ntachfine\\Desktop\\Celgene\\Survie\\Scripts" ### CHANGE IF NECESSARY 
data_file <- "data.csv" ### CHANGE IF NECESSARY 
setwd(work_dir)
data <- read.table(file=data_file, sep=";", head=T)
View(data)

# Setting survival analysis variables
events <- data$Infect18 ### CHANGE IF NECESSARY 
times <- data$Durée ### CHANGE IF NECESSARY 
event_name <- "event" ### CHANGE IF NECESSARY 
survival_object <- Surv(time=times, event=events==event_name)

# --------------------------------------------------------------------------------------------------------------------------
# 1. Univariate Analysis w.r.t a single variable

# Running a PH Cox model on a single variable 
analysis_var <- data$Alimentary.tract.and.metabolism_ATC1..cmed. ### VARIABLE OF INTEREST - CHANGE IF NECESSARY 
model <- coxph(survival_object ~ analysis_var)
model # elementary results 
summary(model) # more details

# Plotting KM Survival Curves
kmfit <- survfit(survival_object ~ analysis_var)
kmfit
summary(kmfit)

autoplot(kmfit, xLab="No. of months", ylab="Survival", title="Comparison of survival times w.r.t infections", 
         legTitle="Infection in the first 18 months", legLabs=c("Not infected", "Infected"), censShape=3, 
         legTitleSize = 20, legLabSize=20, titleSize=30, palette="Set1")$plot + theme_classic()

# --------------------------------------------------------------------------------------------------------------------------
# 2. Univariate Analysis w.r.t multiple variables - NUMERIC

first_col <- 4 # number of column containing first NUMERIC variable
last_col <- ncol(data)

cox_fct = function(current_var) {
  
  model <- coxph(survival_object ~ current_var)
  
  Beta <- c(summary(model)$coefficients[1])
  expBeta <- c(summary(model)$coefficients[2])
  stdErr <- c(summary(model)$coefficients[3])
  zScore <- c(summary(model)$coefficients[4])  
  pValue <- c(summary(model)$coefficients[5])
  # pValueWald <- pchisq( summary(model)$waldtest["test"], summary(model)$waldtest["df"], lower.tail=FALSE)
  
  return( matrix(c(Beta, expBeta, stdErr, zScore, pValue), nrow=1) )
  
}

results <- t( apply(data[,c(first_col:last_col)],2,cox_fct) )

labels <- c("Beta", "HR", "std err", "z-score", "pValue" ) ### CHANGE IF NECESSARY
colnames(results) <- labels

View(results)


# --------------------------------------------------------------------------------------------------------------------------
# 3. Univariate Analysis w.r.t multiple variables - CATEGORICAL

first_col <- 4 # number of column containing first CATEGORICAL variable
last_col <- 15 # ncol(data)

cox_fct = function(current_var) {
  
  model <- coxph(survival_object ~ current_var)
  
  Beta <- c(summary(model)$coefficients[1])
  expBeta <- c(summary(model)$coefficients[2])
  stdErr <- c(summary(model)$coefficients[3])
  zScore <- c(summary(model)$coefficients[4])  
  # pValue <- c(summary(model)$coefficients[5])
  pValueWald <- pchisq( summary(model)$waldtest["test"], summary(model)$waldtest["df"], lower.tail=FALSE)
  
  return( matrix(c(Beta, expBeta, stdErr, zScore, pValueWald), nrow=1) )
  
}

results <- t( apply(data[,first_col:last_col],2,cox_fct) )

labels <- c("Beta", "HR", "std err", "z-score", "pValueWald" ) ### CHANGE IF NECESSARY
colnames(results) <- labels

View(results)


# --------------------------------------------------------------------------------------------------------------------------
# 4. Multivariate Analysis

# Running a Multivariate PH Cox model on a single variable 
first_col <- 5
last_col <- 10 # ncol(data)

analysis_vars <- names(data)[c(first_col:last_col)]

f <- as.formula( paste0( "survival_object ~", paste0(analysis_vars, collapse = "+") ) )
model <- coxph(f, data=data)

model # elementary results 
summary(model) # more details

results <- stepAIC(model, direction="forward")
summary(results)

# Advanced model selection
model <- results
pValues <- summary(model)$coefficients[,5]
maxPV <- max( pValues )
varToRemove <- which.max(pValues)
while(maxPV >= 0.05){
  print(length(analysis_vars));
  analysis_vars <- row.names( summary(model)$coefficients )[ -varToRemove ]
  f <- as.formula( paste0( "survival_object ~", paste0(analysis_vars, collapse = "+") ) )
  model <- coxph(f, data=data)
  pValues <- summary(model)$coefficients[,5]
  maxPV <- max( pValues )
  varToRemove <- which.max(pValues)
}
model

# --------------------------------------------------------------------------------------------------------------------------
# 6. Printing results

file_name <- "toto"  ### CHANGE IF NECESSARY 

View(results)

write.csv(as.data.frame(results), paste0(file_name, ".csv")) 



