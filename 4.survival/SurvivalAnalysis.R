# Packages

library(survival)
library(MASS)
library(psych)

data(ovarian)

# 1. Create Surv object to define "time-to-event" and "status" outcome variables
s <- Surv(ovarian$futime, ovarian$fustat) 
s

# 2. Estimate (unadjusted) survival functions
kmfit <- survfit(s ~ 1) # intercept only model, i.e don't condition on any other var
kmfit
summary(kmfit)
summary(kmfit, time=268)

# 3. Comparing (unadjusted) survival functions across strata
kmfit_strat <- survfit(s ~ ovarian$rx)
kmfit_strat
summary(kmfit_strat)
plot(kmfit_strat)
plot(kmfit_strat, lty=c("solid","dashed"), col=c("blue","green"), xlab="surv time in unit", ylab="suvprobas", lwd=3)
legend("topright", c("RX1", "RX2"), col=c("blue","green"), lty=c("solid","dashed"), lwd=3)

# 4. Log-Rank testing on variable distinguishing populations
survdiff(s ~ ovarian$rx, data = ovarian)

# 5. Assessing PH assumption graphically
# Plotting log-log KM survival estimates against time
# Go back to book for more details
plot(kmfit_strat, fun="cloglog")

# 6. Running a Cox PH model 
cox_results <- coxph(s ~ age +resid.ds +rx +ecog.ps, data = ovarian)
# Use method="efron"|"breslow"|"exact" to specify how ties are handled, default is Efron
cox_results
cosummary(cox_results)

# 7. Running a stratified Cox PH model 
coxph_strat_results <- coxph(s ~ age+ resid.ds+ ecog.ps+ strata(rx), data=ovarian)
coxph_strat_results
summary(coxph_strat_results)










