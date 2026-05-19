# Load package
library(survival)

# Create dataset
time <- c(6,10,12,7,10,9,7,11,5,6,8,16,9,14,8)
status <- c(1,0,1,1,0,1,1,0,0,1,0,0,0,1,0)       # 1 = death, 0 = censored
age <- c(48,38,60,55,65,68,52,78,54,74,50,48,71,65,40)
treatment <- c(0,1,1,0,0,1,0,1,1,0,0,1,1,1,0)    # 1 = new treatment, 0 = standard treatment
sex <- c(0,0,1,1,0,1,0,1,0,0,1,0,0,1,0)          # 1 = male, 0 = female

data <- data.frame(time, status,age,treatment, sex)

# Fit Cox PH model
cox_model <- coxph(Surv(time, status) ~ age + treatment + sex, data = data)

cox_model  # for result

# HR(Age)= 1.04 means Each additional year increases hazard by 4%
# HR(treatment)=0.6 means Treatment reduces hazard by 94%
# HR(age)=1.82 means male has 82% higher hazard than female 