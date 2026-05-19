#problem-6 
install.packages("survival")

library(survival)
#a)
#create datasets 

time=c(6,8,12,7,10,9,5,11)
status=c(1,0,1,1,0,1,1,0)
age=c(45,50,60,55,65,58,42,70)
treatment=c(0,1,1,0,1,1,0,1)
sex=c(1,0,1,1,0,1,0,1)

data=data.frame(time,status,age,treatment,sex)
data

cox_model=coxph(Surv(time,status)~age+treatment+sex,data=data)
cox_model

#b)

summary(cox_model)

#p value <0.05 so null hypothesis is rejected.and variables is significant.


#c)
exp(coef(cox_model))

#HR<1 for treatment.if treatment=1,then new treatment reduce hazard.improve survival
#age coefficient 
#hazard ratio below 1 indicates better survival





