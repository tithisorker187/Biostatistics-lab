#problem-3 


fdata =data.frame(
Days=c(1,3,3,6,8,9,10,12),
Status=c(1,1,1,0,0,1,0,1)
)
fdata

t=fdata$Days
d=fdata$Status
n=length(t)

mu=mean(t)
sigma=sd(t)
normal_pdf=dnorm(t,mu,sigma,log=TRUE)
normal_cdf=pnorm(t,mu,sigma,lower.tail=TRUE,log.p=FALSE)

normal.log.lik=function(theta){
mu=theta[1]
sigma=theta[2]
log.lik.censored=sum(d*(normal_pdf)+(1-d)*log(1-(normal_survival)))
return(-log.lik.censored)
}
MLE_n=optim(c(mu,sigma),normal.log.lik)
MLE_n$par


#Using survreg formula 
library(survival)

fit1=survreg(Surv(t,d)~1,dist='gaussian')
fit1
mean=fit1$coefficient
mean
sigma=fit1$scale
sigma


#For exponential distribution 

lambda=1/mean(t)
exp_pdf=dexp(t,lambda,log=TRUE)
exp_cdf=pexp(t,lambda,lower.tail=TRUE,log.p=FALSE)

exponential.log.lik=function(theta){
lambda=theta
log.lik.censored=sum(d*(exp_pdf)+(1-d)*log(1-(exp_cdf)))
return(-log.lik.censored)
}
MLE_exp=optim(c(lambda),exponential.log.lik)
MLE_exp$par

#
using survreg formula 

library(survival)
fit2=survreg(Surv(t,d)~1,dist="gaussian")
fit2
lambda_hat=1/exp(fit2$coefficient)
lambda_hat





















