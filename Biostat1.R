#Geenerate Data

set.seed(123)

t=seq(0,2,0.2)
t
#Functions of normal distribution 

mu=3
sigma=9 
f.norm = dnorm(t,mu,sigma,log=FALSE)
F.norm = pnorm(t,mu,sigma,lower.tail=TRUE, log.p=FALSE)
S.norm = 1-F.norm
h.norm = f.norm /S.norm 

normal_df=data.frame(
          t=t,
          PDF=f.norm,
          CDF=F.norm,
          survival_function=S.norm,
          hazard_function=h.norm
)

normal_df

#Graphs for all function of normal dist 
par(mfrow=c(2,2))
plot(t,f.norm,type="l",col="red",lwd=2,main="Normal PDF")
plot(t,F.norm,type="l",col="red",lwd=2,main="Normal CDF")
plot(t,S.norm,type="l",col="red",lwd=2,main="Normal Survival curve")
plot(t,h.norm,type="l",col="red",lwd=2,main="Normal Hazard function curve")


#Exponential distribution 
theta=0.12
theta
f.exp=dexp(t,theta,log=FALSE)
F.exp=pexp(t,theta,lower.tail=TRUE,log.p=FALSE)
S.exp=1-F.exp
h.exp=f.exp/S.exp

exp_df=data.frame(t=t,
       PDF=f.exp,
       CDF=F.exp,
       survival_function=S.exp,
       hazard_function=h.exp)
exp_df

#plot for exponential distribution 

par(mfrow=c(2,2))
plot(t,f.exp,type="l",col="red",lwd=2,main="Exponential PDF")
plot(t,F.exp,type="l",col="red",lwd=2,main="Exponential CDF")
plot(t,S.exp,type="l",col="red",lwd=2,main="Exponential Survival curve")
plot(t,h.exp,type="l",col="red",lwd=2,main="Exponential Hazard function curve")


#Weibul Distribution 

beta = 1.5
alfa = 18

f.weibull = dweibull(t,beta,alfa,log=FALSE)
F.weibull = pweibull(t,beta,alfa,lower.tail=TRUE, log.p=FALSE)
S.weibull = 1-F.weibull
h.weibull = f.weibull/S.weibull


weibull_df=data.frame(t=t,
          PDF=f.weibull,
          CDF=F.weibull,
          Survival_function=1-F.weibull,
           hazard_function=f.weibull/S.weibull)
          


weibull_df



#curves 
par(mfrow=c(2,2))
plot(t,f.weibull,type="l",col="red",lwd=2,main="Weibull PDF")
plot(t,F.weibull,type="l",col="red",lwd=2,main="Weibull CDF")
plot(t,S.weibull,type="l",col="red",lwd=2,main="weibull Survival curve")
plot(t,h.weibull,type="l",col="red",lwd=2,main="weibull Hazard function curve")











