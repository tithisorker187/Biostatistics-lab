#Generate sample 
#......................................(a).................................
set.seed(1234)

x=rnorm(1000,mu=2,sd=9)
x
pdf_n=dnorm(x,mu,sigma,log=TRUE)

normal.log.lik=function(theta){
mu=theta[2]
sigma=theta[2]
n=length(x)

log.lik=sum(pdf_n)
return=(-log.lik)


}
normal.log.lik
MLE=optim(c(mu=mean(x),sigma=sd(x)),normal.log.lik)
MLE$par

#.........................................(b)...................................

F = pnorm(x,mu,sigma,lower.tail=TRUE, log.p=FALSE)
f <- dnorm(x,mu,sigma,log=FALSE)
S <- 1-F
h=f/S
par(mfrow=c(2,2))
plot(x,F,type="l",col="blue",main="CDF curve")
plot(x,f,type="l",col="orange",main="pdf curve")
plot(x,S,type="l",col="green",main="Survival curve")
plot(x,h,type="l",col="red",main="hazard curve")


#c)


beta = 1.02
alfa = 2364

B.10 = alfa*((-log(1-0.1))^(1/beta))

B.10

################ MTTF of weibull dist @@@@@@@@@@@@@@@@@@

MTTF <- alfa*gamma(1+1/beta)

MTTF









