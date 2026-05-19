
library(survival)
data=data.frame(
time=c(5,7,9,14,17,19,20,26,30,40,45,46,49,55,61,69),
ind=c(1,1,0,1,1,0,1,1,0,1,1,1,0,1,0,1),
freq=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
)
data


# Print the first few rows of the data frame
#head(data)
time <- data$time
ind <-  data$ind
freq <- data$freq
 

KM.fit <- survfit(Surv(time,ind)~1,weight=freq,type="kaplan-meier")

summary(KM.fit)
t <- summary(KM.fit)$time
KM.rel <- summary(KM.fit)$surv
KM.F <- 1-KM.rel


#~~~~~~~~~~~Calculate MLE for different dist using optim function~~~~~~~~~~~~~~~~
#~~~~~~~~~~  Weibull distribution~~~~~~~~~~
shape <- 1.2
scale <- 18.0

weibull.log.lik<-function(theta){
shape <- theta[1]
scale <- theta[2]

# Calculate the log-likelihood


log.lik.censored <- sum( d*log(dweibull(time, shape=theta[1], scale =theta[2], log = FALSE))
    + (1-d)*log(1-pweibull(time, shape=theta[1], scale =theta[2], lower.tail = TRUE, log.p =  FALSE)))

  return(-log.lik.censored)  #     Return the negative log-likelihood (to minimize)
}

MLE.weib<-optim(c(shape,scale),weibull.log.lik )

MLE.weib$par


#~~~~~~~~~~~~~~~~~~~~  Exponential distribution~~~~~~~~~~~~~~~~~~~~~~~

lambda<- 1/mean(time)

exponential.log.lik<-function(theta){
lambda <- theta

# Calculate the log-likelihood


log.lik.censored <- sum( d*log(dexp(time, lambda , log = FALSE))
    + (1-d)*log(1-pexp(time, lambda , lower.tail = TRUE, log.p =  FALSE)))

  return(-log.lik.censored)  #     Return the negative log-likelihood (to minimize)
}

MLE.exp<-optim(c(lambda),exponential.log.lik )

MLE.exp$par


#Calculating Maximum likelihood estimator for 
#censored failure data gives the following result

 beta <- 1.47
 alfa <- 47.97

W.F <- pweibull(t,beta,alfa,lower.tail=TRUE,log.p=FALSE)
W.rel <- 1-W.F
transform(table(W.rel))

lambda <- .02
E.F <- pexp(t,lambda,lower.tail=TRUE,log.p=FALSE)
E.rel <- 1-E.F



plot(t,KM.F, main="Comparison of CDFs", xlab=" Time", ylab="CDF", col=1, lty= 1, type="s", lwd=1)
lines(t,W.F, col=2, lty=2,type="l", lwd=2)
lines(t,E.F, col=3, lty=3,type="l", lwd=2)
legend("bottomright", c("KM cdf", "Weibull cdf","Exponential cdf"), col=c(1,2,3), text.col=c(1,2,3), lty=c(1,2,2),lwd=2)
abline(h=0.5,col=2)  # finding the median time 
#abline(v=591.215,col=2)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~ AIC~~~~~~~~~~~~~~~~~~~~~~~~

k=2
log.lik.weibull <- sum( d*log(dweibull(time, shape=1.47, scale =47.97, log = FALSE))
    + (1-d)*log(1-pweibull(time, shape=1.47, scale =47.97, lower.tail = TRUE, log.p =  FALSE)))

AIC.Weibull <- 2*k-2*log.lik.weibull
AIC.Weibull 

kk=1
log.lik.Expon <- sum( d*log(dexp(time, 0.02 , log = FALSE))
    + (1-d)*log(1-pexp(time, 0.02 , lower.tail = TRUE, log.p =  FALSE)))
AIC.Expon <- 2*kk-2*log.lik.Expon
AIC.Expon


