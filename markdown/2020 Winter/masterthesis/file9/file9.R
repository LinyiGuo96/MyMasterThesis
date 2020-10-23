rm(list = ls())
setwd('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file9')
load('.RData')

library(seasonal)
library(KFAS)
library(doParallel)
library(dplyr)
library(ggplot2)
library(scales)
library(forecast)
library(extraDistr)
library(lubridate)
library(dfoptim)
library(fitdistrplus)
# section 2
# section 3



unemp_x11 <- seas(unemp,x11='')
unemp_ssm0 <- SSModel(unemp~SSMtrend(1,Q=list(NA))+SSMseasonal(12,sea.type='dummy',Q=NA),H=NA)
unemp_fit <- fitSSM(unemp_ssm0, inits = c(0,0,0))
unemp_kfs0 <- KFS(unemp_fit$model)
unemp_fit$model['Q']
unemp_fit$model['H']

unemp_ssm111 <- SSModel(unemp~SSMtrend(1,Q=list(1))+SSMseasonal(12,sea.type='dummy',Q=1),H=1)
unemp_kfs111 <- KFS(unemp_ssm111)

plot(signal(unemp_kfs0, states = 'trend')$signal)
plot(series(unemp_x11, 'd12'))
#unemp_fit <- fitSSM(unemp_ssm0, inits = c(1,1,1))
#unemp_fit$model['Q']
#unemp_fit$model['H']

par(mfrow=c(1,2))
plot(unemp, ylim=c(5500, 15500), xlim=c(1990,2016), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(unemp-signal(unemp_kfs0, 'seasonal')$signal,col=alpha(4,0.7), ylim=c(5500, 15500), xlim=c(1990,2016), ylab='', lwd=1)
par(new=TRUE)
plot(series(unemp_x11, 'd11'), ylim=c(5500, 15500), xlim=c(1990,2016), ylab='unemp', col=alpha(2,0.9), lwd=1)
title(main='Seasonally adjusted series')
legend('topleft', c('raw data','X-11', 'SSM_MLE'), col=c(1,2,4), lty=1, cex=.8)

plot(unemp, ylim=c(5500, 15500), xlim=c(1990,2016), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(signal(unemp_kfs0, 'trend')$signal,col=alpha(4, 0.7), ylim=c(5500, 15500), xlim=c(1990,2016), ylab='')
par(new=TRUE)
plot(series(unemp_x11, 'd12'), ylim=c(5500, 15500), xlim=c(1990,2016), ylab='unemp', col=alpha(2,0.9))
title(main='Trend series')
legend('topleft', c('raw data','X-11', 'SSM_MLE'), col=c(1,2,4), lty=1, cex=.8)
par(mfrow=c(1,1))


plot(series(unemp_x11, 'd10'), ylim=c(-700,850), xlim=c(1990,2016), ylab='unemp')
par(new=TRUE)
plot(signal(unemp_kfs0, 'seasonal')$signal, col=2, ylim=c(-700, 850), xlim=c(1990,2016), ylab='')
title(main='Seasonal series')


par(mfrow=c(1,2))
plot(window(unemp, start = c(2000,01), end = c(2004,12)), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(window(series(unemp_x11, 'd11'),start = c(2000,01), end = c(2004,12)), col=alpha(2,.9), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='unemp')
par(new=TRUE)
plot(window(unemp-signal(unemp_kfs0, 'seasonal')$signal, start=c(2000,1), end=c(2004,12)),col=alpha(4,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
title(main='Seasonally adjusted series')
legend('topleft', c('raw data','X-11', 'SSM_MLE'), col=c(1,2,4), cex=.8, lty=1)

plot(window(unemp, start = c(2000,01), end = c(2004,12)), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(window(series(unemp_x11, 'd12'),start = c(2000,01), end = c(2004,12)), col=alpha(2,.9), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='unemp')
par(new=TRUE)
plot(window(signal(unemp_kfs0, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(4,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
title(main='Trend series')
legend('topleft', c('raw data','X-11', 'SSM_MLE'), col=c(1,2,4), cex=.8, lty=1)
par(mfrow=c(1,1))


par(mfrow=c(1,2))
plot(window(unemp, start = c(2000,01), end = c(2004,12)), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(window(series(unemp_x11, 'd11'),start = c(2000,01), end = c(2004,12)), col=alpha(2,.9), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='unemp')
par(new=TRUE)
plot(window(unemp-signal(unemp_kfs0, 'seasonal')$signal, start=c(2000,1), end=c(2004,12)),col=alpha(4,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(unemp-signal(unemp_kfs111, 'seasonal')$signal, start=c(2000,1), end=c(2004,12)),col=alpha(5,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
title(main='Seasonally adjusted series')
legend('topleft', c('raw data','X-11', 'SSM_MLE', 'SSM_111'), col=c(1,2,4,5), cex=.8, lty=1)

plot(window(unemp, start = c(2000,01), end = c(2004,12)), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(window(series(unemp_x11, 'd12'),start = c(2000,01), end = c(2004,12)), col=alpha(2,.9), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='unemp')
par(new=TRUE)
plot(window(signal(unemp_kfs0, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(4,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(signal(unemp_kfs111, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(5,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
title(main='Trend series')
legend('topleft', c('raw data','X-11', 'SSM_MLE', 'SSM_111'), col=c(1,2,4,5), cex=.8, lty=1)
par(mfrow=c(1,1))



unemp_ssm3 <- SSModel(unemp~SSMtrend(1, Q=list(2.90625))+SSMseasonal(12,sea.type='dummy',Q=1.875),H=3.9375)
unemp_kfs3 <- KFS(unemp_ssm3)

unemp_ssm5 <- SSModel(unemp~SSMtrend(1, Q=list(3))+SSMseasonal(12,sea.type='dummy',Q=2.3125),H=4.46875)
unemp_kfs5 <- KFS(unemp_ssm5)


par(mfrow=c(1,2))

plot(window(series(unemp_x11, 'd11'),start = c(2000,01), end = c(2004,12)), col=alpha(1,.5), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='unemp',lwd=2)
par(new=TRUE)
plot(window(unemp-signal(unemp_kfs0, 'seasonal')$signal, start=c(2000,1), end=c(2004,12)),col=alpha(2,.8), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(unemp-signal(unemp_kfs3, 'seasonal')$signal, start = c(2000,1), end=c(2004,12)),col=alpha(3,.5), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='',lwd=1)
par(new=TRUE)
plot(window(unemp-signal(unemp_kfs5, 'seasonal')$signal, start = c(2000,1), end=c(2004,12)),col=alpha(4,.5), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='',lwd=1)
title(main='Seasonally adjusted series')
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_Loss1','SSM_Loss2'), col=c(1,2,3,4), lwd=c(2,1,1,1), cex=.8)

plot(window(series(unemp_x11, 'd12'),start = c(2000,01), end = c(2004,12)), col=alpha(1,.5), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='unemp',lwd=2)
par(new=TRUE)
plot(window(signal(unemp_kfs0, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(2,.8), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(signal(unemp_kfs3, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(3,.5), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='',lwd=1)
par(new=TRUE)
plot(window(signal(unemp_kfs5, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(4,.5), ylim=c(5500, 10000), xlim=c(2000,2005), ylab='',lwd=1)
title(main='Trend series')
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_Loss1', 'SSM_Loss2'), col=c(1,2,3,4), lwd=c(2,1,1,1), cex=.8)

par(mfrow=c(1,1))

sum((series(unemp_x11, 'd12') - signal(unemp_kfs0, 'trend')$signal)^2) #7525182
sum((series(unemp_x11, 'd12') - signal(unemp_kfs3, 'trend')$signal)^2) #822059.9
sum((series(unemp_x11, 'd12') - signal(unemp_kfs5, 'trend')$signal)^2) #823149.1

sum((series(unemp_x11, 'd11') - (unemp-signal(unemp_kfs0, 'seasonal')$signal))^2) #3333002
sum((series(unemp_x11, 'd11') - (unemp-signal(unemp_kfs3, 'seasonal')$signal))^2) #1010155
sum((series(unemp_x11, 'd11') - (unemp-signal(unemp_kfs5, 'seasonal')$signal))^2) #1022638


# section 3


ssm <- SSModel(matrix(NA,180,1) ~ SSMtrend(1, Q=list(10)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=20)
sim <- simulateSSM(ssm, "obs", nsim = 1000)
simlist_new <- lapply(1:1000, function(x) sim[,,x])
simlist_new <- lapply(1:1000, function(x) ts(simlist_new[[x]], start = c(2000,01), frequency = 12))


simlist_14yrs_new <- lapply(simlist_new, function(data) {window(data, end=c(2013,12))})
simlist_test_new <- lapply(simlist_new, function(data) window(data, start=c(2014,01)) ) 

f1 <- function(data) {
  f2 <- function(var) {
    x11 <- seas(data, x11='')
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    kfs <- KFS(ssm)
    
    seasonal_x11 <- series(x11, "d10")
    trend_x11 <- series(x11, "d12")
    
    seasonal_kfs <- signal(kfs, "seasonal")$signal
    trend_kfs <- signal(kfs, "trend")$signal
    
    l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
    
    return(l)
  }
  var0 <- c(0,0)
  
  return(hjk(var0, f2, control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
idemat_new <- t(parSapply(cl, simlist_14yrs_new, f1))
stopCluster(cl)


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
mlemat_new <- t(parSapply(cl, simlist_14yrs_new, function(data) {
  
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  
  return(c(var_I, var_T))
}))

stopCluster(cl)


par(mfrow=c(1,2))
plot(density(mlemat_new[,1]), col=alpha('black',.5), lwd=2, ylim=c(0,0.15), xlim=c(0,50), main='', xlab = '')
par(new=TRUE)
plot(density(idemat_new[,1]), col=alpha('red',.5), lwd=2, ylim=c(0,0.15), xlim=c(0,50), main='', xlab = '')
abline(v=20, lty=2)
legend('topright', c('MLE', 'Loss','True'), col=c(1,2,1), lwd=2, lty=c(1,1,2))
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(mlemat_new[,2]), col=alpha('black',.5), lwd=2, ylim=c(0,0.45), xlim=c(0,25), main='', xlab = '')
par(new=TRUE)
plot(density(idemat_new[,2]), col=alpha('red',.5), lwd=2, ylim=c(0,0.45), xlim=c(0,25), main='', xlab = '')
abline(v=10,lty=2)
legend('topright', c('MLE', 'Loss', 'True'), col=c(1,2,1), lwd=2, lty=c(1,1,2))
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))

#################################################################################
# section 4


hnorm <- function(x) {
  
  data <- x
  
  f2 <- function(var) {
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(dhnorm(sqrt(var[1]), sigma = sqrt(40)/3))
    lp2 <- log(dhnorm(sqrt(var[2]), sigma = sqrt(10)/3))
    
    lp <- ll + lp1 + lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, f2, lower=c(0,0), control=list(tol=0.001))$par)
}


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
  library(extraDistr)
})

postmat_hnorm_new <- t(parSapply(cl, simlist_14yrs_new, hnorm))
#postmat2_hnorm <- t(parSapply(cl, simlist2, hnorm))

stopCluster(cl)


par(mfrow=c(1,2))
plot(density(postmat_hnorm_new[,1]), xlim=c(0,50), col=4, ylim=c(0,0.15), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), col=2, lwd=2, main='', xlab='')
abline(v=20, lty=2)
legend('topright', c('MAP','MLE', 'Loss'), col=c(4,1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat_hnorm_new[,2]), xlim=c(0,25), col=4, ylim=c(0,0.45), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,2]), xlim=c(0,25), ylim=c(0,0.45),  lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,2]), xlim=c(0,25), ylim=c(0,0.45), col=2, lwd=2, main='', xlab='')
abline(v=10, lty=2)
legend('topright', c('MAP','MLE', 'Loss'), col=c(4,1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))


# error

cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, varlist = c('simlist_14yrs_new', 'idemat_new', 'mlemat_new', 'postmat_hnorm_new'))
error_decomp_new <- t(parSapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  
  x11 <- seas(data, x11='')
  
  ssm0 <- SSModel(data ~ SSMtrend(1, Q=list(mlemat_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=mlemat_new[i,1])
  kfs0 <- KFS(ssm0)
  
  ssm5 <- SSModel(data ~ SSMtrend(1, Q=list(idemat_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat_new[i,1])
  kfs5 <- KFS(ssm5)
  
  ssmpost <- SSModel(data ~ SSMtrend(1, Q=list(postmat_hnorm_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_hnorm_new[i,1])
  kfspost <- KFS(ssmpost)
  
  
  L0 <- sum((series(x11, 'd12') - signal(kfs0, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs0, 'seasonal')$signal)^2)
  L1 <- sum((series(x11, 'd12') - signal(kfs5, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs5, 'seasonal')$signal)^2)
  L2 <- sum((series(x11, 'd12') - signal(kfspost, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost, 'seasonal')$signal)^2)
  
  L <- c(MLE = L0, Loss = L1, MAP_hnormal=L2)
  
}))

stopCluster(cl)
error_decomp_new <- as.data.frame(error_decomp_new)


summary(error_decomp_new)
apply(error_decomp_new,2,sd)


boxplot(error_decomp_new)
abline(h=median(error_decomp_new$MAP), col=2, lwd=2)

head(error_decomp_new)
ggplot(data = error_decomp_new) + geom_density(aes(x=MLE), size=1) +
  geom_density(aes(x=Loss), size=1, colour='red') +
  geom_density(aes(x=MAP_hnormal), size=1, colour='blue') +
  labs(x='Error', title = 'Error distributions from three estimators')

# test 
friedman.test(as.matrix(error_decomp_new[,c(1,3)]))
wilcox.test(x=error_decomp_new$MLE, y=error_decomp_new$MAP_hnormal)


# Friedman rank sum test
# 
# data:  as.matrix(error_decomp_new[, c(1, 3)])
# Friedman chi-squared = 583.7, df = 1, p-value < 2.2e-16
# 
#
#
# Wilcoxon rank sum test with continuity correction
# 
# data:  error_decomp_new$MLE and error_decomp_new$MAP_hnormal
# W = 570603, p-value = 4.565e-08
# alternative hypothesis: true location shift is not equal to 0

################################################################################################

# section 5

par(mfrow=c(1,2))
plot(density(idemat9[,1], n=2024), ylim=c(0,0.15), xlim=c(0,60), main='',xlab='')
par(new=TRUE)
plot(density(idemat9[,1], bw=1.5, n=2024), ylim=c(0,0.15), xlim=c(0,60), col=2, main='', xlab='variance')
title(main=expression(paste('Prior distribution of ', sigma[I]^2)))
legend('topright', c('original', 'smoothed'), col=c(1,2), lty = 1)

plot(density(idemat9[,2], n=2024), ylim=c(0,0.5), xlim=c(0,20), main='',xlab='')
par(new=TRUE)
plot(density(idemat9[,2], bw=0.5, n=2024), ylim=c(0,0.5), xlim=c(0,20), col=2, main='', xlab='variance')
title(main=expression(paste('Prior distribution of ', sigma[T]^2)))
legend('topright', c('original', 'smoothed'), col=c(1,2), lty = 1)
par(mfrow=c(1,1))



emprior1 <- density(idemat9[,1], n=2024)
emprior2 <- density(idemat9[,2], n=2024)
emprior1$x[which.max(emprior1$y)]
emprior2$x[which.max(emprior2$y)]

dnorm(14.5, 8.8, 2.9)
x <- seq(0, 50, 0.01)
plot(x=seq(0, 50, 0.01), dexp(x, 0.2), type='l')
which.min(abs(0.019935 - dexp(x, 0.2)))
x[which.min(abs(0.019935 - dexp(x, 0.2)))]

y1 <- c(dnorm(emprior1$x[emprior1$x<14.5], mean=8.8, sd=2.9), dexp(emprior1$x[emprior1$x>=14.5]-14.5+11.53, 0.2))

y2 <- c(dnorm(emprior2$x[emprior2$x<4.2], mean=2.46, sd=0.83), dexp(emprior2$x[emprior2$x>=4.2]-4.2+2.93, 1))


plot(x=emprior1$x, y=emprior1$y, ylim=c(0,0.15), xlab='', ylab='', type = 'l')
par(new=TRUE)
plot(x=emprior1$x, y=y1, ylim=c(0,0.15), col=2, xlab='variance',ylab='density', type='l')

plot(x=emprior2$x, y=emprior2$y, ylim=c(0, .5), xlab='', ylab='', type = 'l')
par(new=TRUE)
plot(x=emprior2$x, y=y2, ylim=c(0,0.5), col=2, xlab='variance', ylab='density', type = 'l')



par(mfrow=c(1,2))
plot(x=emprior1$x, y=emprior1$y, ylim=c(0,0.15), xlab='', ylab='', type = 'l')
par(new=TRUE)
plot(x=emprior1$x, y=y1, ylim=c(0,0.15), col=2, xlab='variance',ylab='density', type='l')
legend('topright', c('Original', 'Approximation'), col=c(1,2), lty = 1)
title(main = expression(paste('The prior distribution of ', sigma[I]^2)))

plot(x=emprior2$x, y=emprior2$y, ylim=c(0, .5), xlab='', ylab='', type = 'l')
par(new=TRUE)
plot(x=emprior2$x, y=y2, ylim=c(0,0.5), col=2, xlab='variance', ylab='density', type = 'l')
legend('topright', c('Original', 'Approximation'), col=c(1,2), lty = 1)
title(main = expression(paste('The prior distribution of ', sigma[I]^2)))
par(mfrow=c(1,1))

emprior1$y <- y1
emprior2$y <- y2


post_extract_empirical <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + lp1 + lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat_emprior <- t(parSapply(cl, simlist_14yrs_new, post_extract_empirical))
stopCluster(cl)



par(mfrow=c(1,2))
plot(density(postmat_hnorm_new[,1]), xlim=c(0,50), ylim=c(0,0.15), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior[,1]), xlim=c(0,50), ylim=c(0,0.15), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss','MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat_hnorm_new[,2]), xlim=c(0,20), col=4, ylim=c(0,0.5), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,2]), xlim=c(0,20), ylim=c(0,0.5), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,2]), xlim=c(0,20), ylim=c(0,0.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior[,2]), xlim=c(0,20), ylim=c(0,0.5), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss', 'MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))



post_extract_empirical_im <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + 2*lp1 + 2*lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat_emprior_im <- t(parSapply(cl, simlist_14yrs_new, post_extract_empirical_im))
stopCluster(cl)


par(mfrow=c(1,2))
plot(density(postmat_hnorm_new[,1]), xlim=c(0,50), ylim=c(0,0.15), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior_im[,1]), xlim=c(0,50), ylim=c(0,0.15), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss','MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat_hnorm_new[,2]), xlim=c(0,20), col=4, ylim=c(0,0.5), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,2]), xlim=c(0,20), ylim=c(0,0.5), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,2]), xlim=c(0,20), ylim=c(0,0.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior_im[,2]), xlim=c(0,20), ylim=c(0,0.5), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss', 'MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))



post_extract_empirical_im2 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + 10*lp1 + 10*lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat_emprior_im2 <- t(parSapply(cl, simlist_14yrs_new, post_extract_empirical_im2))
stopCluster(cl)


par(mfrow=c(1,2))
plot(density(postmat_hnorm_new[,1]), xlim=c(0,50), ylim=c(0,0.15), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,1]), xlim=c(0,50), ylim=c(0,0.15), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior_im2[,1]), xlim=c(0,50), ylim=c(0,0.15), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss','MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat_hnorm_new[,2]), xlim=c(0,20), col=4, ylim=c(0,0.5), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,2]), xlim=c(0,20), ylim=c(0,0.5), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,2]), xlim=c(0,20), ylim=c(0,0.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior_im2[,2]), xlim=c(0,20), ylim=c(0,0.5), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss', 'MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))


post_extract_empirical_im <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + 0.5*lp1 + 0.5*lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat_emprior_0.5 <- t(parSapply(cl, simlist_14yrs_new, post_extract_empirical_im))
stopCluster(cl)


post_extract_empirical_im <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + 5*lp1 + 5*lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat_emprior_5 <- t(parSapply(cl, simlist_14yrs_new, post_extract_empirical_im))
stopCluster(cl)



post_extract_empirical_im <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + 50*lp1 + 50*lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat_emprior_50 <- t(parSapply(cl, simlist_14yrs_new, post_extract_empirical_im))
stopCluster(cl)



post_extract_empirical_im <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + 0.1*lp1 + .1*lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat_emprior_0.1 <- t(parSapply(cl, simlist_14yrs_new, post_extract_empirical_im))
stopCluster(cl)



cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, c('simlist_14yrs_new', 'mlemat_new', 'idemat_new', 'postmat_hnorm_new', 
                    'postmat_emprior', 'postmat_emprior_im','postmat_emprior_im2','postmat_emprior_0.5',
                    'postmat_emprior_5','postmat_emprior_50','postmat_emprior_0.1'))
error_decomp_emp <- t(parSapply(cl, 1:1000, function(i){
  
  data <- simlist_14yrs_new[[i]]
  x11 <- seas(data, x11='')
  
  ssm0 <- SSModel(data ~ SSMtrend(1, Q=list(mlemat_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=mlemat_new[i,1])
  kfs0 <- KFS(ssm0)
  
  ssm5 <- SSModel(data ~ SSMtrend(1, Q=list(idemat_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat_new[i,1])
  kfs5 <- KFS(ssm5)
  
  ssmpost_hn <- SSModel(data ~ SSMtrend(1, Q=list(postmat_hnorm_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_hnorm_new[i,1])
  kfspost_hn <- KFS(ssmpost_hn)
  
  ssmpost_emp <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior[i,1])
  kfspost_emp <- KFS(ssmpost_emp)
  
  ssmpost_emp0.1 <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_0.1[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior_0.1[i,1])
  kfspost_emp0.1 <- KFS(ssmpost_emp0.1)
  
  ssmpost_emp0.5 <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_0.5[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior_0.5[i,1])
  kfspost_emp0.5 <- KFS(ssmpost_emp0.5)
  
  ssmpost_emp2 <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_im[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior_im[i,1])
  kfspost_emp2 <- KFS(ssmpost_emp2)
  
  ssmpost_emp5 <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_5[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior_5[i,1])
  kfspost_emp5 <- KFS(ssmpost_emp5)
  
  ssmpost_emp10 <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_im2[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior_im2[i,1])
  kfspost_emp10 <- KFS(ssmpost_emp10)
  
  ssmpost_emp50 <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_50[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior_50[i,1])
  kfspost_emp50 <- KFS(ssmpost_emp50)
  
  L0 <- sum((series(x11, 'd12') - signal(kfs0, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs0, 'seasonal')$signal)^2)
  L1 <- sum((series(x11, 'd12') - signal(kfs5, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs5, 'seasonal')$signal)^2)
  L2 <- sum((series(x11, 'd12') - signal(kfspost_hn, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_hn, 'seasonal')$signal)^2)
  L3 <- sum((series(x11, 'd12') - signal(kfspost_emp0.1, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_emp0.1, 'seasonal')$signal)^2)
  L4 <- sum((series(x11, 'd12') - signal(kfspost_emp0.5, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_emp0.5, 'seasonal')$signal)^2)
  L5 <- sum((series(x11, 'd12') - signal(kfspost_emp, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_emp, 'seasonal')$signal)^2)
  L6 <- sum((series(x11, 'd12') - signal(kfspost_emp2, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_emp2, 'seasonal')$signal)^2)
  L7 <- sum((series(x11, 'd12') - signal(kfspost_emp5, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_emp5, 'seasonal')$signal)^2)
  L8 <- sum((series(x11, 'd12') - signal(kfspost_emp10, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_emp10, 'seasonal')$signal)^2)
  L9 <- sum((series(x11, 'd12') - signal(kfspost_emp50, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost_emp50, 'seasonal')$signal)^2)
  
  L <- c(MLE = L0, Loss = L1, MAP_hnormal=L2, MAP_emp0.1 = L3, 
         MAP_emp0.5 = L4, MAP_emp = L5, MAP_emp2 = L6, MAP_emp5 = L7,
         MAP_emp10 = L8, MAP_emp50 = L9 )
  
}))
stopCluster(cl)

summary(error_decomp_emp)
#       MLE              Loss         MAP_hnormal       MAP_emp0.1       MAP_emp0.5        
#   Min.   : 293.2   Min.   : 282.2   Min.   : 280.9   Min.   : 288.5   Min.   : 284.2  
#   1st Qu.: 633.7   1st Qu.: 550.1   1st Qu.: 602.2   1st Qu.: 622.8   1st Qu.: 604.5  
#   Median : 761.9   Median : 645.2   Median : 715.2   Median : 747.8   Median : 716.9  
#   Mean   : 785.2   Mean   : 657.1   Mean   : 733.3   Mean   : 767.4   Mean   : 734.5  
#   3rd Qu.: 896.7   3rd Qu.: 749.4   3rd Qu.: 847.0   3rd Qu.: 877.5   3rd Qu.: 844.0  
#   Max.   :1763.9   Max.   :1396.8   Max.   :1447.3   Max.   :1727.3   Max.   :1453.3  

#        MAP_emp       MAP_emp2         MAP_emp5        MAP_emp10        MAP_emp50     
#   Min.   : 286.9  Min.   : 285.5   Min.   : 303.5   Min.   : 308.3   Min.   : 323.0  
#   1st Qu.: 597.5  1st Qu.: 599.8   1st Qu.: 573.8   1st Qu.: 569.2   1st Qu.: 569.1  
#   Median : 709.2  Median : 714.8   Median : 675.2   Median : 662.3   Median : 662.8  
#   Mean   : 728.6  Mean   : 738.5   Mean   : 700.6   Mean   : 680.8   Mean   : 679.1  
#   3rd Qu.: 841.3  3rd Qu.: 863.4   3rd Qu.: 805.1   3rd Qu.: 778.8   3rd Qu.: 770.0  
#   Max.   :1649.0  Max.   :1710.1   Max.   :1673.9   Max.   :1424.6   Max.   :1422.8

apply(error_decomp_emp, 2, sd)
#    MLE          Loss    MAP_hnormal  MAP_emp0.1  MAP_emp0.5     MAP_emp    MAP_emp2    MAP_emp5 
#   207.1050    150.5360    179.8359    197.3232    180.6615    181.3250    190.3706    179.6753 
#
#   MAP_emp10   MAP_emp50 
#   155.7274    152.1393

# testing
boxplot(error_decomp_emp[,c(1,2,3,6)])
abline(h=median(error_decomp_emp[,6]), col=2, lwd=2)

friedman.test(as.matrix(error_decomp_emp[,c(1,3,6)]))
friedman.test(as.matrix(error_decomp_emp[,c(1,6)]))
friedman.test(as.matrix(error_decomp_emp[,c(2,6)]))
friedman.test(as.matrix(error_decomp_emp[,c(3,6)]))

wilcox.test(x=error_decomp_new$MLE, y=error_decomp_new$MAP_emp)
wilcox.test(x=error_decomp_new$MAP_hnormal, y=error_decomp_new$MAP_emp)
wilcox.test(x=error_decomp_new$Loss, y=error_decomp_new$MAP_emp)
#> friedman.test(as.matrix(error_decomp_emp[,c(1,3,6)]))
#
#Friedman rank sum test
#
#data:  as.matrix(error_decomp_emp[, c(1, 3, 6)])
#Friedman chi-squared = 690.82, df = 2, p-value < 2.2e-16
#
#> friedman.test(as.matrix(error_decomp_emp[,c(1,6)]))
#
#Friedman rank sum test
#
#data:  as.matrix(error_decomp_emp[, c(1, 6)])
#Friedman chi-squared = 336.4, df = 1, p-value < 2.2e-16
#
#> friedman.test(as.matrix(error_decomp_emp[,c(2,6)]))
#
#Friedman rank sum test
#
#data:  as.matrix(error_decomp_emp[, c(2, 6)])
#Friedman chi-squared = 739.6, df = 1, p-value < 2.2e-16
#
#> friedman.test(as.matrix(error_decomp_emp[,c(3,6)]))
#
#Friedman rank sum test
#
#data:  as.matrix(error_decomp_emp[, c(3, 6)])
#Friedman chi-squared = 65.536, df = 1, p-value = 5.706e-16
 
# > wilcox.test(x=error_decomp_new$MLE, y=error_decomp_new$MAP_emp)
# 
# Wilcoxon signed rank test with continuity correction
# 
# data:  error_decomp_new$MLE
# V = 500500, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0
# 
# > wilcox.test(x=error_decomp_new$MAP_hnormal, y=error_decomp_new$MAP_emp)
# 
# Wilcoxon signed rank test with continuity correction
# 
# data:  error_decomp_new$MAP_hnormal
# V = 500500, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0
# 
# > wilcox.test(x=error_decomp_new$Loss, y=error_decomp_new$MAP_emp)
# 
# Wilcoxon signed rank test with continuity correction
# 
# data:  error_decomp_new$Loss
# V = 500500, p-value < 2.2e-16
# alternative hypothesis: true location is not equal to 0

head(idemat_new)
head(cbind(idemat_new, 'idemat_new'))

idemat_tol <- rbind(cbind(idemat_new, 'idemat_new'),cbind(idemat2, 'idemat2'), cbind(idemat3,'idemat3'),cbind(idemat4,'idemat4'))
idemat_tol <- as.data.frame(idemat_tol)
idemat_tol$V1 <- as.numeric(as.character(idemat_tol$V1))
idemat_tol$V2 <- as.numeric(as.character(idemat_tol$V2))
head(idemat_tol)

ggplot(idemat_tol, aes(x=V1, fill=V3)) + geom_density(alpha=.5)
ggplot(idemat_tol, aes(x=V2, fill=V3)) + geom_density(alpha=.5)

summary(mlemat4)
summary(mlemat3)
summary(mlemat2)
summary(mlemat_new)

###################################################################################

hnorm <- function(x) {
  
  data <- x
  
  f2 <- function(var) {
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(dhnorm(sqrt(var[1]), sigma = sqrt(40)/3))
    lp2 <- log(dhnorm(sqrt(var[2]), sigma = sqrt(10)/3))
    
    lp <- ll + lp1 + lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, f2, lower=c(0,0), control=list(tol=0.001))$par)
}


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
  library(extraDistr)
})

postmat2_hnorm <- t(parSapply(cl, simlist2, hnorm))
#postmat2_hnorm <- t(parSapply(cl, simlist2, hnorm))

stopCluster(cl)


hnorm <- function(x) {
  
  data <- x
  
  f2 <- function(var) {
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- 100*log(dhnorm(sqrt(var[1]), sigma = sqrt(40)/3))
    lp2 <- 100*log(dhnorm(sqrt(var[2]), sigma = sqrt(10)/3))
    
    lp <- ll + lp1 + lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, f2, lower=c(0,0), control=list(tol=0.001))$par)
}


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
  library(extraDistr)
})

postmat2_hnorm_improper <- t(parSapply(cl, simlist2, hnorm))
#postmat2_hnorm <- t(parSapply(cl, simlist2, hnorm))

stopCluster(cl)



emprior1 <- density(idemat2[1:700,1], n=2024)
emprior2 <- density(idemat2[1:700,2], n=2024)

post_extract_empirical <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + lp1 + lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

postmat2_emprior <- t(sapply(simlist2[701:1000], post_extract_empirical))


post_extract_empirical <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- 100* log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- 100* log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    
    lp <- ll + lp1 + lp2
    
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

postmat2_emprior_improper <- t(sapply(simlist2[701:1000], post_extract_empirical))



par(mfrow=c(1,2))
plot(density(postmat2_hnorm_improper[701:1000,1]), xlim=c(0,160), ylim=c(0,0.3), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat2[701:1000,1]), xlim=c(0,160), ylim=c(0,0.3), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[701:1000,1]), xlim=c(0,160), ylim=c(0,0.3), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior[,1]), xlim=c(0,160), ylim=c(0,0.3), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss','MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat2_hnorm_improper[701:1000,2]), xlim=c(0,70), col=4, ylim=c(0,0.5), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat2[701:1000,2]), xlim=c(0,70), ylim=c(0,0.5), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[701:1000,2]), xlim=c(0,70), ylim=c(0,0.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior[,2]), xlim=c(0,70), ylim=c(0,0.5), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss', 'MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))

summary(emprior1$y)
summary(emprior1$x)

plot(x=emprior1$x, y=log(emprior1$y), type='l')
plot(x=emprior2$x, y=log(emprior2$y), type='l')


ssm <- SSModel(simlist2[[1]]~SSMtrend(1,Q=list(NA))+SSMseasonal(12,Q=1,sea.type='dummy'), H=NA)
fit <- fitSSM(ssm,inits = c(1,1))
fit$model['H'] #105.6445
fit$model['Q'] #19.73813

ll1 <- sapply(seq(0, 200, 0.1), function(x){
  ssm <- SSModel(simlist2[[1]]~SSMtrend(1,Q=list(19.73813))+SSMseasonal(12,Q=1,sea.type='dummy'), H=x)
  logLik(ssm)
})

plot(ll1,type='l')
plot(ll1[100:length(ll1)],type='l')


ll2 <- sapply(seq(0, 50, 0.05), function(x){
  ssm <- SSModel(simlist2[[1]]~SSMtrend(1,Q=list(x))+SSMseasonal(12,Q=1,sea.type='dummy'), H=105.6445)
  logLik(ssm)
})

plot(ll2,type='l')
plot(ll2[100:length(ll2)],type='l')


ssm <- SSModel(simlist3[[1]]~SSMtrend(1,Q=list(1))+SSMseasonal(12,Q=1,sea.type='dummy'), H=1)
logLik(ssm)

ssm <- SSModel(window(simlist3[[1]],end=c(2014,12))~SSMtrend(1,Q=list(1))+SSMseasonal(12,Q=1,sea.type='dummy'), H=1)
logLik(ssm)


##########################################################################################

ggplot(idemat_tol, aes(x=V1, colour=V3)) + geom_density(alpha=.5, size=1)
ggplot(idemat_tol, aes(x=V2, colour=V3)) + geom_density(alpha=.5)

plot(x=seq(0,100,0.1),y = df(x=seq(0,100,0.1), df1=10, df2=400), type='l')
plot(x=seq(0,100,0.1),y = dchisq(x=seq(0,100,0.1), df=10), type='l')
plot(x=seq(0,100,0.1),y = dchisq(x=seq(0,100,0.1), df=20), type='l')
abline(v=20)

##########################################################################################


ssm <- SSModel(matrix(NA,180,1) ~ SSMtrend(1, Q=list(0.25)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=1)
sim <- simulateSSM(ssm, "obs", nsim = 1000)
simlist5 <- lapply(1:1000, function(x) sim[,,x])
simlist5 <- lapply(1:1000, function(x) ts(simlist5[[x]], start = c(2000,01), frequency = 12))


f1 <- function(data) {
  f2 <- function(var) {
    x11 <- seas(data, x11='')
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    kfs <- KFS(ssm)
    
    seasonal_x11 <- series(x11, "d10")
    trend_x11 <- series(x11, "d12")
    
    seasonal_kfs <- signal(kfs, "seasonal")$signal
    trend_kfs <- signal(kfs, "trend")$signal
    
    l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
    
    return(l)
  }
  var0 <- c(0,0)
  
  return(hjk(var0, f2, control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
idemat5 <- t(parSapply(cl, simlist5, f1))
stopCluster(cl)


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
mlemat5 <- t(parSapply(cl, simlist5, function(data) {
  
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  
  return(c(var_I, var_T))
}))

stopCluster(cl)


plot(density(idemat5[,1]))
plot(density(idemat5[,2]))

plot(density(mlemat5[,1]))
plot(density(mlemat5[,2]))

##########################################################################################


par(mfrow=c(2,2))
plot(density(idemat6[,1]))
plot(density(idemat_new[,1]))

plot(density(idemat6[,2]))
plot(density(idemat_new[,2]))
par(mfrow=c(1,1))

plot(density(mlemat6[,1]))
plot(density(mlemat6[,2]))


par(mfrow=c(6,2))
plot(density(idemat_new[,1]))
plot(density(idemat_new[,2]))

plot(density(idemat2[,1]))
plot(density(idemat2[,2]))

plot(density(idemat3[,1]))
plot(density(idemat3[,2]))

plot(density(idemat4[,1]))
plot(density(idemat4[,2]))

plot(density(idemat5[,1]))
plot(density(idemat5[,2]))


plot(density(idemat6[,1]))
plot(density(idemat6[,2]))
par(mfrow=c(1,1))



idemat_tol <- rbind(idemat_tol, cbind(idemat5,'idemat5'),cbind(idemat6,'idemat6'))
idemat_tol <- as.data.frame(idemat_tol)
idemat_tol$V1 <- as.numeric(as.character(idemat_tol$V1))
idemat_tol$V2 <- as.numeric(as.character(idemat_tol$V2))
head(idemat_tol)


ggplot(idemat_tol, aes(x=V1, colour=V3)) + geom_density(alpha=.5,size=1)
ggplot(idemat_tol, aes(x=V2, colour=V3)) + geom_density(alpha=.5,size=1)

#############################################################################################

n=1000
sim <- lapply(1:n, function(i) {
  set.seed(i)
  ssm <- SSModel(matrix(NA,180,1) ~ SSMtrend(1, Q=list(rnorm(1,0,10)^2)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=rnorm(1,0,10)^2)
  simulateSSM(ssm, "obs", nsim = 1)
})

simlist7 <- lapply(1:n, function(x) sim[[x]][,,1])
simlist7 <- lapply(1:n, function(x) ts(simlist7[[x]], start = c(2000,01), frequency = 12))


f1 <- function(data) {
  f2 <- function(var) {
    x11 <- seas(data, x11='')
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    kfs <- KFS(ssm)
    
    seasonal_x11 <- series(x11, "d10")
    trend_x11 <- series(x11, "d12")
    
    seasonal_kfs <- signal(kfs, "seasonal")$signal
    trend_kfs <- signal(kfs, "trend")$signal
    
    l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
    
    return(l)
  }
  var0 <- c(0,0)
  
  return(hjk(var0, f2, control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
idemat7 <- t(parSapply(cl, simlist7, f1))
stopCluster(cl)


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
mlemat7 <- t(parSapply(cl, simlist7, function(data) {
  
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  
  return(c(var_I, var_T))
}))

stopCluster(cl)




plot(density(mlemat7[,1]))
plot(density(mlemat7[,2]))

plot(density(idemat7[,1]),ylim=c(0,0.11),xlim=c(0,120))
par(new=TRUE)
plot(density(idemat_tol[,1]),ylim=c(0,0.11),xlim=c(0,120))

plot(density(idemat7[,2]),ylim=c(0,0.5),xlim=c(0,15))
par(new=TRUE)
plot(density(idemat_tol[,2]),ylim=c(0,0.5),xlim=c(0,15))




idemat_tol <- rbind(idemat_tol, cbind(idemat7,'idemat7'))
idemat_tol <- as.data.frame(idemat_tol)
idemat_tol$V1 <- as.numeric(as.character(idemat_tol$V1))
idemat_tol$V2 <- as.numeric(as.character(idemat_tol$V2))

#############################################################################################
#save(idemat_tol, file='idemat_tol')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file10/simlist8.RData')



n=1000
sim <- lapply(1:n, function(i) {
  set.seed(i)
  ssm <- SSModel(matrix(NA,360,1) ~ SSMtrend(1, Q=list(rnorm(1,0,10)^2)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=rnorm(1,0,10)^2)
  simulateSSM(ssm, "obs", nsim = 1)
})

simlist8 <- lapply(1:n, function(x) sim[[x]][,,1])
simlist8 <- lapply(1:n, function(x) ts(simlist8[[x]], start = c(2000,01), frequency = 12))


f1 <- function(data) {
  f2 <- function(var) {
    x11 <- seas(data, x11='')
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    kfs <- KFS(ssm)
    
    seasonal_x11 <- series(x11, "d10")
    trend_x11 <- series(x11, "d12")
    
    seasonal_kfs <- signal(kfs, "seasonal")$signal
    trend_kfs <- signal(kfs, "trend")$signal
    
    l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
    
    return(l)
  }
  var0 <- c(0,0)
  
  return(hjk(var0, f2, control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
idemat8 <- t(parSapply(cl, simlist8, f1))



mlemat8 <- t(parSapply(cl, simlist8, function(data) {
  
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  
  return(c(var_I, var_T))
}))
stopCluster(cl)

plot(density(idemat8[,1]), ylim=c(0,0.11), xlim=c(0,50))
par(new=TRUE)
plot(density(idemat7[,1]), ylim=c(0,0.11), xlim=c(0,50), col=2)
par(new=TRUE)
plot(density(idemat_tol[,1]), ylim=c(0,0.11), xlim=c(0,50), col=3)

plot(density(idemat8[,2]), ylim=c(0,0.65), xlim=c(0,15))
par(new=TRUE)
plot(density(idemat7[,2]), ylim=c(0,0.65), xlim=c(0,15), col=2)
par(new=TRUE)
plot(density(idemat_tol[,2]), ylim=c(0,0.65), xlim=c(0,15), col=3)



idemat_tol <- rbind(idemat_tol, cbind(idemat8,'idemat8'))
idemat_tol <- as.data.frame(idemat_tol)
idemat_tol$V1 <- as.numeric(as.character(idemat_tol$V1))
idemat_tol$V2 <- as.numeric(as.character(idemat_tol$V2))

ggplot(idemat_tol, aes(x=V1, colour=V3)) + geom_density(alpha=0.5,size=1)
ggplot(idemat_tol, aes(x=V2, colour=V3)) + geom_density(alpha=0.5,size=1)


#fitdist(idemat_tol[,1], 'gamma')
#
#fitdist(idemat_tol[,2], 'gamma')
#
#plot(density(idemat[,2]), ylim=c(0,0.65), xlim=c(0,15), lwd=2)
#par(new=TRUE)
#plot(x=seq(0,15,0.01), y=dgamma(seq(0,15,0.01),shape = 2.61,rate = 1.08), ylim=c(0,0.65), xlim=c(0,15),type='l', col=2, lwd=2)
#
#plot(x=seq(0,15,0.01), y=dBCPEo(seq(0,15,0.01), mu=0.67,sigma=-0.48,nu=-0.07,tau=0.59), type='l')



#  Section 5 empirical prior
par(mfrow=c(1,2))
plot(density(idemat_tol[,1]), xlim=c(0,150), ylim=c(0,0.1), xlab='', main='', lwd=2, col=alpha(1,0.5))
par(new=TRUE)
plot(density(idemat_tol[,1],bw=2.5), xlim=c(0,150), ylim=c(0,0.1), col=2, xlab='', main='', lwd=2)
legend('topright', c('empirical', 'smoothed'), col=c(1,2), lty=1)
title(main=expression(paste('Prior distribution of ', sigma[I]^2)))

plot(density(idemat_tol[,2]), xlim=c(0,35), ylim=c(0,0.4), xlab='', main='', lwd=2, col=alpha(1,0.5))
par(new=TRUE)
plot(density(idemat_tol[,2],bw=0.5), xlim=c(0,35), ylim=c(0,0.4), col=2, xlab='', main='', lwd=2)
legend('topright', c('empirical', 'smoothed'), col=c(1,2), lty=1)
title(main=expression(paste('Prior distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))

f1 <- density(idemat_tol[,1],bw=3,n=2048)
str(f1)
plot(f1)
#save(idemat_tol, file = 'idemat_tol.RData')
#########################################################################################

# Section 5 empirical prior
emprior1 <- density(idemat_tol[,1], n=2024, bw=2.5)
emprior2 <- density(idemat_tol[,2], n=2024, bw=0.5)

post_empirical_adt <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1], 0, 50/3))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2], 0, 5))
    lp <- ll + lp1 + lp2
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

post_empirical_adt2 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1], 0, 50/3))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2], 0, 5))
    lp <- ll + 2*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

post_empirical_adt3 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1], 0, 50/3))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2], 0, 5))
    lp <- ll + 10*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt4 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1], 0, 50/3))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2], 0, 5))
    lp <- ll + 20*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt5 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1], 0, 50/3))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2], 0, 5))
    lp <- ll + 50*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt6 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1], 0, 50/3))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2], 0, 5))
    lp <- ll + 100*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))

postmat2_emprior_update <- t(parSapply(cl, simlist2, post_empirical_adt))
postmat2_emprior_update2 <- t(parSapply(cl, simlist2, post_empirical_adt2))
postmat2_emprior_update3 <- t(parSapply(cl, simlist2, post_empirical_adt3))
postmat2_emprior_update4 <- t(parSapply(cl, simlist2, post_empirical_adt4))
postmat2_emprior_update5 <- t(parSapply(cl, simlist2, post_empirical_adt5))
postmat2_emprior_update6 <- t(parSapply(cl, simlist2, post_empirical_adt6))
stopCluster(cl)


par(mfrow=c(1,2))
plot(density(postmat2_hnorm[,1]), xlim=c(0,160), ylim=c(0,0.1), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat2[,1]), xlim=c(0,160), ylim=c(0,0.1), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,1]), xlim=c(0,160), ylim=c(0,0.1), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update[,1]), xlim=c(0,160), ylim=c(0,0.1), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss','MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat2_hnorm[,2]), xlim=c(0,50), col=4, ylim=c(0,0.6), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat2[,2]), xlim=c(0,50), ylim=c(0,0.6), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,2]), xlim=c(0,50), ylim=c(0,0.6), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update[,2]), xlim=c(0,50), ylim=c(0,0.6), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss', 'MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))



par(mfrow=c(1,2))
plot(density(mlemat2[,1]), xlim=c(0,160), ylim=c(0,.8), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,1]), xlim=c(0,160), ylim=c(0,.8), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update[,1]), xlim=c(0,160), ylim=c(0,.8), col=3, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update2[,1]), xlim=c(0,160), ylim=c(0,.8), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update3[,1]), xlim=c(0,160), ylim=c(0,.8), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update4[,1]), xlim=c(0,160), ylim=c(0,.8), col=6, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update5[,1]), xlim=c(0,160), ylim=c(0,.8), col=7, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update6[,1]), xlim=c(0,160), ylim=c(0,.8), col=8, lwd=2, main='', xlab='')
legend('topright', c('MLE', 'Loss','k=1', 'k=2', 'k=10','k=20','k=50','k=100'), col=c(1,2,3,4,5,6,7,8), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[I]^2)))


plot(density(mlemat2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update[,2]), xlim=c(0,50), ylim=c(0,3.5), col=3, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update3[,2]), xlim=c(0,50), ylim=c(0,3.5), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update4[,2]), xlim=c(0,50), ylim=c(0,3.5), col=6, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update5[,2]), xlim=c(0,50), ylim=c(0,3.5), col=7, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_update6[,2]), xlim=c(0,50), ylim=c(0,3.5), col=8, lwd=2, main='', xlab='')
legend('topright', c('MLE', 'Loss','k=1', 'k=2', 'k=10','k=20','k=50','k=100'), col=c(1,2,3,4,5,6,7,8), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))



emprior1 <- density(idemat_tol[,1], n=2024, bw=2.5)
emprior2 <- density(idemat_tol[,2], n=2024, bw=0.5)

post_empirical_adt <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log( emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log( emprior2$y[which.min(abs(emprior2$x - var[2]))])
    lp <- ll + lp1 + lp2
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

post_empirical_adt2 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log(emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log(emprior2$y[which.min(abs(emprior2$x - var[2]))])
    lp <- ll + 2*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

post_empirical_adt3 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log( emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log( emprior2$y[which.min(abs(emprior2$x - var[2]))])
    lp <- ll + 10*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

post_empirical_adt4 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log( emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log( emprior2$y[which.min(abs(emprior2$x - var[2]))])
    lp <- ll + 20*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

post_empirical_adt5 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log( emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log( emprior2$y[which.min(abs(emprior2$x - var[2]))])
    lp <- ll + 50*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt6 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log( emprior1$y[which.min(abs(emprior1$x - var[1]))])
    lp2 <- log( emprior2$y[which.min(abs(emprior2$x - var[2]))])
    lp <- ll + 100*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2'))

postmat2_emprior <- t(parSapply(cl, simlist2, post_empirical_adt))
postmat2_emprior2 <- t(parSapply(cl, simlist2, post_empirical_adt2))
postmat2_emprior3 <- t(parSapply(cl, simlist2, post_empirical_adt3))
postmat2_emprior4 <- t(parSapply(cl, simlist2, post_empirical_adt4))
postmat2_emprior5 <- t(parSapply(cl, simlist2, post_empirical_adt5))
postmat2_emprior6 <- t(parSapply(cl, simlist2, post_empirical_adt6))
stopCluster(cl)


par(mfrow=c(1,2))
plot(density(mlemat2[,1]), xlim=c(0,160), ylim=c(0,.8), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,1]), xlim=c(0,160), ylim=c(0,.8), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior[,1]), xlim=c(0,160), ylim=c(0,.8), col=3, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior2[,1]), xlim=c(0,160), ylim=c(0,.8), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior3[,1]), xlim=c(0,160), ylim=c(0,.8), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior4[,1]), xlim=c(0,160), ylim=c(0,.8), col=6, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior5[,1]), xlim=c(0,160), ylim=c(0,.8), col=7, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior6[,1]), xlim=c(0,160), ylim=c(0,.8), col=8, lwd=2, main='', xlab='')
legend('topright', c('MLE', 'Loss','k=1', 'k=2', 'k=10','k=20','k=50','k=100'), col=c(1,2,3,4,5,6,7,8), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[I]^2)))


plot(density(mlemat2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior[,2]), xlim=c(0,50), ylim=c(0,3.5), col=3, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior3[,2]), xlim=c(0,50), ylim=c(0,3.5), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior4[,2]), xlim=c(0,50), ylim=c(0,3.5), col=6, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior5[,2]), xlim=c(0,50), ylim=c(0,3.5), col=7, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior6[,2]), xlim=c(0,50), ylim=c(0,3.5), col=8, lwd=2, main='', xlab='')
legend('topright', c('MLE', 'Loss','k=1', 'k=2', 'k=10','k=20','k=50','k=100'), col=c(1,2,3,4,5,6,7,8), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))



emprior1 <- density(idemat_tol[,1], n=2024, bw=2.5)
emprior2 <- density(idemat_tol[,2], n=2024, bw=0.5)
p1 <- sum(emprior1$y[emprior1$x>50]) / sum(emprior1$y)
p2 <- sum(emprior2$y[emprior2$x>15]) / sum(emprior2$y)
q1 <- qnorm(1-p1, 0, 1)
q2 <- qnorm(1-p2, 0, 1) 


post_empirical_adt <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1]-50+q1, 0, 1))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2]-15+q2, 0, 1))
    lp <- ll + lp1 + lp2
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}

post_empirical_adt2 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1]-50+q1, 0, 1))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2]-15+q2, 0, 1))
    lp <- ll + 2*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt3 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1]-50+q1, 0, 1))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2]-15+q2, 0, 1))
    lp <- ll + 10*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt4 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1]-50+q1, 0, 1))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2]-15+q2, 0, 1))
    lp <- ll + 20*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt5 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1]-50+q1, 0, 1))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2]-15+q2, 0, 1))
    lp <- ll + 50*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


post_empirical_adt6 <- function(data){
  g1 <- function(var){
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    lp1 <- log((var[1]<50)* emprior1$y[which.min(abs(emprior1$x - var[1]))] + 
                 (var[1]>=50)* dnorm(var[1]-50+q1, 0, 1))
    lp2 <- log((var[2]<15)* emprior2$y[which.min(abs(emprior2$x - var[2]))] + 
                 (var[2]>=15)* dnorm(var[2]-15+q2, 0, 1))
    lp <- ll + 100*(lp1 + lp2)
    return(-lp)
  }
  var0 <- c(0,0)
  return(hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par)
}


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
clusterExport(cl, varlist = c('emprior1', 'emprior2', 'q1', 'q2'))

postmat2_emprior_new <- t(parSapply(cl, simlist2, post_empirical_adt))
postmat2_emprior_new2 <- t(parSapply(cl, simlist2, post_empirical_adt2))
postmat2_emprior_new3 <- t(parSapply(cl, simlist2, post_empirical_adt3))
postmat2_emprior_new4 <- t(parSapply(cl, simlist2, post_empirical_adt4))
postmat2_emprior_new5 <- t(parSapply(cl, simlist2, post_empirical_adt5))
postmat2_emprior_new6 <- t(parSapply(cl, simlist2, post_empirical_adt6))
stopCluster(cl)


par(mfrow=c(1,2))
plot(density(postmat2_hnorm[,1]), xlim=c(0,160), ylim=c(0,0.1), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat2[,1]), xlim=c(0,160), ylim=c(0,0.1), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,1]), xlim=c(0,160), ylim=c(0,0.1), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new[,1]), xlim=c(0,160), ylim=c(0,0.1), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss','MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat2_hnorm[,2]), xlim=c(0,50), col=4, ylim=c(0,0.6), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat2[,2]), xlim=c(0,50), ylim=c(0,0.6), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,2]), xlim=c(0,50), ylim=c(0,0.6), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new[,2]), xlim=c(0,50), ylim=c(0,0.6), col=5, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnor','MLE', 'Loss', 'MAP_emp'), col=c(4,1,2,5), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))



par(mfrow=c(1,2))
plot(density(mlemat2[,1]), xlim=c(0,160), ylim=c(0,.8), lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,1]), xlim=c(0,160), ylim=c(0,.8), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new[,1]), xlim=c(0,160), ylim=c(0,.8), col=3, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new2[,1]), xlim=c(0,160), ylim=c(0,.8), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new3[,1]), xlim=c(0,160), ylim=c(0,.8), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new4[,1]), xlim=c(0,160), ylim=c(0,.8), col=6, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new5[,1]), xlim=c(0,160), ylim=c(0,.8), col=7, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new6[,1]), xlim=c(0,160), ylim=c(0,.8), col=8, lwd=2, main='', xlab='')
legend('topright', c('MLE', 'Loss','k=1', 'k=2', 'k=10','k=20','k=50','k=100'), col=c(1,2,3,4,5,6,7,8), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[I]^2)))


plot(density(mlemat2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=1, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new[,2]), xlim=c(0,50), ylim=c(0,3.5), col=3, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new2[,2]), xlim=c(0,50), ylim=c(0,3.5), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new3[,2]), xlim=c(0,50), ylim=c(0,3.5), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new4[,2]), xlim=c(0,50), ylim=c(0,3.5), col=6, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new5[,2]), xlim=c(0,50), ylim=c(0,3.5), col=7, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat2_emprior_new6[,2]), xlim=c(0,50), ylim=c(0,3.5), col=8, lwd=2, main='', xlab='')
legend('topright', c('MLE', 'Loss','k=1', 'k=2', 'k=10','k=20','k=50','k=100'), col=c(1,2,3,4,5,6,7,8), lwd=2, lty=1,cex=0.8)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))


####################################################################################

ssm <- SSModel(matrix(NA,180,1) ~ SSMtrend(1, Q=list(10)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=20)
sim <- simulateSSM(ssm, "obs", nsim = 3000)
simlist9 <- lapply(1:1000, function(x) sim[,,x])
simlist9 <- lapply(1:1000, function(x) ts(simlist9[[x]], start = c(2000,01), frequency = 12))


f1 <- function(data) {
  f2 <- function(var) {
    x11 <- seas(data, x11='')
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    kfs <- KFS(ssm)
    
    seasonal_x11 <- series(x11, "d10")
    trend_x11 <- series(x11, "d12")
    
    seasonal_kfs <- signal(kfs, "seasonal")$signal
    trend_kfs <- signal(kfs, "trend")$signal
    
    l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
    
    return(l)
  }
  var0 <- c(0,0)
  
  return(hjk(var0, f2, control=list(tol=0.001))$par)
}

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})
system.time({
  idemat9 <- t(parSapply(cl, simlist9, f1))
})
stopCluster(cl)


#######################################################################################

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, c('simlist_14yrs_new', 'mlemat_new', 'idemat_new', 'postmat_hnorm_new', 
                    'postmat_emprior', 'postmat_emprior_im','postmat_emprior_im2','postmat_emprior_0.5',
                    'postmat_emprior_5','postmat_emprior_50','postmat_emprior_0.1'))
sim14yrs_x11pre <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  x11 <- seas(data, x11='')
  return(series(x11, 'fct')[,1])
})

sim14yrs_mlepre <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(mlemat_new[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=mlemat_new[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})

sim14yrs_idepre <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(idemat_new[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=idemat_new[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})

sim14yrs_emppre0.1 <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_0.1[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=postmat_emprior_0.1[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})


sim14yrs_emppre0.5 <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_0.5[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=postmat_emprior_0.5[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})


sim14yrs_emppre <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=postmat_emprior[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})


sim14yrs_emppre2 <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_im[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=postmat_emprior_im[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})

sim14yrs_emppre5 <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_5[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=postmat_emprior_5[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})

sim14yrs_emppre10 <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_im2[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=postmat_emprior_im2[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})

sim14yrs_emppre50 <- parLapply(cl, 1:1000, function(i){
  data <- simlist_14yrs_new[[i]]
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior_50[i,2]))+ SSMseasonal(12, sea.type='dummy',Q=1), H=postmat_emprior_50[i,1])
  pre <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  return(pre)
})

stopCluster(cl)


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, c('sim14yrs_x11pre', 'sim14yrs_mlepre', 'sim14yrs_idepre', 'sim14yrs_emppre', 
                    'simlist_test_new', 'sim14yrs_emppre0.1', 'sim14yrs_emppre0.5',
                    'sim14yrs_emppre2', 'sim14yrs_emppre5', 'sim14yrs_emppre10',
                    'sim14yrs_emppre50'))

x11pre_error <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_x11pre[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

mlepre_error <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_mlepre[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

idepre_error <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_idepre[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

emppre_error0.1 <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_emppre0.1[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

emppre_error0.5 <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_emppre0.5[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

emppre_error <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_emppre[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

emppre_error2 <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_emppre2[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

emppre_error5 <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_emppre5[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

emppre_error10 <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_emppre10[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})

emppre_error50 <- parSapply(cl, 1:1000, function(i){
  dif <- sim14yrs_emppre50[[i]] - simlist_test_new[[i]]
  return(sum(dif^2))
})
stopCluster(cl)

predict_error <- cbind(x11pre_error, mlepre_error, idepre_error, emppre_error0.1, emppre_error0.5,
                       emppre_error, emppre_error2, emppre_error5, emppre_error10,
                       emppre_error50)
summary(predict_error)  

#   x11pre_error     mlepre_error     idepre_error     emppre_error0.1   emppre_error0.5  
#   Min.   : 164.9   Min.   :  92.3   Min.   :  99.52   Min.   :  90.92   Min.   :  87.71  
#   1st Qu.: 644.2   1st Qu.: 571.0   1st Qu.: 577.00   1st Qu.: 569.51   1st Qu.: 564.26  
#   Median :1044.4   Median : 947.4   Median : 946.37   Median : 944.73   Median : 943.51  
#   Mean   :1499.1   Mean   :1310.2   Mean   :1327.50   Mean   :1309.62   Mean   :1310.07  
#   3rd Qu.:1809.7   3rd Qu.:1628.5   3rd Qu.:1629.06   3rd Qu.:1620.69   3rd Qu.:1606.77  
#   Max.   :9137.2   Max.   :8688.0   Max.   :8421.36   Max.   :8653.57   Max.   :8558.81 
    
#   emppre_error     emppre_error2     emppre_error5     emppre_error10    emppre_error50  
#   Min.   :  86.06   Min.   :  85.86   Min.   :  94.94   Min.   :  98.14   Min.   : 102.7  
#   1st Qu.: 562.26   1st Qu.: 561.45   1st Qu.: 568.77   1st Qu.: 567.90   1st Qu.: 571.3  
#   Median : 940.13   Median : 945.64   Median : 935.54   Median : 935.38   Median : 942.5  
#   Mean   :1314.05   Mean   :1316.53   Mean   :1319.82   Mean   :1321.46   Mean   :1323.6  
#   3rd Qu.:1617.81   3rd Qu.:1623.39   3rd Qu.:1650.22   3rd Qu.:1633.48   3rd Qu.:1628.3  
#   Max.   :8629.95   Max.   :8574.55   Max.   :8458.97   Max.   :8458.03   Max.   :8476.1 

apply(predict_error, 2, sd)
#x11pre_error    mlepre_error emppre_error0.1 emppre_error0.5    emppre_error   emppre_error2 
#1319.804        1121.327        1119.985        1119.356        1123.506        1126.389 

#emppre_error5  emppre_error10  emppre_error50 idepre_error
#1129.023        1132.451        1133.275         1135.614

#two empirical priors
#dnorm(emprior1$x[emprior1$x<14.5], mean=8.8, sd=2.9), dexp(emprior1$x[emprior1$x>=14.5]-14.5+11.53, 0.2))
#dnorm(emprior2$x[emprior2$x<4.2], mean=2.46, sd=0.83), dexp(emprior2$x[emprior2$x>=4.2]-4.2+2.93, 1))

unemp_ssm0 <- SSModel(unemp_decomp ~ SSMtrend(1, Q=list(NA))+SSMseasonal(12,sea.type = 'dummy',Q=NA), H=NA)
unemp_fit0 <- fitSSM(unemp_ssm0, inits = c(1,1,1))
unemp_kfs0 <- KFS(unemp_fit0$model)

unemp_decomp <- window(unemp, end=c(2015,11))
unemp_pred <- window(unemp, start=c(2015,12))

unemp_ssmmle <- SSModel(unemp_decomp ~ SSMtrend(1, Q=list(NA))+SSMseasonal(12,sea.type = 'dummy',Q=NA), H=NA)
unemp_fitmle <- fitSSM(unemp_ssmmle, inits = c(1,1,1))
unemp_kfsmle <- KFS(unemp_fitmle$model)
unemp_fitmle$model['H'] # 7.420803
unemp_fitmle$model['Q'] # 66084.02  0.399136
unemp_ssmmle_pred <- predict(unemp_fitmle$model,n.ahead = 12, interval = 'prediction', level = .95)

unemp_x11 <- seas(unemp_decomp, x11='')
unemp_x11_pred <- series(unemp_x11, 'fct')

  
g2 <- function(var){
  ssm <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=var[3]), H=var[1])
  kfs <- KFS(ssm)
  x11 <- seas(unemp_decomp, x11='')
  
  l <- loss5(x11, kfs)
  
  return(l)
}
var0 <- c(0,0,0)
hjkb(var0, g2, lower = c(0,0,0), control = list(tol=0.001))$par 
# 1.855240 1.268692 # this is the ideal value when fix seasonal variance at 1
# 2.929688 2.003906 1.578125 for three

optim <- function(x){
  ssm <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(1.268692*x))+SSMseasonal(12,sea.type = 'dummy',Q=x), H=1.85524*x)
  ll <- logLik(ssm)
  return(ll)
}
optimize(optim, interval = c(0,100000), maximum = TRUE) #20861.78

unemp_ssmideal <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(1.268692*20861.78))+SSMseasonal(12,sea.type = 'dummy',Q=20861.78), H=1.855240*20861.78)
unemp_kfsideal <- KFS(unemp_ssmideal)
unemp_ssmideal_pred <- predict(unemp_ssmideal, n.ahead = 12, interval = 'prediction', level = .95)
ssm <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(1.268692))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=1.855240)
logLik(ssm)  # 3000000+


g1 <- function(var){
  ssm <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(var[2]*var[3]))+SSMseasonal(12,sea.type = 'dummy',Q=var[3]), H=var[1]*var[3])
  ll <- logLik(ssm)
  
  lp1 <- log((var[1]<14.5) * dnorm(var[1], mean=8.8, sd=2.9) + 
              (var[1]>=14.5) * dexp(var[1]-14.5+11.53, 0.2))
  lp2 <- log((var[2]<4.2) * dnorm(var[2], mean=2.46, sd=0.83) +
              (var[2]>=4.2) * dexp(var[2]-4.2+2.93, 1))
  
  lp <- ll + lp1 + lp2
  
  return(-lp)
}

var0 <- c(1,1,1)
hjkb(var0, g1, lower=c(0,0,0), control=list(tol=0.001))$par # 3715.0938  746.4023

optim <- function(x){
  ssm <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(746.4023*x))+SSMseasonal(12,sea.type = 'dummy',Q=x), H=3715.0938*x)
  ll <- logLik(ssm)
  return(ll)
}
optimize(optim, interval = c(0,1000), maximum = TRUE) # 31.32604


unemp_ssmemp <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(746.4023*31.32604))+SSMseasonal(12,sea.type = 'dummy',Q=31.32604), H=3715.0938*31.32604)
unemp_kfsemp <- KFS(unemp_ssmemp)
unemp_ssmemp_pred <- predict(unemp_ssmemp, n.ahead = 12, interval = 'prediction', level = .95)



g1 <- function(var){
  ssm <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
  ll <- logLik(ssm)
  
  lp1 <- log((var[1]<14.5) * dnorm(var[1], mean=8.8, sd=2.9) + 
               (var[1]>=14.5) * dexp(var[1]-14.5+11.53, 0.2))
  lp2 <- log((var[2]<4.2) * dnorm(var[2], mean=2.46, sd=0.83) +
               (var[2]>=4.2) * dexp(var[2]-4.2+2.93, 1))
  
  lp <- ll + 1000*lp1 + 1000*lp2
  
  return(-lp)
}
var0 <- c(1,1)
hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par # 93.125 57.875


g1 <- function(var){
  ssm <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
  ll <- logLik(ssm)
  
  lp1 <- log((var[1]<14.5) * dnorm(var[1], mean=8.8, sd=2.9) + 
               (var[1]>=14.5) * dexp(var[1]-14.5+11.53, 0.2))
  lp2 <- log((var[2]<4.2) * dnorm(var[2], mean=2.46, sd=0.83) +
               (var[2]>=4.2) * dexp(var[2]-4.2+2.93, 1))
  
  lp <- ll + 10000*lp1 + 10000*lp2
  
  return(-lp)
}
var0 <- c(1,1)
hjkb(var0, g1, lower=c(0,0), control=list(tol=0.001))$par #[1] 26.76562 18.35547

##############################################################
##################### decomposition ##########################

unempdecomp <- decompose(unemp, type='additive')
autoplot(unempdecomp) + labs(title='Decomposition of unemployment')

par(mfrow=c(1,2))
plot(window(unemp_decomp, start = c(2000,01), end = c(2004,12)), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(window(series(unemp_x11, 'd11'),start = c(2000,01), end = c(2004,12)), col=alpha(2,.9), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='unemp')
par(new=TRUE)
plot(window(unemp_decomp-signal(unemp_kfsmle, 'seasonal')$signal, start=c(2000,1), end=c(2004,12)),col=alpha(3,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(unemp_decomp-signal(unemp_kfsideal, 'seasonal')$signal, start=c(2000,1), end=c(2004,12)),col=alpha(4,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(unemp_decomp-signal(unemp_kfsemp, 'seasonal')$signal, start=c(2000,1), end=c(2004,12)),col=alpha(5,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
title(main='Seasonally adjusted series')
legend('bottomright', c('raw data','X-11', 'SSM_MLE', 'SSM_IDEAL', 'SSM_MAP'), col=c(1,2,3,4,5), cex=.8, lty=1)

plot(window(unemp_decomp, start = c(2000,01), end = c(2004,12)), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='', col=alpha(1, 0.5))
par(new=TRUE)
plot(window(series(unemp_x11, 'd12'),start = c(2000,01), end = c(2004,12)), col=alpha(2,.9), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='unemp')
par(new=TRUE)
plot(window(signal(unemp_kfsmle, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(3,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(signal(unemp_kfsideal, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(4,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
par(new=TRUE)
plot(window(signal(unemp_kfsemp, 'trend')$signal,start = c(2000,01), end = c(2004,12)),col=alpha(5,.7), ylim=c(5000, 10000), xlim=c(2000,2005), ylab='')
title(main='Trend series')
legend('bottomright', c('raw data','X-11', 'SSM_MLE', 'SSM_IDEAL', 'SSM_MAP'), col=c(1,2,3,4,5), cex=.8, lty=1)
par(mfrow=c(1,1))
# the irregular component from SSM_MLE is very weak 

sum_x11 <- sum((diff(series(unemp_x11, 'd10')))^2) + sum((diff(series(unemp_x11, 'd12')))^2) + sum((diff(series(unemp_x11, 'd13')))^2) 
prop_x11 <- c(sum((diff(series(unemp_x11, 'd10')))^2) / sum_x11, sum((diff(series(unemp_x11, 'd12')))^2) / sum_x11, sum((diff(series(unemp_x11, 'd13')))^2) / sum_x11)
sum_emp <- sum(diff(signal(unemp_kfsemp, 'seasonal')$signal)^2) + sum(diff(signal(unemp_kfsemp, 'trend')$signal)^2) + sum(diff(residuals(unemp_kfsemp,'response'))^2)
prop_emp <- c( sum(diff(signal(unemp_kfsemp, 'seasonal')$signal)^2) / sum_emp, sum(diff(signal(unemp_kfsemp, 'trend')$signal)^2) / sum_emp, sum(diff(residuals(unemp_kfsemp,'response'))^2) / sum_emp)
sum_mle <- sum(diff(signal(unemp_kfsmle, 'seasonal')$signal)^2) + sum(diff(signal(unemp_kfsmle, 'trend')$signal)^2) + sum(diff(residuals(unemp_kfsmle,'response'))^2)
prop_mle <- c(sum(diff(signal(unemp_kfsmle, 'seasonal')$signal)^2) / sum_mle, sum(diff(signal(unemp_kfsmle, 'trend')$signal)^2) / sum_mle, sum(diff(residuals(unemp_kfsmle,'response'))^2) / sum_mle)

sum_ideal <- sum(diff(signal(unemp_kfsideal, 'seasonal')$signal)^2) + sum(diff(signal(unemp_kfsideal, 'trend')$signal)^2) + sum(diff(residuals(unemp_kfsideal,'response'))^2)
prop_ideal <- c(sum(diff(signal(unemp_kfsideal, 'seasonal')$signal)^2) / sum_ideal, sum(diff(signal(unemp_kfsideal, 'trend')$signal)^2) / sum_ideal, sum(diff(residuals(unemp_kfsideal,'response'))^2) / sum_ideal)

model <- c(rep('X-11', 3), rep('SSM_MAP',3), rep('SSM_MLE', 3), rep('SSM_IDEAL', 3))
component <- rep(c('seasonal', 'trend', 'irregular'), 4)
error_prop <- data.frame(model, component, value=c(prop_x11, prop_emp, prop_mle, prop_ideal))

ggplot(error_prop, aes(fill=component, y=value, x=model)) + 
  geom_bar(position="fill", stat="identity") + labs(x='', y='', title='Variability absorbed by each component')
##################### prediction ##########################

sum((unemp_x11_pred[,1] - unemp_pred)^2) # 2009890
sum((unemp_ssmmle_pred[,1] - unemp_pred)^2) # [1] 1026737
sum((unemp_ssmideal_pred[,1] - unemp_pred)^2) # [1] 1470003
sum((unemp_ssmemp_pred[,1] - unemp_pred)^2) # [1] 1771164


plot(unemp_pred, ylim=c(6400, 9000), ylab='')
par(new=TRUE)
plot(unemp_x11_pred[,1], ylim=c(6400, 9000), col=2, ylab='')
par(new=TRUE)
plot(unemp_ssmmle_pred[,1], ylim=c(6400, 9000), col=3, ylab='')
par(new=TRUE)
plot(unemp_ssmideal_pred[,1], ylim=c(6400, 9000), col=4, ylab='')
par(new=TRUE)
plot(unemp_ssmemp_pred[,1], ylim=c(6400, 9000), col=5, ylab='')
title(main='Prediction comparison')
legend('bottomleft', c('TRUE','X-11', 'SSM_MLE', 'SSM_IDEAL','SSM_MAP'), col=c(1,2,3,4,5), cex=.8, lty=1)

x <- c(1:12)
  
par(mfrow=c(2,2))
plot(x, unemp_x11_pred[,1],ylim=c(3000, 11100), type='l', ylab='')
polygon(c(x,rev(x)),c(unemp_x11_pred[,2],rev(unemp_x11_pred[,3])),col = "grey75", border = FALSE)
lines(x, unemp_pred, lwd=2)
lines(x, unemp_x11_pred[,1], col="red",lwd = 2)
lines(x, unemp_x11_pred[,2], col="red",lty=2)
lines(x, unemp_x11_pred[,3], col="red",lty=2)
legend('bottomleft', c('True', 'Prediction'), col=c(1,2), lty=1, lwd=2, cex = 0.8)
title(main='Prediction from X-11')

plot(x, unemp_ssmmle_pred[,1],ylim=c(3000, 11100), type='l', ylab='')
polygon(c(x,rev(x)),c(unemp_ssmmle_pred[,2],rev(unemp_ssmmle_pred[,3])),col = "grey75", border = FALSE)
lines(x, unemp_pred, lwd=2)
lines(x, unemp_ssmmle_pred[,1], col="red", lwd = 2)
lines(x, unemp_ssmmle_pred[,2], col="red",lty=2)
lines(x, unemp_ssmmle_pred[,3], col="red",lty=2)
legend('bottomleft', c('True', 'Prediction'), col=c(1,2), lty=1, lwd=2, cex = 0.8)
title(main='Prediction from SSM(MLE)')


plot(x, unemp_ssmideal_pred[,1],ylim=c(3000, 11100), type='l', ylab='')
polygon(c(x,rev(x)),c(unemp_ssmideal_pred[,2],rev(unemp_ssmideal_pred[,3])),col = "grey75", border = FALSE)
lines(x, unemp_pred, lwd=2)
lines(x, unemp_ssmideal_pred[,1], col="red", lwd = 2)
lines(x, unemp_ssmideal_pred[,2], col="red",lty=2)
lines(x, unemp_ssmideal_pred[,3], col="red",lty=2)
legend('bottomleft', c('True', 'Prediction'), col=c(1,2), lty=1, lwd=2, cex = 0.8)
title(main='Prediction from SSM(IDEAL)')

plot(x, unemp_ssmemp_pred[,1],ylim=c(3000, 11100), type='l', ylab='')
polygon(c(x,rev(x)),c(unemp_ssmemp_pred[,2],rev(unemp_ssmemp_pred[,3])),col = "grey75", border = FALSE)
lines(x, unemp_pred, lwd=2)
lines(x, unemp_ssmemp_pred[,1], col="red", lwd = 2)
lines(x, unemp_ssmemp_pred[,2], col="red",lty=2)
lines(x, unemp_ssmemp_pred[,3], col="red",lty=2)
legend('bottomleft', c('True', 'Prediction'), col=c(1,2), lty=1, lwd=2, cex = 0.8)
title(main='Prediction from SSM(MAP)')
par(mfrow=c(1,1))


unemp_ssm_loss1 <- SSModel(unemp_decomp ~ SSMtrend(1,Q=list(2.90625))+SSMseasonal(12,sea.type = 'dummy',Q=1.875), H=3.93750)
unemp_kfs_loss1 <- KFS(unemp_ssm_loss1)

sum_loss1 <- sum(diff(signal(unemp_kfs_loss1, 'seasonal')$signal)^2) + sum(diff(signal(unemp_kfs_loss1, 'trend')$signal)^2) + sum(diff(residuals(unemp_kfs_loss1,'response'))^2)
prop_loss1 <- c(sum(diff(signal(unemp_kfs_loss1, 'seasonal')$signal)^2) / sum_loss1, sum(diff(signal(unemp_kfs_loss1, 'trend')$signal)^2) / sum_loss1, sum(diff(residuals(unemp_kfs_loss1,'response'))^2) / sum_loss1)

model2 <- c(rep('X-11', 3), rep('SSM_MLE', 3), rep('SSM_LOSS1', 3), rep('SSM_LOSS2', 3))
component <- rep(c('seasonal', 'trend', 'irregular'), 4)
error_prop2 <- data.frame(model2, component, value=c(prop_x11, prop_mle, prop_loss1, prop_ideal))

ggplot(error_prop2, aes(fill=component, y=value, x=model2)) + 
  geom_bar(position="fill", stat="identity") + labs(x='', y='', title='Variability absorbed by each component')
