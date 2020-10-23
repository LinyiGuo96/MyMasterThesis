rm(list = ls())
setwd('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file8')
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


load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/simlist.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/simlist_14yrs.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/simlist_test.RData')
#load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file4/idemat.RData')
#load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/postmat.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file3/functions.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file7/error_pre.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file7/error_decomp.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file7/reallist.RData')
statcan <- read.csv(file='C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/20100008.csv', header = TRUE)

#head(error_decomp)
#head(error_pre)


f1 <- function(var) {
  x11 <- seas(unemp, x11='')
  ssm <- SSModel(unemp ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=var[3]), H=var[1])
  kfs <- KFS(ssm)
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2)
  
  return(l)
}

var0 <- c(0,0,0)
system.time({ob1 <- hjk(var0, f1, control=list(tol=0.01))}) # 77s
ob1 # 3.93750 2.90625 1.87500



f2 <- function(var) {
  x11 <- seas(unemp, x11='')
  ssm <- SSModel(unemp ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=var[3]), H=var[1])
  kfs <- KFS(ssm)
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}

var0 <- c(0,0,0)
system.time({ob2 <- hjk(var0, f2, control=list(tol=0.01))}) # 93s
ob2 # 4.46875 3.00000 2.31250



f2 <- function(var) {
  x11 <- seas(reallist_pre[[1]], x11='')
  ssm <- SSModel(log(reallist_pre[[1]]) ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=var[3]), H=var[1])
  kfs <- KFS(ssm)
  
  seasonal_x11 <- log(series(x11, "d10"))
  trend_x11 <- log(series(x11, "d12"))
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}

var0 <- c(0,0,0)
system.time({ob2 <- hjk(var0, f2, control=list(tol=0.001))}) # 104s
ob2 # 7.878906 2.000000 2.242188


f2 <- function(var) {
  x11 <- seas(reallist_pre[[1]], x11='')
  ssm <- SSModel(log(reallist_pre[[1]]) ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
  kfs <- KFS(ssm)
  
  seasonal_x11 <- log(series(x11, "d10"))
  trend_x11 <- log(series(x11, "d12"))
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}

var0 <- c(0,0)
system.time({ob2 <- hjk(var0, f2, control=list(tol=0.001))}) # 74s
ob2 # 3.511719 0.890625
# and this is the ratio of the previous ideal value

########################################################################

#ssm <- SSModel(matrix(NA,180,1) ~ SSMtrend(1, Q=list(10)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=20)
#sim <- simulateSSM(ssm, "obs", nsim = 100)
#simlist2 <- lapply(1:100, function(x) sim[,,x])
#simlist2 <- lapply(1:100, function(x) ts(simlist2[[x]], start = c(2000,01), frequency = 12))

#rm(list=c('ssm', 'sim', 'simlist2'))

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
idemat <- t(parSapply(cl, simlist_14yrs, f1))
stopCluster(cl)

plot(density(idemat[,1]))
plot(density(idemat[,2]))

# MLE

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
mlemat <- t(parSapply(cl, simlist_14yrs, function(data) {
  
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  
  return(c(var_I, var_T))
}))

stopCluster(cl)
idemat_new <- idemat
mlemat_new <- mlemat

par(mfrow=c(1,2))
plot(density(mlemat_new[,1]), col=alpha(1,.5), lwd=2, ylim=c(0,0.2), xlim=c(0,40), main='', xlab = '')
par(new=TRUE)
plot(density(idemat_new[,1]), col=alpha(2,.5), lwd=2, ylim=c(0,0.2), xlim=c(0,40), main='', xlab = '')
legend('topright', c('MLE', 'Loss'), col=c(1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(mlemat_new[,2]), col=alpha(1,.5), lwd=2, ylim=c(0,0.5), xlim=c(0,20), main='', xlab = '')
par(new=TRUE)
plot(density(idemat_new[,2]), col=alpha(2,.5), lwd=2, ylim=c(0,0.5), xlim=c(0,20), main='', xlab = '')
legend('topright', c('MLE', 'Loss'), col=c(1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))


#load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file4/idemat.RData')
#load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/postmat.RData')

#plot(density(idemat[,1]), col=alpha(1,.5), lwd=2, ylim=c(0,0.2), xlim=c(0,40), main='', xlab = '')
#par(new=TRUE)
#plot(density(idemat_new[,1]), col=alpha(2,.5), lwd=2, ylim=c(0,0.2), xlim=c(0,40), main='', xlab = '')

#plot(density(idemat[,2]), col=alpha(1,.5), lwd=2, ylim=c(0,0.5), xlim=c(0,20), main='', xlab = '')
#par(new=TRUE)
#plot(density(idemat_new[,2]), col=alpha(2,.5), lwd=2, ylim=c(0,0.5), xlim=c(0,20), main='', xlab = '')

########################################################################################

ssm <- SSModel(matrix(NA,180,1) ~ SSMtrend(1, Q=list(25)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=100)
sim <- simulateSSM(ssm, "obs", nsim = 1000)
simlist2 <- lapply(1:1000, function(x) sim[,,x])
simlist2 <- lapply(1:1000, function(x) ts(simlist2[[x]], start = c(2000,01), frequency = 12))


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
idemat2 <- t(parSapply(cl, simlist2, f1))
stopCluster(cl)


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
mlemat2 <- t(parSapply(cl, simlist2, function(data) {
  
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  
  return(c(var_I, var_T))
}))

stopCluster(cl)


par(mfrow=c(1,2))
plot(density(mlemat2[,1]), col=alpha(1,.5), lwd=2, ylim=c(0,0.1), xlim=c(0,160), main='', xlab = '')
par(new=TRUE)
plot(density(idemat2[,1]), col=alpha(2,.5), lwd=2, ylim=c(0,0.1), xlim=c(0,160), main='', xlab = '')
abline(v=100, lty=2)
legend('topright', c('MLE', 'Loss'), col=c(1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(mlemat2[,2]), col=alpha(1,.5), lwd=2, ylim=c(0,0.35), xlim=c(0,65), main='', xlab = '')
par(new=TRUE)
plot(density(idemat2[,2]), col=alpha(2,.5), lwd=2, ylim=c(0,0.35), xlim=c(0,65), main='', xlab = '')
abline(v=25,lty=2)
legend('topright', c('MLE', 'Loss'), col=c(1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))

# compare two prior

par(mfrow=c(1,2))
plot(density(idemat2[,1]), col=alpha(1,.5), lwd=2, ylim=c(0,0.2), xlim=c(0,40), main='', xlab = '')
par(new=TRUE)
plot(density(idemat_new[,1]), col=alpha(2,.5), lwd=2, ylim=c(0,0.2), xlim=c(0,40), main='', xlab = '')
legend('topright', c('MLE', 'Loss'), col=c(1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(idemat2[,2]), col=alpha(1,.5), lwd=2, ylim=c(0,0.5), xlim=c(0,20), main='', xlab = '')
par(new=TRUE)
plot(density(idemat_new[,2]), col=alpha(2,.5), lwd=2, ylim=c(0,0.5), xlim=c(0,20), main='', xlab = '')
legend('topright', c('MLE', 'Loss'), col=c(1,2), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))

########################################################################################


# uniform 

uniform <- function(x) {
  
  data <- x
  
  f2 <- function(var) {
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(1/75)
    lp2 <- log(1/20)
    
    lp <- ll + lp1 + lp2
    return(-lp)
  }
  
  var0 <- c(0,0)
  
  return(hjkb(var0, f2, lower=c(0,0), upper=c(75,20), control=list(tol=0.001))$par)
}

ob1 <- t(sapply(simlist2, uniform))


# half-t

hcauchy <- function(x) {
  
  data <- x
  
  f2 <- function(var) {
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(dhcauchy(sqrt(var[1]), sigma = 1.25))
    lp2 <- log(dhcauchy(sqrt(var[2]), sigma = 0.75))
    
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
ob2 <- t(parSapply(cl, simlist2[1:100], hcauchy))
stopCluster(cl)

plot(density(ob2[,1]))
plot(density(ob2[,2]))

###########################################################################

# section 4

plot(density(mlemat_new[,1]))
plot(density(mlemat_new[,2]))
plot(density(idemat_new[,1])) #sqrt(20)/4
plot(density(idemat_new[,2])) #sqrt(5)/4


hnorm <- function(x) {
  
  data <- x
  
  f2 <- function(var) {
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    ll <- logLik(ssm)
    
    lp1 <- log(dhnorm(sqrt(var[1]), sigma = sqrt(20)/3))
    lp2 <- log(dhnorm(sqrt(var[2]), sigma = sqrt(5)/3))
    
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

postmat_hnorm <- t(parSapply(cl, simlist_14yrs, hnorm))
#postmat2_hnorm <- t(parSapply(cl, simlist2, hnorm))

stopCluster(cl)

par(mfrow=c(1,2))
plot(density(postmat_hnorm[,1]), xlim=c(0,30), ylim=c(0,0.22), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,1]), xlim=c(0,30), ylim=c(0,0.22), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,1]), xlim=c(0,30), ylim=c(0,0.22), col=4, lwd=2, main='', xlab='')
legend('topright', c('MAP','MLE', 'Loss'), col=c(1,2,4), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat_hnorm[,2]), xlim=c(0,15), ylim=c(0,0.5), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[,2]), xlim=c(0,15), ylim=c(0,0.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[,2]), xlim=c(0,15), ylim=c(0,0.5), col=4, lwd=2, main='', xlab='')
legend('topright', c('MAP','MLE', 'Loss'), col=c(1,2,4), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))



plot(density(postmat2_hnorm[,1]), xlim=c(0,160), ylim=c(0,0.1))
par(new=T)
plot(density(mlemat2[,1]), xlim=c(0,160), ylim=c(0,0.1), col=2)
par(new=T)
plot(density(idemat2[,1]), xlim=c(0,160), ylim=c(0,0.1), col=4)

plot(density(postmat2_hnorm[,2]), xlim=c(0,60), ylim=c(0,0.3))
par(new=T)
plot(density(mlemat2[,2]), xlim=c(0,60), ylim=c(0,0.3), col=2)
par(new=T)
plot(density(idemat2[,2]), xlim=c(0,60), ylim=c(0,0.3), col=4)

###########################################################################

# section 4
cl <- makeCluster(detectCores())

clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, varlist = c('simlist_14yrs', 'idemat_new', 'mlemat_new', 'postmat_hnorm'))
error_decomp_new <- t(parSapply(cl, 1:1000, function(i){
  data <- simlist_14yrs[[i]]
  
  x11 <- seas(data, x11='')
  
  ssm0 <- SSModel(data ~ SSMtrend(1, Q=list(mlemat_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=mlemat_new[i,1])
  kfs0 <- KFS(ssm0)
  
  ssm5 <- SSModel(data ~ SSMtrend(1, Q=list(idemat_new[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat_new[i,1])
  kfs5 <- KFS(ssm5)
  
  ssmpost <- SSModel(data ~ SSMtrend(1, Q=list(postmat_hnorm[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_hnorm[i,1])
  kfspost <- KFS(ssmpost)
 
  
  L0 <- sum((series(x11, 'd12') - signal(kfs0, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs0, 'seasonal')$signal)^2)
  L1 <- sum((series(x11, 'd12') - signal(kfs5, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs5, 'seasonal')$signal)^2)
  L2 <- sum((series(x11, 'd12') - signal(kfspost, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost, 'seasonal')$signal)^2)
  
  L <- c(MLE = L0, Loss = L1, MAP=L2)
  
}))

stopCluster(cl)
error_decomp_new <- as.data.frame(error_decomp_new)
summary(error_decomp_new)
apply(error_decomp_new,2,sd)


boxplot(error_decomp_new)
abline(h=median(error_decomp_new$MAP), col=2, lwd=2)

# test 
friedman.test(as.matrix(error_decomp_new[,c(1,3)]))
wilcox.test(x=error_decomp_new$MLE, y=error_decomp_new$MAP)

###########################################################################

# section 5
emprior1 <- density(idemat_new[1:700,1], n=2024)
emprior2 <- density(idemat_new[1:700,2], n=2024)

post_extract_new <- function(data){
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

postmat_emprior <- t(sapply(simlist_14yrs[701:1000], post_extract_new))



par(mfrow=c(1,2))
plot(density(postmat_hnorm[701:1000,1]), xlim=c(0,30), ylim=c(0,0.3), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[701:1000,1]), xlim=c(0,30), ylim=c(0,0.3), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[701:1000,1]), xlim=c(0,30), ylim=c(0,0.3), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior[,1]), xlim=c(0,30), ylim=c(0,0.3), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[1:700,1]), xlim=c(0,30), ylim=c(0,0.3), col=6, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnormal','MLE', 'Loss','MAP_empir','Emp Prior'), col=c(1,2,4,5,6), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[I]^2)))

plot(density(postmat_hnorm[701:1000,2]), xlim=c(0,15), ylim=c(0,0.5), lwd=2, main='', xlab='')
par(new=T)
plot(density(mlemat_new[701:1000,2]), xlim=c(0,15), ylim=c(0,0.5), col=2, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[701:1000,2]), xlim=c(0,15), ylim=c(0,0.5), col=4, lwd=2, main='', xlab='')
par(new=T)
plot(density(postmat_emprior[,2]), xlim=c(0,15), ylim=c(0,0.5), col=5, lwd=2, main='', xlab='')
par(new=T)
plot(density(idemat_new[1:700,2]), xlim=c(0,15), ylim=c(0,0.5), col=6, lwd=2, main='', xlab='')
legend('topright', c('MAP_hnormal','MLE', 'Loss', 'MAP_empir','Emp Prior'), col=c(1,2,4,5,6), lwd=2, lty=1)
title(main=expression(paste('Distribution of ', sigma[T]^2)))
par(mfrow=c(1,1))

error_decomp_emp <- sapply(701:1000, function(i){
  
  data <- simlist_14yrs[[i]]
  x11 <- seas(data, x11='')
  ssmpost <- SSModel(data ~ SSMtrend(1, Q=list(postmat_emprior[i-700,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_emprior[i-700,1])
  kfspost <- KFS(ssmpost)
  
  sum((series(x11, 'd12') - signal(kfspost, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfspost, 'seasonal')$signal)^2)
  
})

summary(error_decomp_emp)
sd(error_decomp_emp)
