setwd("C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis")


library(seasonal)
library(KFAS)

ssm_360 <- SSModel(matrix(NA,360,1) ~ SSMtrend(1, Q=list(5)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=10)
sim_360 <- simulateSSM(ssm_360, "obs", nsim = 1000)
simlist_360 <- lapply(1:1000, function(x) sim_360[,,x])
simlist_360 <- lapply(1:1000, function(x) ts(simlist_360[[x]], start = c(2000,01), frequency = 12))

simlist_360_1hf <- lapply(1:1000, function(x) simlist_360[[x]][1:180])
simlist_360_2hf <- lapply(1:1000, function(x) simlist_360[[x]][181:360])

mle_extract <- function(data) {
  # note the output is actually the ratio of mle
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=NA), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  var_S <- fit$model["Q"][2,2,1]
  
  return(c(var_I/var_S, var_T/var_S))
}


library(doParallel)

cl <- detectCores()
cl <- makeCluster(cl)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(KFAS)
})

system.time({
  mlemat_360 <- t(parSapply(cl, simlist_360, mle_extract))
  mlemat_360_1hf <- t(parSapply(cl, simlist_360_1hf, mle_extract))
  mlemat_360_2hf <- t(parSapply(cl, simlist_360_2hf, mle_extract))
})

stopCluster(cl)



plot(density(mlemat_360[,1]), lwd=3, xlim=c(0,20), ylim=c(0,0.3),main='')
par(new=TRUE)
plot(density(mlemat_360_1hf[,1]), col=2, lwd=3, ylim=c(0,0.3),xlim=c(0,20),main='')
par(new=TRUE)
plot(density(mlemat_360_2hf[,1]), col=4, lwd=3, ylim=c(0,0.3),xlim=c(0,20),main='')
title(main="Irregular variance mle distribution")
legend("topleft", c('length360', 'first half', 'second half'), col=c(1,2,4), lwd=3, lty=1)


plot(density(mlemat_360[,2]), lwd=3, xlim=c(0,10), ylim=c(0,0.45),main='')
par(new=TRUE)
plot(density(mlemat_360_1hf[,2]), col=2, lwd=3, ylim=c(0,0.45),xlim=c(0,10),main='')
par(new=TRUE)
plot(density(mlemat_360_2hf[,2]), col=4, lwd=3, ylim=c(0,0.45),xlim=c(0,10),main='')
title(main="Trend variance mle distribution")
legend("topleft", c('length360', 'first half', 'second half'), col=c(1,2,4), lwd=3, lty=1)


plot(density(mlemat_360[,3]), lwd=3, xlim=c(-1,3), ylim=c(0,1.5),main='')
par(new=TRUE)
plot(density(mlemat_360_1hf[,3]), col=2, lwd=3, ylim=c(0,1.5),xlim=c(-1,3),main='')
par(new=TRUE)
plot(density(mlemat_360_2hf[,3]), col=4, lwd=3, ylim=c(0,1.5),xlim=c(-1,3),main='')
title(main="Seasonal variance mle distribution")
legend("topleft", c('length360', 'first half', 'second half'), col=c(1,2,4), lwd=3, lty=1)



#########################################################################

rm(list=ls())
load("simlist.RData")
library(seasonal)
library(KFAS)


data1_x11 <- seas(simlist[[1]], x11='')
data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
data1_fit <- fitSSM(data1_ssm, inits = c(1,1,1))
data1_kfs <- KFS(data1_fit$model)

data1_fit$model['Q'] # 6.54/1.84 = 3.55
data1_fit$model['H'] # 6.77/1.84 = 3.67

# note the ssm6 and kfs6 are results from loss5 acturally
data1_ssm6 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(2.3)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=7.6)
data1_kfs6 <- KFS(data1_ssm6)

plot(series(data1_x11, "d12"), ylim=c(-20,55), ylab='', lwd=2)
par(new=TRUE)
plot(signal(data1_kfs6, "trend")$signal, ylim=c(-20,55), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(data1_kfs6, "trend", filtered = TRUE)$signal, ylim=c(-20,55), ylab='', col=4, lwd=2)
title(main='loss5')


plot(series(data1_x11, "d12")[-180],type='l', ylim=c(-20,55), ylab='', lwd=2)
par(new=TRUE)
plot(signal(data1_kfs6, "trend")$signal[-180],type='l', ylim=c(-20,55), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(data1_kfs6, "trend", filtered = TRUE)$signal[-1], type = 'l', ylim=c(-20,55), ylab='', col=4, lwd=2)
title(main='loss5')


# seasonal
plot(series(data1_x11, "d10"), ylim=c(-15,15), ylab='', lwd=2)
par(new=TRUE)
plot(signal(data1_kfs6, "seasonal")$signal, ylim=c(-15,15), ylab='',col=2,lwd=2)
par(new=TRUE)
plot(signal(data1_kfs6, "seasonal", filtered = TRUE)$signal, ylim=c(-15,15), ylab='', col=4,lwd=2)
title(main='loss5')



###################################################################################

library(KFAS)
library(seasonal)

# check other data
dataeg <- simlist[[521]]

dataeg_x11 <- seas(dataeg, x11='')


dataeg_ssm0 <- SSModel(dataeg ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataeg_fit0 <- fitSSM(dataeg_ssm0, inits = c(1,1,1))
dataeg_kfs0 <- KFS(dataeg_fit0$model)




plot(series(dataeg_x11, "d12"),ylim=c(-25,25), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend")$signal, ylim=c(-25,25), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend",filtered = TRUE)$signal, ylim=c(-25,25), ylab='',col=4, lwd=2)




plot(series(dataeg_x11, "d12")[-180],type='l', ylim=c(-25,25), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend")$signal[-180],type='l', ylim=c(-25,25), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend", filtered = TRUE)$signal[-1], type = 'l', ylim=c(-25,25), ylab='', col=4, lwd=2)



library(doParallel)

system.time({
  
  cl <- detectCores()
  cl <- makeCluster(cl)
  registerDoParallel(cl)
  clusterExport(cl, c("loss5"))
  
  lossmat5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
    foreach(j = 1:200, .combine = "rbind") %dopar% {
      
      dataeg_ssm <- SSModel(dataeg ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
      dataeg_kfs <- KFS(dataeg_ssm)
      
      l <- loss5(dataeg_x11, dataeg_kfs)
      
      c(i*0.1, j*0.1, l)
      
    }
  
  stopCluster(cl)
})

lossmat5[which.min(lossmat5[,3]),]


dataeg_ssm5 <- SSModel(dataeg ~ SSMtrend(1, Q=list(1.4)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=4.6)
dataeg_kfs5 <- KFS(dataeg_ssm5)


par(mfrow=c(1,2))
plot(series(dataeg_x11, "d12"),ylim=c(-25,25), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "trend")$signal, ylim=c(-25,25), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "trend",filtered = TRUE)$signal, ylim=c(-25,25), ylab='',col=4, lwd=2)
legend('topleft',c('x11', 'smoothed', 'unsmoothed'), col=c(1,2,4), lty=1, bty='n')
title(main="no lag")


plot(series(dataeg_x11, "d12")[-180],type='l', ylim=c(-25,25), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "trend")$signal[-180],type='l', ylim=c(-25,25), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "trend", filtered = TRUE)$signal[-1], type = 'l', ylim=c(-25,25), ylab='', col=4, lwd=2)
legend('topleft', c('x11', 'smoothed', 'unsmoothed'), col=c(1,2,4), lty=1, bty='n')
title(main='lag1')

par(mfrow=c(1,1))


#####################################################################################
library(forecast)

plot(series(dataeg_x11, "d12")[-180],type='l',ylim=c(-25,25), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "trend")$signal[-180],type='l', ylim=c(-25,25), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(ma(signal(dataeg_kfs5, "trend", filtered = TRUE)$signal, order=5)[-1],type='l',ylim=c(-25,25), ylab='', lwd=2, col=3)


plot(dataeg - signal(dataeg_kfs5, "trend")$signal - signal(dataeg_kfs5, "seasonal")$signal - residuals(dataeg_kfs5, "recursive"))

plot(dataeg - signal(dataeg_kfs5, "trend", filtered = TRUE)$signal - signal(dataeg_kfs5, "seasonal", filtered = TRUE)$signal - residuals(dataeg_kfs5, "recursive"))
# thus the previous seasonal adjusted series is not accurate

# seasonal adjusted
plot(series(dataeg_x11, "d11"), ylim=c(-25,25), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs5, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-25,25))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs5, "seasonal", filtered = TRUE)$signal, ylab='', lwd=2, col=4, ylim=c(-25,25))
# but apparently the smoothed version is better


# seasonal

plot(series(dataeg_x11, 'd10'), ylim=c(-10,10), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-10,10))
par(new=TRUE)
plot(signal(dataeg_kfs5, "seasonal", filtered = TRUE)$signal, ylab='', lwd=2, col=4, ylim=c(-10,10))

length(signal(dataeg_kfs5, "seasonal", filtered = TRUE)$signal) #180
length(signal(dataeg_kfs5, "trend", filtered = TRUE)$signal) #180



