rm(list = ls())
load("function.RData")

library(forecast)

set.seed(465)

Datalist240 <- simlist1(10,240)

par(mfrow=c(5,2))
for(i in 1:10)  {
  plot(Datalist240[[i]])
}
par(mfrow=c(1,1))


library(seasonal)
library(KFAS)
library(doParallel)


cl <- detectCores()
cl <- makeCluster(cl)

clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(doParallel)
})

x11list240 <- parLapply(cl, Datalist240, x11)
transformation240 <- parSapply(cl, x11list240, transformfunction)
datalist240 <- parLapply(cl, x11list240, rmoutliers)
x11list240 <- parLapply(cl, datalist240, x11)
datalist240 <- parLapply(cl, x11list240, addtransform)
x11list240 <- parLapply(cl, datalist240, x11)
datalist240 <- parLapply(cl, x11list240, addtransform)
x11list240 <- parLapply(cl, datalist240, x11)

ssmlist240 <- parLapply(cl, datalist240, ssmfit)
kfslist240 <- parLapply(cl, ssmlist240, function(x) KFS(x$model) )


stopCluster(cl)

par(mfrow=c(5,2))
for(i in 1:10)  {
  plot(datalist240[[i]])
}
par(mfrow=c(1,1))

lapply(x11list240, transformfunction)

par(mfrow=c(2,3))
plot(series(x11list240[[1]], 'd12'))
plot(series(x11list240[[1]], 'd10'))
plot(series(x11list240[[1]], 'd13'))

plot(signal(kfslist240[[1]], states = "trend")$signal)
plot(signal(kfslist240[[1]], states = "seasonal")$signal)
plot(residuals(kfslist240[[1]], type = "recursive"))

par(mfrow=c(1,1))


#ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ#

dataeg <- simulation1(180)
plot(dataeg)
x11eg <- seas(dataeg, x11='')
summary(x11eg)
dataeg_rmout <- rmoutliers(x11eg)
plot(dataeg_rmout)
x11eg <- seas(dataeg_rmout, x11='')
summary(x11eg)
dataeg_log <- log(dataeg_rmout)
x11eg_log <- seas(dataeg_log, x11='')
summary(x11eg_log)



#ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ#

library(seasonal)
library(KFAS)


model <- SSModel(matrix(NA, 180, 1) ~ SSMtrend(1, Q=list(5)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=10)
sim <- simulateSSM(model, "obs")
sim <- ts(sim[,,1], start = c(2000,01), frequency = 12)


ssm <- SSModel(sim ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
fit <- fitSSM(ssm, inits = c(1,1,1))
kfs <- KFS(fit$model)

fit$model["Q"]
fit$model["H"]
  
model_x11 <- seas(sim, x11='')
summary(model_x11)


ssm2 <- SSModel(sim ~ SSMtrend(1, Q=list(5)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=10)
kfs2 <- KFS(ssm2)

par(mfrow=c(3,3))
plot(series(model_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main= "mle")
plot(signal(kfs2, state="seasonal")$signal, main="try")
plot(series(model_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs2, state="trend")$signal, main="try")
plot(series(model_x11, "d13"))
plot(residuals(kfs,"recursive"), main="mle")
plot(residuals(kfs2, "recursive"), main="try")
par(mfrow=c(1,1))




plot(series(model_x11, "d10"), ylim=c(-10,10), ylab='')
par(new=TRUE)
plot(signal(kfs, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=2, lty=2)
par(new=TRUE)
plot(signal(kfs2, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=3, lty=3, lwd=2)



plot(series(model_x11, "d12"), ylim=c(-5,10), ylab='', lwd=2.5)
par(new=TRUE)
plot(signal(kfs, state="trend")$signal, ylim=c(-5,10), ylab='', col=2, lty=2, lwd=2)
par(new=TRUE)
plot(signal(kfs2, state="trend")$signal, ylim=c(-5,10), ylab='', col=3, lty=3, lwd=3)


#ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ#

library(doParallel)
registerDoParallel(8)

system.time({
  
  logPost <- foreach(i = 1:100, .packages = c("seasonal", "KFAS"), .combine = "rbind") %:% 
    foreach(j = 1:100, .combine = "rbind")  %dopar% {
      
      ssm <- SSModel(sim ~ SSMtrend(1, Q=list(j*0.2)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=0.2*i)
      
      logpost <- logLik(ssm) + df(df1=10, df2=5, x=j*0.2, log=TRUE) + dgamma(x=i*0.2, shape=9, rate=3, log=TRUE)
      
      c(i*.2, j*.2, logpost)
      
    }
  
})

stopCluster(8)

logPost[which.max(logPost[,3]),]

ssm2 <- SSModel(sim ~ SSMtrend(1, Q=list(1)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=1.2)
kfs2 <- KFS(ssm2)


par(mfrow=c(2,3))
plot(series(model_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main= "mle")
plot(signal(kfs2, state="seasonal")$signal, main="try")
plot(series(model_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs2, state="trend")$signal, main="try")
par(mfrow=c(1,1))


#ZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZZ#
unempssm1 <- SSModel(unemp ~ SSMtrend(1, Q= list(3)) + SSMseasonal(12, sea.type = 'dummy', Q=2), H=1)
unempkfs1 <- KFS(unempssm1)

unempssm2 <- SSModel(unemp ~ SSMtrend(1, Q= list(6)) + SSMseasonal(12, sea.type = 'dummy', Q=4), H=2)
unempkfs2 <- KFS(unempssm2)


plot(signal(unempkfs1, "seasonal")$signal, lwd=2)
par(new=TRUE)
plot(signal(unempkfs2, "seasonal")$signal, col=2, lwd=2, lty=2)

plot(signal(unempkfs1, "trend")$signal, lwd=2)
par(new=TRUE)
plot(signal(unempkfs2, "trend")$signal, col=2, lwd=2, lty=2)

plot(residuals(unempkfs1, "recursive"), lwd=2)
par(new=TRUE)
plot(residuals(unempkfs2, "recursive"), col=2, lwd=2, lty=2)

