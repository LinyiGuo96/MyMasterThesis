getwd()
setwd("C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis")
rm(list=ls())

library(seasonal)
library(KFAS)

ssm <- SSModel(matrix(NA,180,1) ~ SSMtrend(1, Q=list(5)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=10)
sim <- simulateSSM(ssm, "obs", nsim = 1000)
simlist <- lapply(1:1000, function(x) sim[,,x])
simlist <- lapply(1:1000, function(x) ts(simlist[[x]], start = c(2000,01), frequency = 12))

mle_extract <- function(data) {
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=NA), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  var_S <- fit$model["Q"][2,2,1]
  
  return(c(var_I, var_T, var_S))
}

#system.time({
#  mlematrix <- sapply(simlist, mle_extract)
#})

library(doParallel)

cl <- detectCores()
cl <- makeCluster(cl)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(KFAS)
})

system.time({
  mlematrix <- t(parSapply(cl, simlist, mle_extract))
})

stopCluster(cl)

hist(mlematrix[,1], breaks = 100)
hist(mlematrix[,2], breaks = 100)
hist(mlematrix[,3], breaks = 100)

plot(density(mlematrix[,1]), main="Irregular variance mle distribution")
plot(density(mlematrix[,2]), main="Trend variance mle distribution")
plot(density(mlematrix[,3]), main="Seasonal variance mle distribution")




# set var_S = 1 
mle_extract <- function(data) {
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]

  return(c(var_I, var_T, 1))
}

#system.time({
#  mlematrix <- sapply(simlist, mle_extract)
#})

library(doParallel)

cl <- detectCores()
cl <- makeCluster(cl)
registerDoParallel(cl)

clusterEvalQ(cl, {
  library(KFAS)
})

system.time({
  mlematrix_new <- t(parSapply(cl, simlist, mle_extract))
})

stopCluster(cl)

hist(mlematrix_new[,1], breaks = 100) 
hist(mlematrix_new[,2], breaks = 100)

plot(density(mlematrix_new[,1]))
plot(density(mlematrix_new[,2]))


loss <- function(x11, kfs) {
  x11_T <- series(x11, "d12")
  x11_S <- series(x11, "d10")
  x11_I <- series(x11, "d13")
  
  ssm_T <- signal(kfs, "trend")$signal
  ssm_S <- signal(kfs, "seasonal")$signal
  ssm_I <- residuals(kfs, "recursive")
  
  loss <- sum((x11_T-ssm_T)^2) + sum((x11_S-ssm_S)^2) + sum((x11_I-ssm_I)^2)
  
  return(loss)
}

data1_x11 <- seas(simlist[[1]],x11='')
lossmat <- c()
for(i in -50:50){
  for (j in -50:50) {
    ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(5+j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=10+i*.1)
    kfs <- KFS(ssm)
    
    L <- loss(data1_x11, kfs)
    
    lossmat <- rbind(lossmat, c(10+i*.1, 5+j*.1, L))
  }
}


mlematrix[1,] #8.772556 6.205378 1.192358
lossmat[which.min(lossmat[,3]),]

ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(6.205378)) + SSMseasonal(12, sea.type = 'dummy', Q=1.192358), H=8.772556)
kfs <- KFS(ssm)

ssm2 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(5.6)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=7.9)
kfs2 <- KFS(ssm2)

par(mfrow=c(3,3))
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main= "mle")
plot(signal(kfs2, state="seasonal")$signal, main="ideal")
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs2, state="trend")$signal, main="ideal")
plot(series(data1_x11, "d13"))
plot(residuals(kfs,"recursive"), main="mle")
plot(residuals(kfs2, "recursive"), main="ideal")
par(mfrow=c(1,1))



par(mfrow=c(2,3))
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs2, state="trend")$signal, main="ideal")
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend", filtered = TRUE)$signal, main="filtered mle")
plot(signal(kfs2, state="trend", filtered = TRUE)$signal, main="filtered ideal")
par(mfrow=c(1,1))


par(mfrow=c(2,3))
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main="mle")
plot(signal(kfs2, state="seasonal")$signal, main="ideal")
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal", filtered = TRUE)$signal, main="filtered mle")
plot(signal(kfs2, state="seasonal", filtered = TRUE)$signal, main="filtered ideal")
par(mfrow=c(1,1))




loss2 <- function(x11, kfs, var) {
  x11_T <- series(x11, "d12")
  x11_S <- series(x11, "d10")
  x11_I <- series(x11, "d13")
  
  ssm_T <- signal(kfs, "trend")$signal
  ssm_S <- signal(kfs, "seasonal")$signal
  ssm_I <- residuals(kfs, "recursive")
  
  loss <- sum((x11_T-ssm_T)^2)/var[2] + sum((x11_S-ssm_S)^2)/var[3] + sum((x11_I-ssm_I)^2)/var[1]
  
  return(loss)
}

data1_x11 <- seas(simlist[[1]],x11='')
lossmat2 <- c()
for(i in -50:50){
  for (j in -50:50) {
    ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(5+j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=10+i*.1)
    kfs <- KFS(ssm)
    var <- c(10+i*.1,5+j*0.1,1)
    
    L <- loss2(data1_x11, kfs, var)
    
    lossmat2 <- rbind(lossmat2, c(10+i*.1, 5+j*.1, L))
  }
}

lossmat2[which.min(lossmat2[,3]), ]


ssm3 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(9.6)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=15)
kfs3 <- KFS(ssm3)


par(mfrow=c(3,3))
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main= "mle")
plot(signal(kfs3, state="seasonal")$signal, main="ideal")
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs3, state="trend")$signal, main="ideal")
plot(series(data1_x11, "d13"))
plot(residuals(kfs,"recursive"), main="mle")
plot(residuals(kfs3, "recursive"), main="ideal")
par(mfrow=c(1,1))



par(mfrow=c(2,3))
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs3, state="trend")$signal, main="ideal")
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend", filtered = TRUE)$signal, main="filtered mle")
plot(signal(kfs3, state="trend", filtered = TRUE)$signal, main="filtered ideal")
par(mfrow=c(1,1))


par(mfrow=c(2,3))
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main="mle")
plot(signal(kfs3, state="seasonal")$signal, main="ideal")
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal", filtered = TRUE)$signal, main="filtered mle")
plot(signal(kfs3, state="seasonal", filtered = TRUE)$signal, main="filtered ideal")
par(mfrow=c(1,1))


# vertical comparison

par(mfrow=c(3,4))
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main= "mle")
plot(signal(kfs2, state="seasonal")$signal, main="loss1")
plot(signal(kfs3, state="seasonal")$signal, main="loss2")
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs2, state="trend")$signal, main="loss1")
plot(signal(kfs3, state="trend")$signal, main="loss2")
plot(series(data1_x11, "d13"))
plot(residuals(kfs,"recursive"), main="mle")
plot(residuals(kfs2, "recursive"), main="loss1")
plot(residuals(kfs3, "recursive"), main="loss2")
par(mfrow=c(1,1))



par(mfrow=c(2,4))
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend")$signal, main="mle")
plot(signal(kfs2, state="trend")$signal, main="loss1")
plot(signal(kfs3, state="trend")$signal, main="loss2")
plot(series(data1_x11, "d12"))
plot(signal(kfs, state="trend", filtered = TRUE)$signal, main="filtered mle")
plot(signal(kfs2, state="trend", filtered = TRUE)$signal, main="filtered loss1")
plot(signal(kfs3, state="trend", filtered = TRUE)$signal, main="filtered loss2")
par(mfrow=c(1,1))


par(mfrow=c(2,4))
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal")$signal, main="mle")
plot(signal(kfs2, state="seasonal")$signal, main="loss1")
plot(signal(kfs3, state="seasonal")$signal, main="loss2")
plot(series(data1_x11, "d10"))
plot(signal(kfs, state="seasonal", filtered = TRUE)$signal, main="filtered mle")
plot(signal(kfs2, state="seasonal", filtered = TRUE)$signal, main="filtered loss1")
plot(signal(kfs3, state="seasonal", filtered = TRUE)$signal, main="filtered loss2")
par(mfrow=c(1,1))




#######################################################################################
save(simlist, file="simlist.RData") # located in file1 folder
getwd()

library(seasonal)
library(KFAS)
# new loss function
loss <- function(data, x11, kfs){
  
  deseasonal_x11 <- series(x11, "d11")
  trend_x11 <- series(x11, "d12")
  
  deseasonal_kfs <- data - signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((deseasonal_x11-deseasonal_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}


data1_x11 <- seas(simlist[[1]], x11='')
data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
data1_fit <- fitSSM(data1_ssm, inits = c(1,1,1))
data1_kfs <- KFS(data1_fit$model)
data1_fit$model["Q"] # 6.547234 1.841068
data1_fit$model["H"] # 6.776355

loss(simlist[[1]], data1_x11, data1_kfs) # 579 for mle


library(doParallel)
cl <- detectCores()
cl <- makeCluster(cl)
clusterExport(cl, c("loss"))

lossmat <- foreach (i = 1:200, .combine = 'rbind', .packages = c('KFAS', 'seasonal')) %:%
  foreach (j = 1:200, .combine = 'rbind') %dopar% {
    
    data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
    data1_kfs <- KFS(data1_ssm)
    
    l <- loss(simlist[[1]], data1_x11, data1_kfs)
    
    c(i*0.1, j*0.1, l)
}

lossmat[which.min(lossmat[,3]), ]
#[1]  8.1000   1.3000 393.5882


data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
data1_fit <- fitSSM(data1_ssm, inits = c(1,1,1))
data1_kfs <- KFS(data1_fit$model)

data1_ssm2 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(1.3)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=8.1)
data1_kfs2 <- KFS(data1_ssm2)

plot(simlist[[1]])

par(mfrow=c(4,3))
plot(series(data1_x11, "d11"))
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, main="mle")
plot(simlist[[1]] - signal(data1_kfs2, "seasonal")$signal, main="loss")

plot(series(data1_x11, "d12"))
plot(signal(data1_kfs, state="trend")$signal, main="mle")
plot(signal(data1_kfs2, state="trend")$signal, main="loss")

plot(series(data1_x11, "d10"))
plot(signal(data1_kfs, state="seasonal")$signal, main= "mle")
plot(signal(data1_kfs2, state="seasonal")$signal, main="loss")

plot(series(data1_x11, "d13"))
plot(residuals(data1_kfs,"recursive"), main="mle")
plot(residuals(data1_kfs2, "recursive"), main="loss")
par(mfrow=c(1,1))


library(scales)

par(mfrow=c(2,2))

plot(series(data1_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs2, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs2, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs2, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data1_kfs,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs2, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

par(mfrow=c(1,1))





# new loss function
loss2 <- function(data, x11, kfs){
  
  deseasonal_x11 <- series(x11, "d11")
  trend_x11 <- series(x11, "d12")
  
  deseasonal_kfs <- data - signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((deseasonal_x11-deseasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2)
  
  return(l)
}


# data1_x11 <- seas(simlist[[1]], x11='')
# data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
# data1_fit <- fitSSM(data1_ssm, inits = c(1,1,1))
# data1_kfs <- KFS(data1_fit$model)
# data1_fit$model["Q"] # 6.547234 1.841068
# data1_fit$model["H"] # 6.776355

loss2(simlist[[1]], data1_x11, data1_kfs) # 627 for mle

lossmat2 <- foreach (i = 1:200, .combine = 'rbind', .packages = c('KFAs', 'seasonal')) %:%
  foreach(j = 1:200, .combine = 'rbind', .packages = c('KFAS', 'seasonal')) %dopar% {
    data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
    data1_kfs <- KFS(data1_ssm)
    
    l <- loss2(simlist[[1]], data1_x11, data1_kfs)
    
    c(i*0.1, j*0.1, l)
  
}

stopCluster(cl)

lossmat2[which.min(lossmat2[,3]), ] #[1]  7.1000   2.6000 526.4603


data1_ssm3 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(2.6)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=7.1)
data1_kfs3 <- KFS(data1_ssm3)




par(mfrow=c(4,3))
plot(series(data1_x11, "d11"))
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, main="mle")
plot(simlist[[1]] - signal(data1_kfs3, "seasonal")$signal, main="loss")

plot(series(data1_x11, "d12"))
plot(signal(data1_kfs, state="trend")$signal, main="mle")
plot(signal(data1_kfs3, state="trend")$signal, main="loss")

plot(series(data1_x11, "d10"))
plot(signal(data1_kfs, state="seasonal")$signal, main= "mle")
plot(signal(data1_kfs3, state="seasonal")$signal, main="loss")

plot(series(data1_x11, "d13"))
plot(residuals(data1_kfs,"recursive"), main="mle")
plot(residuals(data1_kfs3, "recursive"), main="loss")
par(mfrow=c(1,1))



par(mfrow=c(2,2))

plot(series(data1_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs3, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs3, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3))

plot(series(data1_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs3, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data1_kfs,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs3, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

par(mfrow=c(1,1))

# compare two loss functions 

par(mfrow=c(2,2))

plot(series(data1_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs2, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs3, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs2, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs3, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3))

plot(series(data1_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs2, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs3, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data1_kfs2,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs3, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

par(mfrow=c(1,1))





# check with another dataset


data2_x11 <- seas(simlist[[2]], x11='')
data2_ssm <- SSModel(simlist[[2]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
data2_fit <- fitSSM(data2_ssm, inits = c(1,1,1))
data2_kfs <- KFS(data2_fit$model)
data2_fit$model["Q"] # 4.955449 1.050081
data2_fit$model["H"] # 7.386918


loss2(simlist[[2]], data2_x11, data2_kfs) # 355 for mle

library(doParallel)
cl <- detectCores()
cl <- makeCluster(cl)
clusterExport(cl, c("loss2"))


lossmat2  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS", "seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    data2_ssm <- SSModel(simlist[[2]] ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
    data2_kfs <- KFS(data2_ssm)
    
    l <- loss2(simlist[[2]], data2_x11, data2_kfs)
    
    c(i*0.1, j*0.1, l)
  
}

lossmat2[which.min(lossmat2[,3]), ] # [1]  11.3000   4.7000 326.7237


data2_ssm3 <- SSModel(simlist[[2]] ~ SSMtrend(1, Q=list(4.7)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=11.3)
data2_kfs3 <- KFS(data2_ssm3)


plot(simlist[[2]])

par(mfrow=c(4,3))
plot(series(data2_x11, "d11"))
plot(simlist[[2]] - signal(data2_kfs, "seasonal")$signal, main="mle")
plot(simlist[[2]] - signal(data2_kfs3, "seasonal")$signal, main="loss")

plot(series(data2_x11, "d12"))
plot(signal(data2_kfs, state="trend")$signal, main="mle")
plot(signal(data2_kfs3, state="trend")$signal, main="loss")

plot(series(data2_x11, "d10"))
plot(signal(data2_kfs, state="seasonal")$signal, main= "mle")
plot(signal(data2_kfs3, state="seasonal")$signal, main="loss")

plot(series(data2_x11, "d13"))
plot(residuals(data2_kfs,"recursive"), main="mle")
plot(residuals(data2_kfs3, "recursive"), main="loss")
par(mfrow=c(1,1))



par(mfrow=c(2,2))

plot(series(data2_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[2]] - signal(data2_kfs, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[2]] - signal(data2_kfs3, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data2_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data2_kfs, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data2_kfs3, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data2_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data2_kfs, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data2_kfs3, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data2_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data2_kfs,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data2_kfs3, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

par(mfrow=c(1,1))


# new loss function

loss3 <- function(x11, kfs){
  
  trend_x11 <- series(x11, "d12")
  seasonal_x11 <- series(x11, "d10")
  
  trend_kfs <- signal(kfs, "trend")$signal
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2)
  
  return(l)
}



data1_x11 <- seas(simlist[[1]], x11='')
data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
data1_fit <- fitSSM(data1_ssm, inits = c(1,1,1))
data1_kfs <- KFS(data1_fit$model)

loss3(data1_x11, data1_kfs) # mle 576

cl <- detectCores()
cl <- makeCluster(cl)
clusterExport(cl, c("loss3"))

lossmat3  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
    data1_kfs <- KFS(data1_ssm)
    
    l <- loss3(data1_x11, data1_kfs)
    
    c(i*0.1, j*0.1, l)
    
}

stopCluster(cl)


lossmat3[which.min(lossmat3[,3]), ] # [1]   8.0000   2.9000 464.0448
# this result is very similar to that from loss2 7.1   2.6


data1_ssm4 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(2.9)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=8)
data1_kfs4 <- KFS(data1_ssm4)




par(mfrow=c(4,3))
plot(series(data1_x11, "d11"))
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, main="mle")
plot(simlist[[1]] - signal(data1_kfs4, "seasonal")$signal, main="loss")

plot(series(data1_x11, "d12"))
plot(signal(data1_kfs, state="trend")$signal, main="mle")
plot(signal(data1_kfs4, state="trend")$signal, main="loss")

plot(series(data1_x11, "d10"))
plot(signal(data1_kfs, state="seasonal")$signal, main= "mle")
plot(signal(data1_kfs4, state="seasonal")$signal, main="loss")

plot(series(data1_x11, "d13"))
plot(residuals(data1_kfs,"recursive"), main="mle")
plot(residuals(data1_kfs4, "recursive"), main="loss")
par(mfrow=c(1,1))



par(mfrow=c(2,2))

plot(series(data1_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs4, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs4, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3))

plot(series(data1_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs4, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data1_kfs,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs4, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

par(mfrow=c(1,1))


# compare different loss function results


par(mfrow=c(2,2))

plot(series(data1_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs2, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs3, "seasonal")$signal,  ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs4, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(7,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2', 'loss3'), col=c(1,2,5,7), lty=1, lwd = c(1.5,3,3,3), cex=.5)

plot(series(data1_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs2, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs3, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs4, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(7,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2','loss3'), col=c(1,2,5,7), lty=1, lwd = c(1.5,3,3,3), cex = .5)

plot(series(data1_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs2, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs3, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs4, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(7,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2','loss3'), col=c(1,2,5,7), lty=1, lwd = c(1.5,3,3,3), cex=.5)

plot(series(data1_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data1_kfs2,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs3, "recursive"), ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs4, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(7,.5), lwd=3)
legend("topleft", c('x11', 'loss1', 'loss2','loss3'), col=c(1,2,5,7), lty=1, lwd = c(1.5,3,3,3), cex=.5)

par(mfrow=c(1,1))




# new loss function

loss4 <- function(x11, kfs){
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}


data1_x11 <- seas(simlist[[1]], x11='')
data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
data1_fit <- fitSSM(data1_ssm, inits = c(1,1,1))
data1_kfs <- KFS(data1_fit$model)

loss4(data1_x11, data1_kfs) # mle 528

cl <- detectCores()
cl <- makeCluster(cl)
clusterExport(cl, c("loss4"))

lossmat4  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
    data1_kfs <- KFS(data1_ssm)
    
    l <- loss4(data1_x11, data1_kfs)
    
    c(i*0.1, j*0.1, l)
    
  }

stopCluster(cl)


lossmat4[which.min(lossmat4[,3]), ] # 9.1000   1.5000 330.0306
# this result(variances' values) is very similar to that from loss1 
#[1]  8.1000   1.3000 393.5882



data1_ssm5 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(1.5)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=9.1)
data1_kfs5 <- KFS(data1_ssm5)




par(mfrow=c(4,3))
plot(series(data1_x11, "d11"))
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, main="mle")
plot(simlist[[1]] - signal(data1_kfs5, "seasonal")$signal, main="loss")

plot(series(data1_x11, "d12"))
plot(signal(data1_kfs, state="trend")$signal, main="mle")
plot(signal(data1_kfs5, state="trend")$signal, main="loss")

plot(series(data1_x11, "d10"))
plot(signal(data1_kfs, state="seasonal")$signal, main= "mle")
plot(signal(data1_kfs5, state="seasonal")$signal, main="loss")

plot(series(data1_x11, "d13"))
plot(residuals(data1_kfs,"recursive"), main="mle")
plot(residuals(data1_kfs5, "recursive"), main="loss")
par(mfrow=c(1,1))



par(mfrow=c(2,2))

plot(series(data1_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs5, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs5, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs5, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data1_kfs,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs5, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

par(mfrow=c(1,1))


# new loss function

loss5 <- function(x11, kfs){
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
    
  return(l)
}



data1_x11 <- seas(simlist[[1]], x11='')
data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
data1_fit <- fitSSM(data1_ssm, inits = c(1,1,1))
data1_kfs <- KFS(data1_fit$model)

loss5(data1_x11, data1_kfs) # mle 747

library(doParallel)

system.time({
  
  cl <- detectCores()
  cl <- makeCluster(cl)
  clusterExport(cl, c("loss5"))
  
  lossmat5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
    foreach(j = 1:200, .combine = "rbind") %dopar% {
      
      data1_ssm <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
      data1_kfs <- KFS(data1_ssm)
      
      l <- loss5(data1_x11, data1_kfs)
      
      c(i*0.1, j*0.1, l)
      
    }
  
  stopCluster(cl)
})


lossmat5[which.min(lossmat5[,3]), ] # [1]  7.6   2.3   513



data1_ssm6 <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(2.3)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=7.6)
data1_kfs6 <- KFS(data1_ssm6)




par(mfrow=c(4,3))
plot(series(data1_x11, "d11"))
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, main="mle")
plot(simlist[[1]] - signal(data1_kfs6, "seasonal")$signal, main="loss")

plot(series(data1_x11, "d12"))
plot(signal(data1_kfs, state="trend")$signal, main="mle")
plot(signal(data1_kfs6, state="trend")$signal, main="loss")

plot(series(data1_x11, "d10"))
plot(signal(data1_kfs, state="seasonal")$signal, main= "mle")
plot(signal(data1_kfs6, state="seasonal")$signal, main="loss")

plot(series(data1_x11, "d13"))
plot(residuals(data1_kfs,"recursive"), main="mle")
plot(residuals(data1_kfs6, "recursive"), main="loss")
par(mfrow=c(1,1))

library(scales)

par(mfrow=c(2,2))

plot(series(data1_x11, "d11"), ylim=c(-20,50), ylab='',lwd=1.5)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs, "seasonal")$signal, ylim=c(-20,50), ylab='', col=alpha(2,0.5), lwd=3)
par(new=TRUE)
plot(simlist[[1]] - signal(data1_kfs6, "seasonal")$signal, main="seasonal adjusted", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d12"), ylim=c(-20,50), ylab='', lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="trend")$signal, ylim=c(-20,50), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs6, state="trend")$signal, main="trend", ylim=c(-20,50), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d10"), ylim=c(-10,10), ylab='',lwd=1.5)
par(new=TRUE)
plot(signal(data1_kfs, state="seasonal")$signal, ylim=c(-10,10), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(signal(data1_kfs6, state="seasonal")$signal, main="seasonal", ylim=c(-10,10), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

plot(series(data1_x11, "d13"), ylim=c(-15,15), ylab='',lwd=1.5)
par(new=TRUE)
plot(residuals(data1_kfs,"recursive"), ylim=c(-15,15), ylab='', col=alpha(2,.5), lwd=3)
par(new=TRUE)
plot(residuals(data1_kfs6, "recursive"), main="irregular", ylim=c(-15,15), ylab='', col=alpha(5,.5), lwd=3)
legend("topleft", c('x11', 'mle', 'loss'), col=c(1,2,5), lty=1, lwd = c(1.5,3,3), cex=.5)

par(mfrow=c(1,1))