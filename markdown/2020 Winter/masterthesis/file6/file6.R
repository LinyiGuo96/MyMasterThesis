rm(list = ls())
setwd('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6')
load('.RData')

load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/simlist.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file4/idemat.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file3/functions.RData')

library(seasonal)
library(KFAS)
library(doParallel)
library(dplyr)
library(ggplot2)
library(scales)
library(forecast)
library(extraDistr)


prior1 <- density(idemat[c(1:700),1], n=2024)
prior2 <- density(idemat[c(1:700),2], n=2024)


error_comparison <- function(i) { 
  data <- simlist[[i]]
  
  datashort <- window(data, end=c(2013,12))
  
  x11 <- seas(datashort, x11='', forecast.save='forecasts')
  
  ssm0 <- SSModel(datashort ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
  fit0 <- fitSSM(ssm0, inits=c(1,1,1))
  kfs0 <- KFS(fit0$model) 
  
  sigma_post <- post_extract(datashort)[c(1,2)]
  ssmpost <- SSModel(datashort ~ SSMtrend(1, Q=list(sigma_post[2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=sigma_post[1])
  kfspost <- KFS(ssmpost) 
  
  ssmloss <- SSModel(datashort ~ SSMtrend(1, Q=list(idemat[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat[i,1])
  kfsloss <- KFS(ssmloss) # could be little bias here
  
  ############## model construction done ##############
  
  prediction_true <- window(data, start=c(2014,01))
  prediction_x11 <- series(x11, 'fct')[,1]
  prediction_SSM_MLE <- predict(fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  prediction_SSM_MAP <- predict(ssmpost, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  prediction_SSM_LOSS <- predict(ssmloss, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  
  error_x11 <- sum((prediction_true-prediction_x11)^2) 
  error_MLE <- sum((prediction_true-prediction_SSM_MLE)^2)
  error_MAP <- sum((prediction_true-prediction_SSM_MAP)^2)
  error_LOSS <- sum((prediction_true-prediction_SSM_LOSS)^2)
  
  ############## loss computation done ##############
  
  error <- c(x11 = error_x11, MLE = error_MLE, MAP = error_MAP, LOSS = error_LOSS)
  return(error)
  }


cl <- makeCluster(detectCores())
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(doParallel)
})
clusterExport(cl,varlist=c('prior1', 'prior2', 'simlist','post_extract', 'idemat'))
errormat <- t(parSapply(cl, 701:1000, error_comparison))
stopCluster(cl)

summary(errormat)

which.max(errormat[,4])

errormat <- as.data.frame(errormat)

save.image()

ggplot(errormat) + 
  geom_histogram(aes(x=x11), binwidth = 100)


#################################################################################
unempx11 <- seas(unemp, x11='')
unempssm <- SSModel(unemp ~ SSMtrend(2, Q=list(0,NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
unempfit <- fitSSM(unempssm, inits = c(1,1,1))
unempkfs <- KFS(unempfit$model)

unempssm0 <- SSModel(unemp ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
unempfit0 <- fitSSM(unempssm0, inits = c(1,1,1))
unempkfs0 <- KFS(unempfit0$model)

plot(signal(unempkfs, 'trend')$signal)
plot(signal(unempkfs0, 'trend')$signal)
plot(series(unempx11, 'd12'))

plot(signal(unempkfs, 'trend', filtered = 'TRUE')$signal)
plot(signal(unempkfs0, 'trend', filtered = 'TRUE')$signal)

unempshort <- window(unemp, end = c(2014,12))

unempshortx11 <- seas(unempshort, x11='', forecast.save='forecasts')
  
unempshortssm <- SSModel(unempshort ~ SSMtrend(2, Q=list(0,NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
unempshortfit <- fitSSM(unempshortssm, inits = c(1,1,1))
unempshortkfs <- KFS(unempshortfit$model)

unempshort_ssm0 <- SSModel(unempshort ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
unempshort_fit0 <- fitSSM(unempshort_ssm0, inits=c(1,1,1))
unempshort_kfs0 <- KFS(unempshort_fit0$model)


plot(unemp, xlim=c(1990,2016), ylim=c(5500, 16500), ylab='')
par(new=TRUE)
plot(series(unempshortx11, 'fct')[,1], col=2, xlim=c(1990, 2016), ylim=c(5500, 16500), type='l', ylab='',lwd=2)
par(new=TRUE)
plot(predict(unempshortfit$model, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=3, xlim=c(1990, 2016), ylim=c(5500, 16500),type='l', ylab='',lwd=2)
par(new=TRUE)
plot(predict(unempshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=4, xlim=c(1990, 2016), ylim=c(5500, 16500), type='l', ylab='', lwd=2)
legend('topleft', c('X-11', 'SSM_LLT', 'SSM_LL'), col=c(2,3,4), lty=1)
# seems that local linear trend has a better fit with x11

i=1
dataeg <- simlist[[i]]

dataegshort <- window(dataeg, end=c(2013,12))

x11 <- seas(dataegshort, x11='', forecast.save='forecasts')

ssm0 <- SSModel(dataegshort ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
fit0 <- fitSSM(ssm0, inits=c(1,1,1))
kfs0 <- KFS(fit0$model) 

ssm1 <- SSModel(dataegshort ~ SSMtrend(2, Q=list(0, NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
fit1 <- fitSSM(ssm1, inits=c(1,1,1))
kfs1 <- KFS(fit1$model) 

sigma_post <- post_extract(dataegshort)[c(1,2)]
ssmpost <- SSModel(dataegshort ~ SSMtrend(1, Q=list(sigma_post[2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=sigma_post[1])
kfspost <- KFS(ssmpost) 

ssmloss <- SSModel(dataegshort ~ SSMtrend(1, Q=list(idemat[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat[i,1])
kfsloss <- KFS(ssmloss) # could be little bias here

############## model construction done ##############

prediction_true <- window(dataeg, start=c(2014,01))
prediction_x11 <- series(x11, 'fct')[,1]
prediction_SSM_MLE0 <- predict(fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]
prediction_SSM_MLE1 <- predict(fit1$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]
prediction_SSM_MAP <- predict(ssmpost, n.ahead = 12, interval = 'prediction', level = .95)[,1]
prediction_SSM_LOSS <- predict(ssmloss, n.ahead = 12, interval = 'prediction', level = .95)[,1]

error_x11 <- sum((prediction_true-prediction_x11)^2) 
error_MLE0 <- sum((prediction_true-prediction_SSM_MLE0)^2)
error_MLE1 <- sum((prediction_true-prediction_SSM_MLE1)^2)
error_MAP <- sum((prediction_true-prediction_SSM_MAP)^2)
error_LOSS <- sum((prediction_true-prediction_SSM_LOSS)^2)

c(x11 = error_x11, MLE0 = error_MLE0, MLE1 = error_MLE1, MAP = error_MAP, LOSS = error_LOSS)
# 1097.7982  208.5686 1010.8108  205.4609  224.2851 


i=121
dataeg <- simlist[[i]]

dataegshort <- window(dataeg, end=c(2013,12))

x11 <- seas(dataegshort, x11='', forecast.save='forecasts')

ssm0 <- SSModel(dataegshort ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
fit0 <- fitSSM(ssm0, inits=c(1,1,1))
kfs0 <- KFS(fit0$model) 

ssm1 <- SSModel(dataegshort ~ SSMtrend(2, Q=list(0, NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
fit1 <- fitSSM(ssm1, inits=c(1,1,1))
kfs1 <- KFS(fit1$model) 

sigma_post <- post_extract(dataegshort)[c(1,2)]
ssmpost <- SSModel(dataegshort ~ SSMtrend(1, Q=list(sigma_post[2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=sigma_post[1])
kfspost <- KFS(ssmpost) 

ssmloss <- SSModel(dataegshort ~ SSMtrend(1, Q=list(idemat[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat[i,1])
kfsloss <- KFS(ssmloss) # could be little bias here

############## model construction done ##############

prediction_true <- window(dataeg, start=c(2014,01))
prediction_x11 <- series(x11, 'fct')[,1]
prediction_SSM_MLE0 <- predict(fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]
prediction_SSM_MLE1 <- predict(fit1$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]
prediction_SSM_MAP <- predict(ssmpost, n.ahead = 12, interval = 'prediction', level = .95)[,1]
prediction_SSM_LOSS <- predict(ssmloss, n.ahead = 12, interval = 'prediction', level = .95)[,1]

error_x11 <- sum((prediction_true-prediction_x11)^2) 
error_MLE0 <- sum((prediction_true-prediction_SSM_MLE0)^2)
error_MLE1 <- sum((prediction_true-prediction_SSM_MLE1)^2)
error_MAP <- sum((prediction_true-prediction_SSM_MAP)^2)
error_LOSS <- sum((prediction_true-prediction_SSM_LOSS)^2)

c(x11 = error_x11, MLE0 = error_MLE0, MLE1 = error_MLE1, MAP = error_MAP, LOSS = error_LOSS)

getwd()
save.image()

#################################################################################################

ssm1 <- SSModel(dataeg ~ SSMtrend(1, Q=list(0,2)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=3)
kfs1 <- KFS(ssm1)

ssm2 <- SSModel(dataeg ~ SSMtrend(1, Q=list(0,4)) + SSMseasonal(12, sea.type = 'dummy', Q=2), H=6)
kfs2 <- KFS(ssm2)

sum(signal(kfs1, 'trend')$signal - signal(kfs2, 'trend')$signal)
sum(signal(kfs1, 'seasonal')$signal - signal(kfs2, 'seasonal')$signal)

# same ratio same decomp
rm(ssm1,ssm2)
rm(kfs1,kfs2)
#################################################################################################

# 2 #

simlist_14yrs <- lapply(simlist, function(data) {window(data, end=c(2013,12))})
simlist_test <- lapply(simlist, function(data) window(data, start=c(2014,01)) ) 

simlist_14yrs[[1]]
simlist_test[[1]]

save(simlist_14yrs, file='simlist_14yrs.RData')
save(simlist_test, file='simlist_test.RData')
# 2a #
error_MLE <- t(sapply(simlist_14yrs, function(data) {
  x11 <- seas(data, x11='')
  ssm0 <- SSModel(data ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
  fit0 <- fitSSM(ssm0, inits = c(1,1,1))
  kfs0 <- KFS(fit0$model)
  ssm1 <- SSModel(data ~ SSMtrend(2, Q=list(0,NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
  fit1 <- fitSSM(ssm1, inits = c(1,1,1))
  kfs1 <- KFS(fit1$model)

  L0 <- sum((series(x11, 'd12') - signal(kfs0, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs0, 'seasonal')$signal)^2)
  L1 <- sum((series(x11, 'd12') - signal(kfs1, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs1, 'seasonal')$signal)^2)
  L <- c(Loss0 = L0, Loss1 = L1)
  return(L)
  }))

error_MLE <- data.frame(error_MLE)

boxplot(error_MLE)
summary(error_MLE)

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, varlist = c('simlist_14yrs', 'idemat'))
error_ide <- parSapply(cl, 1:1000, function(i) {
  data <- simlist_14yrs[[i]]
  x11 <- seas(data, x11='')
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(idemat[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat[i,1])
  kfs <- KFS(ssm)
  
  L <- sum((series(x11, 'd12') - signal(kfs, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs, 'seasonal')$signal)^2)
  return(Loss_ide = L)
})
stopCluster(cl)

#error_ide <- data.frame(error_ide)
#error_decomp <- cbind(error_MLE, error_ide)
#summary(error_decomp)
#boxplot(error_decomp)

ggplot(error_decomp) + 
  geom_histogram(aes(x=Loss0, y=..density..), fill='black', alpha=.5,binwidth = 20)+
  geom_histogram(aes(x=Loss1, y=..density..), fill='red', alpha=.5,binwidth = 20)+
  geom_histogram(aes(x=error_ide, y=..density..), fill='blue', alpha=.5,binwidth = 20)

ggplot(error_decomp) + 
  geom_density(aes(x=Loss0), size=1) + 
  geom_density(aes(x=Loss1), size=1, colour='red') + 
  geom_density(aes(x=error_ide), size=1, colour='blue') 

error_decomp_map <- sapply(1:1000, function(i){
  data <- simlist_14yrs[[i]]
  x11 <- seas(data, x11='')
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=idemat[i,1])
  kfs <- KFS(ssm)
  
  L <- sum((series(x11, 'd12') - signal(kfs, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs, 'seasonal')$signal)^2)
  return(Loss_map = L)
})

#error_decomp <- cbind(error_decomp, error_decomp_map)
#rm(error_decomp_map)

error_decomp$error_ide <- error_ide
error_decomp$error_decomp_map <- error_decomp_map

ggplot(error_decomp) + 
  geom_density(aes(x=Loss0), colour='red') + 
  geom_density(aes(x=Loss1), colour='blue') +
  geom_density(aes(x=error_ide), colour='orange') + 
  geom_density(aes(x=error_decomp_map),colour='green') 
# if I want to use legend I have to adjust the data arrangement

plot(density(error_decomp[,1]), col=2, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,2]), col=3, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,3]), col=4, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,4]), col=5, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
title(main='Decomposition error comparison(X11 is standard)')
legend('topright', c('MLE_local level', 'MLE_local linear trend', 'LOSS', 'MAP'), col=c(2,3,4,5), lty=1, lwd=2)


# 2b #

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, varlist = c('simlist'))
error_pre_MLE1  <- parSapply(cl, 701:1000, function(i){
  
  dataeg <- simlist[[i]]
  
  dataegshort <- window(dataeg, end=c(2013,12))
  
  x11 <- seas(dataegshort, x11='', forecast.save='forecasts')
  
  ssm1 <- SSModel(dataegshort ~ SSMtrend(2, Q=list(0, NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
  fit1 <- fitSSM(ssm1, inits=c(1,1,1))
  kfs1 <- KFS(fit1$model) 
  ############## model construction done ##############
  
  prediction_true <- window(dataeg, start=c(2014,01))
  prediction_SSM_MLE1 <- predict(fit1$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  
  error_MLE1 <- sum((prediction_true-prediction_SSM_MLE1)^2)
  
} )
stopCluster(cl)

error_pre <- cbind(errormat, error_pre_MLE1)
summary(error_pre)
boxplot(error_pre)


plot(density(error_pre[,1]), col=alpha(1,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,2]), col=alpha(2,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,5]), col=alpha(3,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,4]), col=alpha(4,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,3]), col=alpha(5,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
title(main='Prediction error comparison(X11 is standard)')
legend('topright', c('X-11','MLE_local level', 'MLE_local linear trend', 'LOSS', 'MAP'), col=c(1,2,3,4,5), lty=1, lwd=2)



######################################################################


prior1 <- density(idemat[c(1:1000),1], n=2024)
prior2 <- density(idemat[c(1:1000),2], n=2024)
system.time({
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(seasonal)
    library(KFAS)
    library(doParallel)
  })
  clusterExport(cl, varlist = c('post_extract', 'prior1', 'prior2'))
  postmat <- t(parSapply(cl,simlist[1:1000], post_extract))
  stopCluster(cl)
})

save(postmat, file = 'postmat.RData')
save.image()

########################################################################



cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, varlist = c('simlist'))

error_decomp_1051  <- parSapply(cl, simlist_14yrs, function(data){
  x11 <- seas(data, x11='')
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(5)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=10)
  kfs <- KFS(ssm)
  
  L <- sum((series(x11, 'd12') - signal(kfs, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs, 'seasonal')$signal)^2)
  return('1051' = L)
})
stopCluster(cl)

error_decomp <- cbind(error_decomp, error_decomp_1051)

plot(density(error_decomp[,1]), col=2, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,2]), col=3, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,3]), col=4, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,4]), col=5, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,5]), col=6, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
title(main='Decomposition error comparison(X11 is standard)')
legend('topright', c('MLE_local level', 'MLE_local linear trend', 'LOSS', 'MAP','1051'), col=c(2,3,4,5,6), lty=1, lwd=2)


cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
})
clusterExport(cl, varlist = c('simlist'))
error_pre_1051  <- parSapply(cl, 701:1000, function(i){
  
  dataeg <- simlist[[i]]
  
  dataegshort <- window(dataeg, end=c(2013,12))
  
  ssm <- SSModel(dataegshort ~ SSMtrend(1, Q=list(5)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=10)
  kfs <- KFS(ssm) 
  
  ############## model construction done ##############
  
  prediction_true <- window(dataeg, start=c(2014,01))
  prediction_1051 <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  
  error_1051 <- sum((prediction_true-prediction_1051)^2)
  
} )
stopCluster(cl)

error_pre <- cbind(error_pre, '1051' = error_pre_1051)
#colnames(error_pre)[5] <- 'MLE1'
summary(error_pre)


plot(density(error_pre[,1]), col=alpha(1,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,2]), col=alpha(2,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,5]), col=alpha(3,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,4]), col=alpha(4,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,3]), col=alpha(5,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,6]), col=alpha(6,.5), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
title(main='Prediction error comparison(X11 is standard)')
legend('topright', c('X-11','MLE_local level', 'MLE_local linear trend', 'LOSS', 'MAP','1051'), col=c(1,2,3,4,5,6), lty=1, lwd=2)


save(error_decomp, file='error_decomp.RData')
save(error_pre, file='error_pre.RData')
#######################################################################################

plot(density(error_decomp$Loss0 - error_decomp$error_decomp_map),col=1, lwd=2, xlab='', main='',ylim=c(0,.01), xlim=c(-300,300))
par(new=TRUE)
plot(density(error_decomp$error_ide - error_decomp$error_decomp_1051),col=2, lwd=2, xlab='', main='',ylim=c(0,.01), xlim=c(-300,300))
#par(new=TRUE)
#plot(density(error_decomp$error_ide - error_decomp$error_decomp_map), col=3, lwd=2, xlab='', main='',ylim=c(0,.03), xlim=c(-300,300))
abline(v=0)
title(main='Dist of difference between various decomposition errors')
legend('topleft', c('MLE - MAP', '1051 - LOSS'), col=c(1,2), lwd=2)


plot(density(error_pre$MLE - error_pre$MAP), col=1, lwd=2, main='', xlab='', ylim=c(0,.02), xlim=c(-300,300))
par(new=TRUE)
plot(density(error_pre$`1051` - error_pre$LOSS), col=2, lwd=2, main='', xlab='', ylim=c(0,.02), xlim=c(-300,300))
abline(v=0)
title(main='Dist of difference between various prediction errors')
legend('topleft', c('MLE - MAP', '1051 - LOSS'), col=c(1,2), lwd=2)

#######################################################################################
#install.packages('extraDistr')
library(extraDistr)

plot(prior1)
plot(prior2)

x <- seq(0,100,0.1)
y <- dhcauchy(x, sigma = 10)
plot(x=x, y=y, type='l')

y <- dhcauchy(x, sigma = 5)
plot(x=x, y=y, type='l')

plot(density(idemat[,1]))
plot(density(idemat[,2]))
#######################################################################################
ssm1 <- SSModel(simlist[[1]]~ SSMtrend(1,Q=list(NA))+ SSMseasonal(12,sea.type = 'dummy',Q=1), H=NA)
fit1 <- fitSSM(ssm1, inits = c(1,1))
kfs1 <- KFS(fit1$model)

ssm2 <- SSModel(simlist[[1]]~ SSMtrend(1,Q=list(NA))+ SSMseasonal(12,sea.type = 'dummy',Q=NA), H=NA)
fit2 <- fitSSM(ssm2, inits = c(1,1,1))
kfs2 <- KFS(fit2$model)

fit1$model['Q'] # 6.358027       1
fit2$model['Q'] # 6.547234       1.841068

fit1$model['H'] # 8.613033
fit2$model['H'] # 6.776355

?logLik
ssm1 <- SSModel(simlist[[1]]~ SSMtrend(1,Q=list(2))+ SSMseasonal(12,sea.type = 'dummy',Q=1), H=3)
ssm2 <- SSModel(simlist[[1]]~ SSMtrend(1,Q=list(4))+ SSMseasonal(12,sea.type = 'dummy',Q=2), H=6)
ssm3 <- SSModel(simlist[[1]]~ SSMtrend(1,Q=list(6))+ SSMseasonal(12,sea.type = 'dummy',Q=3), H=9)
logLik(ssm1) #-578
logLik(ssm2) #-532
logLik(ssm3) #-531
#######################################################################################

