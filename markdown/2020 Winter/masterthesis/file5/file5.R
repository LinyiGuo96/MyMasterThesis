rm(list = ls())
setwd('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file5')
load('.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/simlist.RData')

library(seasonal)
library(KFAS)
library(doParallel)
library(dplyr)
library(ggplot2)
library(scales)
library(forecast)

statcan <- read.csv(file='C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/20100008.csv', header = TRUE)
head(statcan)

substatcan <- statcan %>% rename(Date= ï..REF_DATE, classification = North.American.Industry.Classification.System..NAICS.) %>% filter(Adjustments == 'Unadjusted', GEO== 'Canada') %>% arrange(classification) %>% select(1,4,12)

substatcan$Date <- as.Date(as.character(substatcan$Date))
ggplot(substatcan, aes(Date, VALUE)) + geom_line(aes(color=classification))

Class <- levels(substatcan$classification)

#####################################################################################

dataeg <- substatcan %>% filter(classification == Class[10]) %>% select(Date, VALUE)
plot(dataeg,type='l')

dataeg <- ts(dataeg$VALUE, start = c(1990,01), frequency = 12)
plot(dataeg)

dataeg_x11 <- seas(dataeg, x11='')
summary(dataeg_x11)

dataeg2 <- series(dataeg_x11, 'b1')

dataeg2_x11 <- seas(dataeg2, x11='')
summary(dataeg2_x11)

par(mfrow=c(2,2))
plot(dataeg2_x11)
plot(series(dataeg2_x11, 'd12'))
plot(series(dataeg2_x11, 'd10'))
plot(series(dataeg2_x11, 'd11'))
par(mfrow=c(1,1))


dataeg2_ssm0 <- SSModel(log(dataeg2) ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataeg2_fit0 <- fitSSM(dataeg2_ssm0, inits = c(1,1,1))
dataeg2_kfs0 <- KFS(dataeg2_fit0$model)
dataeg2_fit0$model['Q']
dataeg2_fit0$model['H']

plot(log(series(dataeg2_x11, 'd12')), ylim=c(13,14.3),ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg2_kfs0, 'trend')$signal, ylim=c(13,14.3),ylab='', col=2, lwd=2)

plot(log(series(dataeg2_x11, 'd10')), ylim=c(-0.25,0.65), ylab='', lwd=2, col=alpha('black', 0.5))
par(new=TRUE)
plot(signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(-0.25,0.65), ylab='', lwd=2, col=alpha('red', 0.5))


# loss + gs

loss5_version2 <- function(x11, kfs){
  
  seasonal_x11 <- log(series(x11, "d10"))
  trend_x11 <- log(series(x11, "d12"))
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}


cl <- detectCores()
cl <- makeCluster(cl)
registerDoParallel(cl)
clusterExport(cl, c("loss5_version2"))

lossmat5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    dataeg2_ssm <- SSModel(log(dataeg2) ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
    dataeg2_kfs <- KFS(dataeg2_ssm)
    
    l <- loss5_version2(dataeg2_x11, dataeg2_kfs)
    
    c(i*0.1, j*0.1, l)
    
  }

stopCluster(cl)

lossmat5[which.min(lossmat5[,3]),] #  3.7000000 0.9000000  0.3856153


dataeg2_ssm5 <- SSModel(log(dataeg2) ~ SSMtrend(1, Q=list(0.9)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=3.7)
dataeg2_kfs5 <- KFS(dataeg2_ssm5)

plot(log(dataeg2))

plot(log(series(dataeg2_x11, 'd11')), ylim=c(12.9, 14.3), ylab='', lwd=2)
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(12.9, 14.3), ylab='', lwd=2, col=2)
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs5, 'seasonal')$signal, ylim=c(12.9, 14.3), ylab='', lwd=2, col=4)
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)
title(main='Comparison of Seasonally Adjusted Series')

plot(log(series(dataeg2_x11, 'd12')), ylim=c(13,14.3),ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg2_kfs0, 'trend')$signal, ylim=c(13,14.3),ylab='', col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg2_kfs5, 'trend')$signal, ylim=c(13,14.3),ylab='', col=4, lwd=2)
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)
title(main='Comparison of Trend Series')

plot(log(series(dataeg2_x11, 'd10')), ylim=c(-0.25,0.65), ylab='', lwd=2)
par(new=TRUE)
#plot(signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(-0.25,0.65), ylab='', lwd=2, col=2)
#par(new=TRUE)
plot(signal(dataeg2_kfs5, 'seasonal')$signal, ylim=c(-0.25,0.65), ylab='', lwd=2, col=4)
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)


plot(log(series(dataeg2_x11, 'd10')) - signal(dataeg2_kfs5, 'seasonal')$signal, ylab='')
abline(h=mean(log(series(dataeg2_x11, 'd10')) - signal(dataeg2_kfs5, 'seasonal')$signal), col=2)
title(main='Difference Between Seasonal Series')

plot(log(series(dataeg2_x11, 'd12')) - signal(dataeg2_kfs5, 'trend')$signal, ylab='')
abline(h=mean(log(series(dataeg2_x11, 'd12')) - signal(dataeg2_kfs5, 'trend')$signal), col=2)
title(main='Difference Between Trend Series')

plot(log(series(dataeg2_x11, 'd13')) - residuals(dataeg2_kfs5, 'response'), ylab='')
abline(h=mean(log(series(dataeg2_x11, 'd13')) - residuals(dataeg2_kfs5, 'response')), col=2)
title(main='Difference Between Irregular Series')



dataeg2_seats <- seas(dataeg2)
summary(dataeg2_seats)

plot(log(series(dataeg2_seats, 's11')), ylim=c(12.9, 14.3), ylab='', lwd=2)
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(12.9, 14.3), ylab='', lwd=2, col=2)
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs5, 'seasonal')$signal, ylim=c(12.9, 14.3), ylab='', lwd=2, col=4)
legend('topleft', c('SEATS', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)

######################################################################

dataegshort <- window(dataeg2, end=c(2017,12))
plot(dataegshort)

dataegshort_fc <- forecast(dataegshort, h=12)
dataegshort_x11 <- seas(dataegshort,x11='', forecast.save='forecasts')
summary(dataegshort_x11)

dataegshort_ssm0 <- SSModel(log(dataegshort) ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataegshort_fit0 <- fitSSM(dataegshort_ssm0, inits=c(1,1,1))
dataegshort_kfs0 <- KFS(dataegshort_fit0$model)


cl <- detectCores()
registerDoParallel(cl)
clusterExport(cl, c("loss5_version2"))

lossmat5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    dataegshort_ssm <- SSModel(log(dataegshort) ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.125)
    dataegshort_kfs <- KFS(dataegshort_ssm)
    
    l <- loss5_version2(dataegshort_x11, dataegshort_kfs)
    
    c(i*0.125, j*0.1, l)
    
  }

stopCluster(cl)


lossmat5[which.min(lossmat5[,3]), ] # 3.875000 0.900000 0.372612

dataegshort_ssm5 <- SSModel(log(dataegshort) ~ SSMtrend(1, Q=list(.9)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=3.875)
dataegshort_kfs5 <- KFS(dataegshort_ssm5)


plot(dataegshort_fc, xlim=c(1990, 2020),ylim=c(2*10^5, 2.7*10^6), ylab='', main='')
par(new=TRUE)
plot(window(dataeg2, start=c(1990,01), end=c(2018,12)), xlim=c(1990, 2020), ylim = c(2*10^5, 2.7*10^6), col=alpha('black',.5), lwd=2, ylab='')
par(new=TRUE)
plot(series(dataegshort_x11, 'fct')[,1], xlim=c(1990, 2020), ylim = c(2*10^5, 2.7*10^6), col=alpha('red', .5), lwd=2, ylab='')
par(new=TRUE)
plot(exp(predict(dataegshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]), col=alpha('green', .5), lwd=2, xlim=c(1990, 2020), ylim=c(2*10^5, 2.7*10^6), ylab='')
par(new=TRUE)
plot(exp(predict(dataegshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1]), col=alpha('orange', .5), lwd=2, xlim=c(1990, 2020), ylim=c(2*10^5, 2.7*10^6), ylab='')
legend('topleft', c('True', 'X-11', 'SSM_MLE','SSM_LOSS', 'Forecast'), col=c(1,2,3,'orange',4), lty=1, lwd=2)
title(main='Comparison of Prediction')

# compute the error
prediction_true <- window(dataeg2, start=c(1990,01), end=c(2018,12))
prediction_x11 <- series(dataegshort_x11, 'fct')[,1]
prediction_SSM_MLE <- exp(predict(dataegshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1])
prediction_SSM_LOSS <- exp(predict(dataegshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1])

sum((prediction_true-prediction_x11)^2) # 260730060688
sum((prediction_true-prediction_SSM_MLE)^2) # 351812656225 max
sum((prediction_true-prediction_SSM_LOSS)^2) # 2.37067e+11 min
######################################################################

# try another dataeg

dataeg <- substatcan %>% filter(classification == Class[21]) %>% select(Date, VALUE)
plot(dataeg,type='l')

dataeg <- ts(dataeg$VALUE, start = c(1990,01), frequency = 12)
plot(dataeg)

dataeg_x11 <- seas(dataeg, x11='')
summary(dataeg_x11)

dataeg2 <- series(dataeg_x11, 'b1')

dataeg2_x11 <- seas(dataeg2, x11='')
summary(dataeg2_x11)

par(mfrow=c(2,2))
plot(dataeg2_x11)
plot(series(dataeg2_x11, 'd12'),main='trend')
plot(series(dataeg2_x11, 'd10'),main='seasonal')
plot(series(dataeg2_x11, 'd11'),main='irregular')
par(mfrow=c(1,1))

dataeg2_ssm0 <- SSModel(log(dataeg2) ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataeg2_fit0 <- fitSSM(dataeg2_ssm0, inits = c(1,1,1))
dataeg2_kfs0 <- KFS(dataeg2_fit0$model)
dataeg2_fit0$model['Q']
dataeg2_fit0$model['H']



plot(log(series(dataeg2_x11, 'd12')), ylim=c(14.8,16.5), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg2_kfs0, 'trend')$signal, ylim=c(14.8,16.5),ylab='', col=2, lwd=2)

plot(log(series(dataeg2_x11, 'd10')), ylim=c(-0.3,0.22), ylab='', lwd=2, col=alpha('black', 0.5))
par(new=TRUE)
plot(signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(-0.3,.22), ylab='', lwd=2, col=alpha('red', 0.5))


# loss + gs

cl <- detectCores()
cl <- makeCluster(cl)
registerDoParallel(cl)
clusterExport(cl, c("loss5_version2"))

lossmat5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    dataeg2_ssm <- SSModel(log(dataeg2) ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
    dataeg2_kfs <- KFS(dataeg2_ssm)
    
    l <- loss5_version2(dataeg2_x11, dataeg2_kfs)
    
    c(i*0.1, j*0.1, l)
    
  }

stopCluster(cl)

lossmat5[which.min(lossmat5[,3]),] #  [1] 4.6000000 1.0000000 0.1280233


dataeg2_ssm5 <- SSModel(log(dataeg2) ~ SSMtrend(1, Q=list(1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=4.6)
dataeg2_kfs5 <- KFS(dataeg2_ssm5)

plot(log(dataeg2))

plot(log(series(dataeg2_x11, 'd11')), ylim=c(14.8,16.5), ylab='', lwd=2, col=alpha('black', .5))
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(14.8,16.5), ylab='', lwd=2, col=alpha('red', .5))
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs5, 'seasonal')$signal, ylim=c(14.8,16.5), ylab='', lwd=2, col=alpha('blue',.5))
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)
title(main='Comparison of Seasonally Adjusted Series')


plot(log(series(dataeg2_x11, 'd12')), ylim=c(14.8,16.5),ylab='', lwd=2,col=alpha('black', .5))
par(new=TRUE)
plot(signal(dataeg2_kfs0, 'trend')$signal, ylim=c(14.8,16.5),ylab='', col=alpha('red', .5), lwd=2)
par(new=TRUE)
plot(signal(dataeg2_kfs5, 'trend')$signal, ylim=c(14.8,16.5),ylab='', col=alpha('blue', .5), lwd=2)
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)
title(main='Comparison of Trend Series')

plot(log(series(dataeg2_x11, 'd10')), ylim=c(-0.3,.22), ylab='', lwd=2)
par(new=TRUE)
#plot(signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(-0.25,0.65), ylab='', lwd=2, col=2)
#par(new=TRUE)
plot(signal(dataeg2_kfs5, 'seasonal')$signal, ylim=c(-0.3,.22), ylab='', lwd=2, col=4)
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)


plot(log(series(dataeg2_x11, 'd10')) - signal(dataeg2_kfs5, 'seasonal')$signal, ylab='')
abline(h=mean(log(series(dataeg2_x11, 'd10')) - signal(dataeg2_kfs5, 'seasonal')$signal), col=2)
title(main='Difference Between Seasonal Series')

plot(log(series(dataeg2_x11, 'd12')) - signal(dataeg2_kfs5, 'trend')$signal, ylab='')
abline(h=mean(log(series(dataeg2_x11, 'd12')) - signal(dataeg2_kfs5, 'trend')$signal), col=2)
title(main='Difference Between Trend Series')

plot(log(series(dataeg2_x11, 'd13')) - residuals(dataeg2_kfs5, 'response'), ylab='')
abline(h=mean(log(series(dataeg2_x11, 'd13')) - residuals(dataeg2_kfs5, 'response')), col=2)
title(main='Difference Between Irregular Series')


dataeg2_seats <- seas(dataeg2)
summary(dataeg2_seats)

plot(log(series(dataeg2_seats, 's11')), ylim=c(14.8,16.5), ylab='', lwd=2)
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs0, 'seasonal')$signal, ylim=c(14.8,16.5), ylab='', lwd=2, col=2)
par(new=TRUE)
plot(log(dataeg2) - signal(dataeg2_kfs5, 'seasonal')$signal, ylim=c(14.8,16.5), ylab='', lwd=2, col=4)
legend('topleft', c('SEATS', 'SSM_MLE', 'SSM_LOSS'), col=c(1,2,4), lty=1, lwd=2)

getwd()
save.image()

############################################################################################

dataegshort <- window(dataeg2, end=c(2017,12))
plot(dataegshort)

dataegshort_fc <- forecast(dataegshort, h=12)
dataegshort_x11 <- seas(dataegshort,x11='', forecast.save='forecasts')
summary(dataegshort_x11)

dataegshort_ssm0 <- SSModel(log(dataegshort) ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataegshort_fit0 <- fitSSM(dataegshort_ssm0, inits=c(1,1,1))
dataegshort_kfs0 <- KFS(dataegshort_fit0$model)


cl <- detectCores()
registerDoParallel(cl)
clusterExport(cl, c("loss5_version2"))

lossmat5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    dataegshort_ssm <- SSModel(log(dataegshort) ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.125)
    dataegshort_kfs <- KFS(dataegshort_ssm)
    
    l <- loss5_version2(dataegshort_x11, dataegshort_kfs)
    
    c(i*0.125, j*0.1, l)
    
  }

stopCluster(cl)


lossmat5[which.min(lossmat5[,3]), ] #  4.250000 0.900000 0.123851

dataegshort_ssm5 <- SSModel(log(dataegshort) ~ SSMtrend(1, Q=list(.9)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=4.25)
dataegshort_kfs5 <- KFS(dataegshort_ssm5)


plot(dataegshort_fc, xlim=c(1990, 2020),ylim=c(10^6, 1.8*10^7), ylab='', main='')
par(new=TRUE)
plot(window(dataeg2, start=c(1990,01), end=c(2018,12)), xlim=c(1990, 2020), ylim = c(10^6, 1.8*10^7), col=alpha('black',.5), lwd=2, ylab='')
par(new=TRUE)
plot(series(dataegshort_x11, 'fct')[,1], xlim=c(1990, 2020), ylim = c(10^6, 1.8*10^7), col=alpha('red', .5), lwd=2, ylab='')
par(new=TRUE)
plot(exp(predict(dataegshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]), col=alpha('green', .5), lwd=2, xlim=c(1990, 2020), ylim=c(10^6, 1.8*10^7), ylab='')
par(new=TRUE)
plot(exp(predict(dataegshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1]), col=alpha('orange', .5), lwd=2, xlim=c(1990, 2020), ylim=c(10^6, 1.8*10^7), ylab='')
legend('topleft', c('True', 'X-11', 'SSM_MLE','SSM_LOSS', 'Forecast'), col=c(1,2,3,'orange',4), lty=1, lwd=2)
title(main='Comparison of Prediction')



prediction_true <- window(dataeg2, start=c(1990,01), end=c(2018,12))
prediction_x11 <- series(dataegshort_x11, 'fct')[,1]
prediction_SSM_MLE <- exp(predict(dataegshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1])
prediction_SSM_LOSS <- exp(predict(dataegshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1])

sum((prediction_true-prediction_x11)^2) # 2.748478e+12
sum((prediction_true-prediction_SSM_MLE)^2) # 3.579011e+12 max
sum((prediction_true-prediction_SSM_LOSS)^2) # 1.300712e+12 min

getwd()
save.image()

############################################################################################

dataeg <- simlist[[1]]

dataeg_x11 <- seas(dataeg, x11='')
dataeg_ssm5 <- SSModel(dataeg ~ SSMtrend(1, Q=list(2.3)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=7.6)
dataeg_kfs5 <- KFS(dataeg_ssm5)


plot(series(dataeg_x11, 'd10') - signal(dataeg_kfs5, 'seasonal')$signal, ylab='')
abline(h=mean(series(dataeg_x11, 'd10') - signal(dataeg_kfs5, 'seasonal')$signal), col=2)
title(main='Difference Between Seasonal Series')

plot(series(dataeg_x11, 'd12') - signal(dataeg_kfs5, 'trend')$signal, ylab='')
abline(h=mean(series(dataeg_x11, 'd12') - signal(dataeg_kfs5, 'trend')$signal), col=2)
title(main='Difference Between Trend Series')

plot(series(dataeg_x11, 'd13') - residuals(dataeg_kfs5, 'response'), ylab='')
abline(h=mean(series(dataeg_x11, 'd13') - residuals(dataeg_kfs5, 'response')), col=2)
title(main='Difference Between Irregular Series')
getwd()
save.image()

##############################################################################################

hi <- function(x){(x[1]**2 + x[2] - 11)**2 + (x[1] + x[2]**2 -7)**2}
optim_nm(fun = hi, k = 2)
optim_sa(fun = hi, start = c(runif(2, min = -1, max = 1)),
         trace = FALSE,
         lower = c(-4, -4),
         upper = c(4, 4),
         control = list(dyn_rf = FALSE,
                        rf = 1.2,
                        t0 = 10,
                        nlimit = 100,
                        r = 0.6,
                        t_min = 0.1
         )
)

##### Rosenbrock function
# minimum at f(1,1) = 0
ro <- function(x){
  100*(x[2]-x[1]^2)^2+(1-x[1])^2
}
# Random start values. Example arguments for the relatively simple Rosenbrock function.
ro_sa <- optim_sa(fun = ro,
                  start = c(runif(2, min = -1, max = 1)),
                  lower = c(-5, -5),
                  upper = c(5, 5),
                  trace = TRUE,
                  control = list(t0 = 100,
                                 nlimit = 550,
                                 t_min = 0.1,
                                 dyn_rf = FALSE,
                                 rf = 1,
                                 r = 0.7
                  )
)
# Visual inspection.
plot(ro_sa)
plot(ro_sa, type = "contour")

##############################################################################################

LOSS <- function(sigma) {
  x11 <- seas(data, x11='')
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(sigma[2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=sigma[1])
  kfs <- KFS(ssm)
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  return(l)
}

sigma <- c(runif(2, min = 0, max = 1))
ssm <- SSModel(data ~ SSMtrend(1, Q=list(sigma[2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=sigma[1])
kfs <- KFS(ssm)

data <- simlist[[1]]

optim_sa(fun = LOSS,
         start = c(runif(2, min = 0, max = 1)),
         lower = c(0, 0),
         upper = c(1000, 1000)
         
         
)
# can't run

optim(par = c(1,1),
      fn = LOSS,
      method = 'Nelder-Mead',
      lower = 0.01,
      upper = 1000)

#install.packages('dfoptim')
library(dfoptim)
nmkb(par=c(1,1), LOSS, lower=0, upper=1000) # can't run
# Error in is.SSModel(model, na.check = TRUE, return.logical = FALSE) : 
# System matrices (excluding Z) contain NA or infinite values, covariance matrices contain values larger than 1e+07 

#####################################################################################################

