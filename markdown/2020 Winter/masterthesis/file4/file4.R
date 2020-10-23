getwd()
rm(list = ls())
setwd('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file4')
load(file='C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/simlist.RData')

library(seasonal)
library(KFAS)
library(doParallel)


cl <- makeCluster(detectCores())
registerDoParallel(cl)


clusterExport(cl, c("loss5"))
idemat650 <- t(sapply(simlist[1:650], gridsearch))
stopCluster(cl)

save(idemat650, file='idemat650.RData')

load(file='idemat650.RData')
# statcandata <- read.csv("C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/20100008.csv")
# leave it for now

# scaling problem

ssm360 <- SSModel(matrix(NA,360,1) ~ SSMtrend(1, Q=list(5)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=10)
sim360 <- simulateSSM(ssm360, "obs", nsim = 1000)
simlist360 <- lapply(1:1000, function(x) sim360[,,x])
simlist360 <- lapply(1:1000, function(x) ts(simlist360[[x]], start = c(1990,01), frequency = 12))

simlist360_1hf <- lapply(1:1000, function(x) simlist360[[x]][1:180])
simlist360_2hf <- lapply(1:1000, function(x) simlist360[[x]][181:360])


simlist360_1hf <- lapply(1:1000, function(x) ts(simlist360[[x]][1:180], start = c(1990,01), frequency=12))
simlist360_2hf <- lapply(1:1000, function(x) ts(simlist360[[x]][181:360], start = c(2005,01), frequency=12))


save(simlist360, file='simlist360.RData')
save(simlist360_1hf, file='simlist360_1hf.RData')
save(simlist360_2hf, file='simlist360_2hf.RData')

save.image()

load('.RData')
load(file='idemat1000.RData')
idemat <- rbind(idemat650, idemat1000)


cl <- makeCluster(detectCores())
registerDoParallel(cl)
clusterExport(cl, c("loss5"))
idemat360_200 <- t(sapply(simlist[1:200], gridsearch))
idemat3601hf_200 <- t(sapply(simlist360_1hf[1:200], gridsearch))
stopCluster(cl)


cl <- makeCluster(detectCores())
registerDoParallel(cl)
clusterExport(cl, c("loss5"))
idemat360_500 <- t(sapply(simlist[201:400], gridsearch))
idemat3601hf_500 <- t(sapply(simlist360_1hf[201:400], gridsearch))
stopCluster(cl)

idemat360_400 <- idemat360_500
idemat3601hf_400 <- idemat3601hf_500

idemat360_400 <- rbind(idemat360_200, idemat360_400)
idemat3601hf_400 <- rbind(idemat3601hf_200, idemat3601hf_400)
  
rm(idemat360_200, idemat3601hf_200)
  
save(idemat, file = 'idemat.RData')
save(idemat360_400, file = 'idemat360_400.RData')
save(idemat3601hf_400, file='idemat3601hf_400.RData')
save.image()



plot(density(idemat360_200[,1]), ylim=c(0,0.2), xlab='irregular variance', 
     main='ideal dist of irregular variance', 
     sub='ts with length 360 vs ts with the first 180', lwd=2, xlim=c(0,28))
par(new=TRUE)
plot(density(idemat3601hf_200[,1]), ylim=c(0,0.2), xlab='', main='', lwd=2, col=2, xlim=c(0,28))
legend('topright', c('360', '180'), col=c(1,2), lwd=2, lty=1)

plot(density(idemat360_200[,2]), ylim=c(0,0.55), xlab='trend variance', 
     main='ideal dist of trend variance', 
     sub='ts with length 360 vs ts with the first 180', lwd=2, xlim=c(0,10))
par(new=TRUE)
plot(density(idemat3601hf_200[,2]), ylim=c(0,0.55), xlab='', main='', lwd=2, col=2, xlim=c(0,10))
legend('topright', c('360', '180'), col=c(1,2), lwd=2, lty=1)




plot(density(idemat360_200[,1]), ylim=c(0,0.2), xlab='irregular variance', 
     main='ideal dist of irregular variance', 
     sub='ts with length 360 vs ts with the first 180 vs simlist(1000 ts with length 180)', lwd=2, xlim=c(0,28))
par(new=TRUE)
plot(density(idemat3601hf_200[,1]), ylim=c(0,0.2), xlab='', main='', lwd=2, col=2, xlim=c(0,28))
par(new=TRUE)
plot(density(idemat650[,1]), ylim=c(0,.2), xlim=c(0,28), xlab='', main='', col=4, lwd=2)
legend('topright', c('360', 'the first half', 'simlist'), col=c(1,2,4), lwd=2, lty=1)

plot(density(idemat360_200[,2]), ylim=c(0,0.55), xlab='trend variance', 
     main='ideal dist of trend variance', 
     sub='ts with length 360 vs ts with the first 180 vs simlist(1000 ts with length 180)', lwd=2, xlim=c(0,10))
par(new=TRUE)
plot(density(idemat3601hf_200[,2]), ylim=c(0,0.55), xlab='', main='', lwd=2, col=2, xlim=c(0,10))
par(new=TRUE)
plot(density(idemat650[,2]), ylim=c(0,.55), xlim=c(0,10), xlab='', main='', col=4, lwd=2)
legend('topright', c('360', 'the first half', 'simlist'), col=c(1,2,4), lwd=2, lty=1)

#############################################################################################

# predict
# seas


library(seasonal)
library(KFAS)
plot(unemp)

unempshort_x11 <- seas(window(unemp, end=c(2010, 12)), x11='',  forecast.save = "forecasts")

plot(series(unempshort_x11, 'fct'))
series(unempshort_x11,'fct')
class(series(unempshort_x11,'fct'))
predict(unempshort_x11)
plot(predict(unempshort_x11)) # useless

plot(series(unempshort_x11, 'fct')[,1])


monthplot(unemp_x11, choice = "irregular")
monthplot(unemp_x11) # interesting

unempshort_x11prediction <- ts(series(unempshort_x11, 'fct')[,1], start = c(2011,1), frequency = 12)
unempshort_x11predictionLCI <- ts(series(unempshort_x11, 'fct')[,2], start = c(2011,1), frequency = 12)
unempshort_x11predictionUCI <- ts(series(unempshort_x11, 'fct')[,3], start = c(2011,1), frequency = 12)

plot(unempshort_x11prediction)

plot(window(unemp, end=c(2011,12)), lwd=2, xlim=c(1990,2012), ylim=c(5500, 18500), ylab='')
par(new=TRUE)
plot(unempshort_x11prediction, xlim=c(1990,2012), ylim=c(5500, 18500), col=2, lwd=2, ylab='')
par(new=TRUE)
plot(unempshort_x11predictionLCI, xlim=c(1990,2012), ylim=c(5500, 18500), col=4, lwd=2, ylab='')
par(new=TRUE)
plot(unempshort_x11predictionUCI, xlim=c(1990,2012), ylim=c(5500, 18500), col=4, lwd=2, ylab='')
title(main='Unemployment with forecast under X-11')


unempshort_ssm0 <- SSModel( window(unemp, end=c(2010, 12)) ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
unempshort_fit0 <- fitSSM(unempshort_ssm0, inits=c(1,1,1))
unempshort_kfs0 <- KFS(unempshort_fit0$model)


unempshort_ssm0prediction <- predict(unempshort_fit0$model,n.ahead=12,interval="prediction",level=0.95)
unempshort_ssm0predictionFIT <- ts(unempshort_ssm0prediction[,1], start = c(2011,1), frequency = 12)
unempshort_ssm0predictionLCI <- ts(unempshort_ssm0prediction[,2], start = c(2011,1), frequency = 12)
unempshort_ssm0predictionUCI <- ts(unempshort_ssm0prediction[,3], start = c(2011,1), frequency = 12)


plot(window(unemp, end=c(2011,12)), lwd=2, xlim=c(1990,2012), ylim=c(5500, 18500), ylab='')
par(new=TRUE)
plot(unempshort_ssm0predictionFIT, xlim=c(1990,2012), ylim=c(5500, 18500), col=2, lwd=2, ylab='')
par(new=TRUE)
plot(unempshort_ssm0predictionLCI, xlim=c(1990,2012), ylim=c(5500, 18500), col=4, lwd=2, ylab='')
par(new=TRUE)
plot(unempshort_ssm0predictionUCI, xlim=c(1990,2012), ylim=c(5500, 18500), col=4, lwd=2, ylab='')
title(main='Unemployment with forecast under SSM_MLE')


#####################################################################################
load(file='.RData')
unempshort <- window(unemp, end = c(2014,12))
plot(unempshort)


unempshort_x11 <- seas(unempshort, x11='',  forecast.save = "forecasts")

cl <- detectCores()
cl <- makeCluster(cl)
registerDoParallel(cl)
clusterExport(cl, c("loss5"))

unempshort_loss5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    unempshort_ssm <- SSModel(unempshort ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.125)
    unempshort_kfs <- KFS(unempshort_ssm)
    
    l <- loss5(unempshort_x11, unempshort_kfs)
    
    c(i*0.125, j*0.1, l)
    
  }

stopCluster(cl)

unempshort_loss5[which.min(unempshort_loss5[,3]), ] #  2.0       1.4 2074491.9


unempshort_ssm5 <- SSModel(unempshort ~ SSMtrend(1, Q=list(1.4)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=2)
unempshort_kfs5 <- KFS(unempshort_ssm5)


plot(unempshort_x11)
plot(series(unempshort_x11, 'fct'))

plot(unemp, xlim=c(1990,2016), ylim=c(5500, 16500))
par(new=TRUE)
plot(series(unempshort_x11, 'fct')[,1], col=2, xlim=c(1990, 2016), ylim=c(5500, 16500))
par(new=TRUE)
plot(predict(unempshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=4, xlim=c(1990, 2016), ylim=c(5500, 16500))

series(unempshort_x11, 'fct')[,1]
series(unempshort_x11, 'fct')[,2]
series(unempshort_x11, 'fct')[,3]
predict(unempshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1]
predict(unempshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,2]
predict(unempshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,3]



unempshort_ssm0 <- SSModel(unempshort ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
unempshort_fit0 <- fitSSM(unempshort_ssm0, inits=c(1,1,1))
unempshort_kfs0 <- KFS(unempshort_fit0$model)


plot(unemp, xlim=c(1990,2016), ylim=c(5500, 16500), ylab='')
par(new=TRUE)
plot(series(unempshort_x11, 'fct')[,1], col=2, xlim=c(1990, 2016), ylim=c(5500, 16500), type='o', ylab='')
par(new=TRUE)
plot(predict(unempshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=4, xlim=c(1990, 2016), ylim=c(5500, 16500),type='o', ylab='')
par(new=TRUE)
plot(predict(unempshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=5, xlim=c(1990, 2016), ylim=c(5500, 16500), type='o', ylab='')
legend('topleft', c('X-11', 'SSM_MLE', 'SSM_LOSS'), col=c(2,5,4), lty=1)


library(forecast)
?forecast
unemp_fc <- forecast(unempshort, h=12)


plot(unemp_fc,xlim=c(1990,2016), ylim=c(5500, 16500), ylab='',main = 'Comparison of predictions on unemployment')
par(new=TRUE)
plot(window(unemp,start=c(2015,01), end=c(2015,12)),xlim=c(1990,2016), ylim=c(5500, 16500), ylab='', lwd=2)
par(new=TRUE)
plot(series(unempshort_x11, 'fct')[,1], col=2, xlim=c(1990, 2016), ylim=c(5500, 16500), lwd=2, ylab='')
par(new=TRUE)
plot(predict(unempshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=5, xlim=c(1990, 2016), ylim=c(5500, 16500),lwd=2, ylab='')
par(new=TRUE)
plot(predict(unempshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=3, xlim=c(1990, 2016), ylim=c(5500, 16500), lwd=2, ylab='')
legend('topleft', c('True','X-11', 'SSM_MLE', 'SSM_LOSS', 'forecast'), col=c(1,2,3,5,4), lty=1,lwd=2)

# almost perfect, but unfortunately this is from the Package 'forecast'


#############################################################
dataeg <- simlist[[1]]
dataegshort <- window(dataeg, end=c(2013,12))
plot(dataeg)
dataegshort_fc <- forecast(dataegshort, h=12)

plot(dataegshort_fc, xlim=c(2000, 2015),ylim=c(-20,70), ylab='')
par(new=TRUE)
plot(window(dataeg, start=c(2014,01), end=c(2014,12)), xlim=c(2000,2015), ylim = c(-20,70), col=1, lwd=2)
# good enough

dataegshort_x11 <- seas(dataegshort, x11='', forecast.save='forecasts')

plot(dataegshort_fc, xlim=c(2000, 2015),ylim=c(-20,70), ylab='')
par(new=TRUE)
plot(window(dataeg, start=c(2014,01), end=c(2014,12)), xlim=c(2000,2015), ylim = c(-20,70), col=1, lwd=2, ylab='')
par(new=TRUE)
plot(series(dataegshort_x11, 'fct')[,1], xlim=c(2000,2015), ylim = c(-20,70), col=2, lwd=2)


cl <- detectCores()
registerDoParallel(cl)
clusterExport(cl, c("loss5"))

lossmat5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
  foreach(j = 1:200, .combine = "rbind") %dopar% {
    
    dataegshort_ssm <- SSModel(dataegshort ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.125)
    dataegshort_kfs <- KFS(dataegshort_ssm)
    
    l <- loss5(dataegshort_x11, dataegshort_kfs)
    
    c(i*0.125, j*0.1, l)
    
  }

stopCluster(cl)

lossmat5[which.min(lossmat5[,3]), ] #  

dataegshort_ssm5 <- SSModel(dataegshort ~ SSMtrend(1, Q=list(1.8)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=6.25)
dataegshort_kfs5 <- KFS(dataegshort_ssm5)


plot(dataegshort_fc, xlim=c(2000, 2015),ylim=c(-20,70), ylab='')
par(new=TRUE)
plot(window(dataeg, start=c(2014,01), end=c(2014,12)), xlim=c(2000,2015), ylim = c(-20,70), col=1, lwd=2, ylab='')
par(new=TRUE)
plot(series(dataegshort_x11, 'fct')[,1], xlim=c(2000,2015), ylim = c(-20,70), col=2, lwd=2, ylab='')
par(new=TRUE)
plot(predict(dataegshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=3, lwd=2, xlim=c(2000, 2015), ylim=c(-20,70), ylab='')
legend('topleft', c('True', 'X-11', 'SSM_LOSS', 'Forecast'), col=c(1,2,3,4), lty=1, lwd=2)


dataegshort_ssm0 <- SSModel(dataegshort ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataegshort_fit0 <- fitSSM(dataegshort_ssm0, inits=c(1,1,1))
dataegshort_kfs0 <- KFS(dataegshort_fit0$model)



plot(dataegshort_fc, xlim=c(2000, 2015),ylim=c(-20,70), ylab='')
par(new=TRUE)
plot(window(dataeg, start=c(2014,01), end=c(2014,12)), xlim=c(2000,2015), ylim = c(-20,70), col=1, lwd=2, ylab='')
par(new=TRUE)
plot(series(dataegshort_x11, 'fct')[,1], xlim=c(2000,2015), ylim = c(-20,70), col=2, lwd=2, ylab='')
par(new=TRUE)
plot(predict(dataegshort_fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=3, lwd=2, xlim=c(2000, 2015), ylim=c(-20,70), ylab='')
par(new=TRUE)
plot(predict(dataegshort_ssm5, n.ahead = 12, interval = 'prediction', level = .95)[,1], col=5, lwd=2, xlim=c(2000, 2015), ylim=c(-20,70), ylab='')
legend('topleft', c('True', 'X-11', 'SSM_MLE','SSM_LOSS', 'Forecast'), col=c(1,2,3,5,4), lty=1, lwd=2)

save.image()


#########################################################################################


load('.RData')
load('idemat360_400.RData')
load('idemat3601hf_400.RData')


plot(density(idemat360_400[,1]), ylim=c(0,0.2), xlab='irregular variance', 
     main='ideal dist of irregular variance', 
     sub='ts with length 360 vs ts with the first 180', lwd=2, xlim=c(0,28))
par(new=TRUE)
plot(density(idemat3601hf_400[,1]), ylim=c(0,0.2), xlab='', main='', lwd=2, col=2, xlim=c(0,28))
par(new=TRUE)
plot(density(idemat[,1]), ylim=c(0,.2), xlim=c(0,28), xlab='', main='', col=4, lwd=2)
legend('topright', c('360', 'the first half', 'simlist'), col=c(1,2,4), lwd=2, lty=1)



plot(density(idemat360_400[,2]), ylim=c(0,0.55), xlab='trend variance', 
     main='ideal dist of trend variance', 
     sub='ts with length 360 vs ts with the first 180', lwd=2, xlim=c(0,10))
par(new=TRUE)
plot(density(idemat3601hf_400[,2]), ylim=c(0,0.55), xlab='', main='', lwd=2, col=2, xlim=c(0,10))
par(new=TRUE)
plot(density(idemat[,2]), ylim=c(0,.55), xlim=c(0,10), xlab='', main='', col=4, lwd=2)
legend('topright', c('360', 'the first half', 'simlist'), col=c(1,2,4), lwd=2, lty=1)

#########################################################################################

