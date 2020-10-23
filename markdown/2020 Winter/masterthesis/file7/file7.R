rm(list = ls())
setwd('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file7')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file7/.RData')
library(seasonal)
library(KFAS)
library(doParallel)
library(dplyr)
library(ggplot2)
library(scales)
library(forecast)
library(extraDistr)
library(lubridate)

load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/simlist.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/simlist_14yrs.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/simlist_test.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file4/idemat.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/postmat.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file3/functions.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/error_decomp.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file6/error_pre.RData')
statcan <- read.csv(file='C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/20100008.csv', header = TRUE)

post_extract_halfcauchy <- function(data) {
  
  x11 <- seas(data, x11='')
  
  postmat <- foreach(i = 1:200, .packages = c('seasonal', 'KFAS', 'extraDistr'), .combine='rbind') %:%
    foreach(j = 1:200, .combine='rbind') %dopar% {
      ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*.125)
      ll <- logLik(ssm)
      
      lp1 <- log(dhcauchy(x=sqrt(i*0.125), sigma=sqrt(10)))
      
      lp2 <- log(dhcauchy(x=sqrt(j*0.1),sigma = sqrt(5)))
      
      c(i*.125, j*.1, ll+lp1+lp2)
    }
  
  post <- postmat[which.max(postmat[,3]),c(1,2)]
  
  return(post)
}


system.time({
  
  cl <- makeCluster(detectCores())
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(seasonal)
    library(KFAS)
    library(doParallel)
    library(extraDistr)
  })
  clusterExport(cl, varlist = c('post_extract_halfcauchy'))
  postmat_halfcauchy <- t(parSapply(cl,simlist_14yrs[1:1000], post_extract_halfcauchy))
  stopCluster(cl)
})

plot(density(postmat_halfcauchy[,1]))
plot(density(postmat_halfcauchy[,2]))

dhcauchy(x=100, sigma = 2)
log(dhcauchy(x=100, sigma = 2))


cl <- makeCluster(detectCores())
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(doParallel)
  library(extraDistr)
})
clusterExport(cl, varlist = c('post_extract_halfcauchy', 'simlist', 'simlist_14yrs','postmat_halfcauchy'))

error_decomp_map2 <- parSapply(cl, 1:1000, function(i) {
  data <- simlist_14yrs[[i]]
  
  x11 <- seas(data, x11='')
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(postmat_halfcauchy[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_halfcauchy[i,1])
  kfs <- KFS(ssm)
  
  L <- sum((series(x11, 'd12') - signal(kfs, 'trend')$signal)^2 + (series(x11, 'd10') - signal(kfs, 'seasonal')$signal)^2)
  return(MAP2 = L)
  })


error_pre_map2 <- parSapply(cl, 701:1000, function(i){
  
  dataeg <- simlist[[i]]
  
  dataegshort <- window(dataeg, end=c(2013,12))
  ssm <- SSModel(dataegshort ~ SSMtrend(1, Q=list(postmat_halfcauchy[i,2])) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=postmat_halfcauchy[i,1])
  prediction_true <- window(dataeg, start=c(2014,01))
  prediction_map2 <- predict(ssm, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  
  error_map2 <- sum((prediction_true-prediction_map2)^2)
} )
  
stopCluster(cl)



error_decomp <- cbind(error_decomp, map2 = error_decomp_map2)
error_pre <- cbind(error_pre, map2 = error_pre_map2)
colnames(error_decomp) <- c("MLE", "MLE1", "LOSS", "MAP","1051", "MAP_halfcauchy")

summary(error_decomp)
summary(error_pre)

save(error_decomp, file='error_decomp.RData')
save(error_pre, file='error_pre.RData')
################################################################################################
load(file='error_decomp.RData')
load(file='error_pre.RData')

error_decomp %>% select(c(1,3,4,6)) %>% summary
boxplot(error_decomp[,c(1,1,3)] - error_decomp[,c(3,6,6)], names =c('X11 - MAP1', 'X11 - MAP2', 'MAP1 - MAP2'))

error_pre %>% select(c(1,2,3,7)) %>% summary

# decomposition
plot(density(error_decomp[,1]), col=2, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,3]), col=3, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,4]), col=4, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,6]), col=5, ylim=c(0,0.005), xlim=c(0,1500), lwd=2, main='', xlab='')
title(main='Decomposition error comparison(X11 is standard)')
legend('topright', c('MLE', 'LOSS', 'MAP', 'MAP_halfCauchy'), col=c(2,3,4,5), lty=1, lwd=2)


plot(density(error_decomp[,1] - error_decomp[,3]), col=2, ylim=c(0,0.027), xlim=c(-400,500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,1] - error_decomp[,6]), col=3, ylim=c(0,0.027), xlim=c(-400,500), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_decomp[,3] - error_decomp[,6]), col=4, ylim=c(0,0.027), xlim=c(-400,500), lwd=2, main='', xlab='')
abline(v=0, lwd=2)
title(main='Distribution of differences among decomposition errors')
legend('topright', c('MLE-MAP', 'MLE-MAP_halfCauchy', 'MAP-MAP_halfCauchy'), col=c(2,3,4), lty=1, lwd=2)




# prediction
plot(density(error_pre[,1]), col=alpha('black',1), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,2]), col=alpha('red',1), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,3]), col=alpha('green',1), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
par(new=TRUE)
plot(density(error_pre[,7]), col=alpha('blue',1), ylim=c(0,0.0015), xlim=c(0,6000), lwd=2, main='', xlab='')
title(main='Prediction error comparison')
legend('topright', c('X-11','MLE', 'MAP', 'MAP_halfCauchy'), col=c(1,2,3,4), lty=1, lwd=2)


par(mfrow=c(2,2))
plot(density(error_pre[,1] - error_pre[,3]), col=alpha('red',.5), lwd=2, main='', xlab='')
abline(v=0, lwd=2)
title(main='X-11 - MAP')

plot(density(error_pre[,2] - error_pre[,3]), col=alpha('green',.5), lwd=2, main='', xlab='')
abline(v=0, lwd=2)
title(main='MLE - MAP')

plot(density(error_pre[,3] - error_pre[,7]), col=alpha('blue',.5), lwd=2, main='', xlab='')
abline(v=0, lwd=2)
title(main = 'MAP - MAP_halfCauchy')

plot(density(error_pre[,2] - error_pre[,7]), col=alpha('lightblue',.5), lwd=2, main='', xlab='')
abline(v=0, lwd=2)
title(main='MLE - MAP_halfCauchy')
par(mfrow=c(1,1))
################################################################################################
summary(statcan)
#substatcan <- statcan %>% rename(Date= ï..REF_DATE, classification = North.American.Industry.Classification.System..NAICS.) %>% filter(Adjustments == 'Unadjusted', GEO== 'Canada') %>% arrange(classification) %>% select(1,4,12)
substatcan <- statcan %>% rename(Date= 锘縍EF_DATE, classification = North.American.Industry.Classification.System..NAICS.) %>% filter(Adjustments == 'Unadjusted', GEO== 'Canada') %>% arrange(classification) %>% 
  select(1,4,12) %>% mutate(year = year(Date),month=month(Date),day=day(Date))

head(substatcan)
Class <- levels(substatcan$classification)

reallist <- lapply(1:30, function(i) {
  ts(data= substatcan%>%filter(classification == Class[i])%>%arrange(Date)%>%select(VALUE), 
     start = as.numeric(as.matrix(data.frame(substatcan%>%filter(classification == Class[i])%>%arrange(Date)%>%select(year,month))[1,])),
     frequency = 12)
})


reallist <- reallist[-c(5,9,23)] # 5,9,23 are 'bad' data

reallist_x11 <- lapply(reallist, function(x) seas(x, x11=''))
reallist_loginfo <- lapply(reallist_x11, transformfunction)
reallist_pre <- lapply(reallist_x11, function(x) series(x, 'b1'))
reallist_x11 <- lapply(reallist_pre, function(x) seas(x, x11=''))
reallist_loginfo <- lapply(reallist_x11, transformfunction)
# reallist_pre is what we shall use for analysis
# all models are multiplicative and without any outliears or calendar effects

# reallist_pre[[1]]
# time(reallist_pre[[1]])[12]
# window(reallist_pre[[1]], end=time(reallist_pre[[1]])[12])
# tail(reallist_pre[[1]], n=1)
# window(reallist_pre[[1]], end=tail(time(reallist_pre[[1]]), n=13)[1])

reallist_part1 <- lapply(reallist_pre, function(x){
  window(x, end=tail(time(x),n=13)[1])
})

#reallist_ssm_MLE <- lapply(log(reallist_pre), function(x) {
#  ssm <- SSModel(x~SSMtrend(1, Q=list(NA))+SSMseasonal(12,sea.type = 'dummy', Q=NA), H=NA)
#  fit <- fitSSM(ssm, inits=c(1,1,1))
#})

cl <- makeCluster(detectCores())
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(doParallel)
  library(extraDistr)
})

realpostmat_hc <- t(parSapply(cl, reallist_part1, post_extract_halfcauchy_version2))

clusterExport(cl, varlist = c('post_extract_halfcauchy_version2', 
                              'reallist_part1', 'reallist_pre', 'realpostmat_hc'))

realerror_decomp <- t(parSapply(cl, 1:27, function(i){
  x <- reallist_part1[[i]]
  
  ############ build model ############
  x11 <- seas(x, x11='')
  
  ssm0 <- SSModel(log(x)~SSMtrend(1,Q=list(NA))+SSMseasonal(12,sea.type='dummy',Q=NA), H=NA)
  fit0 <- fitSSM(ssm0, inits=c(1,1,1))
  kfs0 <- KFS(fit0$model)
  
  ssm_hc <- SSModel(log(x)~SSMtrend(1,Q=list(realpostmat_hc[i,2]))+SSMseasonal(12,sea.type='dummy',Q=1), H=realpostmat_hc[i,1])
  kfs_hc <- KFS(ssm_hc)
  
  ############ compute decomp error ############
  error_decomp_mle <- sum((log(series(x11, 'd12')) - signal(kfs0, 'trend')$signal)^2 + (log(series(x11, 'd10')) - signal(kfs0, 'seasonal')$signal)^2)
  error_decomp_map_hc <- sum((log(series(x11, 'd12')) - signal(kfs_hc, 'trend')$signal)^2 + (log(series(x11, 'd10')) - signal(kfs_hc, 'seasonal')$signal)^2)
  
  realerror_decomp = c(MLE=error_decomp_mle, MAP_hc=error_decomp_map_hc)
  return(realerror_decomp)
}))

realerror_pre <- t(parSapply(cl, 1:27, function(i){
  x <- reallist_part1[[i]]
  
  ############ build model ############
  x11 <- seas(x, x11='',forecast.save='forecasts')
  
  ssm0 <- SSModel(log(x)~SSMtrend(1,Q=list(NA))+SSMseasonal(12,sea.type='dummy',Q=NA), H=NA)
  fit0 <- fitSSM(ssm0, inits=c(1,1,1))

  ssm_hc <- SSModel(log(x)~SSMtrend(1,Q=list(realpostmat_hc[i,2]))+SSMseasonal(12,sea.type='dummy',Q=1), H=realpostmat_hc[i,1])

  ############ compute pre error ############
  
  prediction_true <- log(window(reallist_pre[[i]], start=tail(time(x),n=12)[1]))
  prediction_x11 <- log(series(x11, 'fct')[,1])
  prediction_SSM_MLE0 <- predict(fit0$model, n.ahead = 12, interval = 'prediction', level = .95)[,1]
  prediction_SSM_MAP_hc <- predict(ssm_hc, n.ahead = 12, interval = 'prediction', level = .95)[,1]

  error_x11 <- sum((prediction_true-prediction_x11)^2) 
  error_MLE <- sum((prediction_true-prediction_SSM_MLE0)^2)
  error_MAP_hc <- sum((prediction_true-prediction_SSM_MAP_hc)^2)

  realerror_pre <- c(X11= error_x11, MLE=error_MLE, MAP_hc=error_MAP_hc)
  return(realerror_pre)
}))

stopCluster(cl)


summary(realerror_decomp)
summary(realerror_pre)
class(realerror_decomp)
realerror_decomp <- as.data.frame(realerror_decomp)
realerror_pre <- as.data.frame(realerror_pre)


realerror_decomp_percentatge <- realerror_decomp/rowSums(realerror_decomp)
realerror_pre_percentatge <- realerror_pre/rowSums(realerror_pre)
summary(realerror_decomp_percentatge)
summary(realerror_pre_percentatge)

boxplot(realerror_decomp_percentatge)
boxplot(realerror_pre_percentatge)

save(list = c('reallist', 'reallist_pre', 'realerror_decomp', 'realerror_pre', 'realpostmat_hc', 
              'reallist_part1'), file = 'reallist.RData')
###############################################################################

summary(error_decomp)
summary(ifelse(error_decomp$MLE - error_decomp$MAP > 0, 1, 0))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   0.765   1.000   1.000 
summary(ifelse(error_decomp$MLE - error_decomp$MAP_halfcauchy > 0, 1, 0))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   1.000   1.000   0.791   1.000   1.000 
summary(ifelse(error_decomp$MAP - error_decomp$MAP_halfcauchy > 0, 1, 0))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.000   0.000   0.000   0.317   1.000   1.000

summary(error_pre)
summary(ifelse(error_pre$x11 - error_pre$MLE > 0, 1, 0))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    1.00    0.59    1.00    1.00 
summary(ifelse(error_pre$x11 - error_pre$MAP > 0, 1, 0))
# Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
# 0.0000  0.0000  1.0000  0.6033  1.0000  1.0000
summary(ifelse(error_pre$x11 - error_pre$map2 > 0, 1, 0))
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  1.0000  0.5967  1.0000  1.0000 
summary(ifelse(error_pre$MLE - error_pre$MAP > 0, 1, 0))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  1.0000  0.5167  1.0000  1.0000
summary(ifelse(error_pre$MLE - error_pre$map2 > 0, 1, 0))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    0.00    0.00    0.48    1.00    1.00 
summary(ifelse(error_pre$MAP - error_pre$map2 > 0, 1, 0))
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.0000  0.0000  0.0000  0.4867  1.0000  1.0000 

error_decomp_percentage <- error_decomp[,c(1,4,6)] / rowSums(error_decomp[,c(1,4,6)])
error_pre_percentage <- error_pre[,c(1,2,3,7)] / rowSums(error_pre[,c(1,2,3,7)])

summary(error_decomp_percentage)
summary(error_pre_percentage)

boxplot(error_decomp_percentage)
boxplot(error_pre_percentage)
#abline(h=mean(error_pre_percentage$x11))
#abline(h=mean(error_pre_percentage$MLE))

###################################################################################

# check normality
head(error_decomp)
ggplot(data = error_decomp, aes(sample=MLE)) + stat_qq() + stat_qq_line()
ggplot(data = error_decomp, aes(sample=MAP)) + stat_qq() + stat_qq_line()
ggplot(data = error_decomp, aes(sample=MAP_halfcauchy)) + stat_qq() + stat_qq_line()
head(error_pre)
ggplot(data = error_pre, aes(sample=x11)) + stat_qq() + stat_qq_line()
ggplot(data = error_pre, aes(sample=MLE)) + stat_qq() + stat_qq_line()
ggplot(data = error_pre, aes(sample=MAP)) + stat_qq() + stat_qq_line()
ggplot(data = error_pre, aes(sample=map2)) + stat_qq() + stat_qq_line()
# not normal so can't use t-test

# non-parametric test
# Mann–Whitney U test
wilcox.test(x=error_decomp$MLE, y=error_decomp$MAP)
wilcox.test(x=error_decomp$MLE, y=error_decomp$MAP_halfcauchy)
wilcox.test(x=error_decomp$MAP, y=error_decomp$MAP_halfcauchy)

wilcox.test(x=error_pre$x11, y=error_pre$MLE)
wilcox.test(x=error_pre$MLE, y=error_pre$MAP)
wilcox.test(x=error_pre$MAP, y=error_pre$map2)
wilcox.test(x=error_pre$x11, y=error_pre$MAP)
wilcox.test(x=error_pre$x11, y=error_pre$map2)


# real data #
ggplot(data = realerror_decomp, aes(sample=MLE)) + stat_qq() + stat_qq_line()
ggplot(data = realerror_decomp, aes(sample=MAP_hc)) + stat_qq() + stat_qq_line()
ggplot(data = realerror_pre, aes(sample=X11)) + stat_qq() + stat_qq_line()
ggplot(data = realerror_pre, aes(sample=MLE)) + stat_qq() + stat_qq_line()
ggplot(data = realerror_pre, aes(sample=MAP_hc)) + stat_qq() + stat_qq_line()
# not normal either

wilcox.test(x=realerror_decomp$MLE, y=realerror_decomp$MAP_hc)
wilcox.test(x=realerror_pre$X11, y=realerror_pre$MLE)
wilcox.test(x=realerror_pre$X11, y=realerror_pre$MAP_hc)
wilcox.test(x=realerror_pre$MLE, y=realerror_pre$MAP_hc)


# friedman test
?friedman.test
RoundingTimes <-
  matrix(c(5.40, 5.50, 5.55,
           5.85, 5.70, 5.75,
           5.20, 5.60, 5.50,
           5.55, 5.50, 5.40,
           5.90, 5.85, 5.70,
           5.45, 5.55, 5.60,
           5.40, 5.40, 5.35,
           5.45, 5.50, 5.35,
           5.25, 5.15, 5.00,
           5.85, 5.80, 5.70,
           5.25, 5.20, 5.10,
           5.65, 5.55, 5.45,
           5.60, 5.35, 5.45,
           5.05, 5.00, 4.95,
           5.50, 5.50, 5.40,
           5.45, 5.55, 5.50,
           5.55, 5.55, 5.35,
           5.45, 5.50, 5.55,
           5.50, 5.45, 5.25,
           5.65, 5.60, 5.40,
           5.70, 5.65, 5.55,
           6.30, 6.30, 6.25),
         nrow = 22,
         byrow = TRUE,
         dimnames = list(1 : 22,
                         c("Round Out", "Narrow Angle", "Wide Angle")))
friedman.test(RoundingTimes)
head(RoundingTimes)
rm(RoundingTimes)
head(error_decomp)
head(error_pre)
friedman.test(as.matrix(error_decomp[,c(1,4)])) # have to add `as.matrix`
friedman.test(as.matrix(error_pre[,c(1,2)]))
friedman.test(as.matrix(error_pre[,c(1,3)]))
friedman.test(as.matrix(error_pre[,c(2,3)]))
friedman.test(as.matrix(error_pre[,c(2,3,7)]))

###################################################################################

# dfoptim
library(dfoptim)

f1 <- function(var) {
  x11 <- seas(unemp, x11='')
  ssm <- SSModel(unemp ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
  kfs <- KFS(ssm)
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}

var0 <- c(0,0)
system.time({ ob1 <- hjk(var0, f1, control=list(tol=0.01))}) # approx 60s
ob1 # 1.84375 1.25000
# 1.9 1.3 is the result from grid search!!!!!!
