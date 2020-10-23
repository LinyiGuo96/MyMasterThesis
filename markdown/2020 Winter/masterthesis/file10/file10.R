rm(list = ls())
setwd('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file10')
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
library(plotly)
library(fitdistrplus)
library(optimization)

load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file3/functions.RData')
load('C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file9/idemat_tol.RData')
data_nike <- read.csv('C:\\Users\\GuoLY\\Desktop\\markdown\\nike.csv',header = TRUE)
data_adi <- read.csv('C:\\Users\\GuoLY\\Desktop\\markdown\\adidas.csv',header = TRUE)
data_nike <- ts(rev(data_nike[3:58,2]), frequency=4, start=c(2005,1))
data_adi <- ts(rev(data_adi[2:57,2]), frequency=4, start=c(2005,1))

ssm <- SSModel(matrix(NA,360,1) ~ SSMtrend(1, Q=list(10)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=20)
sim <- simulateSSM(ssm, "obs", nsim = 1000)
simlist9 <- lapply(1:1000, function(x) sim[,,x])
simlist9 <- lapply(1:1000, function(x) ts(simlist9[[x]], start = c(2000,01), frequency = 12))
simlist9_1half <- lapply(simlist9, function(x)  window(x, end = c(2014,12)) )

cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  
  library(seasonal)
  library(KFAS)
  library(dfoptim)
})

idemat9 <- t(parSapply(cl, simlist9, f1))
idemat9_1half <- t(parSapply(cl, simlist9_1half, f1))


mlemat9 <- t(parSapply(cl, simlist9, function(data) {
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  return(c(var_I, var_T))
}))

mlemat9_1half <- t(parSapply(cl, simlist9_1half, function(data) {
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=1), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1))
  kfs <- KFS(fit$model)
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  return(c(var_I, var_T))
}))


emprior1 <- density(idemat_tol[,1], n=2024)
emprior2 <- density(idemat_tol[,2], n=2024)

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

clusterExport(cl, varlist = c('emprior1', 'emprior2'))
postmat9_emprior_adt <- t(parSapply(cl, simlist9, post_empirical_adt))
postmat9_1half_emprior_adt <- t(parSapply(cl, simlist9_1half, post_empirical_adt))

stopCluster(cl)

ggplot() + geom_density(data=as.data.frame(idemat9), aes(x=V1), colour='red') +
  geom_density(data=as.data.frame(idemat9_1half), aes(x=V1), colour='red', linetype='dashed') +
  geom_density(data=as.data.frame(mlemat9), aes(x=V1)) +
  geom_density(data=as.data.frame(mlemat9_1half), aes(x=V1), linetype='dashed') + 
  geom_density(data=as.data.frame(postmat9_emprior_adt), aes(x=V1), colour='blue') +
  geom_density(data=as.data.frame(postmat9_1half_emprior_adt), aes(x=V1), colour='blue', linetype='dashed') +
  geom_density(data=as.data.frame(idemat_tol), aes(x=V1), colour='green', size=1) +
  xlim(0,50)


ggplot() + geom_density(data=as.data.frame(idemat9), aes(x=V2), colour='red') +
  geom_density(data=as.data.frame(idemat9_1half), aes(x=V2), colour='red', linetype='dashed') +
  geom_density(data=as.data.frame(mlemat9), aes(x=V2)) +
  geom_density(data=as.data.frame(mlemat9_1half), aes(x=V2), linetype='dashed') + 
  geom_density(data=as.data.frame(postmat9_emprior_adt), aes(x=V2), colour='blue') +
  geom_density(data=as.data.frame(postmat9_1half_emprior_adt), aes(x=V2), colour='blue', linetype='dashed') +
  geom_density(data=as.data.frame(idemat_tol), aes(x=V2), colour='green', size=1) + 
  xlim(0,20) 


loglikelihood_eg <- t(sapply(1:5000, function(i){
  ssm1 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(simlist9_1half[[1]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll2 <- logLik(ssm2)
  
  l <- c(ll1=ll1, ll2=ll2)
  return(l)
}))

ggplot(data=as.data.frame(loglikelihood_eg)) + geom_line(aes(x=(1:length(ll1))*0.1, y=ll1)) +
  geom_line(aes(x=(1:length(ll2))*0.1, y=ll2), colour='red') +
  geom_line(aes(x=(1:length(ll1))*0.1, y=0.5*ll1), colour='blue') +
  labs(y='loglikelihood', x='Irregular variance', title = 'Likelihood comparison of datasets with different lengths')


loglikelihood_eg2 <- t(sapply(1:1000, function(i){
  ssm1 <- SSModel(simlist9[[2]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(simlist9_1half[[2]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll2 <- logLik(ssm2)
  
  l <- c(ll1=ll1, ll2=ll2)
  return(l)
}))

ggplot(data=as.data.frame(loglikelihood_eg2)) + geom_line(aes(x=(1:length(ll1))*0.1, y=ll1)) +
  geom_line(aes(x=(1:length(ll2))*0.1, y=ll2), colour='red') +
  geom_line(aes(x=(1:length(ll1))*0.1, y=0.5*ll1), colour='blue')


loglikelihood_eg3 <- t(sapply(1:1000, function(i){
  ssm1 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(20))+SSMseasonal(12,sea.type = 'dummy',Q=2), H=i*0.2)
  ll2 <- logLik(ssm2)
  
  l <- c(ll1=ll1, ll2=ll2)
  return(l)
}))

ggplot(data=as.data.frame(loglikelihood_eg3)) + geom_line(aes(x=1:length(ll1), y=ll1)) +
  geom_line(aes(x=1:length(ll2), y=ll2), colour='red') 


loglikelihood_eg4 <- t(sapply(1:5000, function(i){
  ssm1 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(i*0.1))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=20)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(simlist9_1half[[1]]~SSMtrend(1,Q=list(i*0.1))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=20)
  ll2 <- logLik(ssm2)
  
  l <- c(ll1=ll1, ll2=ll2)
  return(l)
}))

ggplot(data=as.data.frame(loglikelihood_eg4)) + geom_line(aes(x=(1:length(ll1))*0.1, y=ll1)) +
  geom_line(aes(x=(1:length(ll2))*0.1, y=ll2), colour='red') +
  geom_line(aes(x=(1:length(ll1))*0.1, y=0.5*ll1), colour='blue') +
  labs(y='loglikelihood', x='Trend variance', title = 'Likelihood comparison of datasets with different lengths(2)')



unemp_ssm0 <- SSModel(unemp ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=NA)
unemp_fit0 <- fitSSM(unemp_ssm0, inits=c(1,1))
unemp_kfs0 <- KFS(unemp_fit0$model) 

unemp_fit0$model["Q"]
unemp_fit0$model['H'] #19.26825 65486.93
post_empirical_adt(unemp) #[1] 641.7930 192.6836



loglikelihood_eg5 <- t(sapply(1:1000, function(i){
  ssm1 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(2*simlist9[[1]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll2 <- logLik(ssm2)
  
  l <- c(ll1=ll1, ll2=ll2)
  return(l)
}))

ggplot(data=as.data.frame(loglikelihood_eg5)) + geom_line(aes(x=(1:length(ll1))*0.1, y=ll1)) +
  geom_line(aes(x=(1:length(ll2))*0.1, y=ll2), colour='red') 



loglikelihood_eg6 <- t(sapply(1:5000, function(i){
  ssm1 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(10))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(5))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.1)
  ll2 <- logLik(ssm2)
  
  l <- c(ll1=ll1, ll2=ll2)
  return(l)
}))

ggplot(data=as.data.frame(loglikelihood_eg6)) + geom_line(aes(x=(1:length(ll1))*0.1, y=ll1)) +
  geom_line(aes(x=(1:length(ll2))*0.1, y=ll2), colour='red')



loglikelihood_eg7 <- t(sapply(1:5000, function(i){
  ssm1 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(i*0.1))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=20)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(i*0.1))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=10)
  ll2 <- logLik(ssm2)
  
  l <- c(ll1=ll1, ll2=ll2)
  return(l)
}))

ggplot(data=as.data.frame(loglikelihood_eg7)) + geom_line(aes(x=(1:length(ll1))*0.1, y=ll1)) +
  geom_line(aes(x=(1:length(ll2))*0.1, y=ll2), colour='red') 



cl <- makeCluster(cl)
registerDoParallel(cl)

loglikelihood_eg8 <- foreach(i=1:200, .packages=c('KFAS'), .combine = 'rbind') %:% 
  foreach(j=1:200, .combine = 'rbind') %dopar% {
    
    ssm1 <- SSModel(simlist9[[1]]~SSMtrend(1,Q=list(j*0.5))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.5)
    ll1 <- logLik(ssm1)
    
    ssm2 <- SSModel(simlist9_1half[[1]]~SSMtrend(1,Q=list(j*0.5))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=i*0.5)
    ll2 <- logLik(ssm2)
    
    c(x= i*0.5, y= j*0.5, ll1=ll1, ll2=ll2)
}
stopCluster(cl)

#save(loglikelihood_eg8, file='loglikelihood_eg8.RData')
load('loglikelihood_eg8.RData')
head(loglikelihood_eg8)
ll1_matrix <- matrix(loglikelihood_eg8[,4], nrow=200, byrow=TRUE)
dim(ll1_matrix)

# plot_ly(z= ll1_matrix, showscale = FALSE) %>% add_surface()

AirPassengers %>% seas(x11='') %>% autoplot() + labs(title='Decomposition of Dataset Airpassengers')
unemp %>% seas(x11='') %>% autoplot() + labs(title='Decomposition of Dataset Unemployment')


###########################################################################

# Section 6
cl <- makeCluster(detectCores())
clusterEvalQ(cl, {
  library(KFAS)
})
clusterExport(cl, 'simlist9')
LL <- t(parSapply(cl, 181:360, function(i){
  data <- ts(simlist9[[1]][1:i], start = c(2000,1), frequency = 12)
  
  ssm1 <- SSModel(data~SSMtrend(1, Q=list(10))+ SSMseasonal(12,sea.type = 'dummy',Q=1), H=20)
  ll1 <- logLik(ssm1)
  
  ssm2 <- SSModel(data~SSMtrend(1, Q=list(20))+ SSMseasonal(12,sea.type = 'dummy',Q=2), H=40)
  ll2 <- logLik(ssm2)
  
  ssm3 <- SSModel(data~SSMtrend(1, Q=list(30))+ SSMseasonal(12,sea.type = 'dummy',Q=3), H=60)
  ll3 <- logLik(ssm3)
  
  ssm4 <- SSModel(data~SSMtrend(1, Q=list(40))+ SSMseasonal(12,sea.type = 'dummy',Q=4), H=80)
  ll4 <- logLik(ssm4)
  
  ssm5 <- SSModel(data~SSMtrend(1, Q=list(50))+ SSMseasonal(12,sea.type = 'dummy',Q=5), H=100)
  ll5 <- logLik(ssm5)
  
  ll <- c(ll1=ll1, ll2=ll2, ll3=ll3, ll4=ll4, ll5=ll5)
  return(ll)
}))
stopCluster(cl)
LL<- as.data.frame(LL)
ggplot(data=LL) +geom_line(aes(x=181:360, y=ll1)) + 
  geom_line(aes(x=181:360, y=ll2), colour='red') + 
  geom_line(aes(x=181:360, y=ll3), colour='green') + 
  geom_line(aes(x=181:360, y=ll4), colour='blue') + 
  geom_line(aes(x=181:360, y=ll5), colour='gold') +
  labs(x='length', title = 'log-likelihood of the same time series with different length')

LLtidy <- cbind(loglikelihood=c(LL$ll1, LL$ll2, LL$ll3, LL$ll4, LL$ll5), variance=c(rep('20,10,1', 180),rep('40,20,2', 180),rep('60,30,3', 180),rep('80,40,4', 180),rep('100,50,5', 180)))
LLtidy <- as.data.frame(LLtidy)
class(LLtidy$loglikelihood)
LLtidy$loglikelihood <- as.numeric(as.character(LLtidy$loglikelihood))
head(LLtidy)

ggplot(LLtidy, aes(x=rep(181:360, 5),y=loglikelihood, group=variance, color=variance)) + geom_line() +
  labs(x='length', title='Loglikelihood of the same time series as length increases')


  decomposeAP <- decompose(AirPassengers,"multiplicative")
autoplot(decomposeAP) + labs(title='Decomposition of International Airline Passengers')

ggplot(data=LL) +geom_line(aes(x=181:360, y=ll1))+
  labs(x='length', title = 'log-likelihood of the same time series with different length')
head(LL)
LL[c(1, 180),]


nike_x11 <- seas(data_nike, x11='')
summary(nike_x11)
autoplot(nike_x11)

adi_x11 <- seas(data_adi, x11='')
summary(adi_x11)
autoplot(adi_x11)

nike_x11 %>% series('b1') %>% seas(x11='') %>% series('d11') -> nike_SA
adi_x11 %>% series('b1') %>% seas(x11='') %>% series('d11') -> adi_SA
nike_x11 %>% series('b1') -> data_nike
adi_x11 %>% series('b1') -> data_adi

data_nike %>% seas(x11='') %>% series('d12') -> nike_trend
data_adi %>% seas(x11='') %>% series('d12') -> adi_trend
autoplot(cbind(A = nike_trend, B=adi_trend)) +
  ylab('Sales') + labs(title='Trend component from X-11')

nike_SA_ssm <- SSModel(nike_SA~SSMtrend(1,Q=list(NA)), H=NA)
nike_SA_fit <- fitSSM(nike_SA_ssm, inits = c(0,0))
nike_SA_fit$model['H'] # 8.175195
nike_SA_fit$model['Q'] # 35028.6
nike_SA_kfs <- KFS(nike_SA_fit$model)

adi_SA_ssm <- SSModel(adi_SA~SSMtrend(1,Q=list(NA)), H=NA)
adi_SA_fit <- fitSSM(adi_SA_ssm, inits = c(0,0))
adi_SA_fit$model['H'] # 7.813234
adi_SA_fit$model['Q'] # 37434.3
adi_SA_kfs <- KFS(adi_SA_fit$model)

autoplot(cbind(A= signal(nike_SA_kfs, states='trend')$signal,B = signal(adi_SA_kfs, states='trend')$signal))+
  ylab('Sales') + labs(title='Trend component without partial pooling')



autoplot(cbind(A_SSM_MLE= signal(nike_SA_kfs, states='trend')$signal, A_X11=nike_trend,
               B_SSM_MLE = signal(adi_SA_kfs, states='trend')$signal, B_X11=adi_trend))+
  ylab('Sales') + labs(title='Trend component comparison')

# customize

f2 <- function(var){

  Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
  Ht <- matrix(c(var[1],0,0,var[1]), 2)
  Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
  Rt <- diag(3)
  Qt <- matrix(c(var[2],0,0,0,var[2],0,0,0,var[3]), 3,3)
  a1 <- matrix(c(0, 0, 0), 3, 1)
  P1 <- matrix(0, 3, 3)
  P1inf <- diag(3)
  
  combo_ssm <- SSModel(cbind(nike_SA, adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
  ll <- logLik(combo_ssm)
  return(-ll)
}
var0 <- c(0,0,0)
hjkb(var0, f2, lower = c(0,0,0))$par # 0.0     0.0 35835.8


Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
Ht <- matrix(c(NA,0,0,NA), 2)
Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
Rt <- matrix(c(1,0,1,0,0,1),3,2,byrow = TRUE)
Qt <- matrix(c(NA,0,0,NA), 2)
a1 <- matrix(c(0, 0, 0), 3, 1)
P1 <- matrix(0, 3, 3)
P1inf <- diag(3)

combo_ssm1 <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, 
                                                        P1 = P1, P1inf = P1inf, index = c(1,2), state_names = c('T1','T2','T3')), H=Ht)
combo_fit1 <- fitSSM(combo_ssm1, inits = c(0,0,0,0))
combo_fit1$model['H'] 
#         [,1]     [,2]
#[1,] 803198.2    0.000
#[2,]      0.0 6664.782
combo_fit1$model['Q']
#             [,1]     [,2]
#[1,] 3.328507e-06     0.00
#[2,] 0.000000e+00 37241.81
combo_kfs1 <- KFS(combo_fit1$model)
autoplot(signal(combo_kfs1, states='custom')$signal) + labs(y='Sales', title = 'Trend component from complete pooling')
autoplot(residuals(combo_kfs, 'response'))



Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
Ht <- matrix(c(NA,0,0,NA), 2)
Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
Rt <- diag(3)
Qt <- matrix(c(NA,0,0,0,NA,0,0,0,NA), 3,3)
a1 <- matrix(c(0, 0, 0), 3, 1)
P1 <- matrix(0, 3, 3)
P1inf <- diag(3)

combo_ssm2 <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
combo_fit2 <- fitSSM(combo_ssm2, inits = c(0,0,0,0,0))
combo_fit2$model['H'] 
#        [,1]         [,2]
#[1,] 7.288344 0.000000e+00
#[2,] 0.000000 1.842641e-08
combo_fit2$model['Q']
#        [,1]     [,2]     [,3]
#[1,] 7644.67      0.0     0.00
#[2,]    0.00 854570.8     0.00
#[3,]    0.00      0.0 28875.73
combo_kfs2 <- KFS(combo_fit2$model)
logLik(combo_fit2$model) #[1] -814.4309
autoplot(signal(combo_kfs2, states='custom')$signal)+ labs(y='Sales', title = 'Trend component from partial pooling')
autoplot(residuals(combo_kfs2))


combo_ssm2 <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
combo_fit2 <- fitSSM(combo_ssm2, inits = c(1,1,1,1,1))
combo_fit2$model['H'] 
#         [,1]     [,2]
# [1,] 715.1754      0.0
# [2,]   0.0000 880252.2
combo_fit2$model['Q']
#            [,1]     [,2]     [,3]
#[1,] 3.287676e-06   0.0000     0.00
#[2,] 0.000000e+00 114.2568     0.00
#[3,] 0.000000e+00   0.0000 35303.55

dif1 <- signal(nike_SA_kfs, states='trend')$signal - signal(adi_SA_kfs, states='trend')$signal
dif2 <- signal(combo_kfs1, states='custom')$signal[,1] - signal(combo_kfs1, states='custom')$signal[,2]
dif3 <- signal(combo_kfs2, states='custom')$signal[,1] - signal(combo_kfs2, states='custom')$signal[,2]


plot(dif1, ylim=c(-1500,3500), col=1, ylab='')
par(new=TRUE)
plot(dif2, ylim=c(-1500,3500), col=2, ylab='')
par(new=TRUE)
plot(dif3, ylim=c(-1500,3500), col=4, ylab='')
legend('topleft', c('no pooling', 'complete pooling', 'partial pooling'), lty=1, col=c(1,2,4))
title(xlab = 'Time', ylab='Difference', main='Difference comparison of three pooling methods')



lik_hm <- function(var){
  Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
  Ht <- matrix(c(var[1],0,0,var[2]), 2)
  Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
  Rt <- diag(3)
  Qt <- matrix(c(var[3],0,0,0,var[4],0,0,0,var[5]), 3,3)
  a1 <- matrix(c(0, 0, 0), 3, 1)
  P1 <- matrix(0, 3, 3)
  P1inf <- diag(3)
  
  ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
  ll <- logLik(ssm)
 
  return(-ll)
}

var0 <- c(0,0,0,0,0)
hjkb(var0, lik_hm, lower = c(0,0,0,0,0))$par
# [1]      0.0 458427.0      0.0 458428.9  35835.8
# optim_sa(post_hm, var0, maximization = FALSE, lower = c(0,0,0,0,0), upper=c(10^6,10^6,10^6,10^6,10^6))$par
#  unstable
# optim(var0, post_hm)


Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
Ht <- matrix(c(0,0,0,458427.0), 2)
Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
Rt <- diag(3)
Qt <- matrix(c(0,0,0,0,458428.9,0,0,0,35835.8), 3,3)
a1 <- matrix(c(0, 0, 0), 3, 1)
P1 <- matrix(0, 3, 3)
P1inf <- diag(3)
hm_ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
hm_kfs <- KFS(hm_ssm)
logLik(hm_ssm)  # -809.8698

autoplot(signal(hm_kfs, states='custom')$signal)+ labs(y='Sales', title = 'Trend component from partial pooling')
autoplot(residuals(hm_kfs))




post_hm <- function(var){
  Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
  Ht <- matrix(c(var[1],0,0,var[2]), 2)
  Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
  Rt <- diag(3)
  Qt <- matrix(c(var[3],0,0,0,var[4],0,0,0,var[5]), 3,3)
  a1 <- matrix(c(0, 0, 0), 3, 1)
  P1 <- matrix(0, 3, 3)
  P1inf <- diag(3)
  
  ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
  ll <- logLik(ssm)
  
  lp1 <- log((var[1]<14.5) * dnorm(var[1], mean=8.8, sd=2.9) + 
               (var[1]>=14.5) * dexp(var[1]-14.5+11.53, 0.2))
  lp2 <- log((var[2]<14.5) * dnorm(var[2], mean=8.8, sd=2.9) + 
               (var[2]>=14.5) * dexp(var[2]-14.5+11.53, 0.2))
  
  lp3 <- log(((var[3] + var[5])<4.2) * dnorm(var[3] + var[5], mean=2.46, sd=0.83) +
               ((var[3] + var[5])>=4.2) * dexp(var[3]+var[5]-4.2+2.93, 1))
  lp4 <- log(((var[4] + var[5]) <4.2) * dnorm(var[4]+var[5], mean=2.46, sd=0.83) +
               ((var[4] + var[5])>=4.2) * dexp(var[4]+var[5]-4.2+2.93, 1))
  
  lpost <- ll + lp1 + lp2 + lp3 + lp4
  return(-lpost)
}
var0 <- c(0,0,0,0,0)
hjkb(var0, post_hm, lower = c(0,0,0,0,0))$par
# 3715.0958 3720.5889    0.0000    0.0000  366.1274

Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
Ht <- matrix(c(3715.0958,0,0,3720.5889), 2)
Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
Rt <- diag(3)
Qt <- matrix(c(0,0,0,0,0,0,0,0,366.1274), 3,3)
a1 <- matrix(c(0, 0, 0), 3, 1)
P1 <- matrix(0, 3, 3)
P1inf <- diag(3)

hm_map_ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
hm_map_kfs <- KFS(hm_map_ssm)
logLik(hm_map_ssm) #[1] -4835.253
autoplot(signal(hm_map_kfs, states='custom')$signal)+ labs(y='Sales', title = 'Trend component from partial pooling')
autoplot(residuals(hm_kfs))



post_hm2 <- function(var){
  Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
  Ht <- matrix(c(var[1],0,0,var[2]), 2)
  Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
  Rt <- diag(3)
  Qt <- matrix(c(var[3],0,0,0,var[4],0,0,0,var[5]), 3,3)
  a1 <- matrix(c(0, 0, 0), 3, 1)
  P1 <- matrix(0, 3, 3)
  P1inf <- diag(3)
  
  ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
  ll <- logLik(ssm)
  
  lp1 <- log((var[1]<14.5) * dnorm(var[1], mean=8.8, sd=2.9) + 
               (var[1]>=14.5) * dexp(var[1]-14.5+11.53, 0.2))
  lp2 <- log((var[2]<14.5) * dnorm(var[2], mean=8.8, sd=2.9) + 
               (var[2]>=14.5) * dexp(var[2]-14.5+11.53, 0.2))
  
  lp3 <- log((var[5]<4.2) * dnorm(var[5], mean=2.46, sd=0.83) +
               (var[5]>=4.2) * dexp(var[5]-4.2+2.93, 1))
  lpost <- ll + lp1 + lp2 + lp3
  return(-lpost)
}
var0 <- c(0,0,0,0,0)
hjkb(var0, post_hm2, lower = c(0,0,0,0,0))$par
# [1] 8.799999e+00 8.799999e+00 3.448626e+06 1.202746e+06 2.470268e+00

Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
Ht <- matrix(c(8.799999,0,0,8.799999), 2)
Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
Rt <- diag(3)
Qt <- matrix(c(3448626,0,0,0,1202746,0,0,0,2.470268), 3,3)
a1 <- matrix(c(0, 0, 0), 3, 1)
P1 <- matrix(0, 3, 3)
P1inf <- diag(3)

hm_map_ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
hm_map_kfs <- KFS(hm_map_ssm)
logLik(hm_map_ssm) #[1] -942.5425
autoplot(signal(hm_map_kfs, states='custom')$signal)+ labs(y='Sales', title = 'Trend component from partial pooling with empirical priors')
autoplot(residuals(hm_kfs))


post_hm3 <- function(var){
  Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
  Ht <- matrix(c(var[1],0,0,var[2]), 2)
  Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
  Rt <- diag(3)
  Qt <- matrix(c(var[3],0,0,0,var[4],0,0,0,var[5]), 3,3)
  a1 <- matrix(c(0, 0, 0), 3, 1)
  P1 <- matrix(0, 3, 3)
  P1inf <- diag(3)
  
  ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
  ll <- logLik(ssm)
  
  lp1 <- log(2*dnorm(var[1], 0, sqrt(40)/3))
  lp2 <- log(2*dnorm(var[2], 0, sqrt(40)/3))
  
  lp3 <- log(2*dnorm(var[3]+var[5], 0, sqrt(15)/3))
  lp4 <- log(2*dnorm(var[4]+var[5], 0, sqrt(15)/3))
  lpost <- ll + lp1 + lp2 + lp3 + lp4
  return(-lpost)
}
var0 <- c(0,0,0,0,0)
hjkb(var0, post_hm3, lower = c(0,0,0,0,0))$par
# [1] 81.29636 81.29636 32.75783 32.75783 17.03329


post_hm4 <- function(var){
  Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
  Ht <- matrix(c(var[1],0,0,var[2]), 2)
  Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
  Rt <- diag(3)
  Qt <- matrix(c(var[3],0,0,0,var[4],0,0,0,var[5]), 3,3)
  a1 <- matrix(c(0, 0, 0), 3, 1)
  P1 <- matrix(0, 3, 3)
  P1inf <- diag(3)
  
  ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
  ll <- logLik(ssm)
  
  lp1 <- log((var[1]<14.5) * dnorm(var[1], mean=8.8, sd=2.9) + 
               (var[1]>=14.5) * dexp(var[1]-14.5+11.53, 0.2))
  lp2 <- log((var[2]<14.5) * dnorm(var[2], mean=8.8, sd=2.9) + 
               (var[2]>=14.5) * dexp(var[2]-14.5+11.53, 0.2))
  
  lp3 <- log(((var[3])<4.2) * dnorm(var[3], mean=2.46, sd=0.83) +
               ((var[3])>=4.2) * dexp(var[3]-4.2+2.93, 1))
  lp4 <- log(((var[4]) <4.2) * dnorm(var[4], mean=2.46, sd=0.83) +
               ((var[4])>=4.2) * dexp(var[4]-4.2+2.93, 1))
  lp5 <- log(((var[5]) <4.2) * dnorm(var[5], mean=2.46, sd=0.83) +
               ((var[5])>=4.2) * dexp(var[5]-4.2+2.93, 1))
  
  lpost <- ll + lp1 + lp2 + lp3 + lp4 +lp5
  return(-lpost)
}
var0 <- c(0,0,0,0,0)
hjkb(var0, post_hm4, lower = c(0,0,0,0,0))$par
# [1] 3720.588905 3720.588905    2.777206    2.776352  542.417755

Zt <- matrix(c(1,0,0,0,1,0), 2, 3, byrow = TRUE)
Ht <- matrix(c(3720.588905,0,0,3720.588905), 2)
Tt <- matrix(c(0,0,1,0,0,1,0,0,1), 3,3, byrow=TRUE)
Rt <- diag(3)
Qt <- matrix(c(2.777206,0,0,0,2.776352,0,0,0,542.417755), 3,3)
a1 <- matrix(c(0, 0, 0), 3, 1)
P1 <- matrix(0, 3, 3)
P1inf <- diag(3)

hm_map_ssm <- SSModel(cbind(A=nike_SA, B=adi_SA) ~ SSMcustom(Z = Zt, T = Tt, R = Rt, Q = Qt, a1 = a1, P1 = P1, P1inf = P1inf), H=Ht)
hm_map_kfs <- KFS(hm_map_ssm)
logLik(hm_map_ssm)
autoplot(signal(hm_map_kfs, states='custom')$signal)+ labs(y='Sales', title = 'Trend component from partial pooling')
autoplot(residuals(hm_kfs))
