rm(list = ls())
setwd("C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis")
load(file='.RData')

load("simlist.RData")

library(seasonal)
library(KFAS)

unemp_ssm0 <- SSModel(unemp ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
unemp_fit0 <- fitSSM(unemp_ssm0, inits=c(1,1,1))
unemp_kfs0 <- KFS(unemp_fit0$model) 

unemp_fit0$model["Q"]
unemp_fit0$model['H']

unemp_x11 <- seas(unemp, x11='')



plot(series(unemp_x11, "d12"),ylim=c(6000, 15000), ylab='', lwd=2)
par(new=TRUE)
plot(signal(unemp_kfs0, "trend")$signal, ylim=c(6000, 15000), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(unemp_kfs0, "trend",filtered = TRUE)$signal, ylim=c(6000, 15000), ylab='',col=4, lwd=2)



library(doParallel)

system.time({
  
  cl <- detectCores()
  cl <- makeCluster(cl)
  registerDoParallel(cl)
  clusterExport(cl, c("loss5"))
  
  unemp_loss5  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
    foreach(j = 1:200, .combine = "rbind") %dopar% {
      
      unemp_ssm <- SSModel(unemp ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
      unemp_kfs <- KFS(unemp_ssm)
      
      l <- loss5(unemp_x11, unemp_kfs)
      
      c(i*0.1, j*0.1, l)
      
    }
  
  stopCluster(cl)
})

unemp_loss5[which.min(unemp_loss5[,3]), ] # 1.9 1.3

unemp_ssm5 <- SSModel(unemp ~ SSMtrend(1, Q=list(1.3)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=1.9)
unemp_kfs5 <- KFS(unemp_ssm5)

unemp_ssm6 <- SSModel(unemp ~ SSMtrend(1, Q=list(1.5)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=2)
unemp_kfs6 <- KFS(unemp_ssm6)


plot(series(unemp_x11, "d12"),ylim=c(5500, 15500), ylab='', lwd=2)
par(new=TRUE)
plot(signal(unemp_kfs6, "trend")$signal, ylim=c(5500, 15500), ylab='',col=4, lwd=2)
par(new=TRUE)
plot(signal(unemp_kfs0, "trend")$signal, ylim=c(5500, 15500), ylab='',col=2, lwd=2)



plot(series(unemp_x11, "d11"), ylim=c(5500, 15500), ylab='', lwd=2)
par(new=TRUE)
plot(unemp - signal(unemp_kfs6, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(5500, 15500))
par(new=TRUE)
plot(unemp - signal(unemp_kfs0, "seasonal")$signal, ylab='', lwd=2, col=4, ylim=c(5500, 15500))



###################################################################

gridsearch <- function(data) {
  
  x11 <- seas(data, x11='')
  
  loss6mat  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
    foreach(j = 1:200, .combine = "rbind") %dopar% {
      
      ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.1)
      kfs <- KFS(ssm)
      
      l <- loss6(x11, kfs)
      
      c(i*0.1, j*0.1, l)
      
    }
  
  return(loss6mat[which.min(loss6mat[,3]),c(1,2)])
}


system.time({
  
  cl <- detectCores()
  cl <- makeCluster(cl)
  registerDoParallel(cl)
  
  clusterExport(cl, c("loss6"))
  
  idemat1 <- t(sapply(simlist[1:100], gridsearch))
  
  stopCluster(cl)
  
}) 

plot(density(idemat1[,1]))
plot(density(idemat1[,2]))

save.image(file='idemat1.RData')

############################################################################
rm(list = ls())
setwd("C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis")
load(file='idemat1.RData')

library(seasonal)
library(KFAS)

mlemat_simlist <- t(sapply(simlist, mle_extract))

plot(density(mlemat_simlist[,1]), xlim=c(0,50), ylim=c(0,0.15), main='', xlab='')
par(new=TRUE)
plot(density(idemat1[,1]), xlim=c(0,50), ylim=c(0,0.15), main='', xlab='', col=2)


plot(density(mlemat_simlist[,2]), xlim=c(0,40), ylim=c(0,0.4), main='', xlab='')
par(new=TRUE)
plot(density(idemat1[,2]), xlim=c(0,40), ylim=c(0,0.4), main='', xlab='', col=2)

# to run these two lines need to import the .RData from BuildThePrior folder
plot(density(Idevalmat[,2]))
plot(density(Idevalmat[,3]))


library(doParallel)

system.time({
  
  cl <- detectCores()
  cl <- makeCluster(cl)
  registerDoParallel(cl)
  
  clusterExport(cl, c("loss6"))
  
  idemat2 <- t(sapply(simlist[101:600], gridsearch))
  
  stopCluster(cl)
  
}) 

save(idemat2, file='idemat2.RData')

load(file='idemat2.RData')

save.image(file='idemat1.RData')

############################################################################

library(doParallel)

cl <- makeCluster(16)
registerDoParallel(cl)
clusterEvalQ(cl, {
  library(KFAS)
  library(seasonal)
})
mlemat <- t(parSapply(cl, simlist, mle_extract))
stopCluster(cl)


plot(density(idemat2[,1]), ylim=c(0,0.15), xlim=c(0,40), lwd=2, xlab='', main='')
par(new=TRUE)
plot(density(mlemat[,1]), ylim=c(0,0.15), xlim=c(0,40), lwd=2, xlab='', main='', col=2)

plot(density(idemat2[,2]), xlim=c(0,20), ylim=c(0,0.35), lwd=2, xlab='', main='')
par(new=TRUE)
plot(density(mlemat[,2]), xlim=c(0,20), ylim=c(0,0.35), lwd=2, xlab='', main='', col=2)

hist(idemat2[,1], breaks = 100)
hist(mlemat[,1], xlim = c(0,50), breaks = 1000)

###############################################################

prior1 <- density(idemat2[,1], n=2024)
prior2 <- density(idemat2[,2], n=2024)


post_extract <- function(data) {
  
  x11 <- seas(data, x11='')
  
  postmat <- foreach(i = 1:200, .packages = c('seasonal', 'KFAS'), .combine='rbind') %:%
    foreach(j = 1:200, .combine='rbind') %dopar% {
      ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*.1)
      ll <- logLik(ssm)
      
      lp1 <- log(prior1$y[which.min(abs(prior1$x - i*.1))])
      
      lp2 <- log(prior2$y[which.min(abs(prior2$x - j*.1))])
      
      c(i*.1, j*.1, ll+lp1+lp2)
    }
  
  post <- postmat[which.max(postmat[,3]),]
  
  return(post)
}

?density

cl <- makeCluster(16)
registerDoParallel(cl)
clusterExport(cl,varlist=c('prior1', 'prior2'))
post_extract(simlist[[1]]) 
# 9.1000   4.9000 -535.3992
# mle is 3.68 3.55 
# ide from loss6 is 7.6 2.3 
stopCluster(cl)

data1_ssmpost <- SSModel(simlist[[1]] ~ SSMtrend(1, Q=list(4.9)) + SSMseasonal(12, 'dummy', Q=1), H=9.1)
data1_kfspost <- KFS(data1_ssmpost)


plot(series(data1_x11, "d12"), ylim=c(-20,55), ylab='', lwd=2)
par(new=TRUE)
plot(signal(data1_kfs, "trend")$signal, ylim=c(-20,55), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(data1_kfspost, "trend")$signal, ylim=c(-20,55), ylab='',col=4, lwd=2)
# effect is not obvious



cl <- makeCluster(16)
registerDoParallel(cl)
clusterExport(cl,varlist=c('prior1', 'prior2'))
postmat <- t(sapply(simlist[1:100], post_extract))
stopCluster(cl)

head(postmat)

plot(density(mlemat[,1]), ylim=c(0,0.30), xlim=c(0,40), lwd=2, xlab='', main='')
par(new=TRUE)
plot(density(idemat1[,1]),ylim=c(0,0.30), xlim=c(0,40), lwd=2, xlab='', main='', col=2)
par(new=TRUE)
plot(density(idemat2[,1]),ylim=c(0,0.30), xlim=c(0,40), lwd=2, xlab='', main='', col=3)
par(new=TRUE)
plot(density(postmat[,1]),ylim=c(0,0.30), xlim=c(0,40), lwd=2, xlab='', main='', col=4)


system.time({
  cl <- detectCores()
  cl <- makeCluster(cl)
  registerDoParallel(cl)
  clusterEvalQ(cl, {
    library(seasonal)
    library(doParallel)
    })
  clusterExport(cl, c("loss6"))
  
  idemat3 <- t(parSapply(cl, simlist[601:700], gridsearch))
  
  stopCluster(cl)
})

getwd()
save.image(file='idemat1.RData')
rm(list=ls())
load(file = 'idemat1.RData')


plot(density(mlemat[c(1:100),2]), ylim=c(0,0.60), xlim=c(0,20), lwd=2, xlab='', main='')
par(new=TRUE)
plot(density(idemat1[,2]),ylim=c(0,0.60), xlim=c(0,20), lwd=2, xlab='', main='', col=2)
par(new=TRUE)
plot(density(idemat2[,2]),ylim=c(0,0.60), xlim=c(0,20), lwd=2, xlab='', main='', col=3)
par(new=TRUE)
plot(density(postmat[,2]),ylim=c(0,0.60), xlim=c(0,20), lwd=2, xlab='', main='', col=4)


idemat <- rbind(idemat1, idemat2, idemat3)

plot(density(idemat[,1]))
plot(density(idemat[,2]))

plot(density(mlemat[,1]))


library(GGally)

simlist100_combo <- cbind(mlemat[c(1:100),], postmat[c(1:100), c(1,2)], idemat1[c(1:100),])
colnames(simlist100_combo) <- c('mle.1', 'mle.2', 'bayes.1', 'bayes.2', 'ideal.1', 'ideal.2')

ggparcoord(simlist100_combo,
           columns = c(1,3,5), 
           showPoints = TRUE, 
           scale="globalminmax", 
           title = "Parallel Coordinate Plot for the variance.1",
           alphaLines = 0.3
) + 
  coord_flip() + 
  theme(
    plot.title = element_text(size=20),
    axis.text = element_text(size=15)
  )


ggparcoord(simlist100_combo,
           columns = c(2,4,6), 
           showPoints = TRUE, 
           scale="globalminmax",
           title = "Parallel Coordinate Plot for the variance.2",
           alphaLines = 0.3
) + 
  coord_flip() + 
  theme(
    plot.title = element_text(size=20),
    axis.text = element_text(size=15)
  )

# congratulations! As a whole, the result is much better than MLEs.


##############################################################################

load(file='.RData')
head(postmat)
postmat1 <- postmat[,c(1,2)]
errormat1 <- idemat1 - postmat1
index_good <- which(rowSums(abs(errormat1)) <= 1) # 19 45 48 54 57 



library(KFAS)
library(seasonal)

# check these dataset

dataeg <- simlist[[19]]

dataeg_x11 <- seas(dataeg, x11='')


dataeg_ssm0 <- SSModel(dataeg ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataeg_fit0 <- fitSSM(dataeg_ssm0, inits = c(1,1,1))
dataeg_kfs0 <- KFS(dataeg_fit0$model)
dataeg_fit0$model['H'] # 8.873249/0.3926274 = 22.60
dataeg_fit0$model['Q'] # 3.69432/0.3926274 = 9.41

mlemat[19, ] # 22.599669  9.409226

plot(series(dataeg_x11, "d12"),ylim=c(-20,30), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend")$signal, ylim=c(-20,30), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend",filtered = TRUE)$signal, ylim=c(-20,30), ylab='',col=4, lwd=2)


plot(series(dataeg_x11, "d11"), ylim=c(-20,30), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs0, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-20,30))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs0, "seasonal", filtered = TRUE)$signal, ylab='', lwd=2, col=4, ylim=c(-20,30))


postmat1[19, ] # 8.3 3.3
idemat[19,] # 8.6 2.8

dataeg_ssmpost <- SSModel(dataeg ~ SSMtrend(1, Q=list(3.3)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=8.3)
dataeg_kfspost <- KFS(dataeg_ssmpost)


plot(series(dataeg_x11, "d12"),ylim=c(-20,30), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfspost, "trend")$signal, ylim=c(-20,30), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfspost, "trend",filtered = TRUE)$signal, ylim=c(-20,30), ylab='',col=4, lwd=2)



plot(series(dataeg_x11, "d11"), ylim=c(-20,30), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-20,30))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal", filtered = TRUE)$signal, ylab='', lwd=2, col=4, ylim=c(-20,30))


# compare x11, mle & bayes

plot(series(dataeg_x11, "d12"),ylim=c(-20,30), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend")$signal, ylim=c(-20,30), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfspost, "trend")$signal, ylim=c(-20,30), ylab='',col=4, lwd=2)



plot(series(dataeg_x11, "d11"), ylim=c(-20,30), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs0, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-20,30))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal")$signal, ylab='', lwd=2, col=4, ylim=c(-20,30))


# 45 

dataeg <- simlist[[45]]

dataeg_x11 <- seas(dataeg, x11='')
summary(dataeg_x11)

dataeg_ssm0 <- SSModel(dataeg ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
dataeg_fit0 <- fitSSM(dataeg_ssm0, inits = c(1,1,1))
dataeg_kfs0 <- KFS(dataeg_fit0$model)

mlemat[45, ] # 12.789068  6.044256

plot(series(dataeg_x11, "d12"),ylim=c(-12,12), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend")$signal, ylim=c(-12,12), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend",filtered = TRUE)$signal, ylim=c(-12,12), ylab='',col=4, lwd=2)


plot(series(dataeg_x11, "d11"), ylim=c(-15,15), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs0, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-15,15))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs0, "seasonal", filtered = TRUE)$signal, ylab='', lwd=2, col=4, ylim=c(-15,15))


postmat1[45, ] # 8.8 3.6
idemat[45,] # 8.9 2.7

dataeg_ssmpost <- SSModel(dataeg ~ SSMtrend(1, Q=list(2.7)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=8.9)
dataeg_kfspost <- KFS(dataeg_ssmpost)



plot(series(dataeg_x11, "d12"),ylim=c(-12,12), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfspost, "trend")$signal, ylim=c(-12,12), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfspost, "trend",filtered = TRUE)$signal, ylim=c(-12,12), ylab='',col=4, lwd=2)



plot(series(dataeg_x11, "d11"), ylim=c(-15,15), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-15,15))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal", filtered = TRUE)$signal, ylab='', lwd=2, col=4, ylim=c(-15,15))


# compare x11, mle & bayes

plot(series(dataeg_x11, "d12"),ylim=c(-12,12), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs0, "trend")$signal, ylim=c(-12,12), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfspost, "trend")$signal, ylim=c(-12,12), ylab='',col=4, lwd=2)



plot(series(dataeg_x11, "d11"), ylim=c(-15,15), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfs0, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-15,15))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal")$signal, ylab='', lwd=2, col=4, ylim=c(-15,15))

#  loss6 seems to be a little bad


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


lossmat5[which.min(lossmat5[,3]),] #  8.7000   2.3000 370.1912

dataeg_ssm5 <- SSModel(dataeg ~ SSMtrend(1, Q=list(2.3)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=8.7)
dataeg_kfs5 <- KFS(dataeg_ssm5)


plot(series(dataeg_x11, "d12"),ylim=c(-12,12), ylab='', lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "trend")$signal, ylim=c(-12,12), ylab='',col=2, lwd=2)
par(new=TRUE)
plot(signal(dataeg_kfs5, "trend",filtered = TRUE)$signal, ylim=c(-12,12), ylab='',col=4, lwd=2)



plot(series(dataeg_x11, "d11"), ylim=c(-15,15), ylab='', lwd=2)
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal")$signal, ylab='', lwd=2, col=2, ylim=c(-15,15))
par(new=TRUE)
plot(dataeg - signal(dataeg_kfspost, "seasonal", filtered = TRUE)$signal, ylab='', lwd=2, col=4, ylim=c(-15,15))



# uniform prior


naivepost_extract <- function(data) {
  
  x11 <- seas(data, x11='')
  
  postmat <- foreach(i = 1:200, .packages = c('seasonal', 'KFAS'), .combine='rbind') %:%
    foreach(j = 1:200, .combine='rbind') %dopar% {
      ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*.125)
      ll <- logLik(ssm)
      
      lp1 <- log((i*.125>0 & i*.125<20) * (1/20) + 10^(-7))
      
      lp2 <- log((j*.1>0 & j*.1<10) * (1/10) + 10^(-7))
      
      c(i*.1, j*.1, ll+lp1+lp2)
    }
  
  post <- postmat[which.max(postmat[,3]),]
  
  return(post)
}


library(doParallel)
library(KFAS)
library(seasonal)

cl <- makeCluster(16)
registerDoParallel(cl)
naivepostmat1 <- t(sapply(simlist[1:100], naivepost_extract))
stopCluster(cl)


simlist100_naivecombo <- cbind(mlemat[c(1:100),], naivepostmat1[c(1:100), c(1,2)], idemat1[c(1:100),])
colnames(simlist100_naivecombo) <- c('mle.1', 'mle.2', 'bayes.1', 'bayes.2', 'ideal.1', 'ideal.2')


library(GGally)
ggparcoord(simlist100_naivecombo,
           columns = c(1,3,5), 
           showPoints = TRUE, 
           scale="globalminmax", 
           title = "Parallel Coordinate Plot for the variance.1(naive)",
           alphaLines = 0.3
) + 
  coord_flip() + 
  theme(
    plot.title = element_text(size=20),
    axis.text = element_text(size=15)
  )


ggparcoord(simlist100_naivecombo,
           columns = c(2,4,6), 
           showPoints = TRUE, 
           scale="globalminmax",
           title = "Parallel Coordinate Plot for the variance.2(naive)",
           alphaLines = 0.3
) + 
  coord_flip() + 
  theme(
    plot.title = element_text(size=20),
    axis.text = element_text(size=15)
  )


##################################################


errormat1 <- idemat1 - postmat1
index_good1 <- which(rowSums(abs(errormat1)) <= 1) # 5: 19 45 48 54 57 
index_good2 <- which(rowSums(abs(errormat1)) <= 2) # 21: 14 19 22 24 28 31 36 41 45 48 51 54 56 57 62 64 73 79 87 88 92
index_good3 <- which(rowSums(abs(errormat1)) <= 3) # 35: 7  9 14 19 22 24 25 27 28 29 31 35 36 41 45 48 49 51 54 55 56 57 62 64 66 67 73 76 79 82 87 88 92 96 97


naiveerrormat1 <- idemat1 - naivepostmat1[, c(1,2)]
naiveindex_good1 <- which(rowSums(abs(naiveerrormat1)) <= 1) # 2: 14 22
naiveindex_good2 <- which(rowSums(abs(naiveerrormat1)) <= 2) # 9: 14 22 26 29 35 47 55 56 62
naiveindex_good3 <- which(rowSums(abs(naiveerrormat1)) <= 3) # 25: 3   7  14  18  22  26  29  30  31  35  38  47  54  55  56  59  61  62  72  73  79  81  83  89 100


mleerrormat1 <- idemat1 - mlemat[c(1:100), ]
mleindex_good1 <- which(rowSums(abs(mleerrormat1)) <= 1) # 2: 14 74
mleindex_good2 <- which(rowSums(abs(mleerrormat1)) <= 2) # 9: 14 22 38 53 61 65 74 79 81
mleindex_good3 <- which(rowSums(abs(mleerrormat1)) <= 3) # 17: 14 18 22 24 25 38 48 50 53 61 62 65 74 79 81 83 95



