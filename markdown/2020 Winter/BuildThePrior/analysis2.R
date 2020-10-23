set.seed(9483)
rm(list=ls())


simulation1 <- function(length){
  
  model <- Arima(ts(rnorm(240,sd=runif(1, 1, 100)),start=c(1980,01),frequency =12), order=c(0,1,1),   
                 seasonal=c(0,1,1), fixed=c(theta=runif(1), Theta=runif(1)))
  
  data <- simulate(model, nsim=length)
  
  # because if we need to take log later, data must be positive
  if(min(data) <= 0) data <- data - min(data) + runif(1, min=0, max = 10^6)
  else data <- data + runif(1, min=0, max=10^6)
  
  return(data)
  
}

simlist1 <- function(n,length) {   
  
  Datalist <- list()
  
  for (i in 1:n)  Datalist[[i]] <- simulation1(length)
  
  return(Datalist)  
  
}


x11 <- function(x){
  
  seas(x, x11='') 
  
}

preprocess <- function(x11) {
  
  if(transformfunction(x11) == 'log') 
    data <- log(series(x11, 'b1'))
  else
    data <- series(x11, 'b1')
  
  return(data)
}


varianceratio <- function(x11) {
  
  noise  <- series(x11, "d13")
  trend  <- series(x11, "d12")
  season <- series(x11, "d10")
  
  var1 <- var(noise)
  var2 <- var(trend)
  var3 <- var(season)
  
  r1 <- var2/var1
  r2 <- var3/var1
  
  return(c(r1,r2))
}


library(doParallel)
library(forecast)

cl <- makeCluster(8)

clusterEvalQ(cl, {
  library(seasonal)
  library(doParallel)
  library(KFAS)
})

system.time({
    
  data1 <- simlist1(1000,120)
  x11list1 <- parLapply(cl, data1, x11)
  Data1 <- parLapply(cl, x11list1, preprocess)
  x11list1 <- parLapply(cl, Data1, x11)
  ratiodf1 <- data.frame(t(parSapply(cl, x11list1, varianceratio)))
  colnames(ratiodf1) <- c("trend", "season")
  
})

stopCluster(cl)


library(GGally)
library(ggpubr)

p1 <- ggplot(data = ratiodf1) +
  geom_histogram(aes(x=trend), color="white", fill="blue")

p2 <- ggplot(data = ratiodf1) +
  geom_histogram(aes(x=season), color="white", fill="red")

p3 <- ggplot(data = ratiodf1) +
  geom_density(aes(x=trend), color="white", fill="blue")

p4 <- ggplot(data = ratiodf1) +
  geom_density(aes(x=season), color="white", fill="red")

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2)



cl <- makeCluster(8)

clusterEvalQ(cl, {
  library(seasonal)
  library(doParallel)
  library(KFAS)
})

system.time({
  
  data2 <- simlist1(1000,180)
  x11list2 <- parLapply(cl, data2, x11)
  Data2 <- parLapply(cl, x11list2, preprocess)
  x11list2 <- parLapply(cl, Data2, x11)
  ratiodf2 <- data.frame(t(parSapply(cl, x11list2, varianceratio)))
  colnames(ratiodf2) <- c("trend", "season")
  
})

stopCluster(cl)


p1 <- ggplot(data = ratiodf2) +
  geom_histogram(aes(x=trend), color="white", fill="blue")

p2 <- ggplot(data = ratiodf2) +
  geom_histogram(aes(x=season), color="white", fill="red")

p3 <- ggplot(data = ratiodf2) +
  geom_density(aes(x=trend), color="white", fill="blue")

p4 <- ggplot(data = ratiodf2) +
  geom_density(aes(x=season), color="white", fill="red")

ggarrange(p1, p2, p3, p4, ncol=2, nrow=2)


# extract some datasets to see the decomposition
class(x11list1[[1]])
library(seasonal)
library(KFAS)

a=123

ssmm <- SSModel(Data1[[a]] ~ SSMtrend(1, Q = list(ratiodf1[a, 1])) + 
                  SSMseasonal(12, sea.type = "dummy", Q=ratiodf1[a,2]), H=1)
ssm_a <- KFS(ssmm)

par(mfrow=c(3,2))

plot(coef(ssm_a, states = "trend"))
plot(series(x11list1[[a]], "d12"))

plot(ts(rowSums(coef(ssm_a, states = "seasonal")), frequency=12, start=c(1990,01)))
plot(series(x11list1[[a]], "d10"))

plot(residuals(ssm_a, type="recursive"))
plot(series(x11list1[[a]], "d13"))

par(mfrow=c(1,1))

var(series(x11list1[[a]], "d12"))/var(series(x11list1[[a]], "d13")) == ratiodf1[a, 1]



x11list3 <- lapply(TS240, x11)

a <-15 

ssmm <- SSModel(TS240[[a]] ~ SSMtrend(1, Q = list(idevalmat3[a, 2])) + 
                  SSMseasonal(12, sea.type = "dummy", Q=1), H=idevalmat3[a,1])
ssm_a <- KFS(ssmm)

par(mfrow=c(3,2))

plot(coef(ssm_a, states = "trend"))
plot(series(x11list3[[a]], "d12"))

plot(ts(-rowSums(coef(ssm_a, states = "seasonal")), frequency=12, start=c(1990,01)))
plot(series(x11list3[[a]], "d10"))

plot(residuals(ssm_a, type="recursive"))
plot(series(x11list3[[a]], "d13"))

par(mfrow=c(1,1))

plot(ssmm)

#########################################################################################

library(forecast)
library(seasonal)
library(KFAS)
rm(list=ls())
set.seed(9483)
data <- simlist1(10, 180) 

par(mfrow=c(5,2))

for(i in 1:10){
  plot(data[[i]])
}

par(mfrow= c(1,1))

x11list <- lapply(data, x11)
Data <- lapply(x11list, preprocess)
x11list <- lapply(Data, x11)

par(mfrow=c(5,2))

for(i in 1:10){
  plot(Data[[i]])
}

par(mfrow= c(1,1))

plot(data[[4]])
plot(Data[[4]])
x11 <- seas(data[[4]], x11="")
plot(x11)
plot(series(x11, "d10"))
plot(series(x11, "d12"))
summary(x11)


ssm <- SSModel(Data[[4]] ~ SSMtrend(1, Q=list(matrix(NA))) + 
                 SSMseasonal(12, sea.type = "dummy", Q=NA), H=NA)
fit <- fitSSM(ssm, inits = c(0,0,0))
kfas <- KFS(fit$model)
fit$model["H"]

plot(signal(kfas,"seasonal")$signal)
plot(signal(kfas,"trend")$signal)

?diff
series(x11, "d10")
series(x11, "d12")
var(series(x11, "d12"))
var(diff(series(x11, "d12"), lag = 1))

seasonal_sum <- function(x11) {
  season <- series(x11, "d10")
  seasonal_sum <- c()
  for( i in 12:length(season)){
    sum <- sum(season[(i-11):i])
    seasonal_sum <- c(seasonal_sum, sum)
  }
  return(seasonal_sum)
}

plot(seasonal_sum(x11), type='l')
var(seasonal_sum(x11))




