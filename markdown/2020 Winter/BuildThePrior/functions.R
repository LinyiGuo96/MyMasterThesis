rm(list=ls())
setwd("C:/Users/guoly/desktop/markdown/2020 Winter/BuildThePrior")
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


rmoutliers <- function(x11) {
  
  data <- series(x11, 'b1')
  return(data)
}


addtransform <- function(x11) {

  if(transformfunction(x11) == "log") data <- log(original(x11))
  else data <- original(x11)
  
  return(data)
} 


ssmfit <- function(data) {
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) + SSMseasonal(12, sea.type = 'dummy', Q=NA), H=NA)
  ssm <- fitSSM(ssm, inits = c(0,0,0))
  
  return(ssm)
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



save.image(file="function.RData")
