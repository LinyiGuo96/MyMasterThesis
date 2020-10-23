rm(list=ls())
loss <- function(data, x11, kfs){
  
  deseasonal_x11 <- series(x11, "d11")
  trend_x11 <- series(x11, "d12")
  
  deseasonal_kfs <- data - signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((deseasonal_x11-deseasonal_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}


loss2 <- function(data, x11, kfs){
  
  deseasonal_x11 <- series(x11, "d11")
  trend_x11 <- series(x11, "d12")
  
  deseasonal_kfs <- data - signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((deseasonal_x11-deseasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2)
  
  return(l)
}


loss3 <- function(x11, kfs){
  
  trend_x11 <- series(x11, "d12")
  seasonal_x11 <- series(x11, "d10")
  
  trend_kfs <- signal(kfs, "trend")$signal
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2)
  
  return(l)
}


loss4 <- function(x11, kfs){
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}


loss5 <- function(x11, kfs){
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}

loss5_version2 <- function(x11, kfs){
  # multiplicative
  seasonal_x11 <- log(series(x11, "d10"))
  trend_x11 <- log(series(x11, "d12"))
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
  
  return(l)
}

loss6 <- function(x11, kfs){
  
  seasonal_x11 <- series(x11, "d10")
  trend_x11 <- series(x11, "d12")
  
  seasonal_kfs <- signal(kfs, "seasonal")$signal
  trend_kfs <- signal(kfs, "trend")$signal
  
  l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + (sum(diff(trend_x11)) - sum(diff(trend_kfs)))^2
  
  return(l)
}

# loss7 <- function(x11, kfs){
#   
#   seasonal_x11 <- series(x11, "d10")
#   trend_x11 <- series(x11, "d12")
#   
#   seasonal_kfs <- signal(kfs, "seasonal", filtered = TRUE)$signal
#   trend_kfs <- signal(kfs, "trend", filtered = TRUE)$signal
#   
#   l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
#   
#   return(l)
# }


gridsearch <- function(data) {
  
  x11 <- seas(data, x11='')
  
  loss5mat  <- foreach (i = 1:200, .combine = "rbind", .packages = c("KFAS","seasonal")) %:% 
    foreach(j = 1:200, .combine = "rbind") %dopar% {
      
      ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*0.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*0.125)
      kfs <- KFS(ssm)
      
      l <- loss5(x11, kfs)
      
      c(i*0.125, j*0.1, l)
      
    }
  
  return(loss5mat[which.min(loss5mat[,3]),c(1,2)])
}


mle_extract <- function(data) {
  # note the output is actually the ratio of mle
  ssm <- SSModel(data ~ SSMtrend(1, Q=list(NA)) +SSMseasonal(12, sea.type = "dummy", Q=NA), H=NA)
  fit <- fitSSM(ssm, inits = c(1,1,1))
  kfs <- KFS(fit$model)
  
  var_I <- fit$model["H"][1,1,1]
  var_T <- fit$model["Q"][1,1,1]
  var_S <- fit$model["Q"][2,2,1]
  
  return(c(var_I/var_S, var_T/var_S))
}



post_extract <- function(data) {
  
  x11 <- seas(data, x11='')
  
  postmat <- foreach(i = 1:200, .packages = c('seasonal', 'KFAS'), .combine='rbind') %:%
    foreach(j = 1:200, .combine='rbind') %dopar% {
      ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*.125)
      ll <- logLik(ssm)
      
      lp1 <- log(prior1$y[which.min(abs(prior1$x - i*.125))])
      
      lp2 <- log(prior2$y[which.min(abs(prior2$x - j*.1))])
      
      c(i*.125, j*.1, ll+lp1+lp2)
    }
  
  post <- postmat[which.max(postmat[,3]),]
  
  return(post)
}


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

post_extract_halfcauchy_version2 <- function(data) {
  
  postmat <- foreach(i = 1:200, .packages = c('seasonal', 'KFAS', 'extraDistr'), .combine='rbind') %:%
    foreach(j = 1:200, .combine='rbind') %dopar% {
      ssm <- SSModel(log(data) ~ SSMtrend(1, Q=list(j*.1)) + SSMseasonal(12, sea.type = 'dummy', Q=1), H=i*.125)
      ll <- logLik(ssm)
      
      lp1 <- log(dhcauchy(x=sqrt(i*0.125), sigma=sqrt(10)))
      
      lp2 <- log(dhcauchy(x=sqrt(j*0.1),sigma = sqrt(5)))
      
      c(i*.125, j*.1, ll+lp1+lp2)
    }
  
  post <- postmat[which.max(postmat[,3]),c(1,2)]
  
  return(post)
}


# this is to compute the ideal(best) value
f1 <- function(data) {
  f2 <- function(var) {
    x11 <- seas(data, x11='')
    ssm <- SSModel(data ~ SSMtrend(1,Q=list(var[2]))+SSMseasonal(12,sea.type = 'dummy',Q=1), H=var[1])
    kfs <- KFS(ssm)
    
    seasonal_x11 <- series(x11, "d10")
    trend_x11 <- series(x11, "d12")
    
    seasonal_kfs <- signal(kfs, "seasonal")$signal
    trend_kfs <- signal(kfs, "trend")$signal
    
    l <- sum((seasonal_x11-seasonal_kfs)^2) + sum((trend_x11-trend_kfs)^2) + sum((diff(trend_x11)-diff(trend_kfs))^2)
    
    return(l)
  }
  var0 <- c(0,0)
  
  return(hjk(var0, f2, control=list(tol=0.001))$par)
}


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

save.image(file = 'C:/Users/GuoLY/desktop/markdown/2020 Winter/masterthesis/file3/functions.RData')

