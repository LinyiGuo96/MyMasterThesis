
rm(list=ls())

library(parallel)
library(forecast)

# functions

simulation1 <- function(length){
  
  model <- Arima(ts(rnorm(120),start=c(1980,01),frequency =12), order=c(0,1,1),   
                 seasonal=c(0,1,1), fixed=c(theta=runif(1), Theta=runif(1)))
  
  data <- simulate(model, nsim=length)
  
  # because if we need to take log later, data must be positive
  if(min(data) <= 0) data <- data - min(data) + runif(1)
  else data <- data
  
  return(data)
  
}

simlist1 <- function(n,length) {   
  
  Datalist <- list()
  
  for (i in 1:n)  Datalist[[i]] <- simulation1(length)
  
  return(Datalist)  
  
}

fun1 <- function(x){
  
  seas(x, x11='') 
  
}

preprocess <- function(x11) {
  
  if(transformfunction(x11) == 'log') 
    data <- log(series(x11, 'b1'))
  else
    data <- series(x11, 'b1')
  
  return(data)
}


# put previous functions 'exhaustion1' and 'Dif1' together

exhaustion1 <- function(data){
  
  Difference <- c()
  index <- c()
  
  x11 <- seas(data, x11='')
  
  for (i in 1:100) {
    for (j in 1:100) {
      
      ssmm <- SSModel(data ~ SSMtrend(1, Q=list(j*0.2)) + 
                        SSMseasonal(12, sea.type = 'dummy', Q = 1),
                      H = i*0.2)
      ssm <- KFS(ssmm)
      
      sigma <- c(i*0.2, j*0.2, 1)
      
      ### difference ###
      x11_trend <- series(x11, 'd12')
      x11_seasonal <- series(x11, 'd10')
      x11_irregular <- series(x11, 'd13')
      
      ssm_trend <- coef(ssm, states = 'trend')
      ssm_seasonal <- -rowSums(coef(ssm, states='seasonal'))
      ssm_irregular <- data[-1] - ssm_trend[-1] - ssm_seasonal[-length(data)]
      
      D <-  sum((x11_irregular[-1]-ssm_irregular)^2)/sigma[1] + 
        sum((x11_trend-ssm_trend)^2)/sigma[2] + 
        sum((x11_seasonal[-1]-ssm_seasonal[-length(data)])^2)/sigma[3] 
      ### end ###
      
      Difference <- c(Difference, D)
      
      index <- rbind(index, sigma)
    }
  }
  
  df <- data.frame(variance=index, difference = Difference)
  return(df)
}

# update exhaustion function with foreach & doParallel

exhaustion1 <- function(data){
  
  Difference <- c()
  index <- c()
  
  x11 <- seas(data, x11='')
  
  df <- foreach(i = 1:100, .combine = "rbind") %:%
    foreach(j = 1:100, .combine = "rbind") %dopar% {
      
      ssmm <- SSModel(data ~ SSMtrend(1, Q=list(j*0.2)) + 
                        SSMseasonal(12, sea.type = 'dummy', Q = 1),
                      H = i*0.2)
      ssm <- KFS(ssmm)
      
      sigma <- c(i*0.2, j*0.2, 1)
      
      ### difference ###
      x11_trend <- series(x11, 'd12')
      x11_seasonal <- series(x11, 'd10')
      x11_irregular <- series(x11, 'd13')
      
      ssm_trend <- coef(ssm, states = 'trend')
      ssm_seasonal <- -rowSums(coef(ssm, states='seasonal'))
      ssm_irregular <- data[-1] - ssm_trend[-1] - ssm_seasonal[-length(data)]
      
      D <-  sum((x11_irregular[-1]-ssm_irregular)^2)/sigma[1] + 
        sum((x11_trend-ssm_trend)^2)/sigma[2] + 
        sum((x11_seasonal[-1]-ssm_seasonal[-length(data)])^2)/sigma[3] 
      ### end ###
      
      c(i*0.2, j*0.2, D)
    }
  
  colnames(df) <- c("variance.1", "variance.2", "difference")
  
  df <- data.frame(df)
  
  return(df)
}

# but the new version seems to be just so so


n <- 100 # amount of datasets
datalist1 <- simlist1(n, 120)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list1 <- parLapply(cl, datalist1, fun1 )
  
  Datalist1 <- parLapply(cl, x11list1, preprocess )
  
  idevallist1_1 <- parLapply(cl, Datalist1, exhaustion1)
  
  idevalmat1_1 <- c()
  
  for (i in 1:n){
    ideval <- idevallist1_1[[i]][which.min(idevallist1_1[[i]]$difference),c(1,2)]
    idevalmat1_1 <- rbind(idevalmat1_1, ideval)
  }
  
  rownames(idevalmat1_1) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat1_1, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat1_1.csv")



n <- 100 # amount of datasets
datalist2 <- simlist1(n, 180)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list2 <- parLapply(cl, datalist2, fun1 )
  
  Datalist2 <- parLapply(cl, x11list2, preprocess )
  
  idevallist2_1 <- parLapply(cl, Datalist2, exhaustion1)
  
  idevalmat2_1 <- c()
  
  for (i in 1:n){
    ideval <- idevallist2_1[[i]][which.min(idevallist2_1[[i]]$difference),c(1,2)]
    idevalmat2_1 <- rbind(idevalmat2_1, ideval)
  }
  
  rownames(idevalmat2_1) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat2_1, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat2_1.csv")




n <- 100 # amount of datasets
datalist3 <- simlist1(n, 240)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list3 <- parLapply(cl, datalist3, fun1 )
  
  Datalist3 <- parLapply(cl, x11list3, preprocess )
  
  idevallist3_1 <- parLapply(cl, Datalist3, exhaustion1)
  
  idevalmat3_1 <- c()
  
  for (i in 1:n){
    ideval <- idevallist3_1[[i]][which.min(idevallist3_1[[i]]$difference),c(1,2)]
    idevalmat3_1 <- rbind(idevalmat3_1, ideval)
  }
  
  rownames(idevalmat3_1) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat3_1, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat3_1.csv")





n <- 100 # amount of datasets
datalist1 <- simlist1(n, 120)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list1 <- parLapply(cl, datalist1, fun1 )
  
  Datalist1 <- parLapply(cl, x11list1, preprocess )
  
  idevallist1_2 <- parLapply(cl, Datalist1, exhaustion1)
  
  idevalmat1_2 <- c()
  
  for (i in 1:n){
    ideval <- idevallist1_2[[i]][which.min(idevallist1_2[[i]]$difference),c(1,2)]
    idevalmat1_2 <- rbind(idevalmat1_2, ideval)
  }
  
  rownames(idevalmat1_2) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat1_2, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat1_2.csv")



n <- 100 # amount of datasets
datalist2 <- simlist1(n, 180)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list2 <- parLapply(cl, datalist2, fun1 )
  
  Datalist2 <- parLapply(cl, x11list2, preprocess )
  
  idevallist2_2 <- parLapply(cl, Datalist2, exhaustion1)
  
  idevalmat2_2 <- c()
  
  for (i in 1:n){
    ideval <- idevallist2_2[[i]][which.min(idevallist2_2[[i]]$difference),c(1,2)]
    idevalmat2_2 <- rbind(idevalmat2_2, ideval)
  }
  
  rownames(idevalmat2_2) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat2_2, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat2_2.csv")




n <- 100 # amount of datasets
datalist3 <- simlist1(n, 240)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list3 <- parLapply(cl, datalist3, fun1 )
  
  Datalist3 <- parLapply(cl, x11list3, preprocess )
  
  idevallist3_2 <- parLapply(cl, Datalist3, exhaustion1)
  
  idevalmat3_2 <- c()
  
  for (i in 1:n){
    ideval <- idevallist3_2[[i]][which.min(idevallist3_2[[i]]$difference),c(1,2)]
    idevalmat3_2 <- rbind(idevalmat3_2, ideval)
  }
  
  rownames(idevalmat3_2) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat3_2, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat3_2.csv")





############################################
########  丢失了一些模拟数据的code  ########
########       不过都大同小异       ########
############################################



n <- 100 # amount of datasets
datalist1 <- simlist1(n, 120)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list1 <- parLapply(cl, datalist1, fun1 )
  
  Datalist1 <- parLapply(cl, x11list1, preprocess )
  
  idevallist1 <- parLapply(cl, Datalist1, exhaustion1)
  
  idevalmat1 <- c()
  
  for (i in 1:n){
    ideval <- idevallist1[[i]][which.min(idevallist1[[i]]$difference),c(1,2)]
    idevalmat1 <- rbind(idevalmat1, ideval)
  }
  
  rownames(idevalmat1) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat1, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat1_6.csv")



n <- 100 # amount of datasets
datalist2 <- simlist1(n, 180)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list2 <- parLapply(cl, datalist2, fun1 )
  
  Datalist2 <- parLapply(cl, x11list2, preprocess )
  
  idevallist2 <- parLapply(cl, Datalist2, exhaustion1)
  
  idevalmat2 <- c()
  
  for (i in 1:n){
    ideval <- idevallist2[[i]][which.min(idevallist2[[i]]$difference),c(1,2)]
    idevalmat2 <- rbind(idevalmat2, ideval)
  }
  
  rownames(idevalmat2) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat2, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat2_6.csv")




n <- 100 # amount of datasets
datalist3 <- simlist1(n, 240)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
} 
)

system.time({
  
  x11list3 <- parLapply(cl, datalist3, fun1 )
  
  Datalist3 <- parLapply(cl, x11list3, preprocess )
  
  idevallist3 <- parLapply(cl, Datalist3, exhaustion1)
  
  idevalmat3 <- c()
  
  for (i in 1:n){
    ideval <- idevallist3[[i]][which.min(idevallist3[[i]]$difference),c(1,2)]
    idevalmat3 <- rbind(idevalmat3, ideval)
  }
  
  rownames(idevalmat3) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat3, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat3_6.csv")


# 7 直接换 8 
library(forecast)

n <- 100 # amount of datasets
datalist1 <- simlist1(n, 120)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
  library(doParallel)
  
} 
)

system.time({
  
  x11list1 <- parLapply(cl, datalist1, fun1 )
  
  Datalist1 <- parLapply(cl, x11list1, preprocess )
  
  idevallist1 <- parLapply(cl, Datalist1, exhaustion1)
  
  idevalmat1 <- c()
  
  for (i in 1:n){
    ideval <- idevallist1[[i]][which.min(idevallist1[[i]]$difference),c(1,2)]
    idevalmat1 <- rbind(idevalmat1, ideval)
  }
  
  rownames(idevalmat1) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat1, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat1_8.csv")
#用户    系统    流逝 
#0.20    0.11 1485.79


n <- 100 # amount of datasets
datalist2 <- simlist1(n, 180)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
  library(doParallel)
} 
)

system.time({
  
  x11list2 <- parLapply(cl, datalist2, fun1 )
  
  Datalist2 <- parLapply(cl, x11list2, preprocess )
  
  idevallist2 <- parLapply(cl, Datalist2, exhaustion1)
  
  idevalmat2 <- c()
  
  for (i in 1:n){
    ideval <- idevallist2[[i]][which.min(idevallist2[[i]]$difference),c(1,2)]
    idevalmat2 <- rbind(idevalmat2, ideval)
  }
  
  rownames(idevalmat2) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat2, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat2_8.csv")

#用户    系统    流逝 
#0.28    0.10 1637.41 


n <- 100 # amount of datasets
datalist3 <- simlist1(n, 240)

cl <- makeCluster(16)

clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
  library(doParallel)
  
} 
)

system.time({
  
  x11list3 <- parLapply(cl, datalist3, fun1 )
  
  Datalist3 <- parLapply(cl, x11list3, preprocess )
  
  idevallist3 <- parLapply(cl, Datalist3, exhaustion1)
  
  idevalmat3 <- c()
  
  for (i in 1:n){
    ideval <- idevallist3[[i]][which.min(idevallist3[[i]]$difference),c(1,2)]
    idevalmat3 <- rbind(idevalmat3, ideval)
  }
  
  rownames(idevalmat3) <- c()
  
})

stopCluster(cl)

write.csv(idevalmat3, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\idevalmat3_8.csv")

# 1728.39


set.seed(9483)
ts120 <- simlist1(100, 120)
ts180 <- simlist1(100, 180)
ts240 <- simlist1(100, 240)

getwd()
setwd("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior")
save(ts120, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts120.RData")
save(ts180, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts180.RData")
save(ts240, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts240.RData")



##################################
############## NOTE ##############
##################################

# update: I forgot to preprocess these datasets.

# preprocess these datasets and compute the 'best value' by the way

load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts240.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts180.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts120.RData")

library(doParallel)
cl <- detectCores()
cl <- makeCluster(cl)
clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
  library(doParallel)} 
)



# new version of exhaustion
system.time({
  
  n = 100
  x11list1 <- parLapply(cl, ts120, fun1 )
  TS120 <- parLapply(cl, x11list1, preprocess )
  idevallist1 <- parLapply(cl, TS120, exhaustion1)
  idevalmat1 <- c()
  for (i in 1:n){
    ideval <- idevallist1[[i]][which.min(idevallist1[[i]]$difference),c(1,2)]
    idevalmat1 <- rbind(idevalmat1, ideval)
  }
  rownames(idevalmat1) <- c()
  
})

# 用户    系统    流逝 
# 0.63    0.08   1517.17 

# when I used the old exhaustion function 
# it is about 1400+s for length 120
write.csv(idevalmat1, "C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\120ideval.csv")


x11list2 <- parLapply(cl, ts180, fun1 )
TS180 <- parLapply(cl, x11list2, preprocess )
idevallist2 <- parLapply(cl, TS180, exhaustion1)
idevalmat2 <- c()
for (i in 1:n){
  ideval <- idevallist2[[i]][which.min(idevallist2[[i]]$difference),c(1,2)]
  idevalmat2 <- rbind(idevalmat2, ideval)
}
rownames(idevalmat2) <- c()



x11list3 <- parLapply(cl, ts240, fun1 )
TS240 <- parLapply(cl, x11list3, preprocess )
idevallist3 <- parLapply(cl, TS240, exhaustion1)
idevalmat3 <- c()
for (i in 1:n){
  ideval <- idevallist3[[i]][which.min(idevallist3[[i]]$difference),c(1,2)]
  idevalmat3 <- rbind(idevalmat3, ideval)
}
rownames(idevalmat3) <- c()

stopCluster(cl)


save(TS120, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS120pre.RData")
save(TS180, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS180pre.RData")
save(TS240, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS240pre.RData")
save(idevalmat1, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS120ideval.RData")
save(idevalmat2, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS180ideval.RData")
save(idevalmat3, file ="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS240ideval.RData")


# add the label of transformation of each dataset

load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts120.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts180.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\ts240.RData")


library(doParallel)

cl <- detectCores()
cl <- makeCluster(cl)
clusterEvalQ(cl,{
  library(seasonal)
  library(KFAS)
  library(doParallel)} 
)

  
n = 100

x11list1 <- parLapply(cl, ts120, fun1 )
transformation1 <- parSapply(cl, x11list1, transformfunction)


x11list2 <- parLapply(cl, ts180, fun1 )
transformation2 <- parSapply(cl, x11list2, transformfunction)


x11list3 <- parLapply(cl, ts240, fun1 )
transformation3 <- parLapply(cl, x11list3, transformfunction)

stopCluster(cl)

