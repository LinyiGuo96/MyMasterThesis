# import data

setwd("C:/Users/guoly/Desktop/markdown/2020 Winter/BuildThePrior")
idevalmat1_1 <- read.csv("data/idevalmat1_1.csv")
idevalmat1_2 <- read.csv("data/idevalmat1_2.csv")
idevalmat1_3 <- read.csv("data/idevalmat1_3.csv")
idevalmat1_4 <- read.csv("data/idevalmat1_4.csv")
idevalmat1_5 <- read.csv("data/idevalmat1_5.csv")
idevalmat1_6 <- read.csv("data/idevalmat1_6.csv")
idevalmat1_7 <- read.csv("data/idevalmat1_7.csv")
idevalmat1_8 <- read.csv("data/idevalmat1_8.csv")

idevalmat2_1 <- read.csv("data/idevalmat2_1.csv")
idevalmat2_2 <- read.csv("data/idevalmat2_2.csv")
idevalmat2_3 <- read.csv("data/idevalmat2_3.csv")
idevalmat2_4 <- read.csv("data/idevalmat2_4.csv")
idevalmat2_5 <- read.csv("data/idevalmat2_5.csv")
idevalmat2_6 <- read.csv("data/idevalmat2_6.csv")
idevalmat2_7 <- read.csv("data/idevalmat2_7.csv")
idevalmat2_8 <- read.csv("data/idevalmat2_8.csv")

idevalmat3_1 <- read.csv("data/idevalmat3_1.csv")
idevalmat3_2 <- read.csv("data/idevalmat3_2.csv")
idevalmat3_3 <- read.csv("data/idevalmat3_3.csv")
idevalmat3_4 <- read.csv("data/idevalmat3_4.csv")
idevalmat3_5 <- read.csv("data/idevalmat3_5.csv")
idevalmat3_6 <- read.csv("data/idevalmat3_6.csv")
idevalmat3_7 <- read.csv("data/idevalmat3_7.csv")
idevalmat3_8 <- read.csv("data/idevalmat3_8.csv")


## put them together
Idevalmat <- rbind(idevalmat1_1,idevalmat1_2,idevalmat1_3,idevalmat1_4,idevalmat1_5,idevalmat1_6,idevalmat1_7,idevalmat1_8,
                   idevalmat2_1,idevalmat2_2,idevalmat2_3,idevalmat2_4,idevalmat2_5,idevalmat2_6,idevalmat2_7,idevalmat2_8,
                   idevalmat3_1,idevalmat3_2,idevalmat3_3,idevalmat3_4,idevalmat3_5,idevalmat3_6,idevalmat3_7,idevalmat3_8)


Idevalmat1 <- rbind(idevalmat1_1, idevalmat1_2, idevalmat1_3, idevalmat1_4, idevalmat1_5, idevalmat1_6,idevalmat1_7,idevalmat1_8)
Idevalmat2 <- rbind(idevalmat2_1, idevalmat2_2, idevalmat2_3, idevalmat2_4, idevalmat2_5, idevalmat2_6,idevalmat2_7,idevalmat2_8)
Idevalmat3 <- rbind(idevalmat3_1, idevalmat3_2, idevalmat3_3, idevalmat3_4, idevalmat3_5, idevalmat3_6,idevalmat3_7,idevalmat3_8)
# note: the first column of Idevalmat,1,2,3 is 1:n instead of variance 

# Analysis

library(ggpubr)

ggplot(Idevalmat) + 
  geom_histogram(aes(x=variance.1), binwidth = 0.2, color = "gray")
ggplot(Idevalmat) +
  geom_histogram(aes(x=variance.2), binwidth = 0.2, color = "gray")


ggplot(Idevalmat1) + 
  geom_histogram(aes(x=variance.1), binwidth = 0.2, color = "gray")
ggplot(Idevalmat1) +
  geom_histogram(aes(x=variance.2), binwidth = 0.2, color = "gray")


ggplot(Idevalmat2) + 
  geom_histogram(aes(x=variance.1), binwidth = 0.2, color = "gray")
ggplot(Idevalmat2) +
  geom_histogram(aes(x=variance.2), binwidth = 0.2, color = "gray")



ggplot(Idevalmat3) + 
  geom_histogram(aes(x=variance.1), binwidth = 0.2, color = "gray")
ggplot(Idevalmat3) +
  geom_histogram(aes(x=variance.2), binwidth = 0.2, color = "gray")
# Review the histograms of the wrong case before
# There are a lot of outliers
# But now it's better


density1 <- ggplot(Idevalmat) + 
  geom_density(aes(x=variance.1), color = "blue")

density2 <- ggplot(Idevalmat) +
  geom_density(aes(x=variance.2), color = "red")


x <- seq(0.2,20,0.2)
Df <- c()

for (i in 1:10) {
  for (j in 1:10) {
    
    y <- df(x, df1 = i, df2 = j) # f distribution's density 
    
    df <- data.frame(x=x, y=y, df1=i, df2=j)
    
    Df <- rbind(Df, df)
  }
  
}



ggplot(data= Df, aes(x= x, y=y) ) + 
  geom_line() + 
  facet_grid(df1~df2) + # rows are df1
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())

  
p2 <- ggplot() + 
  geom_line(data= Df, aes(x= x, y=y)) + 
  facet_grid(df1~df2) +  # rows are df1
  theme(axis.text.x = element_blank(),axis.text.y = element_blank(),axis.ticks = element_blank())
p2 <- p2 + geom_density(data= Idevalmat, aes(x=variance.2), color="red") 
p2

# the distribution of variance.2 seems to be a f distribution (10,5 perhaps)



Df2 <- c()

for (i in 1:10) {
  for (j in 1:10) {
    
    y <- dgamma(x, shape = i, rate = j)
    
    df <- data.frame(x=x, y=y, shape=i, rate = j)
    
    Df2 <- rbind(Df2, df)
  }
  
}


ggplot(Df2) + 
  geom_line(aes(x=x, y=y)) + 
  facet_grid(shape~rate) + # row is shape
  theme(axis.text = element_blank(), axis.ticks = element_blank())


p1 <- ggplot() + geom_line(data=Df2, aes(x=x, y=y)) +
  facet_grid(shape~rate) + 
  theme(axis.text = element_blank(), axis.ticks = element_blank())
p1 <- p1 + geom_density(data=Idevalmat, aes(x=variance.1), color="blue")
p1
 
# the distribution of variance.1 is likely a gamma distribution(gamma(rate=3,shape=9) ? )


f1 <- density(Idevalmat$variance.1, n = 1000)
f2 <- density(Idevalmat$variance.2, n = 1000)
# f is a list
f1$x <- round(f1$x, 2)
f2$x <- round(f2$x, 2)
str(f1)
str(f2)




# learn foreach

library(doParallel) # will import foreach and parallel

cl <- detectCores()
cl <- makeCluster(cl)
registerDoParallel(cl)

# system.time({
#   output <- foreach(i = 1:100000) %dopar% {i+1}
# })
# 
#  用户  系统  流逝 
# 22.83  1.77 24.61 

# system.time({
#   output <- foreach(i = 1:100000) %do% {i+1}
# })
# 
# 用户  系统  流逝 
#14.95  0.15 15.14 
getDoParWorkers()
stopCluster(cl)
# Note well that this is not a practical use of doParallel. This is our “Hello, world”
# program for parallel computing. It tests that everything is installed and set up properly,
# but don’t expect it to run faster than a sequential for loop, because it won’t! sqrt
# executes far too quickly to be worth executing in parallel, even with a large number of
# iterations. With small tasks, the overhead of scheduling the task and returning the result
# can be greater than the time to execute the task itself, resulting in poor performance.
# In addition, this example doesn’t make use of the vector capabilities of sqrt, which it
# must to get decent performance. This is just a test and a pedagogical example, not a
# benchmark.


###### MLE ######

load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS120pre.RData") # TS120
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS180pre.RData") # TS180
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS240pre.RData") # TS240


llmat <- function(data) {
  
    llmat <- foreach(i=1:200, .combine = "rbind") %:%
  
    foreach(j=1:200, .combine = "rbind") %dopar% {
  
    ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*.1)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=i*.1)
  
    ll <- logLik(ssm)
  
    c(i*.1,j*.1,ll) } 
  
    return(llmat)

}

library(doParallel)

cl <- detectCores()
cl <- makeCluster(cl)

clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(doParallel)

  })

system.time({

  llmatlist1 <- parLapply(cl, TS120, llmat) 
  llmatlist2 <- parLapply(cl, TS180, llmat) 
  llmatlist3 <- parLapply(cl, TS240, llmat) 
  
})
# 100*100 
# 用户   系统   流逝 
# 0.41   0.12 696.37 

# 200*200
# 用户    系统    流逝 
# 2.04    1.11 2841.33 

stopCluster(cl)


mle <- function(llmat) {
  mle <- llmat[which.max(llmat[,3]),c(1,2)]
  return(mle)
}


mlelist1 <- t(sapply(llmatlist1, mle)) # mlelist is actually a matrix not a list
mlelist2 <- t(sapply(llmatlist2, mle))
mlelist3 <- t(sapply(llmatlist3, mle))
mlelist1 <- data.frame(mlelist1)
mlelist2 <- data.frame(mlelist2)
mlelist3 <- data.frame(mlelist3)

mlelist <- rbind(mlelist1, mlelist2, mlelist3)


colnames(mlelist1) <- c("variance.1", "variance.2")
colnames(mlelist2) <- c("variance.1", "variance.2")
colnames(mlelist3) <- c("variance.1", "variance.2")
colnames(mlelist) <- c("variance.1", "variance.2")

save(mlelist, file="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist.RData")
save(mlelist1, file="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist1.RData")
save(mlelist2, file="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist2.RData")
save(mlelist3, file="C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist3.RData")

load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist1.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist2.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\mlelist3.RData")



p11 <- ggplot(data.frame(mlelist1)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, binwidth = 1, color="white") +
  ggtitle("ts120")

p21 <- ggplot(data.frame(mlelist2)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, binwidth = 1, color="white") +
  ggtitle("ts180")

p31 <- ggplot(data.frame(mlelist3)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, binwidth = 1, color="white") +
  ggtitle("ts240")


p41 <- ggplot(data.frame(mlelist)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, binwidth = 1, color="white") +
  ggtitle("total")


ggarrange(p11,p21,p31,p41, nrow = 2, ncol = 2) 


p12 <- ggplot(data.frame(mlelist1)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, binwidth = 1, color="white") +
  ggtitle("ts120")

p22 <- ggplot(data.frame(mlelist2)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, binwidth = 1, color="white") +
  ggtitle("ts180")

p32 <- ggplot(data.frame(mlelist3)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, binwidth = 1, color="white") + 
  ggtitle("ts240")


p42 <- ggplot(data.frame(mlelist)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, binwidth = 1, color="white") +
  ggtitle("total")


ggarrange(p12,p22,p32,p42, nrow = 2, ncol = 2) 

# this plot is the distribution of MLEs in different cases


# the distribution of 'best' values

load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS120ideval.RData") #idevalmat1
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS180ideval.RData") #idevalmat2
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\TS240ideval.RData") #idevalmat3

idevalmat <- rbind(idevalmat1, idevalmat2, idevalmat3)

library(ggpubr)

q11 <- ggplot(data.frame(idevalmat1)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("ts120")

q21 <- ggplot(data.frame(idevalmat2)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("ts180")

q31 <- ggplot(data.frame(idevalmat3)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, bins = 20, color="white") +
  ggtitle("ts240")

q41 <- ggplot(data.frame(idevalmat)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("total")

ggarrange(q11,q21,q31,q41, nrow = 2, ncol = 2) 



q12 <- ggplot(data.frame(idevalmat1)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, bins = 20, color="white") +
  ggtitle("ts120")

q22 <- ggplot(data.frame(idevalmat2)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, bins = 20, color="white") +
  ggtitle("ts180")

q32 <- ggplot(data.frame(idevalmat3)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, bins = 20, color="white") + 
  ggtitle("ts240")

q42 <- ggplot(data.frame(idevalmat)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, binwidth = 1,  color="white") +
  ggtitle("total")

ggarrange(q12,q22,q32,q42, nrow = 2, ncol = 2) 



# ideval120 <- read.csv("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\120ideval.csv")
# ideval120 should be as exactly same as TS120ideval, which is idevalmat1 when imported

# ideval120[,c(2,3)] - idevalmat1 # 0, which verify our result
# no need to worry about the distribution of 'best' values



##### posterior #####
library(doParallel)

# logprior

plot(x=f1$x, y=f1$y, type='l')
plot(x=f2$x, y=f2$y, type='l')
str(f1)
f1$y[1]
f1$y[which(f1$x == 0.12)]

f1$y[which.min(abs(f1$x - 10))] # switch 10 with the variance.1 value that we want to test
log(f1$y[which.min(abs(f1$x - 1.5))]) 

f1 <- density(Idevalmat$variance.1, n = 1000)
f2 <- density(Idevalmat$variance.2, n = 1000)


prior <- list(f1, f2)
prior[1]
prior[1]$x


logpostmat <- function(data) {
  
  
  mat <- foreach(i = 1:100, .combine = 'rbind') %:%
    foreach(j = 1:100, .combine = 'rbind') %dopar% {
      
      ssm <- SSModel(data ~ SSMtrend(1, Q=list(j*.2)) + SSMseasonal(12, sea.type = "dummy", Q=1), H=i*.2)
      
      ll <- logLik(ssm) 
      
      lp1 <- log(f1$y[which.min(abs(f1$x - i*0.2))])
      
      lp2 <- log(f2$y[which.min(abs(f2$x - j*0.2))])
      
      lp <- lp1 + lp2
      
      logpost <- ll + lp
      
      c(i*.2, j*.2, logpost)
      
    }
  
  return(data.frame(mat))
}


library(doParallel)

cl <- detectCores()
cl <- makeCluster(cl)

clusterEvalQ(cl, {
  library(seasonal)
  library(KFAS)
  library(doParallel)

})

clusterExport(cl, c("f1","f2"))

logpostlist1 <- parLapply(cl, TS120, logpostmat)
logpostlist2 <- parLapply(cl, TS180, logpostmat)
logpostlist3 <- parLapply(cl, TS240, logpostmat)

stopCluster(cl)


load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\logpostlist1.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\logpostlist2.RData")
load("C:\\Users\\guoly\\Desktop\\markdown\\2020 Winter\\BuildThePrior\\data\\logpostlist3.RData")

class(logpostlist1)
class(logpostlist1[1])
class(logpostlist1[[1]])



for (i in 1:100) {
  map <- logpostlist1[[i]][which.max(data.frame(logpostlist1)$logpost),]
  mapmat1 <- rbind(mapmat1, map)
}


library(doParallel)

cl <- detectCores()
registerDoParallel(cl)

mapmat1 <- foreach(i = 1:100, .combine = "rbind") %dopar% {
  
  map <- logpostlist1[[i]][which.max(logpostlist1[[i]][,3]),]
  
  map
} 

mapmat2 <- foreach(i = 1:100, .combine = "rbind") %dopar% {
  
  map <- logpostlist2[[i]][which.max(logpostlist2[[i]][,3]),]
  
  map
} 

mapmat3 <- foreach(i = 1:100, .combine = "rbind") %dopar% {
  
  map <- logpostlist3[[i]][which.max(logpostlist3[[i]][,3]),]
  
  map
} 

stopCluster(cl)

mapmat1 <- data.frame(mapmat1)
colnames(mapmat1) <- c("variance.1", "variance.2", "logpost")

mapmat2 <- data.frame(mapmat2)
colnames(mapmat2) <- c("variance.1", "variance.2", "logpost")

mapmat3 <- data.frame(mapmat3)
colnames(mapmat3) <- c("variance.1", "variance.2", "logpost")

mapmat <- rbind(mapmat1, mapmat2, mapmat3)

library(ggpubr)
###############

map11 <- ggplot(mapmat1) +
  geom_histogram(aes(x=variance.1), color="white", fill = "blue", alpha=.5) +
  ggtitle("map of ts120")

mle11 <- ggplot(data.frame(mlelist1)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("mle of ts120")

ide11 <- ggplot(data.frame(idevalmat1)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("ideal of ts120")

ggarrange(map11, mle11, ide11, nrow=3)


map12 <- ggplot(mapmat1) +
  geom_histogram(aes(x=variance.2), color="white", fill = "red", alpha=.5) +
  ggtitle("map of ts120")

mle12 <- ggplot(data.frame(mlelist1)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, color="white") +
  ggtitle("mle of ts120")

ide12 <- ggplot(data.frame(idevalmat1)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, color="white") +
  ggtitle("ideal of ts120")

ggarrange(map12, mle12, ide12, nrow=3)
###############




map21 <- ggplot(mapmat2) +
  geom_histogram(aes(x=variance.1), color="white", fill = "blue", alpha=.5) +
  ggtitle("map of ts180")

mle21 <- ggplot(data.frame(mlelist2)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("mle of ts180")

ide21 <- ggplot(data.frame(idevalmat2)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("ideal of ts180")

ggarrange(map21, mle21, ide21, nrow=3)


map22 <- ggplot(mapmat2) +
  geom_histogram(aes(x=variance.2), color="white", fill = "red", alpha=.5) +
  ggtitle("map of ts180")

mle22 <- ggplot(data.frame(mlelist1)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, color="white") +
  ggtitle("mle of ts180")

ide22 <- ggplot(data.frame(idevalmat2)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, color="white") +
  ggtitle("ideal of ts180")

ggarrange(map22, mle22, ide22, nrow=3)
###############



map31 <- ggplot(mapmat3) +
  geom_histogram(aes(x=variance.1), color="white", fill = "blue", alpha=.5) +
  ggtitle("map of ts240")

mle31 <- ggplot(data.frame(mlelist3)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("mle of ts240")

ide31 <- ggplot(data.frame(idevalmat3)) +
  geom_histogram(aes(x=variance.1), fill="blue", alpha=.5, color="white") +
  ggtitle("ideal of ts240")

ggarrange(map31, mle31, ide31, nrow=3)


map32 <- ggplot(mapmat3) +
  geom_histogram(aes(x=variance.2), color="white", fill = "red", alpha=.5) +
  ggtitle("map of ts240")

mle32 <- ggplot(data.frame(mlelist3)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, color="white") +
  ggtitle("mle of ts240")

ide32 <- ggplot(data.frame(idevalmat3)) +
  geom_histogram(aes(x=variance.2), fill="red", alpha=.5, color="white") +
  ggtitle("ideal of ts240")

ggarrange(map32, mle32, ide32, nrow=3)



########################################################################

head(mapmat1)
head(mlelist1)
head(idevalmat1)

estimate120 <- cbind(mlelist1, mapmat1[,-3], idevalmat1)
estimate180 <- cbind(mlelist2, mapmat2[,-3], idevalmat2)
estimate240 <- cbind(mlelist3, mapmat3[,-3], idevalmat3)
estimatetot <- cbind(mlelist, mapmat[,-3], idevalmat)

colnames(estimate120) <- c("mle1", "mle2", "map1", "map2", "ide1", "ide2")
colnames(estimate180) <- c("mle1", "mle2", "map1", "map2", "ide1", "ide2")
colnames(estimate240) <- c("mle1", "mle2", "map1", "map2", "ide1", "ide2")
colnames(estimatetot) <- c("mle1", "mle2", "map1", "map2", "ide1", "ide2")

head(estimate120)


library(GGally)
library(ggpubr)


est11 <- ggparcoord(data=estimate120, columns = c(1,3,5),scale="globalminmax", title = "ts120", showPoints = TRUE,alphaLines = .2) + xlab("") 
est21 <- ggparcoord(data=estimate180, columns = c(1,3,5),scale="globalminmax", title = "ts180", showPoints = TRUE,alphaLines = .2) + xlab("")
est31 <- ggparcoord(data=estimate240, columns = c(1,3,5),scale="globalminmax", title = "ts240", showPoints = TRUE,alphaLines = .2) + xlab("")

ggarrange(est11, est21, est31, ncol=3)


est12 <- ggparcoord(data=estimate120, columns = c(2,4,6),scale="globalminmax", title = "ts120", showPoints = TRUE,alphaLines = .2) + xlab("") 
est22 <- ggparcoord(data=estimate180, columns = c(2,4,6),scale="globalminmax", title = "ts180", showPoints = TRUE,alphaLines = .2) + xlab("")
est32 <- ggparcoord(data=estimate240, columns = c(2,4,6),scale="globalminmax", title = "ts240", showPoints = TRUE,alphaLines = .2) + xlab("")

ggarrange(est12, est22, est32, ncol=3)


##########

library(cowplot)

pairs11 <- ggpairs(estimate120[,c(1,3,5)], title = "ts120_var1", ggplot2::aes(alpha=.001))
pairs21 <- ggpairs(estimate180[,c(1,3,5)], title = "ts180_var1", ggplot2::aes(alpha=.001))
pairs31 <- ggpairs(estimate240[,c(1,3,5)], title = "ts240_var1", ggplot2::aes(alpha=.001))
pairs1 <- ggpairs(estimatetot[,c(1,3,5)], title = "total_var1", ggplot2::aes(alpha=.001))

plot_grid(
  ggmatrix_gtable(pairs11),
  ggmatrix_gtable(pairs21),
  ggmatrix_gtable(pairs31),
  ggmatrix_gtable(pairs1),
  ncol = 4
)


pairs12 <- ggpairs(estimate120[,c(2,4,6)], title = "ts120_var2", ggplot2::aes(alpha=.001))
pairs22 <- ggpairs(estimate180[,c(2,4,6)], title = "ts180_var2", ggplot2::aes(alpha=.001))
pairs32 <- ggpairs(estimate240[,c(2,4,6)], title = "ts240_var2", ggplot2::aes(alpha=.001))
pairs2 <- ggpairs(estimatetot[,c(2,4,6)], title = "total_var2", ggplot2::aes(alpha=.001))

plot_grid(
  ggmatrix_gtable(pairs12),
  ggmatrix_gtable(pairs22),
  ggmatrix_gtable(pairs32),
  ggmatrix_gtable(pairs2),
  ncol = 4
)



##############################################
#                                            #
#                                            #
#                   BREAK                    #
#                                            #
#                                            #
##############################################



