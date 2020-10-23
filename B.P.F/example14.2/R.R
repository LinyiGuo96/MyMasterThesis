#initial state
x<-0
y<-0
Vx=1
Vy=1
#simulate target moves
a<-rnorm(1000)
b<-rnorm(1000)
d<-rnorm(1000)
X<-c()
Y<-c()
Z<-c()
for (i in 1:100) {
  x=x+Vx
  y=y+Vy
  Vx=Vx+0.05*a[i]
  Vy=Vy+0.05*b[i]
  X<-c(X,x)
  Y<-c(Y,y)
  Z<-c(Z,atan(y/x)+0.01*d[i])
}
plot(X,Y,pch=1,cex=0.5)

s<-matrix(c(0.05^2,0,0,0.05^2),nrow=2)
v<-mvrnorm(n=1000,mu=c(1,1),Sigma=s)  # 1000 particles' velocity at t1
omega<-dmvnorm(v,mean=c(1,1),sigma=s) # 1000 particles' weights (before normalizing)
omega<-omega/sum(omega) # normalize 1000 particles' weights
position<-mvrnorm(n=1000,mu=c(0,0),Sigma = matrix(c(.5^2,0,0,.3^2),nrow=2)) # 1000 particles' position at t1

# I M P O R T A N C E    S A M P L I N G
# ´óµ¨µÄ»­Í¼£¡³¢ÊÔ£¡TRY IT!
library(MASS)
library(mvtnorm)
# stepwise importance sampling
# for each time t, plot (x_t,y_t) corresponding to the highest weight omega_t
v_stepwise<-v
omega_stepwise<-omega
position_stepwise<-position
position_stepwiseIS<-position_stepwise[which.max(omega_stepwise),]
max.seq<-c(which.max(omega_stepwise))

for (t in 2:100) 
  {for(i in 1:1000)
   {
    position_stepwise[i,]<-position_stepwise[i,]+v_stepwise[i,]
    omega_stepwise[i]<-omega_stepwise[i]*dnorm(Z[t],mean=atan(position_stepwise[i,2]/position_stepwise[i,1]),sd=0.01)
    v_stepwise[i,]<-mvrnorm(n=1,mu=v_stepwise[i,],Sigma = s)
  }
  omega_stepwise<-omega_stepwise/sum(omega_stepwise)
  max.seq<-c(max.seq,which.max(omega_stepwise))
  position_stepwiseIS<-rbind(position_stepwiseIS,position_stepwise[which.max(omega_stepwise),])
}

plot(X,Y,pch=1,cex=0.5,xlim=c(0,120),ylim=c(0,130))
par(new=T)
plot(position_stepwiseIS[,1],position_stepwiseIS[,2],xlim=c(0,120),ylim=c(0,130),ty='l',xlab='',ylab='')


#final importance sampling
#plot the path (x_t,y_t) whose w_T is the highest weight at the end of the period.
v_final<-v
omega_final<-omega
omega_final<-omega_final/sum(omega_final)
position_final<-position
position_finalc<-position
which.max(omega_final)
for(t in 2:100)
{for(i in 1:1000)
{
  position_final[i,]<-position_final[i,]+v_final[i,]
  omega_final[i]<-omega_final[i]*dnorm(Z[t],mean=atan(position_final[i,2]/position_final[i,1]),sd=0.01)
  v_final[i,]<-mvrnorm(n=1,mu=v_final[i,],Sigma = s)
}
  omega_final<-omega_final/sum(omega_final)
  position_finalc<-cbind(position_finalc,position_final)
}

#find the trajectory of the point whose weight is the largest in the end.
finalpoint_x<-c()
  for(i in 1:100){finalpoint_x<-c(finalpoint_x,position_finalc[which.max(omega_final),2*i-1])}
finalpoint_y<-c()
  for(i in 1:100){finalpoint_y<-c(finalpoint_y,position_finalc[which.max(omega_final),2*i])}

plot(X,Y,ty='l',xlim=c(0,120),ylim=c(0,130),lwd=2)
par(new=T)
plot(position_stepwiseIS[,1],position_stepwiseIS[,2],xlim=c(0,120),ylim=c(0,130),ty='l',col='red',xlab='',ylab='')
par(new=T)
plot(finalpoint_x,finalpoint_y,ty='l',col='blue',xlim=c(0,120),ylim=c(0,130),xlab='',ylab='')




# P A R T I C L E   F I L T E R  
# stepwise
position_pfstepwise<-position
v_pfstepwise<-v
omega_pfstepwise<-omega/sum(omega)
position_pfs<-position_pfstepwise[which.max(omega_pfstepwise),]

for(t in 2:100)
{for(i in 1:1000)
{
  position_pfstepwise[i,]<-position_pfstepwise[i,]+v_pfstepwise[i,]
  omega_pfstepwise[i]<-omega_pfstepwise[i]*dnorm(Z[t],
                                        mean=atan(position_pfstepwise[i,2]/position_pfstepwise[i,1]),sd=0.01)
  v_pfstepwise[i,]<-mvrnorm(n=1,mu=v_pfstepwise[i,],Sigma = s)
}
  position_pfs<-rbind(position_pfs,position_pfstepwise[which.max(omega_pfstepwise),])
  order<-sample(1:1000,1000,replace=T,prob=omega_pfstepwise)
  position_pfstepwise<-position_pfstepwise[order,]
  v_pfstepwise<-v_pfstepwise[order,]
  omega_pfstepwise<-c(rep(0.001,1000))
}

# compare the results of importance sampling and particle filter (stepwise)
par(mfrow=c(1,2))
plot(X,Y,ty='l',xlim=c(0,120),ylim=c(0,130),lwd=2)
par(new=T)
plot(position_pfs[,1],position_pfs[,2],ty='l',col='red',xlim=c(0,120),ylim=c(0,130),xlab='',ylab='')

plot(X,Y,ty='l',xlim=c(0,120),ylim=c(0,130),lwd=2)
par(new=T)
plot(position_stepwiseIS[,1],position_stepwiseIS[,2],xlim=c(0,120),ylim=c(0,130),ty='l',col='red',xlab='',ylab='')


# final particle filter
v_pffinal<-v
position_pffinal<-position
omega_pffinal<-omega/sum(omega)
position_pff<-position_pffinal

for (t in 2:100) 
  {for (i in 1:1000)
  {
    position_pffinal[i,]<-position_pffinal[i,]+v_pffinal[i,]
    omega_pffinal[i]<-omega_pffinal[i]*dnorm(Z[t],mean=atan(position_pffinal[i,2]/position_pffinal[i,1]),sd=0.01)
    v_pffinal[i,]<-mvrnorm(n=1,mu=v_pffinal[i,],Sigma = s)#v_pfstepwise[i,]<-mvrnorm(n=1,mu=v_pfstepwise[i,],Sigma = s)
  }
  position_pff<-cbind(position_pff,position_pffinal) 
  order<-sample(1:1000,1000,replace=T,prob=omega_pffinal)
  position_pffinal<-position_pffinal[order,]
  v_pffinal<-v_pffinal[order,]
  omega_pffinal<-c(rep(.001,1000))
}

# pick up the trajectory we need
pffinalpoint_x<-c()
for (i in 1:100) pffinalpoint_x<-c(pffinalpoint_x,position_pff[which.max(omega_pffinal),2*i-1])
pffinalpoint_y<-c()
for (i in 1:100) pffinalpoint_y<-c(pffinalpoint_y,position_pff[which.max(omega_pffinal),2*i])

#compare the results of importance sampling and particle filter
par(mfrow=c(1,2))
plot(X,Y,ty='l',xlim=c(0,120),ylim=c(0,130),lwd=2)
par(new=T)
plot(pffinalpoint_x,pffinalpoint_y,ty='l',col='blue',xlim=c(0,120),ylim=c(0,130),xlab='',ylab='')
par(new=T)
plot(position_pfs[,1],position_pfs[,2],ty='l',col='red',xlim=c(0,120),ylim=c(0,130),xlab='',ylab='')

plot(X,Y,ty='l',xlim=c(0,120),ylim=c(0,130),lwd=2)
par(new=T)
plot(position_stepwiseIS[,1],position_stepwiseIS[,2],xlim=c(0,120),ylim=c(0,130),ty='l',col='red',xlab='',ylab='')
par(new=T)
plot(finalpoint_x,finalpoint_y,ty='l',col='blue',xlim=c(0,120),ylim=c(0,130),xlab='',ylab='')

save.image()
quit()


