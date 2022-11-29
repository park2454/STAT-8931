set.seed(1992)

# scenario 1
pdf("scenario.pdf",12,8)
par(mfrow=c(2,2))
xt = 1:150
yt = c(rnorm(50,0), rnorm(50,2), rnorm(50,1))
plot(xt,yt, main="piecewise const. mean, const. var", xlab="t",ylab="y_t")
lines(c(0,50),c(0,0), lwd=2, col="blue")
lines(c(50,100),c(2,2), lwd=2, col="blue")
lines(c(100,150),c(1,1), lwd=2, col="blue")

# scenario 2
xt = 1:150
yt = c(rnorm(50,seq(0,2,length=50)), 
       rnorm(50,seq(2,-1,length=50)),
       rnorm(50,seq(-1,3,length=50)))
plot(xt,yt, main="piecewise linear mean (cont.), const. var", xlab="t",ylab="y_t")
lines(c(0,50),c(0,2), lwd=2, col="blue")
lines(c(50,100),c(2,-1), lwd=2, col="blue")
lines(c(100,150),c(-1,3), lwd=2, col="blue")

# scenario 3
xt = 1:150
yt = c(rnorm(50,seq(0,2,length=50)), 
       rnorm(50,seq(1,-1,length=50)),
       rnorm(50,seq(1,3,length=50)))
plot(xt,yt, , main="piecewise const. mean (discont.), const. var", xlab="t",ylab="y_t")
lines(c(0,50),c(0,2), lwd=2, col="blue")
lines(c(50,100),c(1,-1), lwd=2, col="blue")
lines(c(100,150),c(1,3), lwd=2, col="blue")

# scenario 4
xt = 1:150
yt = c(rnorm(50,0,1), 
       rnorm(50,2,2),
       rnorm(50,1,1/2))
plot(xt,yt, main="piecewise const. mean and var", xlab="t",ylab="y_t")
lines(c(0,50),c(0,0), lwd=2, col="blue")
lines(c(50,100),c(2,2), lwd=2, col="blue")
lines(c(100,150),c(1,1), lwd=2, col="blue")
dev.off()

# binary segmentation
xt = 1:100
yt = c(rnorm(40,2),
       rnorm(60,4))
yt2 = c(rnorm(40,2),
       rnorm(20,5),
       rnorm(20,4),
       rnorm(20,3))
pdf("bs.pdf",12,4)
par(mfrow=c(1,2))
plot(xt,yt, main="single change point", xlab="t",ylab="y_t")
lines(c(0,40),c(2,2), lwd=2, col="blue")
lines(c(40,100),c(4,4), lwd=2, col="blue")
abline(v=40)
plot(xt,yt2, main="multiple change point", xlab="t",ylab="y_t")
lines(c(0,40),c(2,2), lwd=2, col="blue")
lines(c(40,60),c(5,5), lwd=2, col="blue")
lines(c(60,80),c(4,4), lwd=2, col="blue")
lines(c(80,100),c(3,3), lwd=2, col="blue")
abline(v=40)
dev.off()

cusum = function(x){
  T = length(x)
  res = rep(0,T-1)
  for (t in 2:T-1){
    pre = mean(x[1:t])
    post = mean(x[-(1:t)])
    res[t] = sqrt(t*(T-t))*abs(pre-post)
  }
  return(res)
}

l = list(
  1:60,
  21:60,
  41:80,
  61:100,
  41:100
)
s = list()
M = 0
for(i in 1:5){
  s[[i]] = cusum(yt2[l[[i]]])
  M = max(M,max(s[[i]]))
}

pdf("wbs.pdf",12,8)
par(mfrow=c(2,1))
plot(xt,yt2, xlab="t",ylab="y_t")
lines(c(0,40),c(2,2), lwd=2, col="blue")
lines(c(40,60),c(5,5), lwd=2, col="blue")
lines(c(60,80),c(4,4), lwd=2, col="blue")
lines(c(80,100),c(3,3), lwd=2, col="blue")
abline(v=20*1:4,lty=2)

plot(0,type="n",xlim=c(1,100),ylim=c(0,M), ylab="C_(s,e)^b",xlab="t")
for(i in 1:5){
  lines(l[[i]][-length(l[[i]])],s[[i]], col=ifelse(i==1,"red","black"))
}
abline(v=20*1:4,lty=2)
dev.off()

pdf("wbs2.pdf",12,4)
plot(0,type="n",xlim=c(1,100),ylim=c(0,M), ylab="C_(s,e)^b",xlab="t")
for(i in 1:5){
  lines(l[[i]][-length(l[[i]])],s[[i]], col=ifelse(i==1,"red","grey"))
}
points(which.max(s[[1]]), max(s[[1]]),col='red',cex=1)
dev.off()

pdf("wbs3.pdf",12,4)
par(mfrow=c(1,2))
plot(0,type="n",xlim=c(1,100),ylim=c(0,M), ylab="C_(s,e)^b",xlab="t")
for(i in 1:5){
  lines(l[[i]][-length(l[[i]])],s[[i]], col=ifelse(i==1,"red","black"))
}
abline(v=20*1:4,lty=2)

plot(xt,yt2, xlab="t",ylab="y_t")
lines(c(0,40),c(2,2), lwd=2, col="blue")
lines(c(40,60),c(5,5), lwd=2, col="blue")
lines(c(60,80),c(4,4), lwd=2, col="blue")
lines(c(80,100),c(3,3), lwd=2, col="blue")
abline(v=20*1:4,lty=2)
dev.off()

pdf("example.pdf",12,4)
xt = 1:1000
yt = c(rnorm(350,seq(1/350,1,length=350),1/20), 
       rnorm(300,seq(1,1,length=300),1/20),
       rnorm(350,seq(1,1/350,length=350),1/20))
plot(xt,yt, xlab="t",ylab="y_t",cex=0.1)
lines(c(0,350),c(1/350,1), lwd=2, col="blue")
lines(c(350,650),c(1,1), lwd=2, col="blue")
lines(c(650,1000),c(1,1/350), lwd=2, col="blue")
dev.off()

library(not)
pdf("heavy_tailed.pdf",12,8)
par(mfrow=c(3,2))
for(df in 3:1){
  y = rep(1:10,each=500) + rt(5000,df)
  plot(not(y, contrast = "pcwsConstMean"))
  plot(not(y, contrast = "pcwsConstMeanHT"))
}
dev.off()

pdf("heavy_tailed_transform.pdf",12,8)
par(mfrow=c(1,2))
y = rep(1:2,each=500) + rt(1000,3)
plot(1:1000,y,type="l")
lines(c(0,500),c(1,1), lwd=2, col="blue")
lines(c(500,1000),c(2,2), lwd=2, col="blue")
newy = ifelse((y-mean(y))>0,2,1)
plot(1:1000,newy,type="p", cex=0.1)
dev.off()

res = array(NA,c(100,2,3))
for(df in 3:1){
  for(iter in 1:100){
    y = rep(1:10,each=500) + rt(5000,df)
    fit = not(y, contrast = "pcwsConstMean")
    res[iter,1,df] = length(features(fit,method="ic",penalty = "sic")$cpt)
    fit = not(y, contrast = "pcwsConstMeanHT")
    res[iter,2,df] = length(features(fit,method="ic",penalty = "sic")$cpt)
  }
}

pdf("freq.pdf",12,8)
par(mfrow=c(3,2))
for(df in 3:1){
  barplot(table(res[,1,df]),xlab=expression(hat(q)),ylab="count")
  barplot(table(res[,2,df]),xlab=expression(hat(q)),ylab="count")
}
dev.off()



