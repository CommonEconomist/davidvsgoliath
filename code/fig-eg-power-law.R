#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Example power law distributions
# Last update: 2018 06 20
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
library(poweRlaw)

load('output/results.RData')

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Estimate power laws
# Israel:Palestine; Myanmar:Karen; Nepal; Rwanda
m1<-displ$new(x[[151]])
est=estimate_xmin(m1)
m1$setXmin(est)

m2<-displ$new(x[[191]])
est=estimate_xmin(m2)
m2$setXmin(est)

m3<-displ$new(x[[198]])
est=estimate_xmin(m3)
m3$setXmin(est)

m4<-displ$new(x[[230]])
est=estimate_xmin(m4)
m4$setXmin(est)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Plot results
par(mar=c(5,5,2,2),las=1,bty='n',cex.lab=2,cex.axis=2,cex.main=2,mfrow=c(2,2))

plot(m1,axes=F,main='Israel:Palestine',xlab='',
     ylab=expression(paste("Pr (",'X'>='x',")")))
lines(m1,col='firebrick3')
axis(2,tick=F,line=-2,at=c(.001,.01,.1,1),label=c(.001,.01,.1,1))
axis(1,tick=F,line=-1,at=c(1,10,100),label=c(1,10,100))
text(100,.2,c('N=1989 \n alpha=2.6 \n xmin=4'),cex=1.5)

plot(m2,axes=F,main='Myanmar:Karen',xlab='',ylab='')
lines(m2,col='firebrick3')
axis(1,tick=F,line=-1,at=c(1,10,100),label=c(1,10,100))
text(100,.2,c('N=314 \n alpha=2.0 \n xmin=4'),cex=1.5)

plot(m3,axes=F,main='Nepal',xlab="Event size x",
     ylab=expression(paste("Pr (",'X'>='x',")")))
lines(m3,col='firebrick3')
axis(2,tick=F,line=-2,at=c(.001,.01,.1,1),label=c(.001,.01,.1,1))
axis(1,tick=F,line=-1,at=c(1,10,100),label=c(1,10,100))
text(100,.2,c('N=3243 \n alpha=2.3 \n xmin=3'),cex=1.5)

plot(m4,axes=F,main='Rwanda',xlab="Event size x",ylab='')
lines(m4,col='firebrick3')
axis(1,tick=F,line=-1,at=c(1,10,100),label=c(1,10,100))
text(100,.2,c('N=255 \n alpha=2.3 \n xmin=26'),cex=1.5)

## FIN