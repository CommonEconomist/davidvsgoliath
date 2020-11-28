#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Figure results
# Last update: 2018 06 19
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
load('output/results.RData')

# Libraries
library(devtools)
library(extrafont)
library(plyr)

source_url("https://raw.githubusercontent.com/sjmurdoch/fancyaxis/master/fancyaxis.R")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Preparation
# Need raw data for conflict type
load("data/ged171.Rdata")
vi<-ddply(data.frame(ged171),.(conflict_1),summarise,type=mean(type_of_vi))
type<-vi$type[vi$conflict_1 %in% conflict]
type<-c(type[1:3],1,type[-1:-3]) # Accounting for Afghanistan split

# Type vector
k<-type
k<-replace(k,k==1,'black')
k<-replace(k,k==2,'steelblue4')
k<-replace(k,k==3,'firebrick3')

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
e<-l<-u<-c()
for(i in 1:N){
  e[i]<-sd(B[[i]]$bootstraps$pars,na.rm=T)
  l[i]<-quantile(B[[i]]$bootstraps$pars,probs=.25,na.rm=T)
  u[i]<-quantile(B[[i]]$bootstraps$pars,probs=.75,na.rm=T)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Plot figure
par(mar=c(5,5,.5,.5),las=1,pty="m",cex.lab=2,cex.axis=2,
    family='arial') # Settings

# Empty plot
plot(p,alpha,xlim=c(0,1),ylim=c(round(min(l),digits=2),round(max(u),digits=2)),
     type="n",bty="n",xlab="",ylab="",axes=FALSE)

# Data and trimmings
rect(0,0,.05,ceiling(max(u)),col="grey90",lwd=0,border=NA) # p<0.05
abline(h=2.5,lty=2,lwd=2,col="black")           # Theoretical alpha-value
segments(p,u,p,l,lwd=.8)
points(p,alpha,cex=1.2,pch=type+15,col=k)       # Results
#text(x+.025,y,lbl)                             # Labels

# Axis
axis(1,tick=F,cex.axis=1.2)
mtext("Goodness-of-fit p",1,cex=1.7,line=3)
minimalrug(p,side=1,line=-.8,lwd=2)

axis(2,tick=F,cex.axis=1.2)
mtext("Estimated power law coefficient",2,cex=1.7,line=3,las=0)
minimalrug(alpha,side=2,line=-.8,lwd=2) 
legend(-.05,6.5,cex=1.5,pch=c(16,17,18),col=c('black','steelblue4','firebrick3'),
       pt.cex=2,bty='n',y.intersp=.5,
       c('Civil conflict','Communal violence','Violence against civilians'))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Statistics
mean(alpha);median(alpha);sd(alpha)
sum(p<.05)

quantile(alpha,probs=(1+c(-1,1)*0.95)/2)

mean(unlist(x));sd(unlist(x))
median(unlist(x))
mean(l);median(l)

## FIN