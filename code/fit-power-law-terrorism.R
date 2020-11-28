#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Analysis: fit power law to data
# Terrorism data
# Last update: 2018 06 11
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
source('code/prepare-data-terrorism.R')
library(poweRlaw)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Fit power law
# NB - this can take some time: ~ 1 hour
for(i in 1:N){
  print(i)
  
  # Create power law environment
  pl=displ$new(x[[i]]) 
  
  # Bootstrap estimates
  bts=bootstrap_p(pl,no_of_sims=1000,seed=42,threads=3)
  
  # Write results to vectors
  M[[i]]<-pl
  B[[i]]<-bts
  
  alpha[i]<-mean(bts$bootstraps$pars,na.rm=T)
  xmin[i]<-mean(bts$bootstraps$xmin,na.rm=T)
  ntail[i]<-mean(bts$bootstraps$ntail,na.rm=T)
  p[i]<-bts$p
  
  print(c(alpha[i],xmin[i]))
  
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Save results
r<-data.frame(group_name=group,N=l,alpha=alpha,xmin=xmin,p=p,tail=ntail)
write.csv(r,file="output/fitted_values_terrorism.csv",row.names=FALSE)
save.image(file='output/results_terrorism.RData')

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Calculate UI
e<-l<-u<-c()
for(i in 1:N){
  e[i]<-sd(B[[i]]$bootstraps$pars,na.rm=T)
  l[i]<-quantile(B[[i]]$bootstraps$pars,probs=.25,na.rm=T)
  u[i]<-quantile(B[[i]]$bootstraps$pars,probs=.75,na.rm=T)
}

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4) Plot figure
par(mar=c(5,5,.5,.5),las=1,pty="m",cex.lab=2,cex.axis=2) # Settings

# Empty plot
plot(p,alpha,xlim=c(0,1),ylim=c(round(min(l),digits=2),round(max(u),digits=2)),
     type="n",bty="n",xlab="",ylab="",axes=FALSE)

# Data and trimmings
rect(0,0,.05,ceiling(max(u)),col="grey90",lwd=0,border=NA) # p<0.05
abline(h=2.5,lty=2,lwd=2,col="black")           # Theoretical alpha-value
segments(p,u,p,l,lwd=.8)
points(p,alpha,cex=1.2,pch=4)             # Results

# Axis
axis(1,tick=F,cex.axis=1.2)
mtext("Goodness-of-fit p",1,cex=1.7,line=3)
minimalrug(p,side=1,line=-.8,lwd=2)

axis(2,tick=F,cex.axis=1.2)
mtext("Estimated power law coefficient",2,cex=1.7,line=3,las=0)
minimalrug(alpha,side=2,line=-.8,lwd=2) 

## FIN