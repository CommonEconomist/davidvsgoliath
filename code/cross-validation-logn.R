#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Out-of-sample cross-validation
# Fatality ratios
# Log-normal distribution
# Last update: 2018 04 27
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
source('code/prepare-cv-data.R')
library(dplyr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Prepare output
load("output/results_ln.RData")  # Results fat tail distributions

# Matrix to data frame
lnorm.df<-data.frame(R.lnorm)

# Set Inf to NA
lnorm.df<-do.call(data.frame,lapply(lnorm.df,
                                    function(x) replace(x, is.infinite(x),NA)))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Cross-validation log-normal model
set.seed(42);seeds<-runif(L,0,1000000)

for(l in 1:L){
  
  # Extract samples
  set.seed(seeds[l]);is<-sample_n(lnorm.df,
                                  1/3*nrow(lnorm.df),replace=FALSE) # In-sample
  ous<-d.a[!(d.a$conflict_name %in% row.names(is)),] # Out-of-sample
  
  # Calculate 95% interval predicted ratio
  conf<-as.vector(lapply(is,quantile,probs=c(.025,.975),na.rm=TRUE))
  intv[l]<-as.vector(conf$X10.15[2]-conf$X10.15[1])/10
  
  # Calculate observed ratios in out-of-sample data
  M<-matrix(nrow=N,ncol=nrow(ous)) 
  M[1,]<-ous$p1/ous$p2
  M[2,]<-ous$p2/ous$p3
  M[3,]<-ous$p2/ous$p4
  M[4,]<-ous$p4/ous$p6
  M[5,]<-ous$p4/ous$p8
  M[6,]<-ous$p5/ous$p10
  M[7,]<-ous$p6/ous$p9
  M[8,]<-ous$p10/ous$p15
  M[9,]<-ous$p10/ous$p20
  M[10,]<-ous$p20/ous$p30
  M[11,]<-ous$p20/ous$p40
  M[12,]<-ous$p30/ous$p45
  M[13,]<-ous$p30/ous$p60
  M[14,]<-ous$p40/ous$p60
  M[15,]<-ous$p40/ous$p80
  M[16,]<-ous$p50/ous$p75
  M[17,]<-ous$p50/ous$p100
  M[18,]<-ous$p60/ous$p90
  
  # Calculate percentage within bounds
  Perc<-c()
  for(i in 1:N){
    Perc[i]=sum(as.numeric(M[i,]>=conf[[i]][1] & 
                             M[i,]<=conf[[i]][2]),na.rm=T)/
      sum(as.numeric(!is.na(M[i,])))*100
  }
  
  # Calculate error (for 10/15 ratio)
  a<-M[8,]-conf$X10.15[1]
  b<-M[8,]-conf$X10.15[2]
  
  error[l]<-sum(c(a^2,b^2),na.rm=TRUE) # Sum squared differences
  rmse[l]<-sqrt(mean((c(a^2,b^2)),na.rm=TRUE)) # Root mean squared error
  
  # Write results to matrix
  R[l,]<-Perc
  colnames(R)<-c("1/2","2/3","2/4","4/6","4/8","6/9","5/10",
                 "10/15","10/20","20/30","20/40","30/45","30/60",
                 "40/60","40/80","50/75","50/100","60/90")
}


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) plot results
par(oma=c(5,4.5,0,0),mar=c(0,0,1,1),las=1,las=0,mfrow=c(2,1),pty="m")

# Odd columns
boxplot(R[,seq(1,N,2)],horizontal=TRUE,axes=FALSE,na.action=rm,notch=TRUE,
        pars=list(ylim=c(0,100)))
axis(2,at=(.5*N):1,label=rev(colnames(R[,seq(1,N,2)])),
     las=1,tick=F,mgp=c(2,.6,0),cex.axis=1.5) 

# Even columns
boxplot(R[,seq(2,N,2)],horizontal=TRUE,axes=FALSE,na.action=rm,notch=TRUE,
        pars=list(ylim=c(0,100)))
axis(1,at=seq(0,100,5),tick=F,cex.axis=1.5)
mtext("Percentage of observations within predicted range",line=3,side=1,cex=1.7)
axis(2,at=(.5*N):1,label=rev(colnames(R[,seq(2,N,2)])),
     las=1,tick=F,mgp=c(2,.6,0),cex.axis=1.5) 


#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4) Compare results with power-law
# Figure: AH-score
load("output/rmse.pl.RData")

# Calculate AH-score
y1=1/rmse.pl
y2=1/rmse

# Plot
par(mar=c(5,5,3,2),pty="s",cex.lab=2,cex.axis=2,cex.main=2,
    bty="n",las=1,family="Arial")

plot(X.pl,y1,ylim=c(0,1),xlim=c(55,100),axes=FALSE,
     main="AH score",ylab="",xlab="Predictions within bounds (%)",
     col='steelblue4')
points(R[,8],y2,pch=0,col='black')
axis(1,tick=FALSE);axis(2,tick=FALSE)

## FIN