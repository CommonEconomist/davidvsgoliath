#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Out-of-sample cross-validation
# Fatality ratios
# Last update: 2018 04 27
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
source('code/prepare-cv-data.R')
library(dplyr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1.1) Cross-validation
set.seed(42);seeds<-runif(L,0,1000000) # This needs to be improved

for(l in 1:L){
  # Extract sample
  set.seed(seeds[l]);is<-sample_n(samp,1/3*nrow(samp),
                                  replace=FALSE)        # In sample
  ous<-samp[!(samp$conflict_name %in% is$conflict_name),] # Out-of-sample
  conf<-as.vector(quantile(is$alpha-1, # Switch to distribution function: subtract 1
                           probs=(1+c(-1,1)*0.95)/2))   # Range of alphas
  
  
  # Calculate predicted ratios 
  for(i in 1:N){
    A[i]=(t2[i]/t1[i])^conf[1]
    B[i]=(t2[i]/t1[i])^conf[2]
  }
  
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
    Perc[i]=sum(as.numeric(M[i,]>=A[i] & 
                             M[i,]<=B[i]),na.rm=T)/
      sum(as.numeric(!is.na(M[i,])))*100
  }
  
  # Calculate error (for 10/15 ratio)
  a<-M[8,]-A[8]
  b<-M[8,]-B[8]
  
  error[l]<-sum(c(a^2,b^2),na.rm=TRUE) # Sum squared differences
  rmse[l]<-sqrt(mean((c(a^2,b^2)),na.rm=TRUE)) # Root mean squared error
  
  # Write results to matrix
  R[l,]<-Perc
  colnames(R)<-c("1/2","2/3","2/4","4/6","4/8","6/9","5/10",
                 "10/15","10/20","20/30","20/40","30/45","30/60",
                 "40/60","40/80","50/75","50/100","60/90")
}

# Save output
X.pl<-R[,8]
rmse.pl<-rmse
save(list=c("X.pl","rmse.pl"),file="output/rmse.pl.RData")

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1.2) Plot results: boxplot
par(oma=c(5,5,3,0),mar=c(0,0,2,1),las=1,las=0,mfrow=c(2,1),pty="m",
    family="Arial")

# Odd columns
boxplot(R[,seq(1,N,2)],horizontal=TRUE,axes=FALSE,na.action=rm,notch=TRUE,
        pars=list(ylim=c(0,100)),main='(a)')
axis(2,at=(.5*N):1,label=rev(colnames(R[,seq(1,N,2)])),
     tick=F,mgp=c(2,.6,0),cex.axis=2,line=-2,las=1) 

# Even columns
boxplot(R[,seq(2,N,2)],horizontal=TRUE,axes=FALSE,na.action=rm,notch=TRUE,
        pars=list(ylim=c(0,100)),main='(b)')
axis(1,at=seq(0,100,5),tick=F,cex.axis=2)
mtext("Percentage of observations within predicted range",line=3,side=1,cex=2)
axis(2,at=(.5*N):1,label=rev(colnames(R[,seq(2,N,2)])),
     las=1,tick=F,mgp=c(2,.6,0),cex.axis=2,line=-2) 

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Compare with in-sample
is<-samp

# Matrix with fatality ratios
M<-matrix(nrow=18,ncol=nrow(is)) 
M[1,]<-is$p1/is$p2
M[2,]<-is$p2/is$p3
M[3,]<-is$p2/is$p4
M[4,]<-is$p4/is$p6
M[5,]<-is$p4/is$p8
M[6,]<-is$p5/is$p10
M[7,]<-is$p6/is$p9
M[8,]<-is$p10/is$p15
M[9,]<-is$p10/is$p20
M[10,]<-is$p20/is$p30
M[11,]<-is$p20/is$p40
M[12,]<-is$p30/is$p45
M[13,]<-is$p30/is$p60
M[14,]<-is$p40/is$p60
M[15,]<-is$p40/is$p80
M[16,]<-is$p50/is$p75
M[17,]<-is$p50/is$p100
M[18,]<-is$p60/is$p90

# Calculate percentage within bounds
Perc<-c()
for(i in 1:18){
  if (i %% 2==0){
    Perc[i]=sum(as.numeric(M[i,]>=1.2 & M[i,]<=3.5),na.rm=T)/
      sum(as.numeric(!is.na(M[i,])))*100
  }else{
    Perc[i]=sum(as.numeric(M[i,]>=1.4 & M[i,]<=8.6),na.rm=T)/
      sum(as.numeric(!is.na(M[i,])))*100
  }
}

# Prepare data for plot
Q<-apply(R,2,quantile)
Q1<-as.vector(Q[2,])
Q3<-as.vector(Q[4,])
ratios<-c("1/2","2/3","2/4","4/6","4/8","6/9","5/10",
          "10/15","10/20","20/30","20/40","30/45","30/60",
          "40/60","40/80","50/75","50/100","60/90")
odd<-seq(1,18,2)
even<-seq(2,18,2)
dev.off()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2.1) Plot results
par(mar=c(5,5,2,2),las=1,cex.lab=1.5,mfrow=c(2,1),family="Arial")

# panel a
plot(Perc[odd],ylim=c(0,100),type="n",axes=FALSE,
     xlab="",ylab="Percentage in range")
polygon(c(rev(1:9),1:9),c(rev(Q1[odd]),Q3[odd]),
        col="grey90",border=NA)
lines(1:9,Perc[odd],lwd=2,type="b",pch=19)
axis(2,tick=FALSE)
axis(1,tick=FALSE,labels=ratios[odd],at=1:9)

# Labels
text(4,40,'Sample',cex=1.2)
text(4,90,'Out-of-sample',cex=1.2)
segments(4,45,4,55,lwd=2)
segments(4,76,4,85,lwd=2)


# panel b
plot(Perc[even],ylim=c(0,100),type="n",axes=FALSE,
     xlab="Fatality ratios (binned)",ylab="Percentage in range")
polygon(c(rev(1:9),1:9),c(rev(Q1[even]),Q3[even]),
        col="grey90",border = NA)
lines(1:9,Perc[even],lwd=2,type="b",pch=19)
axis(2,tick=FALSE)
axis(1,tick=FALSE,labels=ratios[even],at=1:9)

## FIN