#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Robustness check: log-normal distribution
# Last update: 2018 04 27 
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
source('code/prepare-data.R')
library(poweRlaw)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Misc. preparation
# Ratios
t1<-c(1,2,2,4,4,5,6,10,10,20,20,30,30,40,40,50,50,60)
t2<-c(2,3,4,6,8,10,9,15,20,30,40,45,60,60,80,75,100,90)
K<-length(t1)
R.lnorm<-matrix(nrow=N,ncol=K)
R.exp<-matrix(nrow=N,ncol=K)

# Lists and vectors for model output
lnorm.dist<-list()
lnorm.est<-list()

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Fit log-normal distribution to data 
for(i in 1:N){
  print(c(i,conflict[i]))
  
  # Log-normal distribution
  lnorm.dist[[i]]=dislnorm$new(x[[i]])
  lnorm.est[[i]]=estimate_xmin(lnorm.dist[[i]])
  
  # Calculate CDF ratios
  m=lnorm.dist[[i]]
  m$setXmin(lnorm.est[[i]]$xmin)
  m$setPars(lnorm.est[[i]]$pars)
  
  for(k in 1:K){
    R.lnorm[i,k]=(1-dist_cdf(m,t1[k]))/(1-dist_cdf(m,t2[k]))
  }
}

# Give names
row.names(R.lnorm)=conflict
colnames(R.lnorm)=paste(t1,t2,sep="/")

## Save results
save(R.lnorm,file="output/results_ln.RData")

## FIN