#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Analysis: fit power law to data
# Last update: 2018 04 27
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
source('code/prepare-data.R') # Need to run twice sometimes
library(poweRlaw)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Fit power law
# NB - this can take some time: ~6 hours
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
r<-data.frame(conflict_name=conflict,N=l,alpha=alpha,xmin=xmin,p=p,tail=ntail)
write.csv(r,file="output/fitted-values.csv",row.names=FALSE)
save.image(file='output/results.RData')

sessionInfo()

## FIN