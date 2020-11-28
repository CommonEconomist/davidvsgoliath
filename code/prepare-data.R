#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Clean data: UCDP conflict event data
# Data source: http://ucdp.uu.se/ged/data.php
# File shoud be sourced
# Last update: 2018 04 27
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
load("data/ged171.Rdata")

library(plyr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Clean data
# Include single events with fatalities
ged<-ged171[ged171$event_clar==1 & ged171$best!=0,] #N=110,655

# Only include conflicts with at least 30 events
f<-ddply(data.frame(ged),.(conflict_1),summarise,n=length(conflict_1)) #N=940
d<-data.frame(ged[ged$conflict_1 %in% f$conflict_1[f$n>=30],]) #N=105,685

# Split Afghanistan conflict
d$conflict_1<-as.character(d$conflict_1)
d$start<-as.Date(as.character(d$date_start), format="%Y-%m-%d")
d$ISAF<-0
d[d$conflict_1=="Afghanistan:Government",]$ISAF<-as.numeric(
  d[d$conflict_1=="Afghanistan:Government",]$start>="2001-10-07")
d[d$ISAF==1,]$conflict_1<-"Afghanistan:Government (ISAF)"

# Split dataframe per conflict
x<-split(d$best,d$conflict_1) #N=273
rm(list=setdiff(ls(), "x"))

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Preparation for power law estimation
# Vectors/lists for results
conflict<-names(x)
l<-as.vector(do.call(rbind, lapply(x,length)))
N<-length(x)
M<-list()
B<-list()

xmin<-vector(mode="numeric", length=0)
alpha<-vector(mode="numeric", length=0)
ntail<-vector(mode="numeric", length=0)
p<-vector(mode="numeric", length=0)

## FIN