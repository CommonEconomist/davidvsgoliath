#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Clean terrorism data
# Data source: http://www.start.umd.edu/gtd/
# See also:
# http://fivethirtyeight.com/features/the-paris-attacks-are-just-a-few-of-125000-entries-in-the-global-terrorism-database/
# http://fivethirtyeight.com/features/global-terrorism-declined-last-year-but-not-in-the-west/
# File should be sourced
# Last update: 2018 04 30
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
load('data/globalterrorismdb_0617dist.RData')

library(plyr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Clean data
# Subset data: fatal events after 1997, certain acts of terrorism
gtd<-gtd[!is.na(gtd$nkill) & gtd$nkill>=1,]  
gtd<-gtd[gtd$iyear>1997 & gtd$doubtterr==0,] # N=42,776

# Only include groups with at least 30 events
f<-ddply(gtd,.(gname),summarise,n=length(gname)) # N=764
d<-data.frame(gtd[gtd$gname %in% f$gname[f$n>=30],]) # N=40,006

# Remove generic group names
d<-d[!(d$gname %in% c("Algerian Islamic Extremists","Chechen Rebels","Gunmen",
                      "Hutu extremists","Militants","Maoists",
                      "Muslim Fundamentalists","Muslim extremists","Other",
                      "Palestinian Extremists","Separatists",
                      "Unaffiliated Individual(s)","Unknown")),] # N = 16,399
d$gname<-factor(d$gname)

# Split dataframe per group
d$nkill<-round(d$nkill,digits=0) # For security
x<-split(d$nkill,factor(d$gname)) # 60 groups

#------------------------------------------------------------------------------
#### 2) Data for power law estimations ####
# Vectors/lists for results
group<-names(x)
l<-as.vector(do.call(rbind, lapply(x,length)))
N<-length(x)
M<-list()
B<-list()

xmin<-vector(mode="numeric", length=0)
alpha<-vector(mode="numeric", length=0)
ntail<-vector(mode="numeric", length=0)
p<-vector(mode="numeric", length=0)

## FIN