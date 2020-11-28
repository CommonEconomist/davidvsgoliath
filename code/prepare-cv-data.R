#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# Prepare data
# Calculate fatality ratios in conflict data
#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
setwd('~/github/replication-data/david-vs-goliath')
load('data/ged171.Rdata')
library(plyr)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 1) Clean data
# Same steps as with data for estimation

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

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 2) Calculate event ratios ####
# NB- There is probably some way to automate this that I haven't found out yet.
d$p1<-as.numeric(d$best>=1)
d$p2<-as.numeric(d$best>=2)
d$p3<-as.numeric(d$best>=3)
d$p4<-as.numeric(d$best>=4)
d$p5<-as.numeric(d$best>=5)
d$p6<-as.numeric(d$best>=6)
d$p8<-as.numeric(d$best>=8)
d$p9<-as.numeric(d$best>=9)
d$p10<-as.numeric(d$best>=10)
d$p15<-as.numeric(d$best>=15)
d$p20<-as.numeric(d$best>=20)
d$p30<-as.numeric(d$best>=30)
d$p40<-as.numeric(d$best>=40)
d$p45<-as.numeric(d$best>=45)
d$p50<-as.numeric(d$best>=50)
d$p60<-as.numeric(d$best>=60)
d$p75<-as.numeric(d$best>=75)
d$p80<-as.numeric(d$best>=80)
d$p90<-as.numeric(d$best>=90)
d$p100<-as.numeric(d$best>=100)

# Aggregate ratios
d.a<-aggregate(cbind(p1,p2,p3,p4,p6,p8,p9,p5,
                     p10,p15,p20,p30,p40,p45,
                     p60,p80,p50,p75,p100,p90)~conflict_1,d,sum,na.rm=T)
d.a[d.a==0]<-NA

# Housekeeping
rm(ged,ged171,f,d)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 3) Load output and merge
load('output/results.RData')
colnames(d.a)[1]<-"conflict_name"
samp<-merge(d.a,r)

#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#-#
# 4) Vectors for results etc. 
# Vectors
t1<-c(1,2,2,4,4,5,6,10,10,20,20,30,30,40,40,50,50,60)
t2<-c(2,3,4,6,8,10,9,15,20,30,40,45,60,60,80,75,100,90)
N<-length(t1)
A<-c()
B<-c()

L<-1000 # Repetitions
R<-matrix(nrow=L,ncol=N) # Matrix for results
intv<-c()
rmse<-c()
error<-c()

## FIN