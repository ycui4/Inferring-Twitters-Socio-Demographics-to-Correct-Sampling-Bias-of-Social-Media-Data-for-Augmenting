library(dplyr)

Data<-read.csv('direction/UserID0.csv',header=T)

a<-filter(Data, followers_count<1000 & followers_count>0)
a<-filter(a, statuses_count>100)
write.csv(a, 'direction/UserID1.csv', row.names=F)