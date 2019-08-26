library(dplyr)

Data<-read.csv('direction/SF_TWS_COR_UNTIL20170216.csv',header=T)
User<-data.frame(table(Data$USER_ID))
User<-User[order(User$Freq, decreasing = T),]
colnames(User)<-c('UserID', 'Freq')
write.csv(User, 'direction/20180205 Potential Users/UserID.csv',row.names=FALSE)







