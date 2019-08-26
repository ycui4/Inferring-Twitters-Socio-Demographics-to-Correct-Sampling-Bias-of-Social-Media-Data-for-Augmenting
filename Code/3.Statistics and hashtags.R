library(dplyr)
file1<-read.csv('direction/TF1-1500.csv', header=T) 
file1$is_tweet_get<-NA
file1$numberOfTweetGet<-NA
file1$numberOfGeoTweet<-NA
file02<-'direction/TF Timeline csv/'

hashtag<-c()


f1<-function(x){
  a1<-strsplit(x, "<")[[1]][2]
  a2<-strsplit(a1,">")[[1]][2]
  return(a2)
}

for(i in 1:nrow(file1)){
  file2<-paste0(file02,file1$UserID[i],'.csv')
  if(file.exists(file2)){
    Data<-read.csv(file2, header=T)
    if(nrow(Data)>0){
      Data$source<-as.character(Data$source)
      Data$source<-matrix(unlist(lapply(Data$source, f1)), ncol=1)
      
      Data$hashtag1<-as.character(Data$hashtag1)
      Data$hashtag2<-as.character(Data$hashtag2)
      hashtag<-unique(c(hashtag, Data$hashtag1))
      hashtag<-unique(c(hashtag, Data$hashtag2))
      
      file1$is_tweet_get[i]<-'true'
      file1$numberOfTweetGet[i]<-nrow(Data)
      file1$numberOfGeoTweet[i]<-length(which(!is.na(Data$lat)))
      
    }else{
      file1$is_tweet_get[i]<-'false'
    }
    
  }
  
  cat(i, '\n')
}

file3<-filter(file1, is_tweet_get=='true')
hashtag<-hashtag[-1]

write.csv(file1, '/Users/yu/Documents/Study/Project/Twitter Demographic/Work/20180308 Facebook Labeling/TF1-1500.csv', row.names=FALSE)
write.csv(matrix(hashtag,ncol=1), '/Users/yu/Documents/Study/Project/Twitter Demographic/Work/20180308 Facebook Labeling/hashtag.csv', row.names = FALSE)













