library(dplyr)

file01<-read.csv('direction/TF1-1500.csv', header=T) 
file1<-filter(file01, is_tweet_get=='true'&!is.na(Gender))
file2<-read.csv('direction/PlantText.csv', header=T)

#### Gender ###
M<-file2[which(file1$Gender=='Male'),]
M$plantText<-tolower(M$plantText)
Fe<-file2[which(file1$Gender=='Female'),]
Fe$plantText<-tolower(Fe$plantText)

write.table(M$plantText, 'direction/MaleText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')
write.table(Fe$plantText, 'direction/FemaleText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')



a<-paste(file2$plantText, collapse = " ")
b<-unlist(strsplit(a, " "))
c<-data.frame(table(b))
c<-c[order(c$Freq, decreasing = T), ]

c1<-filter(c, Freq>100)
c1<-filter(c1, Freq<10000)
c1<-filter(c1, Freq>500)
c1<-filter(c1, Freq>1000)
fivenum(c1$Freq)

d1<-read.csv('direction/a.csv', header=T)
for(i in 1:nrow(d1)){
  if(d1$Male[i]==1){
    d1$TRUE.[i]<-"Male"
  }else{
    d1$TRUE.[i]<-"Female"
  }
  
  if(d1$Male.1[i]>d1$Female.1){
    d1$Predict[i]<-"Male"
  }else{
    d1$Predict[i]<-"Female"
  }
  
   cat(i, '\n')
}

d2<-filter(d1, TRUE.!=Predict)
write.csv(d1, 'direction/a.csv', row.names = FALSE)

### Age ###
age30<-file2[which(file1$Age<30),]
age30$plantText<-tolower(age30$plantText)
for(i in 1:nrow(age30)){
  c<-strsplit(gsub("[^[:alnum:] ]", "", age30$plantText[i]), " +")[[1]]
  d<-paste(c, collapse = " ")
  age30$plantText[i]<-d
  cat(i, '\n')
}
age3045<-file2[which(file1$Age>=30&file1$Age<=45),]
age3045$plantText<-tolower(age3045$plantText)
for(i in 1:nrow(age3045)){
  c<-strsplit(gsub("[^[:alnum:] ]", "", age3045$plantText[i]), " +")[[1]]
  d<-paste(c, collapse = " ")
  age3045$plantText[i]<-d
  cat(i, '\n')
}
age45<-file2[which(file1$Age>45),]
age45$plantText<-tolower(age45$plantText)
for(i in 1:nrow(age45)){
  c<-strsplit(gsub("[^[:alnum:] ]", "", age45$plantText[i]), " +")[[1]]
  d<-paste(c, collapse = " ")
  age45$plantText[i]<-d
  cat(i, '\n')
}



write.table(age30$plantText, 'direction/Age30Text.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')
write.table(age3045$plantText, 'direction/Age3045Text.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')
write.table(age45$plantText, 'direction/Age45Text.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')


file1<-read.csv('direction/a.csv',header=T)
for(i in 1:nrow(file1)){
  a1<-file1[i, 2:4]
  a2<-which.max(a1)
  if(a2==1){
    file1$TRUE.[i]<-1
  }else if(a2==2){
    file1$TRUE.[i]<-2
  }else if(a2==3){
    file1$TRUE.[i]<-3
  }
  b1<-file1[i, 6:8]
  b2<-which.max(b1)
  if(b2==1){
    file1$Predict[i]<-1
  }else if(b2==2){
    file1$Predict[i]<-2
  }else if(b2==3){
    file1$Predict[i]<-3
  }
}

table(file1$TRUE., file1$Predict)

write.csv(file1, 'direction/a.csv', row.names=FALSE)

### Education ###
file1<-filter(file01, is_tweet_get=='true'&Highest.Education!="")
file1$Highest.Education<-as.character(file1$Highest.Education)
for(i in 1:nrow(file1)){
  if(file1$Highest.Education[i]%in%c(" College", "AAS", "BA", "Banchelor", "BS", "College", "Univeristy", "University", "UNiversity", "Vocational school", "Trend School", "Culinary school")){
    file1$Highest.Education[i]<-'Banchelor'
  }else if(file1$Highest.Education[i]%in%c("DVM, PhD", "MA", "Master", "MBA", "MBA,MS", "PhD")){
    file1$Highest.Education[i]<-'Graduate'
  }else{
    file1$Highest.Education[i]<-'Lower'
  }
}

Banchelor<-file2[which(file1$Highest.Education=='Banchelor'),]
Banchelor$plantText<-tolower(Banchelor$plantText)
Graduate<-file2[which(file1$Highest.Education=='Graduate'),]
Graduate$plantText<-tolower(Graduate$plantText)
Lower<-file2[which(file1$Highest.Education=='Lower'),]
Lower$plantText<-tolower(Lower$plantText)

write.table(Banchelor$plantText, 'direction/BanchelorText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')
write.table(Graduate$plantText, 'direction/GraduateText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')
write.table(Lower$plantText, 'direction/LowerText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')

file1<-filter(file01, is_tweet_get=='true'&Race!="")
file1$Race<-as.character(file1$Race)
for(i in 1:nrow(file1)){
  if(file1$Race[i]%in%c("Black or Afreica American", "Black or Africa American")){
    file1$Race[i]<-'Black'
  }else if(file1$Race[i]%in%c("Asian", "Asian/Pacific islander")){
    file1$Race[i]<-'Asian'
  }else{
    file1$Race[i]<-'Other'
  }
}

Black<-file2[which(file1$Race=='Black'),]
Black$plantText<-tolower(Black$plantText)
Asian<-file2[which(file1$Race=='Asian'),]
Asian$plantText<-tolower(Asian$plantText)
Other<-file2[which(file1$Race=='Other'),]
Other$plantText<-tolower(Other$plantText)

write.table(Black$plantText, 'direction/BlackText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')
write.table(Asian$plantText, 'direction/AsianText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')
write.table(Other$plantText, 'direction/OtherText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')


##### All #####
a<-filter(file1, numberOfGeoTweet>0)
b<-file2[which(file2$userID%in%a$UserID),]
b$plantText<-tolower(b$plantText)

for(i in 1:nrow(b)){
  c<-strsplit(gsub("[^[:alnum:] ]", "", b$plantText[i]), " +")[[1]]
  d<-paste(c, collapse = " ")
  b$plantText[i]<-d
  cat(i, '\n')
}


write.table(b$plantText, 'direction/PredictText.txt', row.names = FALSE, col.names = FALSE, quote=FALSE, sep='\n')



