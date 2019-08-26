###### Age ######
### Age bins <=30, 26-44, >=45
library(dplyr)

file01<-read.csv('Age.R/TF1-1500.csv', header=T) 
file1<-filter(file01, is_tweet_get=='true'&!is.na(Age))
file02<-'Age.R/TF Timeline csv/'

hashtag0<-c()

for(i in 1:nrow(file1)){
  if(file1$is_tweet_get[i]){
    file2<-paste0(file02,file1$UserID[i],'.csv')
    Data<-read.csv(file2, header=T)
    Data$hashtag1<-tolower(as.character(Data$hashtag1))
    Data$hashtag2<-tolower(as.character(Data$hashtag2))
    #hashtag0<-unique(c(hashtag, Data$hashtag1))
    #hashtag0<-unique(c(hashtag, Data$hashtag2))
    hashtag0<-c(hashtag0, Data$hashtag1)
    hashtag0<-c(hashtag0, Data$hashtag2)
    
  }
  cat(i, '\n')
}

hashtag<-data.frame(table(hashtag0))
hashtag<-hashtag[-1,]
#hashtag<-hashtag[order(hashtag$Freq, decreasing = T),]

hashtag$userFreq<-0
for(i in 1:nrow(file1)){
  file2<-paste0(file02,file1$UserID[i],'.csv')
  Data<-read.csv(file2, header=T)
  hashtag0<-c()
  hashtag0<-c(hashtag0, tolower(as.character(Data$hashtag1)))
  hashtag0<-c(hashtag0, tolower(as.character(Data$hashtag2)))
  hashtag0<-unique(hashtag0)
  
  hashtag$userFreq[which(hashtag$hashtag0%in%hashtag0)]<-hashtag$userFreq[which(hashtag$hashtag0%in%hashtag0)]+1
  
  cat(i, '\n')
}

write.csv(hashtag, '/Users/yu/Desktop/hashtag.csv', row.names=FALSE)

modelingHashtag<-filter(hashtag, userFreq>=10)

hashtagData<-data.frame(matrix(0, nrow=0, ncol=2))
for(i in 1:nrow(file1)){
  x<-data.frame(matrix(0, nrow=1, ncol=(nrow(modelingHashtag)+3)))
  file2<-paste0(file02,file1$UserID[i],'.csv')
  Data<-read.csv(file2, header=T)
  
  a<-c()
  a<-c(a, tolower(as.character(Data$hashtag1)))
  a<-c(a, tolower(as.character(Data$hashtag2)))
  b<-data.frame(table(a))
  b<-filter(b, b$a!="")
  
  x[1,1]<-file1$UserID[i]
  x[1,2]<-file1$Age[i]
  if(file1$Age[i]<30){
    x[1,3]<-1
  }else if(file1$Age[i]>45){
    x[1,3]<-3
  }else{
    x[1,3]<-2
  }
  x[1,c(which(modelingHashtag$hashtag0%in%b$a)+3)]<-b$Freq[which(b$a%in%modelingHashtag$hashtag0)]
  
  hashtagData<-rbind(hashtagData, x)
  cat(i, '\n')
}

colnames(hashtagData)<-c("userID", 'Age', 'AgeBin', as.character(modelingHashtag$hashtag0))
#write.csv(hashtagData, '/Users/yu/Desktop/a.csv', row.names=FALSE)

###### Random Forest ######
library(randomForest)

######### Unbanlance ###########
hashtagData$AgeBin<-as.factor(hashtagData$AgeBin)
rf10<-randomForest(x=hashtagData[,4:ncol(hashtagData)], y=hashtagData[,3])
pre10<-predict(rf10, hashtagData[4:ncol(hashtagData)])
table(hashtagData$AgeBin, pre10)

rf10.cv<-rfcv(trainx=hashtagData[,4:ncol(hashtagData)], trainy=hashtagData[,3], cv.fold=5)
with(rf10.cv, plot(n.var, error.cv)) # 1094  547  274  137   68   34   17    9    4    2    1
rf10.cv$n.var

library(caret)
library(rpart)
train_control<-trainControl(method='cv', number=5, savePredictions = TRUE)
model<-train(x=hashtagData[,4:ncol(hashtagData)], y=hashtagData[,3], trControl = train_control, method="rf")
model$results

a<-data.frame(model$pred)

length(which(a$pred==a$obs&a$Resample=='Fold1'))/length(which(a$Resample=='Fold1'))
length(which(a$pred==a$obs&a$Resample=='Fold2'))/length(which(a$Resample=='Fold2'))
length(which(a$pred==a$obs&a$Resample=='Fold3'))/length(which(a$Resample=='Fold3'))
length(which(a$pred==a$obs&a$Resample=='Fold4'))/length(which(a$Resample=='Fold4'))
length(which(a$pred==a$obs&a$Resample=='Fold5'))/length(which(a$Resample=='Fold5'))

table(a$pred[which(a$Resample=='Fold1')], a$obs[which(a$Resample=='Fold1')])
table(a$pred[which(a$Resample=='Fold2')], a$obs[which(a$Resample=='Fold2')])
table(a$pred[which(a$Resample=='Fold3')], a$obs[which(a$Resample=='Fold3')])
table(a$pred[which(a$Resample=='Fold4')], a$obs[which(a$Resample=='Fold4')])
table(a$pred[which(a$Resample=='Fold5')], a$obs[which(a$Resample=='Fold5')])

#### feature selection 137
b<-data.frame(modelingHashtag$hashtag0, rf10$importance)
b<-b[order(b$MeanDecreaseGini, decreasing = T),]
hashtagData137<-hashtagData[,c(1,2,3, which(modelingHashtag$hashtag0%in%b$modelingHashtag.hashtag0[1:137])+3)]
model137<-train(x=hashtagData137[,4:ncol(hashtagData137)], y=hashtagData137[,3], trControl = train_control, method="rf")
model137$results

rf137.cv<-rfcv(trainx=hashtagData137[,4:ncol(hashtagData137)], trainy=hashtagData137[,3], cv.fold=5)
with(rf137.cv, plot(n.var, error.cv)) # 137  68  34  17   9   4   2   1
rf137.cv$n.var

rf137<-randomForest(x=hashtagData137[,4:ncol(hashtagData137)], y=hashtagData137[,3])

#### feature selection 68
b<-data.frame(names(hashtagData137[4:length(hashtagData137)]), rf137$importance)
b<-b[order(b$MeanDecreaseGini, decreasing = T),]
colnames(b)<-c('feature', 'Gini')
hashtagData68<-hashtagData137[,c(1,2,3, which(names(hashtagData137)%in%b$feature[1:68]))]
model68<-train(x=hashtagData68[,4:ncol(hashtagData68)], y=hashtagData68[,3], trControl = train_control, method="rf")
model68$results

rf68.cv<-rfcv(trainx=hashtagData68[,4:ncol(hashtagData68)], trainy=hashtagData68[,3], cv.fold=5)
with(rf68.cv, plot(n.var, error.cv)) # 771 386 193  96  48  24  12   6   3   1
rf68.cv$n.var

a<-data.frame(model68$pred)

length(which(a$pred==a$obs&a$Resample=='Fold1'))/length(which(a$Resample=='Fold1'))
length(which(a$pred==a$obs&a$Resample=='Fold2'))/length(which(a$Resample=='Fold2'))
length(which(a$pred==a$obs&a$Resample=='Fold3'))/length(which(a$Resample=='Fold3'))
length(which(a$pred==a$obs&a$Resample=='Fold4'))/length(which(a$Resample=='Fold4'))
length(which(a$pred==a$obs&a$Resample=='Fold5'))/length(which(a$Resample=='Fold5'))

table(a$pred[which(a$Resample=='Fold1')], a$obs[which(a$Resample=='Fold1')])


#### svm ###
library(e1071)
library(knn)
library(nnet)
library(LiblineaR)
library(kernlab)
obj<-tune(svm, train.x=hashtagData[,4:ncol(hashtagData)], train.y = hashtagData[,3], cost = c(0.0001, 0.001, 0.01, 0.1, 1, 2), gamma = c(0.0001, 0.001, 0.01, 0.1, 1, 2), kernel = "linear")
obj$best.performance

obj<-svm(x=hashtagData[,4:ncol(hashtagData)], y = hashtagData[,3])
presvm<-predict(obj, x=hashtagData[,4:ncol(hashtagData)], y = hashtagData[,3])
table(hashtagData[,3], presvm)

trctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 3, savePredictions = TRUE)

knn_fit <- train(x=hashtagData[,4:ncol(hashtagData)], y=hashtagData[,3], method = "svmExpoString", trControl=trctrl)
knn_fit$results

##--------------------------------------------------------------------------------------------------------------------##
## One vs All ##
f1<-function(x){
  if(x==1){
    return(1)
  }else{
    return(2)
  }
}
f2<-function(x){
  if(x==2){
    return(1)
  }else{
    return(2)
  }
}
f3<-function(x){
  if(x==3){
    return(1)
  }else{
    return(2)
  }
}

ageBin1<-hashtagData
ageBin1$AgeBin<-unlist(lapply(hashtagData$AgeBin, f1))
ageBin1$AgeBin<-as.factor(ageBin1$AgeBin)
ageBin2<-hashtagData
ageBin2$AgeBin<-unlist(lapply(hashtagData$AgeBin, f2))
ageBin3<-hashtagData
ageBin3$AgeBin<-unlist(lapply(hashtagData$AgeBin, f3))

library(randomForest)
rfBin1<-randomForest(x=ageBin1[,4:ncol(ageBin1)], y=ageBin1[,3])
preBin1<-predict(rfBin1, ageBin1[,4:ncol(ageBin1)])
table(ageBin1[,3], preBin1)

rfBin1.cv<-rfcv(trainx=ageBin1[,4:ncol(ageBin1)], trainy=ageBin1[,3], cv.fold=5)
with(rfBin1.cv, plot(n.var, error.cv)) # 1094  547  274  137   68   34   17    9    4    2    1
rfBin1.cv$n.var

train_control<-trainControl(method='cv', number=5, savePredictions = TRUE)
modelBin1<-train(x=ageBin1[,4:ncol(ageBin1)], y=ageBin1[,3], trControl = train_control, method="rf")
modelBin1$results
a<-modelBin1$pred
b<-data.frame(matrix(0, nrow=0, ncol=6))
for(i in 1:nrow(hashtagData)){
  x<-data.frame(matrix(0, nrow=1, ncol=6))
  c<-filter(a, rowIndex==i)
  x[1,1]<-i
  x[1,2:4]<-c$pred[1:3]
  d<-data.frame(table(c$pred))
  x[1,5]<-which.max(d$Freq)
  x[1,6]<-unique(c$obs)
  
  b<-rbind(b,x)
  cat(i, '\n')
}



