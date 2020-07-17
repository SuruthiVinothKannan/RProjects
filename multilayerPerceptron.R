# Deep Learning #####

library(keras)
library(tensorflow)
library(openxlsx)


data<-read.xlsx(file.choose())
str(data)

# change to matrix
data<-as.matrix(data)
dimnames(data)<-NULL


#Normalize
data[,3:12]<-normalize(data[,3:12])
summary(data)

#Data Partition
set.seed(123)
d<-sample((nrow(data))*0.8)

train<-data[d,3:12]
test<-data[-d,3:12]
traintarget<-data[d,2]
testtarget<-data[-d,2]

#One Hot Encoding
trainlabels<- to_categorical(traintarget)
testlabels<- to_categorical(testtarget)

testlabels
