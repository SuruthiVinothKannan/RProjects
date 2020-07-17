######### Random Forest##########
#********************************

library(randomForest)

getwd()

library(randomForest)
setwd("C://Users//Suruthi//Desktop//Data//RandomForest")

# Getting Data
new<-Boston

# Understanding Data
str(Boston)
colSums(is.na(new))
summary(new)
cor(new)
plot(cor(new))

# Data Partition
set.seed(123)
d<-sample(new,nrow(new)*0.7)
train<-new[d]
test<-new[-d]

# Model
rf<-randomForest(medv ~ .,data=Boston,subset = train ,ntree=400)

