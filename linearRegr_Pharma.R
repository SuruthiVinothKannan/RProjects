#**********Linear Regression*********#

getwd()

library("openxlsx")
data= read.xlsx("Pharma.xlsx")

#Data Preparation & EDA
summary(data)
quantile(as.numeric(data$Cooling),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE)
quantile(as.numeric(data$Com_1),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE)
quantile(as.numeric(data$Com_2 ),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE)
quantile(as.numeric(data$Com_3),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE)
quantile(as.numeric(data$Yield_KG),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE)

cor(data)
plot(data)

# library("dplyr")
# new =filter(data, data$Com_3 >= 28)
# cor(new)

# Creating Subset of the data for Model Development and Validation
set.seed(123)                    #to get the same random samples on each time 
d=sort(sample(nrow(data), nrow(data)*.7))

#select training and test sample 
train<-data[d,] 
test<-data[-d,]

model = lm(Yield_KG ~ Cooling+Com_1+Com_2+Com_3, data=train)
summary(model)

model = lm(Yield_KG ~ Com_1+Com_2, data=test)
summary(model)

train$predicted=fitted(model)
train

test$Predicted= predict(model,test)
test

score=read.xlsx("Pharma Score.xlsx")

score$Yield_KG= predict(model,score)
score
