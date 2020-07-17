
###############################CHAID Analysis###############################################
############################################################################################

##############CUElection Prediction#################

install.packages("C:/Users/Suruthi/Desktop/Data/Decision tree/CHAID_0.1-2.zip", repos = NULL, type ="source")
library("CHAID")
temp=USvote

train=sort(sample(nrow(USvote), nrow(USvote)*.7)) 
str(USvote)
training <- USvote[train,]
Validation <- USvote[-train,]

ctrl <- chaid_control(minsplit = 3000)
chaidUS <- chaid(vote3 ~ ., data = training, control = ctrl)
print(chaidUS)
# Plot the decision tree
plot(chaidUS)
# Prediction on Development Sample
Development_Pred<-predict(chaidUS,newdata=training)
Dev_prediction=cbind(training,Development_Pred)
write.csv(Dev_prediction,"USVoteDev_PredictionCHAID.csv")
table(Dev_prediction$vote3,Dev_prediction$Development_Pred)

# Prediction on Validation Sample
Validation_Pred<-predict(chaidUS,newdata=Validation)
Val_prediction=cbind(Validation,Validation_Pred)
write.csv(Val_prediction,"USVoteVal_PredictionCHAID.csv")
table(Val_prediction$vote3,Val_prediction$Validation_Pred)

################# HR Model########################
library("openxlsx")

getwd()
setwd("C://Users//Suruthi//Desktop//Data//Decision tree")

# Loading the data
HRdata=read.xlsx("CHAID_v1.xlsx")
names(HRdata)
str(HRdata)
HRdata<-HRdata[,-1]
# Variable Convertion
HRdata$EMPLOYEE_INTERN_TERM=as.factor(as.character(HRdata$EMPLOYEE_INTERN_TERM))
HRdata$YEARS_IN_JOB_RANGE=as.factor(as.character(HRdata$YEARS_IN_JOB_RANGE))
HRdata$YEARS_GRADE_RANGE=as.factor(as.character(HRdata$YEARS_GRADE_RANGE))
HRdata$YEARS_IN_SERVICE_RANGE=as.factor(as.character(HRdata$YEARS_IN_SERVICE_RANGE))
HRdata$CURRENT_COMPA_RATIO_GROUPING=as.factor(as.character(HRdata$CURRENT_COMPA_RATIO_GROUPING))
HRdata$GENDER=as.factor(as.character(HRdata$GENDER))

table(HRdata$EMPLOYEE_INTERN_TERM)
# Splitting the data for Development and model validation
train=sort(sample(nrow(HRdata), nrow(HRdata)*.7)) 

training <- HRdata[train,]
Validation <- HRdata[-train,]
str(training)

# Model Development
ctrl <- chaid_control(minsplit = 0)
chaidHR <- chaid(EMPLOYEE_INTERN_TERM ~ ., data = training, control = ctrl)
print(chaidHR)
# Plot the decision tree
plot(chaidHR)
# Prediction on Development Sample
Development_Pred<-predict(chaidHR,newdata=training)
Dev_prediction=cbind(training,Development_Pred)
write.csv(Dev_prediction,"HRDev_PredictionCHAID.csv")

# Prediction on Validation Sample
Validation_Pred<-predict(chaidHR,newdata=Validation)
Val_prediction=cbind(Validation,Validation_Pred)
write.csv(Val_prediction,"HRVal_PredictionCHAID.csv")




###############################CART Analysis###############################################
############################################################################################
##########Iris data########################
iris
names(iris)
table(iris$Species)
train=sort(sample(nrow(iris), nrow(iris)*.7)) 

training <- iris[train,]
Validation <- iris[-train,]

table(training$Species)
# CART Model
library(rpart)
library(dplyr)
fit <- rpart(Species ~. ,method="class", data=training)
print(fit)
plot(fit)
# Prediction on Development Sample
Development_Pred<-predict(fit,newdata=training)
Dev_prediction=cbind(training,Development_Pred)

maximum=function(a,b,c)
  {
  if(a>b & a>c)
  Predict<-"Setosa";
  if(b>a & b>c)
    Predict<-"Versicolor";
  if(c>a & c>b)
    Predict<-"Virginica";
  return(Predict);
}

a<-Dev_prediction$setosa
b<-Dev_prediction$versicolor
c<-Dev_prediction$virginica
 for(i in 1:105) {
   Pred[i]<-maximum(a[i],b[i],c[i])
   Dev_prediction$Predict[i]<-Pred[i]}

# Confusion Matrix
table(Dev_prediction$Species,Dev_prediction$Predict)

write.csv(Dev_prediction,"IrisDev_PredictionCART.csv")

# Prediction on Validation Sample
Validation_Pred<-predict(fit,newdata=Validation)
Val_prediction=cbind(Validation,Validation_Pred)

a<-Val_prediction$setosa
b<-Val_prediction$versicolor
c<-Val_prediction$virginica
for(i in 1:45) {
  Pred[i]<-maximum(a[i],b[i],c[i])
  Val_prediction$Predict[i]<-Pred[i]}

write.csv(Val_prediction,"IrisVal_PredictionCART.csv")
table(Val_prediction$Species,Val_prediction$Predict)

################################# CART Model-US Elections################
library(rpart)
train=sort(sample(nrow(USvote), nrow(USvote)*.7)) 

training <- USvote[train,]
Validation <- USvote[-train,]
names(training)
table(training$vote3)

fit <- rpart(vote3 ~. ,method="class", data=training)

# Prediction on Development Sample
Development_Pred<-predict(fit,newdata=training)
Dev_prediction=cbind(training,Development_Pred)
write.csv(Dev_prediction,"USvoteDev_PredictionCART.csv")

# Prediction on Validation Sample
Validation_Pred<-predict(fit,newdata=Validation)
Val_prediction=cbind(Validation,Validation_Pred)
write.csv(Val_prediction,"USVoteVal_PredictionCART.csv")
