# Regression vs XGBOOST

# Regression Model
library('openxlsx')
getwd()
setwd("C://Users//Suruthi//Desktop//Data//XGBoost")

com_data = read.xlsx("14 Model Data on Revenue.xlsx")
names(com_data)
str(com_data)
head(com_data)
table(com_data$Time)
nrow(com_data) 

train = com_data[1:636,]
nrow(train)

test =com_data[637:nrow(com_data),]
nrow(test)


model = lm(Sales ~ Orders+Business+Government+Regular+Small.Business+Technology+Data+Mapping+Net+Other+Routers+Safety+Services_calls+Services_Design+Services_Hardware+Services_wifi+Software+Technology+Technology.Change+Video,data=train)
summary(model)

model1 = lm(Sales ~ Orders+Business+Government+Regular+Technology+Data+Net+Routers+Safety+Services_wifi+Software+Technology+Technology.Change+Video,data=train)
summary(model1)

test$predict=predict(model1,test)
names(test)

write.csv(test,"test.csv")

score=read.xlsx("11_3 Score.xlsx")
score$predict= predict(model1,score)
write.csv(score,"score.csv")

# XG Boost Model
install.packages("xgboost")
library(xgboost)
names(train)
xvar=names(train)[8:ncol(train)]

# nrounds[default=100]
# It controls the maximum number of iterations. For classification, it is similar to the number of trees to grow.
# Should be tuned using CV
# eta[default=0.3][range: (0,1)]
# It controls the learning rate, i.e., the rate at which our model learns patterns in data. After every round, it shrinks the feature weights to reach the best optimum.
# Lower eta leads to slower computation. It must be supported by increase in nrounds.
# Typically, it lies between 0.01 - 0.3
depth=5
eta=0.16
nrounds=21

# booster [default=gbtree]
# which booster to use, can be gbtree, gblinear or dart. gbtree and dart use tree based model while gblinear uses linear function.
bst <- xgboost(data =as.matrix(train[,xvar]), label = train$Sales,max_depth = depth, eta = eta, nrounds = nrounds)

train$pred=NULL
train$pred <- predict(bst, as.matrix(train[,xvar]))
# cor(train$ORDuration,train$pred)

test$pred=NULL
test$pred <- predict(bst, as.matrix(test[,xvar]))
# cor(test$ORDuration,test$pred)

train_err_sq = sum((train[,"Sales"]-train[,"pred"])**2)
train_var = sum((train[,"Sales"]-mean(train[,"pred"]))**2)
R2_Train = 1 - (train_err_sq/train_var)

  
test_err_sq = sum((test[,"Sales"]-test[,"pred"])**2)
test_var = sum((test[,"Sales"]-mean(test[,"pred"]))**2)
R2_Test = 1 - (test_err_sq/test_var)
#CHAID vs XGBOOST

################# HR Model########################
library("CHAID")
library("openxlsx")
# Loading the data
getwd()
setwd("C:/Users/Suruthi/Desktop/Data/Decision tree")
HRdata=read.xlsx("CHAID_v1.xlsx")
names(HRdata)
str(HRdata)
# Variable Convertion
HRdata$EMPLOYEE_INTERN_TERM=as.factor(as.character(HRdata$EMPLOYEE_INTERN_TERM))
HRdata$YEARS_IN_JOB_RANGE=as.factor(as.character(HRdata$YEARS_IN_JOB_RANGE))
HRdata$YEARS_GRADE_RANGE=as.factor(as.character(HRdata$YEARS_GRADE_RANGE))
HRdata$YEARS_IN_SERVICE_RANGE=as.factor(as.character(HRdata$YEARS_IN_SERVICE_RANGE))
HRdata$CURRENT_COMPA_RATIO_GROUPING=as.factor(as.character(HRdata$CURRENT_COMPA_RATIO_GROUPING))
HRdata$GENDER=as.factor(as.character(HRdata$GENDER))

table(HRdata$EMPLOYEE_INTERN_TERM)
# Splitting the data for Development and model validation
train=sort(sample(nrow(HRdata), nrow(HRdata)*.9)) 

training <- HRdata[train,]
Validation <- HRdata[-train,]
table(training$EMPLOYEE_INTERN_TERM)
training = training[-1]
names(training)
Validation=Validation[-1]
names(Validation)
# Model Development
ctrl <- chaid_control(minsplit = 100, minbucket = 100, minprob = 0)
chaidHR <- chaid(EMPLOYEE_INTERN_TERM ~ ., data = training, control = ctrl)
print(chaidHR)
# Plot the decision tree
plot(chaidHR)
# Prediction on Development Sample
Development_Pred<-predict(chaidHR,newdata=training)
Dev_prediction=cbind(training,Development_Pred)
write.csv(Dev_prediction,"Development_Prediction.csv")
names(Dev_prediction)
install.packages("gmodels")
library(gmodels)
CrossTable(Dev_prediction$EMPLOYEE_INTERN_TERM, Dev_prediction$Development_Pred)
table(training$EMPLOYEE_INTERN_TERM)

#XG BOOST 

data=HRdata
# Automatic Dummy variable Creation
str(data)
data=na.omit(data)
CatVars=c(
  "YEARS_IN_JOB_RANGE"                    
  , "YEARS_GRADE_RANGE" 
  ,"YEARS_IN_SERVICE_RANGE"
  ,"CURRENT_COMPA_RATIO_GROUPING"
  , "GENDER"
)
data[,CatVars]=lapply(data[,CatVars], factor)
DummyVarList1=data.frame(model.matrix(~ .,data=data[,CatVars]),data[,"ID"])
DummyVarList1=DummyVarList1[,-1]
names(DummyVarList1)[ncol(DummyVarList1)]="ID"
rm(CatVars)


ModelData=merge(x = data, y = DummyVarList1, by = "ID", all.x = TRUE)
names(ModelData)
ModelData$EMPLOYEE_INTERN_TERM=as.numeric(ModelData$EMPLOYEE_INTERN_TERM)
ModelData$EMPLOYEE_INTERN_TERM=ModelData$EMPLOYEE_INTERN_TERM-1
str(ModelData)

xvar=names(ModelData)[8:ncol(ModelData)]

# nrounds[default=100]
# It controls the maximum number of iterations. For classification, it is similar to the number of trees to grow.
# Should be tuned using CV
# eta[default=0.3][range: (0,1)]
# It controls the learning rate, i.e., the rate at which our model learns patterns in data. After every round, it shrinks the feature weights to reach the best optimum.
# Lower eta leads to slower computation. It must be supported by increase in nrounds.
# Typically, it lies between 0.01 - 0.3
depth=5
eta=0.16
table(ModelData$EMPLOYEE_INTERN_TERM)

bst <- xgboost(data =as.matrix(ModelData[,xvar]), label = ModelData$EMPLOYEE_INTERN_TERM,objective = "reg:logistic", eta = eta, nrounds = 100)

ModelData$pred=NULL
ModelData$pred <- predict(bst, as.matrix(ModelData[,xvar]))
ModelData$pred <- ifelse (ModelData$pred > 0.7,1,0)

write.csv(ModelData,"ModelData.csv")
library(gmodels)
CrossTable(ModelData$EMPLOYEE_INTERN_TERM, ModelData$pred)



