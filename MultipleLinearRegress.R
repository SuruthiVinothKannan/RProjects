#********************************************************************#
#************ Multiple Linear Regression ***************************#
#*******************************************************************#


getwd()
setwd("C:\\Users\\Suruthi\\Desktop\\Data\\Regression Analysis")

library(openxlsx)


data<-read.xlsx("11_3 Model Data on Revenue.xlsx")
names(data) 
head(data)

# The data has 8 variables and names are
#  "Time" "Year" "Location" "Customer" "Product.Name" "Revenue" "Sales" "Orders"  
# Y<- Sales   X<- Orders(Numeric), Product.Name (Char), Customer (char), Time (Char)
plot(data$Sales,data$Orders)

#****************************************************#
#**********Data Preparation/ Data Cleaning***********#
#****************************************************#

#*** Cheecking for Null Values and outliers in Numeric Variables*******#

sum(is.na(data))  # No NA values

round(quantile(data$Sales,c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE))

round(quantile(data$Orders,c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1),na.rm=TRUE))


# round(quantile(data$Sales,c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1),na.rm=TRUE))
# 0%  1%  2%  3%  4%  5%  6%  7%  8%  9% 10% 
# -31   0   0   0   0   1   2   3   5   6   8 
# 10% 11% 12% 13% 14% 15% 16% 17% 18% 19% 
#  8   8  11  13  15  16  18  20  22  24 
# round(quantile(data$Orders,c(0,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.1),na.rm=TRUE))
# 0%    1%    2%    3%    4%    5%    6%    7%    8%    9%   10% 
# -3231 -1578 -1491 -1375 -1219 -1058  -938  -859  -780  -709  -612 
#  10%  11%  12%  13%  14%  15%  16%  17%  18%  19% 
# -612 -542 -471 -391 -304 -174  -41   -5    0    0 

# Noticed in the dataset,there are negative values till 17% on the variable.
# And the sales is 0 till 5% and too low  till 10%.

# Filtering the data with less than 1 sales.

library(dplyr)
Lessthan1sales<-filter(data,Sales< 1)

# Removing the rows with less than 1 sales values
data1<-filter(data,Sales>=1)
round(quantile(as.numeric(data1$Sales),c(0.25,0.30,0.35,0.40,0.45,0.50),na.rm=TRUE))

round(quantile(as.numeric(data1$Orders),c(0.10,0.11,0.12,0.13,0.14,0.15,0.16,0.17,0.18,0.19),na.rm=TRUE))

#Filtering the rows with zero orders
Lessthan0Orders<-filter(data1,Orders<=0)
data2<-filter(data1,Orders>0)

round(quantile(as.numeric(data2$Sales),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE))
round(quantile(as.numeric(data2$Orders),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE))

# Orders
# 0%    1%    5%   10%   25%   50%   75%   90%   95%   99%  100% 
# 0     1    14    30    82  2454  6389 11013 14393 18960 22836 

# Sales
# 0%    1%    5%   10%   25%   50%   75%   90%   95%   99%  100% 
# 1     3    14    22    68  1834  4800  7277 10284 15079 19668 

round(quantile(as.numeric(data2$Sales),c(0.35,0.36,0.37,0.38,0.39,0.40),na.rm=TRUE))
round(quantile(as.numeric(data2$Orders),c(0.35,0.36,0.37,0.38,0.39,0.40),na.rm=TRUE))

library(sqldf)

# analysing sales data
SalesbyCust<-sqldf("Select Customer,avg(Sales) from data2 
      group by Customer ")
data2<-rename(data2,Prod = Product.Name)
names(data2)
SalesbyProd<-sqldf("Select Prod,avg(Sales) from data2 
      group by Prod")    

# Analysing Orders data
OrdersbyCust<-sqldf("Select Customer,avg(Orders) from data2 
      group by Customer ")

OrdersbyProd<-sqldf("Select Prod,avg(Orders) from data2 
                   group by Prod")  

Lessthan1.Orders<-sqldf("Select * from data 
      where Orders> 0 AND Orders<=1")

plot(data2$Sales,data2$Orders)


# By anlaysing,data needs to be analysed in two ways
# Orders less than 100 (ie, 159 records) and orders more than 100 (ie, 38)
# Splitting the data accordingly

#SetLow<-sqldf("select * from data2
 #             where Orders <= 100")
#SetHigh<-sqldf("select * from data2
  #             where Orders > 100")


#******************************************************#
#***************Exploratory Data Analysis**************#
#******************************************************#

#**** Indepen Vs Depen, correlation, plot *************#

plot(SetHigh$Orders,SetHigh$Sales)
head(SetHigh)

#dummy varibles for catego rical variable customer
table(data2$Customer)
table(data2$Prod)

sqldf("select * from data2 
      where Customer == 'Other' ")
sqldf("select * from data2 
      where Prod == 'Other' ")
sqldf("select * from data2 
      where Prod == 'Net' ")

# dummy variables for Customer variable, neglected "Other" for multicolinearity
data2$cust.Regular<-ifelse(data2$Customer=="Regular",1,0)
data2$cust.Bussiness<-ifelse(data2$Customer=="Business",1,0)
data2$cust.Government<-ifelse(data2$Customer=="Government",1,0)
data2$cust.Technology<-ifelse(data2$Customer=="Technology",1,0)

#dummy varialbes for products, neglected "other" for multicolinearity
data2$Prod.Software<-ifelse(data2$Prod=="Software",1,0)
data2$Prod.Technology<-ifelse(data2$Prod=="Technology",1,0)
data2$Prod.Data<-ifelse(data2$Prod=="Data",1,0)
data2$Prod.Mapping<-ifelse(data2$Prod=="Mapping",1,0)
data2$Prod.Routers<-ifelse(data2$Prod=="Routers",1,0)
data2$Prod.Video<-ifelse(data2$Prod=="Video",1,0)
data2$Prod.TechChange<-ifelse(data2$Prod=="Technology Change",1,0)
data2$Prod.Net<-ifelse(data2$Prod=="Net",1,0)
data2$Prod.Safety<-ifelse(data2$Prod=="Safety",1,0)
data2$Prod.Ser_design<-ifelse(data2$Prod=="Services_Design",1,0)
data2$Prod.Ser_hardware<-ifelse(data2$Prod=="Services_Hardware",1,0)
data2$Prod.Ser_wifi<-ifelse(data2$Prod=="Services_wifi",1,0)

#dummy variables for quaters
table(data$Time)

#Q1<-sqldf("select * from data1
#          where Time like '%Q1'")
#data1$Q1<-ifelse(data1$Time=="2014Q1"|data1$Time=="2015Q1"|data1$Time=="2016Q1"|data1$Time=="2017Q1" ,1,0)
#data1<-select(data1,-Q1)

data2$Q1<-ifelse(data2$Time=="2014Q1"|data2$Time=="2015Q1"|data2$Time=="2016Q1"|data2$Time=="2017Q1" ,1,0)
data2$Q2<-ifelse(data2$Time=="2014Q2"|data2$Time=="2015Q2"|data2$Time=="2016Q2",1,0)
data2$Q3<-ifelse(data2$Time=="2014Q3"|data2$Time=="2015Q3"|data2$Time=="2016Q3",1,0)
#data2$Q4<-ifelse(data2$Time=="2014Q4"|data2$Time=="2015Q4"|data2$Time=="2016Q4",1,0)

data2$order<-data2$Orders
data2$Y_Sales<-data2$Sales
names(data2)

#Determining the independent variables

Data<-select(data2,-Time,-Year,-Location,-Customer,-Prod,-Revenue,-Sales,-Orders)


summary(Data)
names(Data)

cor(Data$Y_Sales,Data$order)
cor(Data$Y_Sales,Data$Q1)
cor(Data$Y_Sales,Data$Q2)
cor(Data$Y_Sales,Data$Q3)
#cor(Data$Y_Sales,Data$Q4)
plot(Data$Y_Sales,Data$order)
names(Data)


#***************************************************************#
#**********************Model Development************************#
#***************************************************************#

set.seed(123)                               #to get the same random samples on each time 
d=sort(sample(nrow(Data), nrow(Data)*.8))

#select training and test sample 
train<-Data[d,] 
test<-Data[-d,]

model = lm( Y_Sales~cust.Regular+cust.Bussiness+cust.Government+cust.Technology+
              Prod.Software+Prod.Technology+Prod.Data+Prod.Mapping+Prod.Routers+
              Prod.Video+Prod.TechChange+Prod.Net+Prod.Safety+Prod.Ser_design+
              Prod.Ser_hardware+Prod.Ser_wifi+Q1+Q2+Q3+order, data=train)
mod_summary<-summary(model)
write.csv(mod_summary,file="Summary_MLR.csv")
summary(model)


#**************** Predicting yHat for training data **************************

train$predicted=fitted(model)
train

plot(train$Y_Sales,train$predicted)

#******* Model Validation on test data *******

model_test = lm( Y_Sales~cust.Regular+cust.Bussiness+cust.Government+cust.Technology+
              Prod.Software+Prod.Technology+Prod.Data+Prod.Mapping+Prod.Routers+
              Prod.Video+Prod.TechChange+Prod.Net+Prod.Safety+Prod.Ser_design+
              Prod.Ser_hardware+Prod.Ser_wifi+Q1+Q2+Q3+order, data=test)



test$Predicted= predict(model_test,test)
test

#************** Score Generation ****************

score=read.xlsx("11_3 Score.xlsx")

# dummy variables for Customer variable, neglected "Other" for multicolinearity
score$cust.Regular<-ifelse(score$Customer=="Regular",1,0)
score$cust.Bussiness<-ifelse(score$Customer=="Business",1,0)
score$cust.Government<-ifelse(score$Customer=="Government",1,0)
score$cust.Technology<-ifelse(score$Customer=="Technology",1,0)

score<-rename(score,Prod = Product.Name)

#dummy varialbes for products, neglected "other" for multicolinearity
score$Prod.Software<-ifelse(score$Prod=="Software",1,0)
score$Prod.Technology<-ifelse(score$Prod=="Technology",1,0)
score$Prod.Data<-ifelse(score$Prod=="Data",1,0)
score$Prod.Mapping<-ifelse(score$Prod=="Mapping",1,0)
score$Prod.Routers<-ifelse(score$Prod=="Routers",1,0)
score$Prod.Video<-ifelse(score$Prod=="Video",1,0)
score$Prod.TechChange<-ifelse(score$Prod=="Technology Change",1,0)
score$Prod.Net<-ifelse(score$Prod=="Net",1,0)
score$Prod.Safety<-ifelse(score$Prod=="Safety",1,0)
score$Prod.Ser_design<-ifelse(score$Prod=="Services_Design",1,0)
score$Prod.Ser_hardware<-ifelse(score$Prod=="Services_Hardware",1,0)
score$Prod.Ser_wifi<-ifelse(score$Prod=="Services_wifi",1,0)

#dummy variables for quaters
table(score$Time)

score$Q1<-ifelse(score$Time=="2014Q1"|score$Time=="2015Q1"|score$Time=="2016Q1"|score$Time=="2017Q1" ,1,0)
score$Q2<-ifelse(score$Time=="2017Q2",1,0)
score$Q3<-ifelse(score$Time=="2017Q3",1,0)

score$order<-score$Orders

score$PredictedSales= predict(model,score)
score
final_score<-select(score,order,PredictedSales)

write.csv(final_score,file="scores_MLR.csv")
 