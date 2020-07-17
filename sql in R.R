#Installing sql in R

install.packages("sqldf")

#Enabling Sql and excel
library(sqldf)
library("openxlsx")

#Set directory
getwd()
setwd("C:\\Users\\Suruthi\\Desktop\\Data\\Kaggle\\Titanic")

#Load table from excel
Sampledata=read.xlsx("test.xlsx")

sqldf("select count(*) from Sampledata")

#Selecting all values in the table
new<-sqldf("select * from Sampledata")
names(new)

#Subset table
new_gender<-new[,c(1,4)]
names(new_gender)
new_age<-new[c("PassengerId","Age")]
names(new_age)
subdata<-sqldf("select PassengerID,Pclass,Name,Sex,Age,Parch from new")
names(subdata)

#Subset of data using "where"Condition /Filter
ladies_30<-sqldf("select PassengerID,Sex,Age,Parch from new 
                 where Age<=30 and Sex= 'female' and Parch='1'")
names(ladies_30)

#Summarize the data
sqldf("select Pclass,Sex as Gender, min(Fare), max(Fare), avg(Fare)  from new
      group by Pclass,Sex ")



