# Revenue with outliers

getwd()
setwd("C:\\Users\\Suruthi\\Desktop\\Data\\Time_Series")
Rev_exe<-read.xlsx("Revenue Exercise.xlsx")


# Data Analysis 
head(Rev_exe)
mean(Rev_exe$Revenue)
min(Rev_exe$Revenue)
max(Rev_exe$Revenue)
summary(Rev_exe)
names(Rev_exe)

any(is.na(names(Rev_exe)))
hist(Rev_exe$Revenue)
