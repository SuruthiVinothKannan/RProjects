# Basic R Commands for Statistics

getwd()
setwd("C:\\Users\\Suruthi\\Desktop\\Data")
getwd()

install.packages("openxlsx")
library(openxlsx)

Wine=read.xlsx("Wine_data.xlsx")

#Basic=summary(Wine)
#write.csv(Basic,"Basic Analysis.csv")

sd(Wine$alcohol)

cor(Wine$alcohol,Wine$quality)

plot(Wine$alcohol,Wine$quality)
