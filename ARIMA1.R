# ARIMA data model
# Revenue Data

getwd()
library("openxlsx")
library("forecast")

# Importing Data
data<-read.xlsx("Revenue Forecasting.xlsx")
str(data)

# Data Preparation
Actual_data<-data[1:36,3]
Validate_data<-data[37:42,3]

# Ploting the data to check Stationarity
ts.plot(Actual_data)
acf(Actual_data)

# Finding the P value
pacf(Actual_data)

# TO change Non stationary to Stationary (Successive Difference) d & q value
Suc_diff<-diff(Actual_data)
ts.plot(Suc_diff)
acf(Suc_diff)
sec_diff<-diff(Suc_diff)
ts.plot(sec_diff)
acf(sec_diff)

# 