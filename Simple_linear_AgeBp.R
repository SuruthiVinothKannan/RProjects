#****** Simple Linear Regression *******#
# Model between two numeric variables  #
#**************************************#

# Reading data and setting path to directory

getwd()
setwd("C:\\Users\\Suruthi\\Desktop\\Data\\Regression Analysis")

# Enabling xlsx
library(openxlsx)

# Creating variable data to store the dataset
data = read.xlsx("BPAge.xlsx", sheet="Sheet3")
names(data)
head(data)


#***********************************************#
#**********Data Preparation / Cleaning**********#
#***********************************************#

#**** Checking for Null value ******#

summary(data)
sum(is.null(data$Bp))
sum(is.null(data$Age))

#**** Checking for outlires ******#

quantile(as.numeric(data$BP),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE)

library(scales)
str(data)

#**********To Replace Outliers with quantile value using squish**********
data$BP_noOutliers <- squish(data$BP,round(quantile(data$BP, c(.05, .95), na.rm = TRUE)))
data

quantile(as.numeric(data$BP_noOutliers),c(0,0.01,0.05,0.1,0.25,0.5,0.75,0.9,0.95,0.99,1),na.rm=TRUE)


# To drop a column from a data frame
#data<-within(data,rm(Bp))

#************************************#
#*************EDA********************#
#******** Indepen vs depen & dropping insignificant value **********#

plot(data$Age,data$BP_noOutliers)
cor(data$Age,data$BP_noOutliers)

data.signif<-within(data,rm(BP))


#**************************************************#
# ******** Model Development **********************#
#**************************************************#

# Splitting the data for training and validating

train= data.signif[1:31,]
train
valid = data.signif[32:nrow(data),]
valid

mod <- lm(BP_noOutliers ~ Age , data=data.signif)
summary(mod) # show results

# Other useful functions
abline(mod)
coefficients(mod) # model coefficients
train$Predicted_BP<-round(fitted(mod)) # Fitted values (Y hat) for train data

#*********Model Accuracy*************#
residuals(mod)
summary(mod)
#************************************#
#******** Model Validation *********#
#***********************************#

valid$BP_Predict=round(predict(mod,valid)) # Predict values using the same model for valid data
valid

summary(mod)
acf(residuals(mod)) # residuals
anova(mod) # anova table 
