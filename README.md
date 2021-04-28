#Data: Pathmatics Data, Category: Fitness and Wellness
# Data Date Range: 01/01/2019 - 10/01/2020
#Aim of the project: Understanding spending behaviour of different advertiser pre and post COVID-19
###############################################################################################################################


##CODE
#Clean Environment
rm(list = ls(all=TRUE))
#install packages
library(tidyverse)
library(lubridate)
library(dplyr)
library(magrittr)
library(broom)
library(xlsxjars)
library(fastDummies)

#upload Data
fnw<- read.csv("C:/Users/raaga/Downloads/Fitness and Weightloss - Monthly Top Advertisers Spend and Impressions.csv")

#Collapse data over variables: Type of Advertiser, Device and Direct/Indirect
clps_fnw <- fnw %>%
group_by(Type.of.Advertiser, Device, Direct.Indirect,Date) %>%
summarize(Spend = sum(`Spend`,na.rm = TRUE))

##OLS Regression for the entire data
cfnw <- clps_fnw

#Creating Dummy variables
cfnw <- dummy_cols(clps_fnw, select_columns = c("Type.of.Advertiser", "Device","Date"),remove_first_dummy = TRUE, remove_selected_columns = TRUE)
cfnw$Direct <- ifelse(clps_fnw$Direct.Indirect=="Direct",1,0)
cfnw = subset(cfnw, select = -c(Impressions,Direct.Indirect,Type.of.Advertiser_NA))

#OLS
model_ols <- lm(formula= Spend ~ .,cfnw)
summary(model_ols)

##Regression looped over time
#to check how the coefficients of the independent variables are changing over time
cfnw_panel <- clps_fnw
cfnw_panel <- dummy_cols(clps_fnw, select_columns = c("Type.of.Advertiser", "Device"), remove_first_dummy = TRUE, remove_selected_columns = TRUE)
cfnw_panel$Direct <- ifelse(clps_fnw$Direct.Indirect=="Direct",1,0)
cfnw_panel$Date <- as.Date(cfnw_panel$Date, format="%m/%d/%Y")
cfnw_panel = subset(cfnw_panel, select = -c(Direct.Indirect,Type.of.Advertiser_NA))
cfnw_panel$YEAR <- year(cfnw_panel$Date)
cfnw_panel$MONTH <- month(cfnw_panel$Date)

#Date vector
dates <- seq.Date(as.Date("2019-01-01"),as.Date("2020-10-01"), by= "months")
years <- year(dates)
months <- month(dates)
n_months <- length(dates)

#creating coefficient matrix
result_ols <- matrix(0,nrow=n_months,ncol = 15)
rownames(result_ols) <- substr(unique(cfnw_panel$Date),1,7)
colnames(result_ols) <- c("(Intercept)",names(cfnw_panel[-c(1:2,17:18)]))
for (i in 1:n_months){

##creating month specific dataset
dat <- cfnw_panel %>%
filter(YEAR==years(i) & MONTH == months(i))

##month specific OLS
loop_ols <- lm(formula= Spend ~ .,dat)

##save coefficient in matrix
result_ols[i,names(loop_ols$coefficients)] <- loop_ols$coefficients
}

##Plot coeffiencts over time
plot(dates[1:n_months],result_ols[1:n_months,2], type="l")
title("Coefficients for Type.of.Advertiser_Clinic")
plot(dates[1:n_months],result_ols[1:n_months,3], type="l")
title("Coefficients for Type.of.Advertiser_Gyms/Studios")
plot(dates[1:n_months],result_ols[1:n_months,4], type="l")
title("Coefficients for Type.of.Advertiser_Magazine")
plot(dates[1:n_months],result_ols[1:n_months,5], type="l")
title("Coefficients for Type.of.Advertiser_Outdoor Workout")
plot(dates[1:n_months],result_ols[1:n_months,6], type="l")
title("Coefficients for Type.of.Advertiser_Products")
plot(dates[1:n_months],result_ols[1:n_months,7], type="l")
title("Coefficients for Type.of.Advertiser_Research")
