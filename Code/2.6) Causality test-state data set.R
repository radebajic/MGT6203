library(lmtest)
library(stats)
library(dplyr)
library(lubridate)
library(tidyverse)

d = read.csv('average_monthly_temperature_by_state_1950-2022.csv') #Load in data
d2 = read.csv('commodity-prices-2016.csv') #Load in data
head(d)

d$Date = paste(as.character(d$year),as.character(d$month),"01",sep="-") #Convert month to first date of the month
d$Date = as.Date(d$Date) #Change to date type

d2$Date = as.Date(d2$Date)  #Change to date type

d = d[(d$state == 'Illinois'),] #Filter for illinois data

df1 = subset(d, select = c(Date,average_temp)) #Create dataframe with relevant data from weather dataframe
df2 = subset(d2, select = c(Date,Soybean.Meal)) #Create dataframe with relevant data from price dataframe


p = merge(df1,df2,by = 'Date')

model = grangertest(p$Soybean.Meal~p$average_temp, order = 3)
model


