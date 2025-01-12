library(lmtest)
library(stats)
library(dplyr)
library(lubridate)
library(tidyverse)
library(forecast)

d = read.csv('Aledo.csv')
d2 = read.csv('commodity-prices-2016.csv')

head(d2)

d$DATE = paste(as.character(d$DATE),"-01",sep="") #Convert month to first date of the month

d2$Date = as.Date(d2$Date)  #Change to date type
d$DATE = as.Date(d$DATE)  #Change to date type

df1 = subset(d, select = c(DATE,TMIN)) #Create dataframe with relevant data from weather dataframe
df2 = subset(d2, select = c(Date,Soybean.Meal)) #Create dataframe with relevant data from price dataframe

df11 = df1 %>% 
  group_by(Date= lubridate::floor_date(DATE, 'month')) %>%
  summarize(AVTMIN = mean(TMIN))  #Convert the weather data to average precipitations per month


p = merge(df11,df2,by = 'Date')

fit = Arima(p$Soybean.Meal,xreg = p$AVTMIN, order=c(1,1,0))
fit

plot(p$Date,p$Soybean.Meal,col="red", type = "l",ylab = 'Fuel price index',xlab = 'Date')
lines(p$Date,fitted(fit),col="blue")

fit2 = Arima(p$Soybean.Meal, order=c(1,1,0))
fit2

fit3 = auto.arima(p$Soybean.Meal,xreg = p$AVTMIN)
fit3
