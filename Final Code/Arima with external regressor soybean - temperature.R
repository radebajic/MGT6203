library(lmtest)
library(stats)
library(dplyr)
library(lubridate)
library(tidyverse)
library(forecast)

d = read.csv('average_monthly_temperature_by_state_1950-2022.csv')
d2 = read.csv('commodity-prices-2016.csv')

head(d)

d$Date = paste(as.character(d$year),as.character(d$month),"01",sep="-") #Convert month to first date of the month
d$Date = as.Date(d$Date) #Change to date type

d2$Date = as.Date(d2$Date)  #Change to date type

offset = 5
temp = d$average_temp[((offset+1):length(d$average_temp))] #Cut of temperature so the right offset is used
Date = d$Date[(1:(length(d$Date)-offset))]

df1 = data.frame(Date,temp) #Create dataframe with relevant data from weather dataframe
df2 = subset(d2, select = c(Date,Soybean.Meal)) #Create dataframe with relevant data from price dataframe

p = merge(df1,df2,by = 'Date')

fit = arima(p$Soybean.Meal,xreg = p$temp, order=c(0,1,0))
fit
accuracy(fit)

plot(p$Date,p$Soybean.Meal,col="red", type = "l",ylab = 'Fuel price index',xlab = 'Date')
lines(p$Date,fitted(fit),col="blue")
legend(x = 3500, y=500, legend = c('True values','Fitted values'), col = c('red','blue'), lty=1, cex=0.8)

fit2 = Arima(p$Soybean.Meal, order=c(0,1,0))
fit2
accuracy(fit2)

fit3 = auto.arima(p$Soybean.Meal,xreg = p$average_temp,)
fit3
