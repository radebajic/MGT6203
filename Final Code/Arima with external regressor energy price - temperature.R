library(lmtest)
library(stats)
library(dplyr)
library(lubridate)
library(tidyverse)
library(forecast)

d = read.csv('average_monthly_temperature_by_state_1950-2022.csv')
d2 = read.csv('commodity-prices-2016.csv')


d2$Fuel.Energy.Index

head(d)

d$Date = paste(as.character(d$year),as.character(d$month),"01",sep="-") #Convert month to first date of the month
d$Date = as.Date(d$Date) #Change to date type

d2$Date = as.Date(d2$Date)  #Change to date type

offset = 1
temp = d$average_temp[((offset+1):length(d$average_temp))] #Cut of temperature so the right offset is used
Date = d$Date[(1:(length(d$Date)-offset))]

df1t = data.frame(Date,temp)

df1 = data.frame(Date,temp) #Create dataframe with relevant data from weather dataframe
df2 = subset(d2, select = c(Date,Fuel.Energy.Index)) #Create dataframe with relevant data from price dataframe

p = merge(df1,df2,by = 'Date')

ptrain = head(p,round(length(p$Date)*0.8))
h = length(p$Date) - length(ptrain$Date)
ptest = tail(p, h)


fit = arima(p$Fuel.Energy.Index,xreg = p$temp, order=c(0,1,0))
fit
accuracy(fit)

plot(p$Date,p$Fuel.Energy.Index,col="red", type = "l",ylab = 'Fuel price index',xlab = 'Date')
lines(p$Date,fitted(fit),col="blue")
legend(x = 3500, y=220, legend = c('True values','Fitted values'), col = c('red','blue'), lty=1, cex=0.8)

fit2 = Arima(ptrain$Fuel.Energy.Index, order=c(1,1,0))
fit2
accuracy(fit2)

fit3 = auto.arima(ptrain$Fuel.Energy.Index,xreg = p$average_temp,)
fit3
