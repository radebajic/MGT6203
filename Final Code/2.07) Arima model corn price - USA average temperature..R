library(lmtest)
library(stats)
library(dplyr)
library(lubridate)
library(tidyverse)
library(forecast)
library(readxl)

d = read_excel('df_clean.xlsx')

#head(d)

offset = 1
temp = d$corn_diff[((offset+1):length(d$average_temp))] #Cut of temperature so the right offset is used
date = d$date[(1:(length(d$date)-offset))]

df1 = data.frame(date,temp) #Create dataframe with relevant data from weather dataframe
df2 = subset(d, select = c(date,corn_diff)) #Create dataframe with relevant data from price dataframe

p = merge(df1,df2,by = 'date')

fit = arima(p$corn_diff,xreg = p$temp, order=c(2,0,1))
fit
accuracy(fit)

plot(p$date,p$corn_diff,col="red", type = "l",ylab = 'Corn price',xlab = 'Date')
lines(p$date,fitted(fit),col="blue")
#legend(x = 5000, y=0.2, legend = c('True values','Fitted values'), col = c('red','blue'), lty=1, cex=0.8)

fit2 = Arima(p$corn_diff, order=c(0,1,0))
fit2
accuracy(fit2)

fit3 = auto.arima(p$corn_diff,xreg = p$temp)
fit3
