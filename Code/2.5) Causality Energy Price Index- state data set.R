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

d = d[(d$state == 'New York'),] #Filter for New York data

df1 = subset(d, select = c(Date,average_temp)) #Create dataframe with relevant data from weather dataframe
df2 = subset(d2, select = c(Date,Fuel.Energy.Index)) #Create dataframe with relevant data from price dataframe

p = merge(df1,df2,by = 'Date')

p_value = c()
month_order = c()

for (i in 1:15){
model = grangertest(p$Fuel.Energy.Index~p$average_temp, order = i)
month_order = c(month_order,i)
p_value = c(p_value,model$`Pr(>F)`[2])
}

plot(month_order,p_value,ylab = 'p value', xlab = 'Months of lag', main = 'Causality energy price based on temperature')


