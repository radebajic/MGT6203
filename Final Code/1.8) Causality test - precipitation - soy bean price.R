library(lmtest)
library(stats)
library(dplyr)
library(lubridate)
library(tidyverse)

d = read.csv('Aledo.csv') #Load in data
d2 = read.csv('commodity-prices-2016.csv') #Load in data
head(d)

d$DATE = paste(as.character(d$DATE),"-01",sep="") #Convert month to first date of the month
d$DATE = as.Date(d$DATE)

d2$Date = as.Date(d2$Date)  #Change to date type
d$DATE = as.Date(d$DATE)  #Change to date type

df1 = subset(d, select = c(DATE,PRCP)) #Create dataframe with relevant data from weather dataframe
df2 = subset(d2, select = c(Date,Soybean.Meal)) #Create dataframe with relevant data from price dataframe

df11 = df1 %>% 
  group_by(Date= lubridate::floor_date(DATE, 'month')) %>%
  summarize(AVPRCP = mean(PRCP))  #Convert the weather data to average precipitations per month

p = merge(df11,df2,by = 'Date')

p_value = c()
month_order = c()

for (i in 1:15){
  model = grangertest(p$Soybean.Meal~p$AVPRCP, order = i)
  #print(i)
  #print(model)
  month_order = c(month_order,i)
  p_value = c(p_value,model$`Pr(>F)`[2])
}

plot(month_order,p_value,ylab = 'p value', xlab = 'Months of lag', main = 'Causality soybean price based on precipitation')

p_value


