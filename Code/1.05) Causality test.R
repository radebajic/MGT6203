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

dsoy = as.Date(d2$Date) #Dates from the commodity set, not used further
Soy = d2$Soybean.Meal #Soybean meal prices, not used further

d2$Date = as.Date(d2$Date)  #Change to date type
d$DATE = as.Date(d$DATE)  #Change to date type

df1 = subset(d, select = c(DATE,PRCP)) #Create dataframe with relevant data from weather dataframe
df2 = subset(d2, select = c(Date,Soybean.Meal)) #Create dataframe with relevant data from price dataframe

df11 = df1 %>% 
  group_by(Date= lubridate::floor_date(DATE, 'month')) %>%
  summarize(AVPRCP = mean(PRCP))  #Convert the weather data to average precipitations per month

p = merge(df11,df2,by = 'Date')

model = grangertest(p$Soybean.Meal~p$AVPRCP, order = 3)
model


