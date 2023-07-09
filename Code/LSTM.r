library(zoo)
library(tidyverse)
library(tseries)
library(TSstudio)
library(keras)
library(tensorflow)
library(TSLSTM)

#read comodity prices and convert to xts object
com_prices_data = read.csv("commodity-prices-2016.csv")
com_prices_data$Date = as.yearmon(com_prices_data$Date)
prices_ts = xts(com_prices_data$Wheat, com_prices_data$Date, frequency = 12)
colnames(prices_ts) = "WheatPrice"

#read temperature data and insert date column
te_data = read.csv("average_monthly_temperature_by_state_1950-2022.csv")
te_data$Date = paste("01", te_data$month,te_data$year, sep = "-")
te_data$Date = as.yearmon(te_data$Date, format = "%d-%m-%Y")

#transform temperature data to colums=states
te_data2 = te_data %>% 
   pivot_wider(id_cols = Date, names_from = state, values_from = average_temp)

#convert to xts object and filter to range 1980-2016
te_data2_ts = xts(te_data2[,1-ncol(te_data2)], te_data2$Date, frequency = 12)
te_data2_ts = te_data2_ts[,-1]
te_data2_ts = te_data2_ts["1980-02-01/2016-02-01"]
#te_data2_ts = apply(te_data2_ts,2,as.numeric)
storage.mode(te_data2_ts) = "double"

ts_plot(prices_ts)

#decompose wheat prices timeseries
ts_decompose(prices_ts)

adf.test(prices_ts)
stat_prices = log(prices_ts, differences = 1)
head(stat_prices)

ts_decompose(stat_prices)

lstm_data = merge(te_data2_ts, prices_ts)

TSLSTM<-ts.lstm(ts = lstm_data$Wheat, 
                xreg = lstm_data[,-ncol(lstm_data)], 
                tsLag=2, 
                xregLag = 0,
                LSTMUnit = 100, 
                #DropoutRate = 0.5,
                Epochs=50)
TSLSTM$AccuracyTable

tested= TSLSTM$TestPredictedValue
fitted= TSLSTM$TrainFittedValue
results = lstm_data$Wheat
results$Fit = c(rep(NA, 2), fitted, rep(NA,(nrow(results)-nrow(fitted)-2)))
results$Test = c(rep(NA, nrow(fitted)+2), tested, rep(NA, 2))
#train_actual = actual_data[c((1+1):n_train)]
  #test_actual<-actual_data[c((1+n_train+1):(n_train+n_test))]
ts_plot(results)
