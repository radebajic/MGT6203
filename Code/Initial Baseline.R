library(stats)

d = read.csv('commodity-prices-2016.csv')   #Create dataframe from data
head(d)     #Explore data
a = ts(d$Agricultural.Raw.Materials.Index, frequency = 12) #Give a shorter name to the agricultural raw materials index
date = as.Date(d$Date)                    #Create date array from the date data

model = HoltWinters(a,seasonal = 'additive')

model$SSE
model$fitted

plot(date,a, col = 'red', , type = "l", ylab = 'Agricultural raw materials index',xlab = 'Year') #Plot the original data in red

b = data.frame(model$fitted)  #Convert the results of the smoothing into a data frame
lines(date[13:length(date)],b$xhat, col = 'green') # plot the smoothing data in green

legend(4000,160,legend=c('Real data','Fitted data'),col=c('red','green'),pch = c('-','-'))
