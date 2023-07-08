
library(dplyr)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(forecast)
library(reshape2)

### Load datasets
file_path <- "commodity-prices-2016.csv"
data_commodity <- read.csv(file_path)
head(data_commodity)

file_path <- "average_monthly_temperature_by_state_1950-2022.csv"
data_weather <- read.csv(file_path)
head(data_weather)

file_path <- "US_macroeconomics.csv"
data_macro <- read.csv(file_path)
head(data_macro)

### Change to date type
data_commodity$Date <- as.Date(data_commodity$Date)
data_macro$Date <- as.Date(data_macro$date)
data_weather$Date <- as.Date(paste(data_weather$year, data_weather$month, "01", sep='-'))

### Clean Commodity dataset
na_columns <- colSums(is.na(data_commodity)) >0 # there are 5-6 columns with NA values. As these seem not to be very important, we delete these columns
# print(names(na_columns)[na_columns])
data_commodity <- data_commodity[, !na_columns]
data_commodity_filt <- data_commodity %>%
  filter(Date >= as.Date('1980-11-01') & Date <= as.Date('2016-02-01')) 

### Clean Weather dataset
na_columns <- colSums(is.na(data_weather)) >0 # there are no NA values in the dataset
#print(names(na_columns)[na_columns])
data_weather <- data_weather[c('Date', 'state', 'average_temp')] #only keep relevant columns for analysis
data_weather <- pivot_wider(data_weather, names_from = state, values_from = average_temp) # pivot states in columns
data_weather_filt <- data_weather %>%
  filter(Date >= as.Date('1980-11-01') & Date <= as.Date('2016-02-01')) 

### Clean Macro dataset
na_columns <- colSums(is.na(data_macro)) >0 # there are no NA values in the dataset
#print(names(na_columns)[na_columns])
data_macro <- data_macro[, -which(names(data_macro) == 'date')] #only keep relevant columns for analysis
data_macro_filt <- data_macro %>%
  filter(Date >= as.Date('1980-11-01') & Date <= as.Date('2016-02-01')) 

### Merge datasets
df <- merge(data_commodity_filt, data_macro_filt, by='Date')
df <- merge(df, data_weather_filt, by='Date') # merged dataset for analysis
colnames(df) <- gsub("\\.", "_", colnames(df))
column_names <- names(df)
print(column_names) # 1 = Date, 2:55 = Commodity Prices, 56:62 = Macro inputs, 63:110 = Average temeratures by state

### LM test
predictors <- df[, (which(names(df)=='CPI')):ncol(df)] # define all macro and weather variables
model <- lm(Beef ~ ., data=cbind(df['Beef'], predictors)) # adjust the dependent variable for an overview of the significance
summary(model)

# Correlation analysis
cor_matrix <- cor(df[, -1])  # Exclude the Date column from correlation calculation

# Variables with the highest correlation
max_correlation <- max(cor_matrix, na.rm = TRUE)
max_correlation_variables <- colnames(cor_matrix)[which(cor_matrix == max_correlation, arr.ind = TRUE)]
print(paste("Variables with the highest correlation:", max_correlation_variables))

# Visualisation
melted_correlation <- melt(cor_matrix, na.rm = TRUE)
color_palette <- c("#FF0000", "#FFFF00", "#00FF00")
ggplot(melted_correlation, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = color_palette, limits = c(-1, 1)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Correlation Matrix - Commodity Variables")

# ARIMA for variable with a highest correlation
# let's analyze the "Beef" commodity prices
beverage_prices <- df$Beverage_Price_Index
arima_model <- auto.arima(beef_prices)
print(arima_model)

# Principal Component Analysis (PCA)
pca_result <- prcomp(df[, 2:ncol(df)], scale = TRUE)  

# Extract the loadings for PC1 from the rotation matrix
loadings_pc1 <- pca_result$rotation[, 1]

# Find the variable with the largest absolute loading on PC1
largest_loading_variable <- names(loadings_pc1)[which.max(abs(loadings_pc1))]

# Print the result
print(largest_loading_variable)


# Scatter plot of Beverage_Price_Index prices vs. CPI
ggplot(data = df, aes(x = CPI, y = Beverage_Price_Index)) +
  geom_point() +
  labs(x = "CPI", y = "Beverage_Price_Index")

# Heatmap of correlation matrix
heatmap(cor_matrix)


