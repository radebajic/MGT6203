library(zoo)
library(tidyverse)
library(tsibble)
library(remotes)
library(dplyr)
library(caret)
library(tidymodels)
library(tft)
library(lubridate)
library(timetk)
library(tibbletime)
library(luz)
library(torch)
set.seed(565)

options(torch.threshold_call_gc = 6000)

#read comodity prices and convert to xts object
com_prices_data = read.csv("commodity-prices-2016.csv")

com_prices_data = select(com_prices_data, Date, #Crude.Oil.petroleum,Aluminum,Bananas,Barley,Beef,Coal,Cocoa.beans,Coffee.Other.Mild.Arabicas,Coffee.Robusta,Rapeseed.oil,Copper,Cotton,Fishmeal,Groundnuts.peanuts,Hides,Lamb,Lead,Soft.Logs,Hard.Logs,Maize.corn,Olive.Oil,Oranges,Palm.oil,Poultry.chicken,Rice,Rubber,Fish.salmon,Hard.Sawnwood,Soft.Sawnwood,Shrimp,Soybean.Meal,Soybean.Oil,Soybeans,Sunflower.oil,Tea,Tin,Uranium,
                         Wheat)

com_prices_data$Date = as.Date(com_prices_data$Date, format = "%Y-%m-%d")

require(curl) # To load from url for data.table's fread
require(data.table) # High performance data frame
require(zoo) # High performance data frame
com_prices_data2 = as.data.table(com_prices_data)
com_prices_data2[, Day := as.Date(Date)]
setkey(com_prices_data2, Day)
start = com_prices_data2$Date[1]
end = tail(com_prices_data2$Date, 1)

date.indexes = seq(
  from = start,
  to = end,
  by = "days"
)

com_prices_data_daily = com_prices_data2[J(date.indexes), roll = 31]

com_prices_data3 <- com_prices_data_daily %>% 
  mutate(Week = lubridate::floor_date(Day, unit = "week")) %>% 
  group_by(Week) %>% 
  summarise(across(everything(), .fns = ~mean(.x, na.rm = TRUE)), .groups = "drop") %>% 
pivot_longer(c(
#Crude.Oil.petroleum,Aluminum,Bananas,Barley,Beef,Coal,Cocoa.beans,Coffee.Other.Mild.Arabicas,Coffee.Robusta,Rapeseed.oil,Copper,Cotton,Fishmeal,Groundnuts.peanuts,Hides,Lamb,Lead,Soft.Logs,Hard.Logs,Maize.corn,Olive.Oil,Oranges,Palm.oil,Poultry.chicken,Rice,Rubber,Fish.salmon,Hard.Sawnwood,Soft.Sawnwood,Shrimp,Soybean.Meal,Soybean.Oil,Soybeans,Sunflower.oil,Tea,Tin,Uranium,
    Wheat)) %>% 
       rename("Price" = "value", "Commodity" = "name")  %>%
group_by(Week, Commodity)  %>%
summarise(across(everything(), .fns = ~mean(.x, na.rm = TRUE)), .groups = "drop")
tt_p = com_prices_data3

tt = tt_p[,-(3:4)]
colnames(tt)[1] = "date"

last_date <- max(tt$date)
train <- tt %>% filter(date <= (last_date - lubridate::weeks(48)))
valid <- tt %>% filter(date > (last_date - lubridate::weeks(48)),
                                date <= (last_date - lubridate::weeks(12)))
test <- tt %>% filter(date > (last_date - lubridate::weeks(12)))

rec <- recipe(Price ~ ., data = train) %>% 
  step_mutate(
    date_time_since_begining = as.numeric(difftime(
      time1 = date, 
      time2 = lubridate::ymd(min(tt$date)), 
      units = "weeks"
    )),
    date_week = as.factor(lubridate::week(date)),
    date_month = as.factor(lubridate::month(date)),
    date_wday = as.factor(lubridate::wday(date))
  ) %>% 
  step_normalize(all_numeric_predictors())

spec <- tft_dataset_spec(rec, train) %>% 
  spec_covariate_index(date) %>%
  spec_covariate_key(Commodity) %>%
  spec_covariate_known(starts_with("date_")) %>%
  spec_time_splits(lookback = 5*25, horizon = 3)

spec <- prep(spec)
spec

model <- temporal_fusion_transformer(
  spec,
  hidden_state_size = 4,
  learn_rate = 1e-3,
  #dropout = 0.5,
  num_attention_heads = 1,
  num_lstm_layers = 1
)

fitted <- model %>%
  fit(
    transform(spec),
    valid_data = transform(spec, new_data = valid),
    epochs = 5,
      callbacks = list(
      luz::luz_callback_keep_best_model(monitor = "valid_loss"),
      luz::luz_callback_early_stopping(
        monitor = "valid_loss",
        patience = 5,
        min_delta = 0.001
      )
    ),
    verbose = TRUE,
    dataloader_options = list(batch_size = 32, num_workers = 4)
      
  )

fitted %>%
  luz::evaluate(
    transform(spec, new_data = test, past_data = bind_rows(train, valid)),
    metrics = list(luz_metric_rmse(),luz_metric_mae())
  )


fitted

forecasts <- generics::forecast(fitted, past_data = bind_rows(train, valid))
as.data.frame(forecasts)

options(repr.plot.width = 10, repr.plot.height =10)
tt %>% 
  filter(date > lubridate::ymd("2015-05-01")) %>% 
  full_join(forecasts) %>% 
  #filter(Commodity == "Copper") %>% 
  ggplot(aes(x = date, y = Price)) +
  geom_line() +
  geom_line(aes(y = .pred), color = "green") +
  geom_ribbon(aes(ymin = .pred_lower, ymax = .pred_upper), alpha = 0.3)
  #facet_wrap(~Commodity)
#theme(text = element_text(size = 10),element_line(size =1))
