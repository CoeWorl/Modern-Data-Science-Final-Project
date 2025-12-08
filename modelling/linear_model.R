source("data_cleaning_and_exploration.R")

citi_bike_train <- citi_bike_train %>%
  mutate(start_hour = floor_date(started_at, unit = "hour"))
citi_bike_train <- citi_bike_train %>%
  left_join(oct_weather_train, by = c("start_hour" = "datetime"))
