library(tidyverse)
library(tidymodels)
library(lubridate)
library(rsample)
library(recipes)
library(dplyr)
library(data.table)

# Loading in Data
# Download the data from the links provided in the README file
# and provide the file paths below
citi_bike_data_1 <- read.csv("~/Documents/Data Science Class/Modern-Data-Science-Final-Project/data/202510-citibike-tripdata/202510-citibike-tripdata_1.csv")
citi_bike_data_2 <- read.csv("~/Documents/Data Science Class/Modern-Data-Science-Final-Project/data/202510-citibike-tripdata/202510-citibike-tripdata_2.csv")
citi_bike_data_3 <- read.csv("~/Documents/Data Science Class/Modern-Data-Science-Final-Project/data/202510-citibike-tripdata/202510-citibike-tripdata_3.csv")
citi_bike_data_4 <- read.csv("~/Documents/Data Science Class/Modern-Data-Science-Final-Project/data/202510-citibike-tripdata/202510-citibike-tripdata_4.csv")
citi_bike_data_5 <- read.csv("~/Documents/Data Science Class/Modern-Data-Science-Final-Project/data/202510-citibike-tripdata/202510-citibike-tripdata_5.csv")

oct_weather <- read.csv("~/Documents/Data Science Class/Modern-Data-Science-Final-Project/data/NYC_October2025_Weather.csv")

# Combining all the citi bike data
citi_bike_data <- rbind(citi_bike_data_1, citi_bike_data_2, citi_bike_data_3,
                        citi_bike_data_4, citi_bike_data_5)

# Dropping NAs in the coordinates columns
citi_bike_data <- citi_bike_data %>%
  drop_na(end_lng, end_lat, start_lng, start_lat)

df <- citi_bike_data %>%
  mutate(
    trip_duration = as.numeric(difftime(ended_at, started_at, units = "mins")),
    start_hour = hour(started_at),
    member_casual = factor(member_casual),
    start_station_id = factor(start_station_id),
    end_station_id = factor(end_station_id)
  )

weather <- oct_weather %>%
  dplyr::select(datetime, temp, feelslike, precip, windspeed, cloudcover, conditions, visibility)

# In order to not exceed memory limit, convert the frames into data tables and
# use a rolling join (less memory capacity)

dt <- as.data.table(df)
weather_dt <- as.data.table(weather)

# Convert Citi Bike columns to POSIXct
dt[, started_at := ymd_hms(started_at)]
dt[, ended_at := ymd_hms(ended_at)]

# Convert weather columns to POSIXct
weather_dt[, datetime := ymd_hms(datetime)]
# Round bike times to nearest hour
dt[, start_hourly := floor_date(started_at, unit = "hour")]
weather_dt[, weather_hour := floor_date(datetime, unit = "hour")]

# Set keys for rolling join
setkey(dt, start_hourly)
setkey(weather_dt, weather_hour)

# Rolling join: match each bike trip to closest weather record in the past
bike_with_weather <- weather_dt[dt, on = .(weather_hour = start_hourly), roll = "nearest"]



# Feature engineering
  bike_with_weather <- bike_with_weather %>%
    mutate(day = day(started_at))
  
  train_data <- bike_with_weather %>%
    filter(day %% 5 != 0)
  test_data <- bike_with_weather %>%
    filter(day %% 5 == 0)
  

duration_recipe <- recipe(trip_duration ~ start_hour +
                            member_casual + temp + feelslike + precip + windspeed +
                            cloudcover + conditions + visibility, data = train_data) %>%
  step_dummy(all_nominal_predictors())

duration_model <- boost_tree(trees = 500, min_n = 5, learn_rate = 0.1) %>%
  set_engine("xgboost") %>%
  set_mode("regression")

duration_workflow <- workflow() %>%
  add_recipe(duration_recipe) %>%
  add_model(duration_model)

duration_fit <- duration_workflow %>%
  fit(data = train_data)

duration_preds <- predict(duration_fit, test_data)
duration_preds <- as.data.table(duration_preds)
duration_preds[, trip_duration := test_data$trip_duration]



# Evaluation

rmse_val <- rmse_vec(truth = duration_preds$trip_duration,
                     estimate = duration_preds$.pred)
print(rmse_val)
# Score: 20.56
#