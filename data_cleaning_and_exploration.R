# Loading in Libraries
library(tidyverse)
library(tidymodels)
library(factoextra)
library(ggplot2)
library(sf)
library(maps)
library(nycgeo)
library(viridis)

# Loading in Data
# Download the data from the links provided in the README file
# and provide the file paths below
citi_bike_data_1 <- read.csv("Your file path here")
citi_bike_data_2 <- read.csv("Your file path here")
citi_bike_data_3 <- read.csv("Your file path here")
citi_bike_data_4 <- read.csv("Your file path here")
citi_bike_data_5 <- read.csv("Your file path here")

subway_stations <- read.csv("Your file path here")
oct_weather <- read.csv("Your file path here")

# Combining all the citi bike data
citi_bike_data <- rbind(citi_bike_data_1, citi_bike_data_2, citi_bike_data_3,
                        citi_bike_data_4, citi_bike_data_5)

# Dropping NAs in the coordinates columns
citi_bike_data <- citi_bike_data %>%
  drop_na(end_lng, end_lat, start_lng, start_lat)

# Changing times to datetime format in citi_bike and oct_weather
citi_bike_data <- citi_bike_data %>%
  mutate(started_at = as.POSIXct(started_at, format = "%Y-%m-%d %H:%M:%S"),
         ended_at = as.POSIXct(ended_at, format = "%Y-%m-%d %H:%M:%S"))

oct_weather <- oct_weather %>%
  mutate(datetime = as.POSIXct(datetime, format = "%Y-%m-%dT%H:%M:%S"))

# Changing station id columns from chr to dbl
citi_bike_data <- citi_bike_data %>%
  mutate(start_station_id = as.double(start_station_id),
         end_station_id = as.double(end_station_id))

# Splitting to train/test sets
# How we will split: 
# We decided to split the dataset by the day. We will have every 5th day as
# part of the test set and the first 4 days will be a part of the training set
#
# For example:
# Oct 1 - 4 -> training set, Oct 5 -> test set
# Oct 6 - 9 -> training set, Oct 10 -> test set
# 
# We will use the data from every hour, so the bike data will be tracked by the 
# of the start time of the trip.

citi_bike_data <- citi_bike_data %>%
  mutate(day = day(started_at))

oct_weather <- oct_weather %>%
  mutate(day = day(datetime))

citi_bike_train <- citi_bike_data %>%
  filter(day %% 5 != 0)
citi_bike_test <- citi_bike_data %>%
  filter(day %% 5 == 0)

oct_weather_train <- oct_weather %>%
  filter(day %% 5 != 0)
oct_weather_test <- oct_weather %>%
  filter(day %% 5 == 0)

# dropping day column
citi_bike_train <- citi_bike_train %>%
  select(-day)
citi_bike_test <- citi_bike_test %>%
  select(-day)

oct_weather_train <- oct_weather_train %>%
  select(-day)
oct_weather_test <- oct_weather_test %>%
  select(-day)
