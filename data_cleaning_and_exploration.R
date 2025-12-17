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

# Creating the map of NYC
nyc_boroughs <- nycgeo::borough_sf
nyc_boroughs <- st_set_crs(nyc_boroughs, st_crs("EPSG:2263"))

bike_data_coords <- citi_bike_train %>%
  st_as_sf(coords = c("start_lng", "start_lat"), crs = 4326) %>%
  st_transform(st_crs(nyc_boroughs))

ggplot() +
  geom_sf(data = nyc_boroughs, fill = "gray95", color = "black", linewidth = 0.5) +
  geom_sf(data = bike_data_coords, size = 0.3, color = "blue", alpha = 0.3) +
  labs(title = "Citi Bike Trip Endpoints in NYC (Oct 2025)",
       caption = "Data source: Citi Bike 202510-tripdata") +
  theme_minimal()

# Most frequent stops used in NYC
bike_data_coords %>%
  count(start_station_name, name = "count") %>%
  arrange(desc(count))

counts <- bike_data_coords %>%
  count(start_station_name, name = "count") %>%
  arrange(desc(count))

# Heat map of the bikes used at each station
ggplot() +
  geom_sf(data = nyc_boroughs, fill = "gray95", color = "black", linewidth = 0.5) +
  geom_sf(data = counts %>% arrange(count), aes(color = count, size = count)) +
  scale_color_viridis_c(
    option = "plasma", 
    direction = -1,    
    name = "Station usage"
  ) +
  scale_size_continuous(range = c(0.5, 3),  name = "Station usage") +
  labs(title = "Heatmap of bike stations in NYC",
       x = "Longitude", y = "Latitude") +
  theme_minimal()
