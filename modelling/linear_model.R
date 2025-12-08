source("data_cleaning_and_exploration.R")

citi_bike_train <- citi_bike_train %>%
  mutate(start_hour = floor_date(started_at, unit = "hour"))
citi_bike_train <- citi_bike_train %>%
  left_join(oct_weather_train, by = c("start_hour" = "datetime"))

# Creating the validation dataset
# validation days: 10/1, 10/6, 10/11, 10/16, 10/21, 10/26, 10/31
citi_bike_train <- citi_bike_train %>%
  mutate(day = day(started_at))

citi_bike_train <- citi_bike_train %>%
  filter(day %% 5 != 1)
citi_bike_val <- citi_bike_train %>%
  filter(day %% 5 == 1)

citi_bike_train <- citi_bike_train %>%
  select(-day)
citi_bike_val <- citi_bike_val %>%
  select(-day)

# Getting number of rides at each station at the hour and some basic weather data
trip_hourly_train <- citi_bike_train %>% 
  mutate(start_hour = floor_date(started_at, "hour"),
         hour_of_day = hour(started_at)) %>%        # numeric (0–23)
  group_by(start_station_name, start_hour, hour_of_day) %>%
  summarise(rides = n(),
            temp = mean(temp),                     # or whatever weather fields you have
            feelslike = mean(feelslike),
            precip = mean(precip),
            windspeed = mean(windspeed),
            windgust = mean(windgust),
            winddir = mean(winddir),
            cloudcover = mean(cloudcover),
            visibility = mean(visibility),
            dew = mean(dew),
            humidity = mean(humidity),
            .groups = "drop")

trip_hourly_val <- citi_bike_val %>% 
  mutate(start_hour = floor_date(started_at, "hour"),
         hour_of_day = hour(started_at)) %>%        # numeric (0–23)
  group_by(start_station_name, start_hour, hour_of_day) %>%
  summarise(rides = n(),
            temp = mean(temp),                     # or whatever weather fields you have
            feelslike = mean(feelslike),
            precip = mean(precip),
            windspeed = mean(windspeed),
            windgust = mean(windgust),
            winddir = mean(winddir),
            cloudcover = mean(cloudcover),
            visibility = mean(visibility),
            dew = mean(dew),
            humidity = mean(humidity),
            .groups = "drop")

# Preprocessing data
lm_recipe <- recipe(rides ~ 
                            hour_of_day +
                            temp + precip + windspeed +
                      windgust + winddir + cloudcover + visibility +
                      dew + humidity, data = trip_hourly_train)
# Model specs
linear_model <- linear_reg(mode = "regression") %>%
  set_engine("lm")
# Workflow
lm_workflow <- workflow() %>%
  add_model(linear_model) %>%
  add_recipe(lm_recipe)

# splitting training data
df_list <- split(trip_hourly_train, trip_hourly_train$start_station_name)

# fitting the model onto all the data sets
lm_fits <- lapply(df_list, function(df) {
  fit(lm_workflow, data = df)
})

lapply(lm_fits["9 Ave & W 33 St"], tidy)

# Evaluation
preds <- predict(lm_fit, trip_hourly_val)
metrics(preds, truth = rides, estimate = .pred)