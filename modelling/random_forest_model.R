source("data_cleaning_and_exploration.R")

library(ranger)


# Finding the top 10 used stations and looking at plots on those 10
top10_stations <- trip_hourly_train %>%
  group_by(start_station_name) %>%
  summarise(total_rides = sum(rides)) %>%
  arrange(desc(total_rides)) %>%
  slice_head(n=10) %>%
  pull(start_station_name)

df_list <- split(trip_hourly_train, trip_hourly_train$start_station_name)
df_list_val <- split(trip_hourly_val, trip_hourly_val$start_station_name)


df_list_top10 <- df_list[top10_stations]


# Preprocessing data: using the same recipe as the linear model
rf_recipe <- recipe(rides ~ 
                      hour_of_day +
                      temp + precip + windspeed +
                      windgust + winddir + cloudcover + visibility +
                      dew + humidity, data = trip_hourly_train)

rf_spec <- rand_forest(
  mode = "regression",
  trees = 500
) %>%
  set_engine("ranger", importance = "impurity")

rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)

# Only used top 10 most used stations to train because using all the stations 
# exceeded my memory (Temporary fix)
rf_fits <- lapply(df_list_top10, function(df) {
  fit(rf_workflow, data = df)
})


# Evaluation

rf_fits_top10 <- rf_fits[top10_stations]

pred_list_rf <- map2(
  rf_fits_top10,
  df_list_val[names(rf_fits_top10)],
  ~ augment(.x, new_data = .y)
)

pred_df_rf <- bind_rows(pred_list_rf, .id = "station")



# RMSE scores
rmse_df_rf <- pred_df_rf %>%
  group_by(station) %>%
  summarise(RMSE = sqrt(mean((rides - .pred)^2)))

ggplot(rmse_df_rf, aes(x = reorder(station, RMSE), y = RMSE)) +
  geom_col() +
  coord_flip() +
  labs(title = "RMSE per Station",
       x = "Station",
       y = "RMSE") +
  theme_bw()


# Predicted vs Actual
ggplot(pred_df_rf, aes(x = .pred, y = rides)) +
  geom_point(alpha = 0.4) +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  facet_wrap(~ station, scales = "free") +
  labs(
    title = "Random Forest: Predicted vs Actual Rides (Top 10 Stations)",
    x = "Predicted rides",
    y = "Actual rides"
  ) +
  theme_bw()

rf_model <- pull_workflow_fit(rf_fits[[5]])$fit
rf_model$variable.importance

importance_df <- data.frame(
  variable = names(rf_model$variable.importance),
  importance = rf_model$variable.importance
)

ggplot(importance_df, aes(x = reorder(variable, importance), y = importance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Random Forest Variable Importance") +
  theme_minimal()

#------------------------------------------------------------------------

# Using Test Data

citi_bike_test <- citi_bike_test %>%
  mutate(start_hour = floor_date(started_at, unit = "hour"))
citi_bike_test <- citi_bike_test %>%
  left_join(oct_weather_test, by = c("start_hour" = "datetime"))

# Getting number of rides at each station at the hour and some basic weather data
trip_hourly_test <- citi_bike_test %>% 
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

df_list_test <- split(trip_hourly_test, trip_hourly_test$start_station_name)

test_pred_rf_list <- map2(
  rf_fits_top10,
  df_list_test[names(rf_fits_top10)],
  ~ augment(.x, new_data = .y)
)

test_pred_rf_df <- bind_rows(test_pred_rf_list, .id = "station")

# RMSE scores
rmse_test_rf <- test_pred_rf_df %>%
  group_by(station) %>%
  summarise(RMSE = sqrt(mean((rides - .pred)^2)))

ggplot(rmse_test_rf, aes(x = reorder(station, RMSE), y = RMSE)) +
  geom_col() +
  coord_flip() +
  labs(title = "RMSE per Station",
       x = "Station",
       y = "RMSE") +
  theme_bw()
