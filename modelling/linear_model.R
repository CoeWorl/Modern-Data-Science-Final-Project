source("data_cleaning_and_exploration.R")

citi_bike_train <- citi_bike_train %>%
  mutate(start_hour = floor_date(started_at, unit = "hour"))
citi_bike_train <- citi_bike_train %>%
  left_join(oct_weather_train, by = c("start_hour" = "datetime"))

# Creating the validation dataset
# validation days: 10/1, 10/6, 10/11, 10/16, 10/21, 10/26, 10/31
citi_bike_train <- citi_bike_train %>%
  mutate(day = day(started_at))

citi_bike_val <- citi_bike_train %>%
  filter(day %% 5 == 1)
citi_bike_train <- citi_bike_train %>%
  filter(day %% 5 != 1)


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

# splitting training data by station
df_list <- split(trip_hourly_train, trip_hourly_train$start_station_name)
df_list_val <- split(trip_hourly_val, trip_hourly_val$start_station_name)

# fitting the model onto all the data sets
lm_fits <- lapply(df_list, function(df) {
  fit(lm_workflow, data = df)
})

lapply(lm_fits["9 Ave & W 33 St"], tidy)

# Evaluation
# coefficient table that gets the coefficients for all variables across all stations
coef_table <- map_df(
  lm_fits,
  ~ tidy(.x),
  .id = "station"
)

# plotting coefficient estimates
# seeing how the variables affect each station differently
coef_top10 %>%
  filter(term != "(Intercept)") %>%
  ggplot(aes(x = term, y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~ station, scales = "free_y") +
  labs(title = "Model Coefficient Estimates by Station",
       x = "Predictor",
       y = "Estimate") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Finding the top 10 used stations and looking at plots on those 10
top10_stations <- trip_hourly_train %>%
  group_by(start_station_name) %>%
  summarise(total_rides = sum(rides)) %>%
  arrange(desc(total_rides)) %>%
  slice_head(n=10) %>%
  pull(start_station_name)

# coefficient table of the 10 stations
coef_top10 <- coef_table %>%
  filter(term != "(Intercept)") %>%
  filter(station %in% top10_stations)

# plotting coefficient heatmap of top 10 stations
ggplot(coef_top10, aes(x = term, y = station, fill = estimate)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "blue",
    mid = "white",
    high = "red",
    midpoint = 0
  ) +
  labs(
    title = "Coefficient Heatmap (Top 10 Most Used Stations",
    x = "Predictor",
    y = "Station",
    fill = "Estimate"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Prediction vs Actual Plots
# filtering df_list and lm_fits to the top 10 stations
df_list_top10 <- df_list[top10_stations]
lm_fits_top10 <- lm_fits[top10_stations]

# creating predictions for each station
pred_list_top10 <- map2(
  lm_fits_top10,
  df_list_val[names(lm_fits_top10)],
  ~ augment(.x, new_data = .y)
)

# making a single dataframe
pred_df_top10 <- bind_rows(pred_list_top10, .id = "station")

# Plot
ggplot(pred_df_top10, aes(x = .pred, y = rides)) +
  geom_point(alpha = 0.4) +
  geom_abline(color = "red") + 
  facet_wrap(~ station, scales = "free") +
  labs(
    title = "Predicted vs Actual Rides (Top 10 Most Used Stations",
    x = "Predicted Rides",
    y = "Actual Rides"
  ) +
  theme_bw()

# RMSE scores
rmse_df_top10 <- pred_df_top10 %>%
  group_by(station) %>%
  summarise(RMSE = sqrt(mean((rides - .pred)^2)))

ggplot(rmse_df_top10, aes(x = reorder(station, RMSE), y = RMSE)) +
  geom_col() +
  coord_flip() +
  labs(title = "RMSE per Station",
       x = "Station",
       y = "RMSE") +
  theme_bw()
