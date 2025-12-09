source("data_cleaning_and_exploration.R")

library(ranger)

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
  set_engine("ranger")

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


