source("data_cleaning_and_exploration.R")

# install.packages("poissonreg")
library(poissonreg)

pois_recipe <- recipe(
  rides ~ hour_of_day + temp + precip +
    windspeed + windgust + winddir + 
    cloudcover + visibility + dew + 
    humidity, 
  data = trip_hourly_train
) %>%
  step_impute_mean(all_numeric_predictors()) #handling missing values

pois_spec <- poisson_reg() %>%
  set_engine("glm")

pois_workflow <- workflow() %>%
  add_recipe(pois_recipe) %>%
  add_model(pois_spec)

# Only used top 10 most used stations to train because using all the stations 
# exceeded my memory (Temporary fix)
pois_fits <- lapply(df_list_top10, function(df) {
  fit(pois_workflow, data = df)
})

# Evaluation

pois_fits_top10 <- pois_fits[top10_stations]

pred_list_pois <- map2(
  pois_fits_top10,
  df_list_val[names(pois_fits_top10)],
  ~ augment(.x, new_data = .y)
)

pred_df_pois <- bind_rows(pred_list_pois, .id = "station")

# Poisson log-likelihood by station
ll_by_station <- pred_df_pois %>%
  group_by(station) %>%
  summarise(
    avg_loglik = mean(
      dpois(rides, lambda = .pred, log = TRUE)
    )
  )

ll_by_station

pois_deviance <- mean(
  dpois(
    x = pred_df_pois$rides,
    lambda = pred_df_pois$.pred,
    log = TRUE
  )
)
pois_deviance

# dispersion
dispersion_by_station <- map_dfr(
  pois_fits,
  function(fit) {
    glm_fit <- pull_workflow_fit(fit)$fit
    data.frame(
      dispersion = sum(residuals(glm_fit, type = "pearson")^2) /
        glm_fit$df.residual
    )
  },
  .id = "station"
)

dispersion_by_station

# RMSE scores
rmse_df_pois <- pred_df_pois %>%
  group_by(station) %>%
  summarise(RMSE = sqrt(mean((rides - .pred)^2)))

ggplot(rmse_df_pois, aes(x = reorder(station, RMSE), y = RMSE)) +
  geom_col() +
  coord_flip() +
  labs(title = "RMSE per Station",
       x = "Station",
       y = "RMSE") +
  theme_bw()


