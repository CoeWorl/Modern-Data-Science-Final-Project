
#Traning Test split of data pulling out 6 days of the month

citibike_weather <- citibike_weather %>%
  mutate(day = day(started_at))

test_days <- seq(5, 30, by = 5)
test_days

test_data <- citibike_weather %>%
  filter(day %in% test_days)

train_data <- citibike_weather %>%
  filter(!day %in% test_days)

table(train_data$day0)
table(test_data$day)




library(tidymodels)
library(dplyr)
#Create the trip duration in minutes
train_data <- train_data %>%
  mutate(
    trip_duration = as.numeric(difftime(ended_at, started_at, units = "secs")),
    trip_duration_min = trip_duration / 60
  )


#Incase the big times mess with model 
filtered_train_data <- train_data %>%
  filter(trip_duration_min > 0,
         trip_duration_min < 120)
#Select predictors
predictors <- c(
  "rideable_type",
  "member_casual",
  "start_hour",
  "day",
  "temp", "feelslike", "humidity",
  "precip", "windspeed", "visibility",
  "cloudcover", "uvindex", "sealevelpressure"
)
#Baseline model 
mean_duration <- mean(filtered_train_data$trip_duration_min, na.rm = TRUE)

mean_preds <- filtered_train_data %>%
  mutate(.pred = mean_duration)

mean_metrics <- metrics(
  mean_preds,
  truth = trip_duration_min,
  estimate = .pred
)

mean_metrics

#Linear model 
lm_recipe <- recipe(
  trip_duration_min ~ .,
  data = filtered_train_data %>% select(trip_duration_min, all_of(predictors))
) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


lm_spec <- linear_reg() %>%
  set_engine("lm")

lm_workflow <- workflow() %>%
  add_recipe(lm_recipe) %>%
  add_model(lm_spec)

lm_fit <- lm_workflow %>%
  fit(data = filtered_train_data)

lm_preds <- predict(lm_fit, filtered_train_data) %>%
  bind_cols(filtered_train_data)

lm_metrics <- metrics(
  lm_preds,
  truth = trip_duration_min,
  estimate = .pred
)

lm_metrics


#Log model 
log_recipe <- recipe(
  trip_duration_min ~ .,
  data = filtered_train_data %>% 
    select(trip_duration_min, all_of(predictors))
) %>%
  step_log(trip_duration_min, skip=TRUE) %>%     
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())


log_lm_workflow <- workflow() %>%
  add_recipe(log_recipe) %>%
  add_model(lm_spec)

log_lm_fit <- log_lm_workflow %>%
  fit(data = filtered_train_data)


log_preds <- predict(log_lm_fit, filtered_train_data) %>%
  mutate(.pred = exp(.pred)) %>%
  bind_cols(filtered_train_data)

log_metrics <- metrics(
  log_preds,
  truth = trip_duration_min,
  estimate = .pred
)

log_metrics
#Looking at metrics 
bind_rows(
  mean_metrics  %>% mutate(model = "Mean baseline"),
  lm_metrics    %>% mutate(model = "Linear regression"),
  log_metrics   %>% mutate(model = "Log-linear regression")
) %>%
  select(model, .metric, .estimate)



# Linear model residuals (TRAIN)
lm_train_preds <- predict(lm_fit, filtered_train_data) %>%
  bind_cols(filtered_train_data) %>%
  mutate(residual = trip_duration_min - .pred)

# Log-linear residuals (TRAIN)
log_train_preds <- predict(log_lm_fit, filtered_train_data) %>%
  mutate(.pred = exp(.pred)) %>%
  bind_cols(filtered_train_data) %>%
  mutate(residual = trip_duration_min - .pred)




#Random Forest 


rf_train <- filtered_train_data %>%
  select(
    trip_duration_min,
    rideable_type,
    member_casual,
    start_hour,
    day,
    temp, feelslike, humidity,
    precip, windspeed, visibility,
    cloudcover, uvindex, sealevelpressure
  )


rf_train <- rf_train %>%
  mutate(
    rideable_type = as.factor(rideable_type),
    member_casual = as.factor(member_casual),
    day = as.factor(day)
  )



library(randomForest)

set.seed(123)

rf_model <- randomForest(
  trip_duration_min ~ .,
  mode="regression",
  data = rf_train,
  ntree = 50,
  mtry = floor(sqrt(ncol(rf_train) - 1)),
  nodesize = 10,
  importance = TRUE
)
rf_model$type 


sqrt(rf_model$mse[length(rf_model$mse)])


varImpPlot(rf_model, n.var = 15)