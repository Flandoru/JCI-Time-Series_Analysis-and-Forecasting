library(pracma)
library(nortest)
library(forecast)
library(lmtest)
library(car)
library(tseries)
library(Metrics)
library(nnet)
library(neuralnet)
library(dplyr)

set.seed(21)
setwd("~/CS STAT Sem 6/Time Series Analysis/Paper")

# 1 Jan 2024 - 13 Mei 2026 (tidak termasuk hari libur)
df <- read.csv('Data Historis Jakarta Stock Exchange Composite.csv')
View(df)
str(df)

# Preprocessing data
terakhir_num <- gsub("\\.", "", df$Terakhir)
terakhir_num <- gsub(",", ".", terakhir_num)

terakhir_num <- as.numeric(terakhir_num)

ts_data <- na.omit(ts(terakhir_num))

length(terakhir_num)
plot(ts_data)

# Splitting data (80:20)
train_data <- ts_data[1:446]
test_data <- ts_data[447:558]
n_train <- length(train_data); n_train
n_test <- length(test_data); n_test

train_data <- ts(train_data)
test_data <- ts(test_data)

# Plot data
plot(train_data)

# Stationary Testing
summary(powerTransform(train_data))
adf.test(train_data)

transformed <- log(train_data)
plot(transformed)

summary(powerTransform(transformed))

adf.test(transformed)
adf.test(diff(transformed))

m0 <- arima(transformed, c(2,3,1))
coeftest(m0)

################################################################
### Naive ###
################################################################
naive_model <- naive(
  train_data,
  h = n_test
)

train_fitted <- fitted(naive_model)
train_rmse <- rmse(
  as.numeric(train_data[2:446]),
  as.numeric(train_fitted[2:446])
)

test_pred <- naive_model$mean
test_rmse <- rmse(
  as.numeric(test_data),
  as.numeric(test_pred)
)

test_mae <- mae(
  as.numeric(test_data),
  as.numeric(test_pred)
)

test_mape <- mape(
  as.numeric(test_data),
  as.numeric(test_pred)
)

results_naive <- data.frame(
  Model = "Naive",
  Train_RMSE = train_rmse,
  Test_RMSE = test_rmse,
  Test_MAE = test_mae,
  Test_MAPE = test_mape * 100
)

results_naive


### Naive with Drift ###
h_values <- c(1, 7, 14, 30, 60, 112)

results_drift <- data.frame()

for(h in h_values){
  
  n <- length(ts_data)
  
  train_data <- ts_data[1:(n - h)]
  
  test_data <- ts_data[(n - h + 1):n]
  
  drift_model <- rwf(
    train_data,
    h = h,
    drift = TRUE
  )
  
  fitted_values <- fitted(drift_model)
  
  train_rmse <- rmse(
    as.numeric(train_data[-1]),
    as.numeric(fitted_values[-1])
  )
  
  test_pred <- drift_model$mean
  
  test_rmse <- rmse(
    as.numeric(test_data),
    as.numeric(test_pred)
  )
  
  test_mae <- mae(
    as.numeric(test_data),
    as.numeric(test_pred)
  )
  
  test_mape <- mape(
    as.numeric(test_data),
    as.numeric(test_pred)
  ) * 100
  
  results_drift <- rbind(
    results_drift,
    data.frame(
      Horizon = h,
      Train_Length = length(train_data),
      Test_Length = length(test_data),
      Train_RMSE = train_rmse,
      Test_RMSE = test_rmse,
      Test_MAE = test_mae,
      Test_MAPE = test_mape
    )
  )
}

results_drift

################################################################
### SES ###
################################################################
alpha_values <- c(0.9, 0.7, 0.5, 0.3, 0.4)

### STORAGE ###
results_ses <- data.frame(
  Alpha = numeric(),
  Train_RMSE = numeric(),
  Test_RMSE = numeric(),
  Test_MAE = numeric(),
  Test_MAPE = numeric()
)

### LOOP ###
for(a in alpha_values){
  
  # SES Model
  ses_model <- ets(
    train_data,
    model = "ANN",
    alpha = a
  )
  
  ### FITTED VALUES ###
  fitted_ses <- fitted(ses_model)
  
  ### TRAIN RMSE ###
  train_rmse <- rmse(
    as.numeric(train_data),
    as.numeric(fitted_ses)
  )
  
  ### FORECAST ###
  pred_ses <- forecast(
    ses_model,
    h = n_test
  )
  
  ### TEST RMSE ###
  test_rmse <- rmse(
    as.numeric(test_data),
    as.numeric(pred_ses$mean)
  )
  
  ### TEST MAE ###
  test_mae <- mae(
    as.numeric(test_data),
    as.numeric(pred_ses$mean)
  )
  
  ### TEST MAPE ###
  test_mape <- mape(
    as.numeric(test_data),
    as.numeric(pred_ses$mean)
  )
  
  ### SAVE RESULTS ###
  results_ses <- rbind(
    results_ses,
    data.frame(
      Alpha = a,
      Train_RMSE = train_rmse,
      Test_RMSE = test_rmse,
      Test_MAE = test_mae,
      Test_MAPE = test_mape * 100
    )
  )
  
}

### RESULTS ###
results_ses

best_ses <- ets(
  train_data,
  model = "ANN",
  alpha = 0.3
)

best_pred_ses <- forecast(
  best_ses,
  h = n_test
)
plot(best_pred_ses)

################################################################
### DES ###
################################################################
params <- data.frame(
  alpha = c(0.3, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
  beta  = c(0.01, 0.05, 0.1, 0.2, 0.3, 0.4, 0.5, 0.3, 0.1, 0.7)
)

### STORAGE ###
results_des <- data.frame()

### LOOP ###
for(i in 1:nrow(params)){
  
  a <- params$alpha[i]
  b <- params$beta[i]
  
  # DES Model
  des_model <- ets(
    train_data,
    model = "AAN",
    alpha = a,
    beta = b
  )
  
  # Train RMSE
  train_rmse <- rmse(
    as.numeric(train_data),
    as.numeric(fitted(des_model))
  )
  
  # Forecast
  pred_des <- forecast(
    des_model,
    h = n_test
  )
  
  # Test Metrics
  test_rmse <- rmse(
    as.numeric(test_data),
    as.numeric(pred_des$mean)
  )
  
  test_mae <- mae(
    as.numeric(test_data),
    as.numeric(pred_des$mean)
  )
  
  test_mape <- mape(
    as.numeric(test_data),
    as.numeric(pred_des$mean)
  ) * 100
  
  # Save
  results_des <- rbind(
    results_des,
    data.frame(
      Alpha = a,
      Beta = b,
      Train_RMSE = train_rmse,
      Test_RMSE = test_rmse,
      Test_MAE = test_mae,
      Test_MAPE = test_mape
    )
  )
}


### RESULTS ###
results_des

################################################################
### ARIMA ###
################################################################

# Models with significant parameters
# (2,1,2), (3,1,2), (0,2,1), (1,2,0), (2,2,0), (3,2,0), (1,3,1), (0,3,1), (1,3,0), (2,3,1)
acf(diff(transformed))
pacf(diff(transformed))

arima_orders <- list(
  c(2,1,2),
  c(3,1,2),
  c(0,2,1),
  c(1,2,0),
  c(2,2,0),
  c(3,2,0),
  c(1,3,1),
  c(0,3,1),
  c(1,3,0),
  c(2,3,1)
)

### STORAGE ###
results_arima <- data.frame(
  Model = character(),
  Train_RMSE = numeric(),
  Test_RMSE = numeric(),
  Test_MAE = numeric(),
  Test_MAPE = numeric(),
  Ljung_Box_P = numeric(),
  Lilliefors_P = numeric()
)

### LOOP ###
for(order_vals in arima_orders){
  
  p <- order_vals[1]
  d <- order_vals[2]
  q <- order_vals[3]
  
  model_name <- paste0(
    "ARIMA(",
    p,",",d,",",q,")"
  )
  
  # Fit ARIMA
  model_arima <- arima(
    transformed,
    order = c(p,d,q)
  )
  
  ### RESIDUAL TESTS ###
  lb_test <- Box.test(
    resid(model_arima),
    type = "Ljung-Box"
  )
  
  lillie_res <- lillie.test(
    resid(model_arima)
  )
  
  ### TRAIN FITTED VALUES ###
  fitted_train <- fitted(model_arima)
  
  # Back-transform
  fitted_original <- exp(fitted_train)
  
  # Remove NA alignment issues
  actual_train <- tail(
    train_data,
    length(fitted_original)
  )
  
  # Train RMSE
  train_rmse <- rmse(
    as.numeric(actual_train),
    as.numeric(fitted_original)
  )
  
  ### FORECAST ###
  pred_arima <- predict(
    model_arima,
    n.ahead = n_test
  )
  
  # Back-transform forecasts
  pred_original <- exp(pred_arima$pred)
  
  # Test RMSE
  test_rmse <- rmse(
    as.numeric(test_data),
    as.numeric(pred_original)
  )
  
  # Test MAE
  test_mae <- mae(
    as.numeric(test_data),
    as.numeric(pred_original)
  )
  
  ### TEST MAPE ###
  test_mape <- mape(
    as.numeric(test_data),
    as.numeric(pred_original)
  )
  
  ### SAVE RESULTS ###
  results_arima <- rbind(
    results_arima,
    data.frame(
      Model = model_name,
      Train_RMSE = train_rmse,
      Test_RMSE = test_rmse,
      Test_MAE = test_mae,
      Test_MAPE = test_mape * 100,
      Ljung_Box_P = lb_test$p.value,
      Lilliefors_P = lillie_res$p.value
    )
  )
  
}

### RESULTS ###
results_arima

# Finding Significant orders
m1 <- arima(transformed, c(2,1,2))
coeftest(m1)

lillie.test(resid(m1))
Box.test(resid(m1))

################################################################
### Neural Network ###
################################################################

## Set Lags and Hidden Layer
# Lags
lag_values <- c(1, 3)

# Hidden Layers
hidden_layers <- c(10, 20)

# Train Size
train_size <- 446

# Multiple random seeds
seed_values <- c(1, 7, 21, 42, 99, 123)

df <- data.frame(
  y = as.numeric(ts_data)
)

for(i in lag_values){
  
  df[[paste0("lag", i)]] <- dplyr::lag(df$y, i)
  
}

df <- na.omit(df)

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

denormalize <- function(x, min_y, max_y){
  x * (max_y - min_y) + min_y
}

min_y <- min(df$y)
max_y <- max(df$y)

norm_df <- as.data.frame(
  lapply(df, normalize)
)

n <- nrow(norm_df)
training <- norm_df[1:train_size, ]
testing <- norm_df[(train_size + 1):n, ]

lag_names <- paste0("lag", lag_values)

formula_nn <- as.formula(
  paste(
    "y ~",
    paste(lag_names, collapse = " + ")
  )
)

results_ann <- data.frame()

for(seed_value in seed_values){
  
  set.seed(seed_value)
  
  nn_model <- neuralnet(
    formula_nn,
    data = training,
    hidden = hidden_layers,
    linear.output = TRUE,
    stepmax = 1e6
  )
  
  pred_test_norm <- predict(
    nn_model,
    testing[, lag_names, drop = FALSE]
  )
  
  pred_train_norm <- predict(
    nn_model,
    training[, lag_names, drop = FALSE]
  )
  
  pred_test <- denormalize(
    pred_test_norm,
    min_y,
    max_y
  )
  
  actual_test <- denormalize(
    testing$y,
    min_y,
    max_y
  )
  
  pred_train <- denormalize(
    pred_train_norm,
    min_y,
    max_y
  )
  
  actual_train <- denormalize(
    training$y,
    min_y,
    max_y
  )
  
  temp_result <- data.frame(
    Seed = seed_value,
    Train_RMSE = rmse(actual_train, pred_train),
    Test_RMSE = rmse(actual_test, pred_test),
    Test_MAE = mae(actual_test, pred_test),
    Test_MAPE = mape(actual_test, pred_test) * 100
  )
  
  results_ann <- rbind(
    results_ann,
    temp_result
  )
  
}

results_ann

mean_results <- data.frame(
  Lags = paste(lag_values, collapse = ","),
  Hidden_Layers = paste(hidden_layers, collapse = "-"),
  Mean_Train_RMSE = mean(results_ann$Train_RMSE),
  Mean_Test_RMSE = mean(results_ann$Test_RMSE),
  Mean_Test_MAE = mean(results_ann$Test_MAE),
  Mean_Test_MAPE = mean(results_ann$Test_MAPE)
)

mean_results

################################################################
### FORECASTING BEST MODEL
################################################################
# Lags
lag_values <- c(1, 3)

# Hidden Layers
hidden_layers <- c(10, 20)

# Train Size
train_size <- 446

# Multiple random seeds
seed_values <- c(1, 7, 21, 42, 99, 123)

df <- data.frame(
  y = as.numeric(ts_data)
)

for(i in lag_values){
  
  df[[paste0("lag", i)]] <- dplyr::lag(df$y, i)
  
}

df <- na.omit(df)

normalize <- function(x){
  (x - min(x)) / (max(x) - min(x))
}

denormalize <- function(x, min_y, max_y){
  x * (max_y - min_y) + min_y
}

min_y <- min(df$y)
max_y <- max(df$y)

norm_df <- as.data.frame(
  lapply(df, normalize)
)

n <- nrow(norm_df)
training <- norm_df[1:train_size, ]
testing <- norm_df[(train_size + 1):n, ]

lag_names <- paste0("lag", lag_values)

formula_nn <- as.formula(
  paste(
    "y ~",
    paste(lag_names, collapse = " + ")
  )
)

best_ann <- neuralnet(
  formula_nn,
  data = training,
  hidden = hidden_layers,
  linear.output = TRUE,
  stepmax = 1e6
)

pred_norm <- predict(
  best_ann,
  testing[, lag_names, drop = FALSE]
)

pred_ann <- denormalize(
  pred_norm,
  min_y,
  max_y
)

actual_ann <- denormalize(
  testing$y,
  min_y,
  max_y
)

plot(
  actual_ann,
  type = "l",
  lwd = 2,
  xlab = "Time",
  ylab = "Close",
  main = "ANN Forecast vs Actual"
)

lines(
  pred_ann,
  col = "red",
  lwd = 2
)

legend(
  "topleft",
  legend = c("Actual", "ANN"),
  col = c("black", "red"),
  lwd = 2
)
