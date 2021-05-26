#Alpha Vantage Dataset, LSTM model
cat("\014")
rm(list =ls())

library(reticulate)
library(keras)
library(tensorflow)
library(quantmod)
library(reshape2)
library(ggplot2)
library(PerformanceAnalytics)

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
options(scipen = 999)
print(getwd())

source('alpha_vantage.R', echo=TRUE)
source('utils.R', echo=TRUE)

#Sys.setenv(RETICULATE_PYTHON = "C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe")
#use_python("C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe", required = TRUE)
#use_condaenv("C:/Users/sergio/Miniconda3/envs/py_36_64", required = TRUE)
#use_miniconda("py_36_64", required = TRUE)
reticulate::py_config()
model_name <- 'lstm_100_3_75_'
target_level <- 0.015

#1) Data Preparation / Feature engeneering
dataset <- read.csv("GGAL_intraday_series.csv")
colnames(dataset) <- c("Time", "Open", "High", "Low", "Close", "Volume")
dataset$Time <- as.POSIXct(as.character(dataset$Time))

dataset_xts <- av_toperiod(dataset, periodo = "minutes", k = 30)
dataset_xts$rtn <- diff(log(dataset_xts$Close), lag = 3)
dataset_xts$volatility <- TTR::volatility(dataset_xts, calc = "yang.zhang", N = 20)
dataset_xts$ma_35 <- TTR::SMA(dataset_xts$Close, n = 35)
dataset_xts$ma_5 <- TTR::SMA(dataset_xts$Close, n = 5)
dataset_xts$ma_20 <- TTR::SMA(dataset_xts$Close, n = 20)
dataset_xts$ewo <- dataset_xts$ma_35 - dataset_xts$ma_5


#2) Encoders
mean_volatility <- mean(na.trim(dataset_xts$volatility))
sd_volatility <- sd(na.trim(dataset_xts$volatility))
vol_encoder <- rangeEncoder(dataset_xts$volatility, mean_volatility + sd_volatility, mean_volatility - sd_volatility, 'volatility')
ewo_encoder <- increaseDecreaseEncoder(dataset_xts$ewo, 'ewo')
ma_enconder <- aboveUnderEncoder(dataset_xts$Close, dataset_xts$ma_20, 'close', 'ma_20')
dataset_xts <- cbind.xts(dataset_xts, vol_encoder, ma_enconder, ewo_encoder)
dataset_xts <- na.trim(dataset_xts)
dataset_xts$rtn_obj <- ifelse(dataset_xts$rtn > target_level, 1, 0)

#3) Scaling
data_mtx <- matrix(dataset_xts, 
                   nrow = nrow(dataset_xts), 
                   ncol = ncol(dataset_xts))

colnames(data_mtx) <- colnames(dataset_xts)
train_index <- nrow(data_mtx) * 0.8
train_mean_sd <- data_mtx[1:train_index, ]
mean <- apply(train_mean_sd, 2, mean)
std <- apply(train_mean_sd, 2, sd) 
rtn_scaled <- scale(data_mtx[,'rtn'], center = mean['rtn'], scale = std['rtn'])
colnames(rtn_scaled) <- 'rtn_scaled'
#data <- scale(data_mtx, center= mean, scale = std)
data <- cbind2(data_mtx, rtn_scaled)
data <- data[, c('rtn_scaled', 'low_volatility', 'medium_volatility', 'high_volatility',
                 'close_above_ma_20', 'close_under_ma_20', 'increase_ewo', 'decrease_ewo', 'rtn_obj')]

#3) Hyperparameters and tensor dimensions 
lookback <- 6
target <- 3 
batch_size <- 128 

train_data <- data[1:train_index,]
test_data <- data[train_index:nrow(data),]

train_set <- returnInputList(train_data, lookback, target)
x_train <- train_set[[1]]
y_train <- train_set[[2]]
printDimsSet("Train", x_train, y_train)

test_set  <- returnInputList(test_data, lookback, target)
x_test <- test_set[[1]]
y_test <- test_set[[2]]
printDimsSet("Test", x_test, y_test)

#3) Crear el modelo y entrenarlo
model <- keras_model_sequential() %>%
  layer_lstm(units = 200, input_shape = c(dim(x_train)[2], dim(x_train)[3])) %>%
  layer_dense(units = 32) %>%
  layer_dense(units = 1, activation = 'sigmoid')


model %>%
  compile (
    optimizer = 'adam',
    loss = 'mse',
    metrics = c('accuracy','mse'))

summary(model)

history <- model %>% fit(x_train, y_train, 
                         epochs = 75,
                         batch_size = batch_size,
                         validation_data = list(x_test, y_test))

#4) Predictions
predictions_train <- model %>% predict(x_train)
compare_train <- cbind.data.frame(predictions_train, y_train)
hist(predictions_train, breaks = 10, probability = TRUE)
compare_train$predictions_train <- ifelse(compare_train$predictions_train > 0.5, 1, 0)
compare_train$pred_true <- ifelse(compare_train$predictions_train == compare_train$y_train, 1, 0)
print(paste('accuracy train', sum(compare_train$pred_true), nrow(compare_train), sum(compare_train$pred_true)/nrow(compare_train)))

predictions <- model %>% predict(x_test)
compare <- cbind.data.frame(predictions, y_test)
hist(predictions, breaks = 10, probability = TRUE)
compare$predictions <- ifelse(compare$predictions > 0.5, 1, 0)
compare$pred_true <- ifelse(compare$predictions == compare$y_test, 1, 0)
print(paste('accuracy test', sum(compare$pred_true), nrow(compare), sum(compare$pred_true)/nrow(compare)))

mean_metrics <- lapply(history$metrics, mean)
write.csv(mean_metrics, paste(model_name, 'mean_metrics.csv', sep = ''))

