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
model_name <- 'price_lstm_128_00'

#1) Data Preparation / Feature engeneering
dataset <- read.csv("GGAL_daily_series.csv")
colnames(dataset) <- c("Time", "Open", "High", "Low", "Close", "Volume")
dataset$Time <- as.POSIXct(as.character(dataset$Time))

#2) Scaling
data_mtx <- data.matrix(dataset[, c(2:ncol(dataset))])
colnames(data_mtx) <- colnames(dataset[, c(2:ncol(dataset))])
train_index <- nrow(data_mtx) * 0.9
train_mean_sd <- data_mtx[1:train_index, ]
mean <- apply(train_mean_sd, 2, mean)
std <- apply(train_mean_sd, 2, sd) 
data <- scale(data_mtx, center= mean, scale = std)
data <- data[, c('High', 'Low', 'Open', 'Close')]

#3) Hyperparameters and tensor dimensions 
lookback <- 22
target <- 1 
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
         layer_lstm(units = 128, input_shape = c(dim(x_train)[2], dim(x_train)[3]), return_sequences = TRUE) %>%
         layer_lstm(units = 64, input_shape = c(dim(x_train)[2], dim(x_train)[3]), return_sequences = FALSE) %>%
         layer_dense(units = 16, activation = 'relu', kernel_initializer = 'uniform') %>%
         layer_dense(units = 1, activation = 'linear', kernel_initializer = 'uniform')


model %>%
  compile (
    optimizer = 'RMSprop',
    loss = 'mse',
    metrics = c('accuracy','mse'))

summary(model)

history <- model %>% fit(x_train, y_train, 
                         epochs = 250,
                         batch_size = batch_size,
                         validation_data = list(x_test, y_test))

#4) Predictions

predictions_train <- model %>% predict(x_train)
predictions_test <- model %>% predict(x_test)

predictions_test_unscaled <- unscale(predictions_test, mean = mean['Close'], standard_dev = std['Close'])
y_test_unscaled <- unscale(y_test, mean = mean['Close'], standard_dev = std['Close'])
compare_test <- cbind.data.frame(predictions_test_unscaled, y_test_unscaled)
compare_test$diff <- compare_test$predictions_test - compare_test$y_test

# par(mfrow=c(1,2))
# plot(c(1:nrow(y_train)), y_train, type = 'l', col = 'blue', main = 'train')
# lines(c(1:nrow(y_train)), predictions_train, col = 'red')
# 
# plot(c(1:nrow(y_test)), y_test, type = 'l', col = 'blue', main = 'test')
# lines(c(1:nrow(y_test)), predictions_test, col = 'red')

plot(c(1:nrow(y_test_unscaled)), y_test_unscaled, type = 'l', col = 'blue', main = 'test')
lines(c(1:nrow(y_test_unscaled)), predictions_test_unscaled, col = 'red')

mean_metrics <- lapply(history$metrics, mean)
write.csv(mean_metrics, paste(model_name, 'mean_metrics.csv', sep = ''))

