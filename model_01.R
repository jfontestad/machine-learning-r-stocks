#Alpha Vantage Dataset, LSTM model
cat("\014")
rm(list =ls())

library(reticulate)
library(keras)
library(tensorflow)
library(quantmod)
library(reshape2)
library(ggplot2)

current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
options(scipen = 999)
print(getwd())

source('alpha_vantage.R', echo=TRUE)

#Sys.setenv(RETICULATE_PYTHON = "C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe")
#use_python("C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe", required = TRUE)
#use_condaenv("C:/Users/sergio/Miniconda3/envs/py_36_64", required = TRUE)
#use_miniconda("py_36_64", required = TRUE)
reticulate::py_config()

rangeEncoder <- function(timeseries, upper_limit, lower_limit, var_name) {
  #Crea one hot encoding para tres regimenes de volatilidad: low, medium, high
  low <- ifelse(timeseries[,1] <= lower_limit, 1, 0)
  medium <- ifelse(timeseries[,1] > lower_limit & timeseries[, 1] < upper_limit, 1, 0)
  high <- ifelse(timeseries[,1] >= upper_limit, 1, 0)
  encoder <- cbind.xts(low, medium, high)
  colnames(encoder) <- c(paste('low', var_name, sep = '_'),
                         paste('medium', var_name, sep = '_'), 
                         paste('high', var_name, sep = '_'))
  return(encoder)
}

increaseDecreaseEncoder <- function(timeseries, var_name) {
  #Crea one hot encoding para el estado de una serie temporal: increase, decrease
  increase <- ifelse(timeseries[,1] - lag(timeseries[,1]) > 0, 1, 0) 
  decrease <- ifelse(timeseries[,1] - lag(timeseries[,1]) <= 0, 1, 0)
  encoder <- cbind.xts(increase, decrease)
  colnames(encoder) <- c(paste('increase', var_name, sep = '_'),
                         paste('decrease', var_name, sep = '_'))
  return(encoder)
}

aboveUnderEncoder <- function(timeseries1, timeseries2, var_name, var_name2) {
  #Crea one hot encoding para la comparacion entre dos series temporales: above, under
  above <- ifelse(timeseries1[, 1] > timeseries2[, 1], 1, 0)
  under <- ifelse(timeseries1[, 1] <= timeseries2[, 1], 1, 0)
  encoder <-cbind.xts(above, under)
  colnames(encoder) <- c(paste(var_name, 'above', var_name2, sep = '_'),
                         paste(var_name, 'under', var_name2, sep = '_'))
  return(encoder)
}

#1) Data
dataset <- read.csv("GGAL_intraday_series.csv")
colnames(dataset) <- c("Time", "Open", "High", "Low", "Close", "Volume")
dataset$Time <- as.POSIXct(as.character(dataset$Time))
head(dataset)
plot(dataset[1:2000, 'Close'])
print(paste("dataset starting:", dataset$Time[nrow(dataset)], "and finishes:", dataset$Time[1]))

dataset_xts <- av_toperiod(dataset, periodo = "minutes", k = 5)
dataset_xts$rtn <- diff(log(dataset_xts$Close))
dataset_xts$rtn[is.na(dataset_xts$rtn)] <- 0.0
dataset_xts$rtn <- dataset_xts$rtn * 100
hist(dataset_xts$rtn, breaks = 500, probability = TRUE)

#2) Indicadores
# dataset <- ggal
# dataset$sd <- TTR::volatility(dataset, calc = "yang.zhang", N = 20)
# 
# plot.xts(dataset$sd)
# lines(xts(rep(0.3, length(index(dataset))), order.by = index(dataset)), col= "blue")
# lines(xts(rep(0.1, length(index(dataset))), order.by = index(dataset)), col= "red")
# 
# dataset$ma_35 <- TTR::SMA(dataset$Close, n = 35)
# dataset$ma_5 <- TTR::SMA(dataset$Close, n = 5)
# dataset$ma_20 <- TTR::SMA(dataset$Close, n = 20)
# dataset$ewo <- dataset$ma_35 - dataset$ma_5
# 
# hist(dataset$ewo, breaks = 200, probability = TRUE)
# 

#3)Encoders
# volEnc <- rangeEncoder(dataset$sd, 0.3, 0.1, 'vol')
# oscEnc <- increaseDecreaseEncoder(dataset$ewo, 'ewo')
# maEnc <- aboveUnderEncoder(dataset$Close, dataset$ma_20, 'close', 'ma_20')
# 
# dataset <- cbind.xts(dataset, volEnc, oscEnc, maEnc)

#4) target, train and test sets
data <- matrix(dataset_xts, 
               nrow = nrow(dataset_xts), ncol = ncol(dataset_xts))

colnames(data) <- colnames(dataset_xts)
#scale sets
train_index <- round(nrow(data) * 0.7)
val_index <- round(nrow(data) * 0.85)
train_data <- data[1:train_index, ]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd) 
data <- scale(data, center= mean, scale = std)

#hyperparameters
lookback <- 35 #take data from 35 bars behind, 3 hours
step <- 6 #sample every 30 mins (5*6)
delay <- 15 #predict 15 bars ahead, 1.25 hours
batch_size <- 128 
# Large batches with few timesteps -> faster
# Small batches with many timesteps -> better generalization/accuracy

#generators
train_gen <- generator(data,
                       lookback = lookback,
                       delay = delay,
                       min_index = 1,
                       max_index = train_index,
                       shuffle = FALSE,
                       step = step,
                       batch_size = batch_size
)

val_gen <- generator(data,
                       lookback = lookback,
                       delay = delay,
                       min_index = train_index + 1,
                       max_index = val_index,
                       shuffle = FALSE,
                       step = step,
                       batch_size = batch_size
)

test_gen <- generator(data,
                       lookback = lookback,
                       delay = delay,
                       min_index = val_index + 1,
                       max_index = NULL,
                       shuffle = FALSE,
                       step = step,
                       batch_size = batch_size
)

val_steps <- (val_index - (train_index + 1) - lookback) / batch_size
test_steps <- (nrow(data) - (val_index + 1) - lookback) / batch_size


#5) Crear el modelo y entrenarlo

model <- keras_model_sequential() %>%
  layer_lstm(units = 300,
             input_shape = list(NULL, dim(data)[[-1]])) %>%
  #layer_dropout(0.5) %>%
  layer_dense(units = 100) %>%
  #layer_dropout(0.5) %>%
  layer_dense(units = 1) 

#predict_proba : sigmoid en output layer si quiero normalizar las predicciones como probabilidades y son dos clases 
#para problema multi clase usar softmax,
#predict_classes : sin activation function necesaria aunque puede estar
#predict: para casos de regresion, cambiar el objetivo al numero que quiero predecir en si. Usar para retornos?

model %>%
  compile (
    optimizer = optimizer_rmsprop(),
    loss = 'mae', #mean squared error = mse mean absolute error =mae
    metrics = 'accuracy')

summary(model)

history <- model %>% fit_generator(
                      train_gen,
                      steps_per_epoch= 500,
                      epochs = 20,
                      validation_data = val_gen,
                      validation_steps = val_steps
)

#6) predecir y medir presicion
#y_pred <- keras::predict_classes(model, x_test, batch_size = batch_size)
#y_pred <- keras::predict_proba(model, x_test, batch_size = batch_size)
# y_pred <- model %>% predict(x_test)
# dim(y_pred)
# 
# score <- model %>%
#   evaluate(
#     x_test,
#     y_test
#   )
# 
# d$y_pred <- NA
# d$y_pred[(split+1):nrow(d), ] <- y_pred
# df <- data.frame(Date = index(d), Actual = d$y, Pred = d$y_pred)
# df <- na.omit(df)
# df$error <- df$y - df$y_pred
# print(mean(df$error))
# View(df)
# 
# P <- tail(df, 10) %>% 
#   plot_ly(x = ~Date, y = ~y, name = 'actual', type = 'scatter', mode = 'markers') %>% 
#   add_trace(y = ~y_pred, name = 'prediction', mode = 'markers')
# P
# 
# yesterday_d <- dataset[nrow(dataset), ]
# yesterday_d <- yesterday_d[, features]
# yesterday_d <- scale(yesterday_d, center= col_means_train,
#                      scale = cols_stddevs_train)
# yesterday_d <- array(yesterday_d, c(dim(yesterday_d)[1], timesteps, features_no))
# 
# tommorrow_pred <- model %>% predict(yesterday_d)
# print(tommorrow_pred)
