#Alpha Vantage Dataset, LSTM model
library(reticulate)
library(keras)
library(tensorflow)
library(quantmod)
library(reshape2)

cat("\014")
rm(list =ls())
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))
options(scipen = 999)

source('MatbaRofexAPI/common/config.R', echo=TRUE)
source('MatbaRofexAPI/common/db_read.R', echo=TRUE)
source('MatbaRofexAPI/graficador/graphs.R', echo=TRUE)
source('MatbaRofexAPI/rofex_api/rfx_rest.R', echo=TRUE)
source('StockAnalisis/continuous_fut.R', echo=TRUE)

Sys.setenv(RETICULATE_PYTHON = "C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe")
use_python("C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe", required = TRUE)
use_condaenv("C:/Users/sergio/Miniconda3/envs/py_36_64", required = TRUE)
use_miniconda("py_36_64", required = TRUE)
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
  #Crea onde hot encoding para la comparacion entre dos series temporales: above, under
  above <- ifelse(timeseries1[, 1] > timeseries2[, 1], 1, 0)
  under <- ifelse(timeseries1[, 1] <= timeseries2[, 1], 1, 0)
  encoder <-cbind.xts(above, under)
  colnames(encoder) <- c(paste(var_name, 'above', var_name2, sep = '_'),
                         paste(var_name, 'under', var_name2, sep = '_'))
  return(encoder)
}


#1) Data
ggal <- getSymbols(Symbols = 'GGAL', auto.assign = FALSE)
head(ggal)
colnames(ggal) <- gsub("GGAL.", '', colnames(ggal))

fig <- plotly_chartSeries(ggal, 'GGAL')
fig


#2) Indicadores
dataset <- ggal
dataset$sd <- TTR::volatility(dataset, calc = "yang.zhang", N = 20)

plot.xts(dataset$sd)
lines(xts(rep(0.3, length(index(dataset))), order.by = index(dataset)), col= "blue")
lines(xts(rep(0.1, length(index(dataset))), order.by = index(dataset)), col= "red")

dataset$ma_35 <- TTR::SMA(dataset$Close, n = 35)
dataset$ma_5 <- TTR::SMA(dataset$Close, n = 5)
dataset$ma_20 <- TTR::SMA(dataset$Close, n = 20)
dataset$ewo <- dataset$ma_35 - dataset$ma_5

hist(dataset$ewo, breaks = 200, probability = TRUE)

dataset$rtn <- PerformanceAnalytics::Return.calculate(dataset$Close, method = 'log')
hist(dataset$rtn, breaks = 500, probability = TRUE)

#3)Encoders
volEnc <- rangeEncoder(dataset$sd, 0.3, 0.1, 'vol')
oscEnc <- increaseDecreaseEncoder(dataset$ewo, 'ewo')
maEnc <- aboveUnderEncoder(dataset$Close, dataset$ma_20, 'close', 'ma_20')

dataset <- cbind.xts(dataset, volEnc, oscEnc, maEnc)

#4) target, train and test sets
dataset$Adjusted <- NULL
dataset$tomorrow_rtn <- lag(dataset$rtn, k= -1)
d <- na.omit(dataset)
features <- c('Open', 'High', 'Low', 'Close', 'Volume', 'rtn',
              'low_vol', 'medium_vol', 'high_vol', 'increase_ewo', 
              'decrease_ewo', 'close_above_ma_20', 'close_under_ma_20') #saco rtn para regression
target <- 'tomorrow_rtn'
x <- d[, features]
y <- d[, target]
d$y <- y

#hyperparametars
timesteps <- 1 #Unidades de 5 minutos que hacen a un dia operativo
features_no <- dim(x)[2] #Atributos de data
split <- round(nrow(d) * 0.7)
batch_size <- 128

x_train <- x[1:split, ] 
x_test  <- x[(split+1):nrow(d), ]
#Target
y_train <- y[1:split, ]
y_test  <- y[(split+1):nrow(d), ]

# Large batches with few timesteps -> faster
# Small batches with many timesteps -> better generalization/accuracy

#scale sets
x_train <- scale(x_train)
col_means_train <- attr(x_train, "scaled:center") 
cols_stddevs_train <- attr(x_train, "scaled:scale") 
x_test <- scale(x_test, center= col_means_train,
                scale = cols_stddevs_train)
#Reshape [samples, timesteps. features]
x_train <- array(x_train, c(dim(x_train)[1], timesteps, features_no))
x_test <- array(x_test, c(dim(x_test)[1], timesteps, features_no))

# print(dim(x_train))
# print(dim(y_train))
# print(dim(x_test))
# print(dim(y_test))


#5) Crear el modelo y entrenarlo

model <- keras_model_sequential() %>%
  layer_lstm(units = 300,
             input_shape = c(timesteps, features_no), return_sequences = TRUE) %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 100, kernel_initializer= 'uniform') %>%
  layer_dropout(0.5) %>%
  layer_dense(units = 1, kernel_initializer= 'uniform') 

#predict_proba : sigmoid en output layer si quiero normalizar las predicciones como probabilidades y son dos clases 
#para problema multi clase usar softmax,
#predict_classes : sin activation function necesaria aunque puede estar
#predict: para casos de regresion, cambiar el objetivo al numero que quiero predecir en si. Usar para retornos?

model %>%
  compile (
    optimizer = 'adam',
    loss = 'mae', #mean squared error = mse mean absolute error =mae
    metrics = 'accuracy')

summary(model)

model %>%
  fit(
    x = x_train,
    y = y_train,
    epochs = 500,
    validation_split = 0.2,
    batch_size = batch_size
  )

#6) predecir y medir presicion
#y_pred <- keras::predict_classes(model, x_test, batch_size = batch_size)
#y_pred <- keras::predict_proba(model, x_test, batch_size = batch_size)
y_pred <- model %>% predict(x_test)
dim(y_pred)

score <- model %>%
  evaluate(
    x_test,
    y_test
  )

d$y_pred <- NA
d$y_pred[(split+1):nrow(d), ] <- y_pred
df <- data.frame(Date = index(d), Actual = d$y, Pred = d$y_pred)
df <- na.omit(df)
df$error <- df$y - df$y_pred
print(mean(df$error))
View(df)

P <- tail(df, 10) %>% 
  plot_ly(x = ~Date, y = ~y, name = 'actual', type = 'scatter', mode = 'markers') %>% 
  add_trace(y = ~y_pred, name = 'prediction', mode = 'markers')
P

yesterday_d <- dataset[nrow(dataset), ]
yesterday_d <- yesterday_d[, features]
yesterday_d <- scale(yesterday_d, center= col_means_train,
                     scale = cols_stddevs_train)
yesterday_d <- array(yesterday_d, c(dim(yesterday_d)[1], timesteps, features_no))

tommorrow_pred <- model %>% predict(yesterday_d)
print(tommorrow_pred)
