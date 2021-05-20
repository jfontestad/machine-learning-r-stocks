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
source('utils.R', echo=TRUE)

#Sys.setenv(RETICULATE_PYTHON = "C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe")
#use_python("C:/Users/sergio/Miniconda3/envs/py_36_64/python.exe", required = TRUE)
#use_condaenv("C:/Users/sergio/Miniconda3/envs/py_36_64", required = TRUE)
#use_miniconda("py_36_64", required = TRUE)
#reticulate::py_config()

#1) Data Preparation
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


#4) target, train and test sets
data <- matrix(dataset_xts, 
               nrow = nrow(dataset_xts), 
               ncol = ncol(dataset_xts))

colnames(data) <- colnames(dataset_xts)
#scale sets
train_index <- 30000
#val_index <- 30000
#test_index <- 40000
train_mean_sd <- data[1:train_index, ]
mean <- apply(train_mean_sd, 2, mean)
std <- apply(train_mean_sd, 2, sd) 
data <- scale(data, center= mean, scale = std)


#hyperparameters
lookback <- 12 #observaciones de un dia entero de trading en barras de 5 min 390 mins/5mins = 78 barras
#step <- 6 #muestras cada hora, 60 mins/ 5 = 12 barras
target <- 6 #objetivo es el retorno dos horas en el futuro
batch_size <- 128 
# Large batches with few timesteps -> faster
# Small batches with many timesteps -> better generalization/accuracy
# input size = (samples, timesteps, series) = (batch_size, lookback/step, features)
train_data <- data[1:train_index,]
test_data <- data[train_index:nrow(data),]

train_set <- returnInputList(train_data, lookback, target)
x_train <- train_set[[1]]
y_train <- train_set[[2]]
print("Train dims")
print(dim(x_train)) 
print(dim(y_train))

test_set  <- returnInputList(test_data, lookback, target)
x_test <- test_set[[1]]
y_test <- test_set[[2]]
print("Test dims")
print(dim(x_train)) 
print(dim(y_train))
#generators
# train_gen <- generator(data,
#                        lookback = lookback,
#                        delay = delay,
#                        min_index = 1,
#                        max_index = train_index,
#                        shuffle = FALSE,
#                        step = step,
#                        batch_size = batch_size
# )

# test <- train_gen()
# el1 <- test[[1]]
# print(dim(el1))
# el2 <- test[[2]]

# val_gen <- generator(data,
#                        lookback = lookback,
#                        delay = delay,
#                        min_index = train_index + 1,
#                        max_index = val_index,
#                        shuffle = FALSE,
#                        step = step,
#                        batch_size = batch_size
# )
# 
# test_gen <- generator(data,
#                        lookback = lookback,
#                        delay = delay,
#                        min_index = val_index + 1,
#                        max_index = test_index,
#                        shuffle = FALSE,
#                        step = step,
#                        batch_size = batch_size
# )
# test <- test_gen()
# el1 <- test[[1]]
# print(dim(el1))
# el2 <- test[[2]]

# val_steps <- (val_index - (train_index + 1) - lookback) / batch_size
# test_steps <- (test_index - (val_index + 1) - lookback) / batch_size


#5) Crear el modelo y entrenarlo

# model <- keras_model_sequential() %>%
#   layer_lstm(units = 300,
#              input_shape = list(NULL, dim(data)[[-1]])) %>%
#   #layer_dropout(0.5) %>%
#   layer_dense(units = 100) %>%
#   #layer_dropout(0.5) %>%
#   layer_dense(units = 1) 

model <- keras_model_sequential() %>%
  layer_flatten(input_shape = c(dim(x_train)[2], dim(x_train)[3])) %>%
  layer_dense(units = 32, activation = "relu") %>%
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

history <- model %>% fit(x_train, y_train, 
                         steps_per_epoch= 500,
                         epochs = 20
                         #validation_data = (x_test, y_test),
                         #validation_steps = 1
)

predictions <- model %>% predict_generator(val_gen(), steps = 128)
#prediction <- model %>% predict(el1)
#TO DO:
#Ver como se utiliza evaluate_generator
#Volver a probar modelo actual con la columna target correcta
#Verificar si samples tiene que ser igual a batch size o esta bien que sea batch size + 1
#Predecir signos de retornos en vez del retorno en si (bi classifier)


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
