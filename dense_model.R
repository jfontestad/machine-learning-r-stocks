#Keras Stock Pricing Model
library(reticulate)
library(keras)
library(tensorflow)
library(quantmod)
library(reshape2)

cat("\014")
rm(list =ls())
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))

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

conn <- dbConnect(dbDriver('PostgreSQL'),
                  dbname=DB_BOLETOS,
                  host=DB_SERVER,
                  port=DB_PORT,
                  user=DB_USER,
                  password=DB_PASSWORD)


instrument <- 'RFX'
instrument_list <- getUniqueSymbolsFromDB(DB_TICKS, 'symbol', conn)
futs_symb <- allFutsSymb(instrument, instrument_list)
cont_fut <- createContinuousFutureFromDB(futs_symb)

dataset <- formatTickOHLCV(cont_fut, 'minutes', 5)
print(head(dataset))
print(tail(dataset))
fig <- plotly_chartSeries(dataset, instrument)
fig

#rofexToken <- getTokenRofx('API_VALCEREAL', 'Nasuti6+', ROFEX_REST_URL)
#ticks <- getRofxMDHist(rofexToken, ROFEX_REST_URL, instrument, Sys.Date()-100, Sys.Date())
#dataset <- formatTickOHLCV(ticks, 'daily')

dataset$high_open <- dataset$High - dataset$Open
dataset$low_open <- dataset$Low - dataset$Open
dataset$ma_3 <- TTR::SMA(dataset$Close, n = 3)
dataset$ma_10 <- TTR::SMA(dataset$Close, n = 10)
dataset$ma_30 <- TTR::SMA(dataset$Close, n = 30)
dataset$RSI <- RSI(dataset$Close, n = 9)
dataset$Std_dev <- rollapply(dataset$Close, sd, width = 5)
dataset$williamsR <- TTR::WPR(dataset[ , c('High', 'Low', 'Close')], n = 7)
dataset$precio_sube <- ifelse(lag(dataset$Close, k = -1) > dataset$Close, 1, 0)
#head(dataset[, 1:ncol(dataset)-1]) <- lag(dataset[, 1:ncol(dataset)-1])

dataset <- na.omit(dataset)
x <- dataset[6:nrow(dataset), c(5:(ncol(dataset)-1))]
y <- dataset[6:nrow(dataset), ncol(dataset)]

#hyperparametars
timesteps <- 1 #Unidades de 5 minutos que hacen a un dia operativo
features_no <- dim(x)[2] #Atributos de data
split <- 5900
batch_size <- 128

x_train <- x[141:5900, ] #sin NA
x_test  <- x[5901:8400, ]
#Target
y_train <- y[c(141:5900), ]
y_test  <- y[5901:8400, ]

# Large batches with few timesteps -> faster
# Small batches with many timesteps -> better generalization/accuracy

#scale sets
x_train <- scale(x_train)
x_test <- scale(x_test, center= attr(x_train, "scaled:center"),
                scale = attr(x_train, "scaled:scale"))
#Reshape [samples, timesteps. features]
# x_train <- array(x_train, c(dim(x_train)[1], timesteps, features_no))
# x_test <- array(x_test, c(dim(x_test)[1], timesteps, features_no))

print(dim(x_train))
print(dim(y_train))
print(dim(x_test))
print(dim(y_test))

#Crear la funcion generadora de batchs que prepare la info para el training del modelo
#cada input pasado al modelo debe tener shape = (batch_size, features_no)


model <- keras_model_sequential() %>%
  layer_dense(units = 128, activation = "relu", input_shape = features_no, kernel_initializer  = 'uniform') %>%
  layer_dense(units = 1, activation = 'sigmoid', kernel_initializer  = 'uniform')

model %>%
      compile (
        optimizer = 'adam',
        loss = 'mae', #mean squared error = mse mean absolute error =mae
        metrics = 'accuracy'
      )

summary(model)

model %>%
  fit(
    x = x_train,
    y = y_train,
    epochs = 50,
    batch_size = batch_size
  )

y_pred <- keras::predict_classes(model, x_test, batch_size = batch_size)
#y_pred <- ifelse(y_pred > 0.5, TRUE, FALSE)

score <- model %>%
  evaluate(
    x_test,
    y_test
  )

dataset$y_pred <- NA
dataset$y_pred[(split + 1): 8400] <- y_pred
trade_dataset <- na.omit(dataset)


trade_dataset$tomorrows_returns <- 0
trade_dataset$tomorrows_returns <- lag(CalculateReturns(trade_dataset$Close, method = 'log'), k = -1)
trade_dataset$strategy_returns <- 0
trade_dataset$strategy_returns <- ifelse(trade_dataset$y_pred == TRUE, trade_dataset$tomorrows_returns, -trade_dataset$tomorrows_returns)
trade_dataset$cumulative_market_rtn <- cumsum(trade_dataset$tomorrows_returns)
trade_dataset$cumulative_strategy_rtn <- cumsum(trade_dataset$strategy_returns)


dd <- melt(data.frame(trade_dataset[, c('cumulative_market_rtn', 'cumulative_strategy_rtn')], 
                      fecha = as.Date(index(trade_dataset))), 
           id=c('fecha'))

ret_plot <- ggplot(dd) + geom_line(aes(x = fecha, y = value, colour = variable), size = 2) +
  scale_colour_manual(values= c("green", "blue")) +
  scale_x_date(date_labels="%Y %m %d",date_breaks  ="1 week") +
  xlab("Fecha") + ylab("Returns") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

ret_plot 
cat('Test accuracy', score$accuracy, '\n')

