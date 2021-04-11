library(alphavantager)

cat("\014")
rm(list =ls())
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(current_path))
options(scipen = 999)
print(getwd())

api_key <- "20V82J34FLG91SI5"
av_api_key(api_key)
api_call <- 1

#1) llamar el dataset completo de alpha vantage. Done
getFullExtendedIntradaySeries <- function(symbol, interval, outputsize, datatype) {
  #interval: 1 min, 5 , 15 , 30 , 60
  #datatype: json csv 
  # outputsize: full, compact (100 data points per call)
  for(year in c(1:2)) {
    for (month in c(1:12)) {
      new_slice <- paste("year", year, "month", month, sep = '')
      print(new_slice)
      data <- av_get(symbol, av_fun = "TIME_SERIES_INTRADAY_EXTENDED", interval = interval, 
                     outputsize = outputsize, datatype = datatype, slice = new_slice)
      
      if(!exists('stock_data')) {
        print("creating new stock data object")
        stock_data <- data
      } else {
        print(paste("appending",new_slice,"info to stock_data"))
        stock_data <- rbind.data.frame(stock_data, data)
      }
      
      if (api_call %% 5 == 0) {
        print(paste("Waiting 80 seconds to begin new calls", api_call))
        Sys.sleep(80)
        print("sleep ends")
      }
      api_call <- api_call + 1
    }
  }
  write.csv(stock_data, file = paste(symbol, "_intraday_series.csv", sep = ""), row.names = FALSE)
}


#2) darle pariodicidad que quiera, ticks, min, horas, dia, semanas, mes
av_toperiod <- function(data, periodo, k = 1) {
  #Espero data: "Time","Open","High","Low","Close","Volume"
  #Periodos validos: "seconds", "minutes", "hours", "days", "weeks", "months", "quarters", and "years"
  #k valido para intraday
  data_xts <- xts(data[,2:ncol(data)], order.by = data[, 1])
  data_xts <- to.period(data_xts, period = periodo, k = k)
  colnames(data_xts) <- gsub("data_xts.", '', colnames(data_xts))
  return(data_xts)
}

#3) preparar el genererator para el modelo de machine learning
generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 6) {
  if (is.null(max_index)) {
    max_index <- nrow(data) - delay - 1
  }
  i <- min_index + lookback
  
  function() {
    if (shuffle) {
      rows <- sample(c((min_index+lookback):max_index), size = batch_size)
    } else {
      if (i + batch_size >= max_index) {
        i <<- min_index + lookback
      }
      rows <- c(i:min(i+batch_size, max_index))
      i <<- i + length(rows)
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback/step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay, 2]
    }
    
    list(samples, targets)
  }
}

#4) conectar el modelo probado en los otros scripts