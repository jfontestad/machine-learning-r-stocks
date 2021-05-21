#Utils

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


#Data generator
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
      rows <- c(i:min(i+batch_size-1, max_index))
      i <<- i + length(rows)
      print(paste("start index", i, "end index", i + length(rows)))
    }
    
    samples <- array(0, dim = c(length(rows),
                                lookback/step,
                                dim(data)[[-1]]))
    targets <- array(0, dim = c(length(rows)))
    
    for (j in 1:length(rows)) {
      indices <- seq(rows[[j]] - lookback, rows[[j]],
                     length.out = dim(samples)[[2]])
      samples[j,,] <- data[indices,]
      targets[[j]] <- data[rows[[j]] + delay, ncol(data)] #asume que la columna target es la ultima columna (higher index)
    }
    
    list(samples, targets)
  }
}

returnInputList <- function(data, lookback, delay) {

    samples <- array(0, dim = c(nrow(data) - lookback, #number of observations
                                lookback,   #number of steps
                                ncol(data))) #number of features
    targets <- array(0, dim = c(nrow(data)) - lookback)
    rows <- c(1 + lookback:nrow(data))
    
    for (i in 1:length(rows)) {
      #browser()
      #print(i)
      if (rows[[i]] + delay - 1 > nrow(data)) {
        break()
      }
      indices <- seq(rows[[i]] - lookback, rows[[i]] - 1,
                     length.out = dim(samples)[[2]])
      samples[i,,] <- data[indices,]
      targets[[i]] <- data[rows[[i]] + delay - 1, ncol(data)] #asume que la columna target es la ultima columna (higher index)
    }
    
    return(list(samples, targets))
}

unscale <- function(data, mean, standard_dev) {
  unscaled_data <- data * standard_dev + mean
  return(unscaled_data)
}
