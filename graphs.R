#Graficador Precio - Open Interest (Matba)
formatTickOHLCV <- function(ticks, periodo, unidades = 1) {
  
  instrument <- ticks$symbol[1]
  instrument <- gsub("\\.", "", instrument)
  
  if (is.na(sum(ticks$size)) | !"size" %in% colnames(ticks)) {
    #Indice o instrumento sin info de volumen
    price_xts <- xts(ticks[, 'price'], order.by = as.POSIXct(ticks$datetime, format = '%Y-%m-%d %H:%M:%OS'))
    colnames(price_xts) <- 'price'
  } else {
    price_xts <- xts(ticks[, c('price', 'size')], order.by = as.POSIXct(ticks$datetime, format = '%Y-%m-%d %H:%M:%OS'))
    colnames(price_xts) <- c('price', 'volume')
  }
  
  symb_ohlcv <- to.period(price_xts, period = periodo, k = unidades)
  colnames(symb_ohlcv) <- gsub("price_xts.", '', colnames(symb_ohlcv))
  index(symb_ohlcv) <- ceiling_date(index(symb_ohlcv), unit = "minutes", 
                                    change_on_boundary = NULL, 
                                    week_start = getOption("lubridate.week.start", 7))
  return(symb_ohlcv)
}

addDirectionInfo <- function(df) {
  for (i in 1:length(df$Fecha)) {
    if (df$Close[i] >= df$Open[i]) {
      df$direction[i] = 'Increasing'
    } else {
      df$direction[i] = 'Decreasing'
    }
    
    if (df$VarOI[i] >= 0) {
      df$oiDirection[i] = 'Increasing'
    } else {
      df$oiDirection[i] = 'Decreasing'
    }
  }
  return(df)
}

addInfoOI <- function(symb_ohlcv, instrument, oi) {
  
  if (dim(oi) == c(0, 0)) {
    oi_xts <- xts(rep(1, length(symb_ohlcv$price_xts.Close)), order.by = index(symb_ohlcv))
  } else {
    oi_xts <- xts(oi$open_interest, order.by = oi$date)
  }
  index(symb_ohlcv) <- as.Date(substring(index(symb_ohlcv), 1, 10))
  symb_xts <- merge.xts(symb_ohlcv, oi_xts)
  symb_xts <- na.omit(symb_xts)
  symb_xts$VarOI <- symb_xts$oi_xts - lag(symb_xts$oi_xts)
  colnames(symb_xts) <- c("Open", "High", "Low", "Close", "Volume", "OI","VarOI")
  
  df <- data.frame(Fecha = index(symb_xts), coredata(symb_xts))
  df$VarOI[is.na(df$VarOI)] <- 0
  df$VarOI[abs(df$VarOI) / mean(df$VarOI) > 10] <- mean(df$VarOI)
  
  return(df)
}

plotMatbaOI <- function(conn, instrument) {
 
  oi <- getOIFromDB(instrument, conn)
  ticks <- getSymbolFromDB(instrument, conn)
  
  df <- formatTickOHLCV(ticks, 'days')
  df <- addInfoOI(df, instrument, oi)
  df <- addDirectionInfo(df)
  
  i <- list(line = list(color = '#17BECF'))
  d <- list(line = list(color = '#d62728'))
  pal <- c('#d62728', '#17BECF')
  
  second_axis <- list(
    tickfont = list(color = "black"),
    overlaying = "y",
    side = "left",
    title = "",
    showticklabels = FALSE,
    showgrid = FALSE)
  
  p <- df %>%
      plot_ly(x = ~Fecha, type="candlestick",
              open = ~Open, close = ~Close,
              high = ~High, low = ~Low, name = "Matba",
              increasing = i, decreasing = d, colors = pal) %>%
      add_lines(X = ~Fecha, y = ~OI, name = "OI",
                yaxis = "y2", line = list(color = "#2ca02c")) %>%
      add_bars(x=~Fecha, y=~Volume, name = "Volume",
               color = ~direction, colors = pal, yaxis = "y2") %>%
      layout(yaxis = list(title = "Price"), 
             xaxis= list(type = "category"),   
             yaxis2 = second_axis,
             automargin = TRUE,
             height = 900) 
     
  pp <- df %>%
      plot_ly(x=~Fecha, y=~VarOI, type='bar', name = "Var OI",
              color = ~oiDirection, colors = pal) %>%
      layout(yaxis = list(title = "Var OI"))
  
  p <- subplot(p, pp, heights = c(0.7,0.2), nrows=2,
               shareX = TRUE, titleY = TRUE) %>%
      layout(title = paste(df$Fecha[1], "-", df$Fecha[length(df$Fecha)]),
             xaxis = list(rangeslider = list(visible = F)),
             legend = list(orientation = 'h', x = 0.5, y = 1,
                           xanchor = 'center', yref = 'paper',
                           font = list(size = 10),
                           bgcolor = 'transparent'),
             hovermode = 'compare') %>% 
      plotly::config(displayModeBar = FALSE)
  
  plotly_build(p)
}



plotly_chartSeries <- function(OHLC, instrument_name) {
  #Nombre de columnas = Open, High, Close, Low 
  if(class(OHLC)[1] == "xts") {
    df <- data.frame(coredata(OHLC), Date = index(OHLC))
  } else {
    df <- OHLC
  }
  
  i <- list(line = list(color = '#17BECF'))
  d <- list(line = list(color = '#d62728'))
  pal <- c('#d62728', '#17BECF')
 
  p <- df %>%
       plot_ly(x = ~Date, type="candlestick",
               open = ~Open, close = ~Close,
               high = ~High, low = ~Low, name = instrument_name,
               increasing = i, decreasing = d, colors = pal) %>%
        layout(yaxis = list(title = "Price", 
                            tickformat = "f",
                            tickmode = "lines"), 
               xaxis= list(type = "category", #no muestra espacios por findes y feriados
                           rangeslider = list(visible = F),
                           ticktext = lubridate::date(df$Date),
                           tickmode = 'array',
                           tickvals = lubridate::date(df$Date),
                           xaxis_tickformat = '%d %B (%a)<br>%Y',
                           dtick = 10),
               hovermode = 'compare'
               ) 
  plotly_build(p)
}


