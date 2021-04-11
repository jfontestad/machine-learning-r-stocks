library(shiny)
library(rstudioapi)

cat("\014")
current_path <- rstudioapi::getActiveDocumentContext()$path
setwd(dirname(dirname(current_path)))

source('MatbaRofexAPI/common/config.R')
source('MatbaRofexAPI/common/aux_functions.R')
source('MatbaRofexAPI/common/db_read.R')
source('MatbaRofexAPI/common/db_update.R')
source('MatbaRofexAPI/matba_api/matba_functions.R')
source('MatbaRofexAPI/matba_api/process_matba.R')
source('MatbaRofexAPI/rofex_api/rfx_bo.R')
source('MatbaRofexAPI/rofex_api/rfx_rest.R')
source('MatbaRofexAPI/rofex_api/rfx_websocket.R')
source('MatbaRofexAPI/rofex_api/process_bo.R')
source('MatbaRofexAPI/shiny_functions/resumen_functions.R')
source('MatbaRofexAPI/shiny_functions/market_trades.R')
source('MatbaRofexAPI/shiny_functions/volatility_functions.R')
source('MatbaRofexAPI/graficador/funciones_graficador.R')
source('MatbaRofexAPI/graficador/graphs.R')

conn <- dbConnect(dbDriver('PostgreSQL'),
                  dbname=DB_BOLETOS,
                  host=DB_SERVER,
                  port=DB_PORT,
                  user=DB_USER,
                  password=DB_PASSWORD)

token <- loginApp(MATBA_API_USER, 
                  MATBA_API_PASSWORD, 
                  MATBA_API_DOCUMENT,
                  paste(MATBA_ROOT_URL, '/authtoken', sep = ''))


rofexToken <- getTokenRofx('API_VALCEREAL', 'Nasuti6+', ROFEX_REST_URL)


allFutsSymb <- function(product, instrument_list) {
  #Obtiene los futuros de diferentes meses de una lista de futuros
  product_list <- c('TRI', 'MAI', 'SOJ', 'DO', 'WTI', 'ORO', 'RFX')
  if(!product %in% product_list) {
    print(paste("Product not found in:", product_list))
    return(NULL)
  }
  futs_symb <- instrument_list$symbol[substring(instrument_list$symbol, 1, nchar(product)) == product]
  futs_symb <- futs_symb[!grepl('c', substrRight(futs_symb, 1)) &
                           !grepl('p', substrRight(futs_symb, 1)) &
                           !grepl('/', futs_symb)]
  futs_symb <- orderRofexNames(futs_symb)
  return(futs_symb)
}

orderFut <- function(fut) {
  #Ordena tick data por mes y año
  fut$month_year <- substrRight(fut$symbol, 5)
  fut$month <- substring(fut$month_year, 1, 3)
  fut$year <- substring(fut$month_year, 4, 5)
  fut$year <- as.numeric(paste(20, fut$year, sep = ''))
  fut$month <- applyHash(hashmap_month, toupper(fut$month))
  fut <- fut[order(fut$year, fut$month, fut$datetime), ]
  fut$year <- NULL
  fut$month <- NULL
  fut$mont_year <- NULL
  return(fut)
}

orderRofexNames <- function(vec) {
  #Ordena un vector de nombres de Rofex por mes y año
  vec <- data.frame(symbol = vec, stringsAsFactors = FALSE)
  vec$month_year <- substrRight(vec$symbol, 5)
  vec$month <- substring(vec$month_year, 1, 3)
  vec$year <- substring(vec$month_year, 4, 5)
  vec$year <- as.numeric(paste(20, vec$year, sep = ''))
  vec$month <- applyHash(hashmap_month, toupper(vec$month))
  vec <- vec[order(vec$year, vec$month), ]
  vec <- as.vector(vec[, 1])
  return(vec)
}

createContinuousFutureFromDB <- function(ordered_futs_symbs) {
  #Devuelve data frame con futuro continuo utilizando todos los 
  #futuros disponibles del producto en database de ticks
  for (i in seq_along(ordered_futs_symbs)) {
    assign(ordered_futs_symbs[i], getSymbolFromDB(ordered_futs_symbs[i], conn))
  }
  
  
  for (i in seq_along(ordered_futs_symbs)){
    if (i == 1) {
      cont_fut <- orderFut(get(ordered_futs_symbs[i]))
    } else {
      #Elijo la fecha para el cambio de contrato. Habilitar otros metodos
      change_date <- as.Date(cont_fut$datetime[nrow(cont_fut)])  
      temp <- orderFut(get(futs_symb[i]))
      temp <- temp[as.Date(temp$datetime) > change_date, ]
      cont_fut <- rbind.data.frame(cont_fut, temp)
    }
  }
  return(cont_fut)
}

# instrument_list <- getUniqueSymbolsFromDB(DB_TICKS, 'symbol', conn)
# futs_symb <- allFutsSymb('RFX', instrument_list)
# cont_fut <- createContinuousFutureFromDB(futs_symb)
# futuro_continuo <- formatTickOHLCV(cont_fut, 'minutes', 5)
# fig <- plotly_chartSeries(futuro_continuo, 'RFX')
# fig



