library(quantmod)
library(PerformanceAnalytics)
library(xts)

r_directory <- 'C:/Users/sergi/Documents/Dev/DevR/market-apis/.'
setwd(dirname(r_directory))
source('MatbaRofexAPI/rofx_rest.R')

ROFEX_REST_USER <- 'API_VALCEREAL'
ROFEX_REST_PASSWORD <- 'Nasuti6+'
ROFEX_REST_URL <- "https://api.primary.com.ar/"

token <- getTokenRofx(ROFEX_REST_USER, ROFEX_REST_PASSWORD, ROFEX_REST_URL)
ohlc_names <- c('Open', 'High', 'Low', 'Close', 'Volume')

merv <- getSymbols('^MERV', auto.assign = FALSE, 
                   from = '2017-01-01', to = Sys.Date())

# Indice Rofex
ind_rfx <- getRofxMDHist(token, ROFEX_REST_URL, 'I.RFX20', '2019-01-01', Sys.Date())
ind_xts <- xts(ind_rfx[, -c(1, 2)], order.by = as.POSIXct(ind_rfx$datetime))
colnames(ind_xts) <- 'price'
ind_xts$volume <- 0
ind_30 <- to.minutes30(ind_xts, ohlc_names)
chartSeries(ind_30['2019-12-27'])
addSAR()

ind_5 <- to.minutes5(ind_xts, ohlc_names)
chartSeries(ind_5['2019-12-27'])
addSAR()


#Futuro Rofex
rfx <- getRofxMDHist(token, ROFEX_REST_URL, 'RFX20Dic19', '2019-01-01', Sys.Date())
rfx_xts <- xts(rfx[, -c(3, 4)], order.by = as.POSIXct(rfx$datetime))
colnames(rfx_xts) <- c('price', 'volume')


rfx_30 <- to.minutes30(rfx_xts, ohlc_names)
rfx_5 <- to.minutes5(rfx_xts, ohlc_names)
chartSeries(rfx_30)
addSAR()

chartSeries(rfx_5['2019-12-27'])
addSAR()


# Hist de retornos
ret <- Return.calculate(merv$MERV.Adjusted, method = 'log')
hist(ret, breaks = 100)

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

print(getmode(na.omit(ret)) * 100)
