library(alphavantager)

api_key <- "20V82J34FLG91SI5"

av_api_key(api_key)

data <- av_get("GGAL", av_fun = "TIME_SERIES_INTRADAY", interval = "5min", outputsize = "full")
