#R machine learning test temperature example
library(tibble)
library(readr)

data_dir <- "C:/Users/sergio/jena_climate"
fname <- file.path(data_dir, "jena_climate_2009_2016.csv")
data <- read_csv(fname)

glimpse(data)
