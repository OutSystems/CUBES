library(fitdistrplus)
library(logspline)

library(readr)

data <- read.csv("instances.csv")

data$input_cells

descdist(data$input_cells)