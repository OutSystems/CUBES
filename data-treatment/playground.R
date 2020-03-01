library(dplyr)
library(readr)
library(lubridate)
library(dbplyr)
library(tidyr)
library(stringr)

concat <- function(s,v) {
  Reduce(function(x, y) paste(x, y, sep = s), v)
}

input1 <- read_csv("tests-examples/scythe/top_rated_posts/tables/i049.csv", col_types = cols(UserId = col_integer(),Alias = col_character()))
expected_output <- read_csv("tests-examples/scythe/top_rated_posts/tables/o049.csv", col_types = cols(UserId = col_integer(),Alias = col_character()))

df1 <- input1 %>% group_by(UserId) %>% summarise(concatAlias = concat(', ', Alias))