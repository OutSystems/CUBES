library(dplyr)
library(readr)
library(lubridate)

input1 <- read_csv("tests-examples/scythe/top_rated_posts/tables/i034.csv", col_types = cols(Train = col_integer(),Dest = col_character(),Time = col_time()))
input1$Time <- lubridate::hm(input1$Time)
expected_output <- read_csv("tests-examples/scythe/top_rated_posts/tables/o034.csv", col_types = cols(Trainj = col_character(),Destj = col_character(),Time = col_character()))
expected_output$Time <- dmy(expected_output$Time)

t <- input1 %>% group_by(cname) %>% summarise(maxavg = max(avg))
o <- inner_join(t, input1, by=c('cname' = 'cname', 'maxavg' = 'avg'))

all_equal(o, expected_output)