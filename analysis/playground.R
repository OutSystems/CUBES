library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(dbplyr)



string_agg <- function(v, s) {
  Reduce(function(x, y) paste(x, y, sep = s), v)
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

cross_join <- function(a, b) {
  full_join(a %>% mutate(tmp.col=1), b %>% mutate(tmp.col=1), by='tmp.col') %>% select(-tmp.col)
}

setwd('..')
setwd('SQUARES')


input1 <- read_csv("tests-examples/text2sql/geography/tables/highlow.csv")
input1
input2 <- read_csv("tests-examples/55-tests/tables/9-2.txt")
input3 <- read_csv("tests-examples/textbook/tables/23-3.txt")
expected_output <- read_csv("tests-examples/spider/apartment_rentals/tables/0014.csv", col_types = cols(apt_number = col_character(),booking_start_date...2 = col_character(),booking_start_date...3 = col_character()))

expected_output$booking_start_date...2 <- parse_datetime(expected_output$booking_start_date...2)

expected_output$booking_start_date...3 <- parse_datetime(expected_output$booking_start_date...3)
expected_output

df1 <- input1 %>% mutate(highest_elevation = max(highest_elevation))
df1
df2 <- input1 %>% filter(highest_elevation > 4399)
df2
df3 <- df1 %>% filter(color == 'red') %>% select(S_name)
df3
df4 <- intersect(df2, df3)
df4
df5 <- df1 %>% group_by(S_name, P_name) %>% summarise(n = max(cost)) %>% ungroup()
df5
df6 <- df5 %>% filter(n == max(n))
df6
df7 <- inner_join(df4, df6)
df7
df8 <- df7 %>% mutate(perc = replace_na(n.y, 0) / n.x)
df8
out <- df1 %>% select(highest_elevation) %>% distinct()
out

all_equal(out, expected_output, convert = T)