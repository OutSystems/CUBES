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

aircraft <- read_csv('tests-examples/textbook/tables/aircraft.txt')
certified <- read_csv('tests-examples/textbook/tables/certified.txt')

highlow <- read_csv('tests-examples/text2sql/geography/tables/highlow.csv')

student <- read_csv("tests-examples/textbook2/tables/student.csv", col_types = cols(snum = col_integer(), sname = col_character(), major = col_character(), level = col_character(), age = col_integer()))
enrolled <- read_csv("tests-examples/textbook2/tables/enrolled.csv", col_types = cols(snum = col_integer(), cname = col_character()))
class <- read_csv('tests-examples/textbook2/tables/class.csv')
faculty <- read_csv('tests-examples/textbook2/tables/faculty.csv')

input1 <- read_csv("tests-examples/text2sql/geography/tables/highlow.csv")
input1
input2 <- read_csv("tests-examples/55-tests/tables/9-2.txt")
input3 <- read_csv("tests-examples/textbook/tables/23-3.txt")
expected_output <- read_csv("tests-examples/text2sql/geography/tables/0044.csv")
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