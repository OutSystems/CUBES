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


con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
df_has_allergy <- read_csv("tests-examples/spider/allergy_1/tables/has_allergy.csv", skip=1, col_names=c('stuid','allergy'), col_types=cols(col_integer(),col_character()))
df_has_allergy <- copy_to(con, df_has_allergy)
expected_output <- read_csv("tests-examples/spider/allergy_1/tables/0033.csv", skip=1, col_names=c('count___'), col_types=cols(col_integer()))

A
V
C
A


4

4
4
4
4


df1 <- df_has_allergy %>% filter(allergy == 'Cat')
df2 <- df1 %>% summarise(n = n())
out <- df2 %>% select(count___ = n) %>% distinct() %>% arrange(count___)

show_query(out)