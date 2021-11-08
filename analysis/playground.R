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


df_teacher <- read_csv("tests-examples/db2csv/course_teach/tables/teacher.csv", skip=1, col_names=c('teacher_id','name','age','hometown'), col_types=cols(col_integer(),col_character(),col_integer(),col_character()))

df_teacher %>% arrange(age) %>% head(1L)