library(dplyr)
library(readr)
library(lubridate)
library(dbplyr)
library(tidyr)
library(stringr)

string_agg <- function(v, s) {
  Reduce(function(x, y) paste(x, y, sep = s), v)
}

mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

setwd('..')
setwd('SQUARES')

aircraft <- read_csv('tests-examples/textbook/tables/aircraft.txt')
certified <- read_csv('tests-examples/textbook/tables/certified.txt')

student <- read_csv("tests-examples/textbook2/tables/student.csv", col_types = cols(snum = col_integer(), sname = col_character(), major = col_character(), level = col_character(), age = col_integer()))
enrolled <- read_csv("tests-examples/textbook2/tables/enrolled.csv", col_types = cols(snum = col_integer(), cname = col_character()))
class <- read_csv('tests-examples/textbook2/tables/class.csv')
faculty <- read_csv('tests-examples/textbook2/tables/faculty.csv')

input1 <- read_csv("tests-examples/textbook/tables/employees.txt")
input2 <- read_csv("tests-examples/textbook/tables/flights.txt")
input3 <- read_csv("tests-examples/textbook/tables/23-3.txt")
expected_output <- read_csv("tests-examples/textbook/tables/26.out")

df1 <- input2 %>%

df1 <- left_join(input1, input2)
df2 <- df1 %>% group_by(OrderID) %>% summarise_all(first) %>% ungroup()
out <- df2 %>% select(OrderNumber, Quantity, Description)

all_equal(out, expected_output, convert = T)