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

cross_join <- function(a, b) {
  full_join(a %>% mutate(tmp.col=1), b %>% mutate(tmp.col=1), by='tmp.col') %>% select(-tmp.col)
}

setwd('..')
setwd('SQUARES')

aircraft <- read_csv('tests-examples/textbook/tables/aircraft.txt')
certified <- read_csv('tests-examples/textbook/tables/certified.txt')

student <- read_csv("tests-examples/textbook2/tables/student.csv", col_types = cols(snum = col_integer(), sname = col_character(), major = col_character(), level = col_character(), age = col_integer()))
enrolled <- read_csv("tests-examples/textbook2/tables/enrolled.csv", col_types = cols(snum = col_integer(), cname = col_character()))
class <- read_csv('tests-examples/textbook2/tables/class.csv')
faculty <- read_csv('tests-examples/textbook2/tables/faculty.csv')

input1 <- read_csv("tests-examples/textbook/tables/certified.txt")
# input1$date = dmy(input1$date)
input2 <- read_csv("tests-examples/textbook/tables/employees.txt")
input3 <- read_csv("tests-examples/scythe/recent_posts/tables/050_3.csv")
expected_output <- read_csv("tests-examples/textbook/tables/33.out")

mean(input2$salary)

df1 <- input2 %>% inner_join(input1)
df1
df2 <- df1 %>% mutate(a = mean(salary))
df2
df3 <- input2 %>% mutate(b = mean(salary))
df3
df4 <- inner_join(df2, df3)
df4
df5 <- df4 %>% mutate(c = a - b)
df5
df6 <- df2 %>% group_by(Request_at) %>% summarise(n = n()) %>% ungroup()
df6
df7 <- left_join(df6, df5, by='Request_at')
df7
df8 <- df7 %>% mutate(perc = replace_na(n.y, 0) / n.x)
df8
out <- df8 %>% select(Day = Request_at, CancellationRate = perc)
out

all_equal(out, expected_output, convert = T)