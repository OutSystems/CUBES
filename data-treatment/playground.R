library(dplyr)
library(readr)
library(lubridate)
library(dbplyr)
library(tidyr)
library(stringr)

concat <- function(s,v) {
  Reduce(function(x, y) paste(x, y, sep = s), v)
}

catalog <- read_csv("tests-examples/55-tests/tables/17-1.txt")
catalog
suppliers <- read_csv("tests-examples/55-tests/tables/17-3.txt")
suppliers
datout <- read_csv("tests-examples/55-tests/tables/22.out")
datout

df1 <- inner_join(catalog,suppliers)
df2 <- df1 %>% filter(S_name  != "SN1")
df3 <- df2 %>% group_by(P_id) %>% summarise(meancost = mean(cost))
df4 <- inner_join(inner_join(catalog, suppliers), df3)
df5 <- df4 %>% filter(cost > meancost)
out <- df5 %>% select(P_id, S_name)

all_equal(datout, out)