library(dplyr)
library(readr)
library(lubridate)
library(dbplyr)
library(tidyr)
library(stringr)

con <- DBI::dbConnect(RSQLite::SQLite(), ":memory:")
input1 <- read_csv("tests-examples/55-tests/tables/19-1.txt", col_types = cols(S_key = col_character(),P_id = col_character()))

input2 <- read_csv("tests-examples/55-tests/tables/19-2.txt", col_types = cols(P_id = col_character(),color = col_character()))

input3 <- read_csv("tests-examples/55-tests/tables/19-3.txt", col_types = cols(S_key = col_character(),S_name = col_character()))

expected_output <- read_csv("tests-examples/55-tests/tables/20.out", col_types = cols(S_key = col_character(),P_id = col_character(),S_name = col_character(),color = col_character()))
