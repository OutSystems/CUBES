library(ggplot2)
library(stringr)
library(dplyr)
library(readr)

setwd('./data-treatment')

status <- c(0, 1, 2, NA)
status_meanings <- c('R & SQL', 'Fail or Just R', 'Just R', 'Timeout')

load_result_squares <- function(file) {
  read_csv(file, col_types = cols(
    name = col_character(),
    timeout = col_logical(),
    real = col_double(),
    cpu = col_double(),
    ram = col_double(),
    status = col_factor(levels = status)
  )) %>% filter(str_detect(name, test_filter))
}

load_result_file <- function(file) {
  read_csv(file, col_types = cols(
    name = col_character(),
    timeout = col_logical(),
    real = col_double(),
    cpu = col_double(),
    ram = col_double(),
    process = col_character(),
    status = col_factor(levels = status)
  )) %>% filter(str_detect(name, test_filter))
}

scatter <- function(A, B) {
  merge(eval(parse(text = A)), eval(parse(text = B)), by = 'name', suffixes = c("_A", "_B")) %>%
    filter(timeout_A == F | timeout_B == F) %>%
    ggplot(aes(x = real_A, y = real_B)) +
    geom_point(color = 'red', alpha = 0.4, size = 2) +
    scale_x_continuous(trans = 'log10') +
    scale_y_continuous(trans = 'log10') +
    geom_abline() +
    geom_hline(yintercept = 600, linetype = "dashed") +
    geom_vline(xintercept = 600, linetype = "dashed") +
    labs(y = B, x = A) +
    ggtitle('Real Time')
}

bars <- function(...) {
  tries <- list(...)
  solved <- bind_rows(tries, .id = 'try')
  results <- factor(solved$status, levels = status, labels = status_meanings, exclude = NULL)
  ggplot(solved, aes(x = factor(try, levels = names(tries)), fill = results)) +
    geom_bar(position = "dodge") +
    scale_fill_brewer(palette = 'Dark2') +
    labs(x='configuration', y='instances')
}

boxplot <- function(...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    filter(timeout != T) %>%
    ggplot(aes(x = factor(try, levels = names(tries)), y = real)) +
    geom_boxplot(position = "dodge") +
    scale_y_continuous(trans = 'log10') +
    scale_fill_brewer(palette = 'Dark2') +
    labs(x='configuration', y='instances')
}

test_filter <- '55-tests'

squares <- load_result_squares('squares.csv')

single <- load_result_file('single.csv')
qffd_r_n <- load_result_file('qffd_r_n.csv')
qffd_r_n_no_prune <- load_result_file('qffd_r_n_no_prune.csv')

# t1 <- load_result_file('try1.csv')
# t2 <- load_result_file('try2.csv')
# t3 <- load_result_file('try3.csv')
# t4 <- load_result_file('try4.csv')
# t5 <- load_result_file('try5.csv')
# t6 <- load_result_file('try6.csv')
# t7 <- load_result_file('try7.csv')
# t8 <- load_result_file('try8.csv')
# t9 <- load_result_file('try9.csv')

c1_2 <- load_result_file('cubes1_2.csv')
c1_4 <- load_result_file('cubes1_4.csv')
c1_8 <- load_result_file('cubes1_8.csv')
c1_16 <- load_result_file('cubes1_16.csv')

scatter('qffd_r_n', 'qffd_r_n_no_prune')

ggplot(t9, aes(x = factor(process))) + geom_bar(fill = "turquoise")

ggplot(c1_2, aes(x = real)) + geom_histogram()

# bars(original = original, t1 = t1, t2 = t2, t3 = t3, t4 = t4, t5 = t5, t6 = t6, t7 = t7)
bars(s = qffd_r_n_no_prune, c1_2 = c1_2, c1_4 = c1_4, c1_8 = c1_8, c1_16 = c1_16)

boxplot(s = qffd_r_n_no_prune, c1_2 = c1_2, c1_4 = c1_4, c1_8 = c1_8, c1_16 = c1_16)
