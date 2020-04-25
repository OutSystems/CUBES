library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(readr)

setwd('./data-treatment')

status_levels <- c(0, 3, 2, 4, 5, 143, NA, 1)
status_meanings <- c('R & SQL', 'Non optimal', 'Just R', 'Just R non optimal', 'No solution', '???', 'Timeout', 'Fail')

timelimit <- 600

load_result_squares <- function(file) {
  t <- read_csv(file, col_types = cols(
    name = col_character(),
    timeout = col_logical(),
    real = col_double(),
    cpu = col_double(),
    ram = col_double(),
    status = col_factor(levels = status_levels)
  )) %>%
    mutate(benchmark = str_sub(str_extract(name, '.*/'), end = -2)) %>%
    filter(!(benchmark %in% test_filter))
  t$status[t$timeout == T] <- NA
  t
}

load_result_file <- function(file) {
  t <- read_csv(paste0(file, '.csv'), col_types = cols(
    name = col_character(),
    timeout = col_logical(),
    real = col_double(),
    cpu = col_double(),
    ram = col_double(),
    process = col_character(),
    status = col_factor(levels = status_levels)
  )) %>%
    mutate(benchmark = str_sub(str_extract(name, '.*/'), end = -2)) %>%
    mutate(log = paste0(file, '/', name, '.log')) %>%
    mutate(hard_h = ifelse(file.exists(log), sapply(log, function(x) { parse_number(first(na.omit(str_match(readLines(x), '\\[(.*)\\]\\[.*\\]\\[INFO\\] Hard problem!')[, 2]))) }), NA)) %>%
    filter(!(benchmark %in% test_filter))
  t$status[t$timeout == T] <- NA
  t
}

scatter <- function(A, B) {
  merge(eval(parse(text = A)), eval(parse(text = B)), by = 'name', suffixes = c("_A", "_B")) %>%
    filter((timeout_A == F | timeout_B == F) & (status_A != 1 | status_B != 1)) %>%
    mutate(real_A = ifelse(status_A == 1, timelimit, real_A)) %>%
    mutate(real_B = ifelse(status_B == 1, timelimit, real_B)) %>%
    ggplot(aes(x = real_A, y = real_B)) +
    geom_point(color = 'red', alpha = 0.4, size = 2) +
    scale_x_continuous(trans = 'log10') +
    scale_y_continuous(trans = 'log10') +
    geom_abline() +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    geom_vline(xintercept = timelimit, linetype = "dashed") +
    labs(y = B, x = A) +
    ggtitle('Real Time')
}

scatter_ram <- function(A, B) {
  t <- merge(eval(parse(text = A)), eval(parse(text = B)), by = 'name', suffixes = c("_A", "_B")) %>%
    mutate(timeout_A = ifelse(status_A == 1, timelimit, timeout_A)) %>%
    mutate(timeout_B = ifelse(status_B == 1, timelimit, timeout_B))
  ggplot(t, aes(x = ram_A * 1000, y = ram_B * 1000)) +
    geom_point(color = 'red', alpha = 0.4, size = 2) +
    scale_y_continuous(trans = 'log10', labels = label_bytes()) +
    scale_x_continuous(trans = 'log10', labels = label_bytes()) +
    geom_abline() +
    labs(y = B, x = A) +
    ggtitle('RAM Usage') +
    expand_limits(x = max(max(t$ram_A), max(t$ram_B)) * 1000, y = max(max(t$ram_A), max(t$ram_B)) * 1000) +
    expand_limits(x = min(min(t$ram_A), min(t$ram_B)) * 1000, y = min(min(t$ram_A), min(t$ram_B)) * 1000)
}

plot_hardness <- function(A) {
  eval(parse(text = A)) %>%
    filter(timeout == T | !is.na(hard_h)) %>%
    mutate(hard_h = ifelse(is.na(hard_h), timelimit+200, hard_h)) %>%
    ggplot(aes(x = hard_h, fill = factor(status, levels=status_levels, labels = status_meanings, exclude = NULL))) +
    geom_histogram() +
    scale_x_continuous(trans = 'log10') +
    # scale_y_continuous(trans = 'log10') +
    geom_vline(xintercept = timelimit, linetype = "dashed") +
    scale_fill_brewer(palette = 'Dark2', '') +
    labs(y = 'instance count', x = 'time') +
    ggtitle('Distribution of hard heuristic activation')
}

bars <- function(...) {
  tries <- list(...)
  solved <- bind_rows(tries, .id = 'try')
  results <- factor(solved$status, levels = status_levels, labels = status_meanings, exclude = NULL)
  ggplot(solved, aes(x = factor(try, levels = names(tries)), fill = results)) +
    geom_bar(position = "stack") +
    scale_y_continuous(breaks = pretty_breaks()) +
    facet_wrap(~benchmark, scales = "free") +
    scale_fill_brewer(palette = 'Dark2') +
    labs(x = 'configuration', y = 'instances') +
    theme(axis.text.x = element_text(size = 12), axis.text.y = element_text(size = 12), legend.text = element_text(size = 12), text = element_text(size = 12))
}

boxplot <- function(func, ...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    group_by(name) %>%
    mutate(real = ifelse(status == 1, timelimit, real)) %>%
    filter(func(timeout != T & status != 1)) %>%
    # filter(timeout != T) %>%
    ggplot(aes(x = factor(try, levels = names(tries)), y = real)) +
    geom_boxplot(position = "dodge2", outlier.shape = NA) +
    facet_wrap(~benchmark, scales = "free") +
    scale_y_continuous(trans = 'log10') +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    labs(x = 'configuration', y = 'time') +
    geom_jitter(aes(fill = factor(try, levels = names(tries)), col = factor(try, levels = names(tries))), show.legend = FALSE, width = .15) +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(length(tries))) +
    theme(axis.text.x = element_text(size = 12), legend.text = element_text(size = 12))
}

boxplot_ram <- function(...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    group_by(name) %>%
    ggplot(aes(x = factor(try, levels = names(tries)), y = ram * 1000)) +
    geom_boxplot(position = "dodge2", outlier.shape = NA) +
    scale_y_continuous(trans = 'log10', labels = label_bytes()) +
    labs(x = 'configuration', y = 'time') +
    geom_jitter(aes(fill = factor(try, levels = names(tries)), col = factor(try, levels = names(tries))), show.legend = FALSE, width = .15) +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(length(tries))) +
    theme(axis.text.x = element_text(size = 12), legend.text = element_text(size = 12))
}

c4_16_h %>% count()

test_filter <- c('scythe/demo_example', 'scythe/sqlsynthesizer', 'scythe/test_examples', 'scythe/newposts')

  {
  squares <- load_result_squares('squares.csv')
  scythe <- load_result_squares('scythe.csv')

  single <- load_result_file('single')
  single_no_prune <- load_result_file('single_no_prune')

  qffd_r <- load_result_file('qffd_r')
  qffd_r_no_prune <- load_result_file('qffd_r_no_prune')

  # old_f_qffd_r_no_prune <- load_result_file('old_f_qffd_r_no_prune')

  # t1 <- load_result_file('try1')
  t2 <- load_result_file('try2')
  # t3 <- load_result_file('try3')
  t4 <- load_result_file('try4')
  t5 <- load_result_file('try5')
  t6 <- load_result_file('try6')
  # t7 <- load_result_file('try7')
  # t8 <- load_result_file('try8')
  # t9 <- load_result_file('try9')

  c0_2 <- load_result_file('cubes0_2')
  c0_4 <- load_result_file('cubes0_4')
  c0_8 <- load_result_file('cubes0_8')
  c0_16 <- load_result_file('cubes0_16')

  c1_2 <- load_result_file('cubes1_2')
  c1_4 <- load_result_file('cubes1_4')
  c1_8 <- load_result_file('cubes1_8')
  c1_16 <- load_result_file('cubes1_16')

  c2_2 <- load_result_file('cubes2_2')
  c2_4 <- load_result_file('cubes2_4')
  c2_8 <- load_result_file('cubes2_8')
  c2_16 <- load_result_file('cubes2_16')

  c2_2_o <- load_result_file('cubes2_2_o')
  c2_4_o <- load_result_file('cubes2_4_o')
  c2_8_o <- load_result_file('cubes2_8_o')
  c2_16_o <- load_result_file('cubes2_16_o')

  c3_2 <- load_result_file('cubes3_2')
  c3_4 <- load_result_file('cubes3_4')
  c3_8 <- load_result_file('cubes3_8')
  c3_16 <- load_result_file('cubes3_16')

  c4_8 <- load_result_file('cubes4_8')
  c4_16 <- load_result_file('cubes4_16')

  c4_16_h <- load_result_file('cubes4_16_h')
  c4_16_h_1h <- load_result_file('cubes4_16_h_1h')
}

scatter('scythe', 'c1_16')
scatter('single', 'qffd_r')
scatter('qffd_r', 'qffd_r_no_prune')
scatter('squares', 'qffd_r_no_prune')

scatter('squares', 't6')
scatter_ram('squares', 'single')

scatter('qffd_r_n_no_prune', 't6')
scatter('qffd_r_n_no_prune', 'c1_2')
scatter('qffd_r_n_no_prune', 'c1_16')

scatter('c1_2', 'c2_2')
scatter('c1_4', 'c2_4')
scatter('c1_8', 'c2_8')
scatter('c1_16', 'c2_16')
scatter('squares', 'c2_16_o')
scatter('squares', 'c2_16')

scatter('c2_2', 'c2_2_o')
scatter('c2_4', 'c2_4_o')
scatter('c2_16', 'c2_16_o')
scatter('c2_8', 'c2_16')

scatter('c2_8', 'c3_8')
scatter('squares', 'c2_2')
scatter('squares', 'qffd_r_no_prune')

scatter('c3_16', 'c4_16')
scatter('c4_16', 'c4_16_h')
scatter('c4_16_h', 'c4_16_h_1h')

plot_hardness('c4_16_h')

ggplot(t6, aes(x = factor(process))) + geom_bar(fill = "turquoise")

c4_16_h %>% mutate(a = replace_na(hard_h, 600))

a <- c4_16_h %>% filter(timeout == T | status == 1)

c3_16 %>% filter(timeout == T | status_levels == 1) %>% count()   # 55
c4_16 %>% filter(timeout == T | status_levels == 1) %>% count()   # 51    -    3 or 4 more instances solved by now
c4_16_h %>% filter(timeout == T | status_levels == 1) %>% count() # 49

b <- inner_join(scythe, c4_16_h, by = 'name') %>% filter((timeout.y == T | status.y == 1) & (timeout.x != T & status.x != 1))
c <- inner_join(scythe, c4_16_h, by = 'name') %>% filter((timeout.x == T | status.x == 1) & (timeout.y != T & status.y != 1))

# ggplot(c1_2, aes(x = real)) + geom_histogram()

bars(scythe = scythe, squares = squares, single = single, single_no_prune = single_no_prune, qffd = qffd_r, qffd_no_prune = qffd_r_no_prune, 't2 (3)' = t2, 't4 (5)' = t4, 't5 (4)' = t5, 't6 (6)' = t6, c0_2 = c0_2, c0_4 = c0_4, c0_8 = c0_8, c0_16 = c0_16, c1_2 = c1_2, c1_4 = c1_4, c1_8 = c1_8, c1_16 = c1_16, c2_2 = c2_2, c2_4 = c2_4, c2_8 = c2_8, c2_16 = c2_16, c2_2_o = c2_2_o, c2_4_o = c2_4_o, c2_8_o = c2_8_o, c2_16_o = c2_16_o, c3_16 = c3_16)
bars(scythe = scythe, squares = squares, c3_2 = c3_2, c3_4 = c3_4, c3_8 = c3_8, c3_16 = c3_16, c4_16 = c4_16, c4_16_h = c4_16_h, c4_16_h_1h = c4_16_h_1h)
bars(scythe = scythe, squares = squares, c3_8 = c3_8, c3_16 = c3_16)
bars(scythe = scythe, squares = squares, qffd_no_prune = qffd_r_no_prune, 't6 (6)' = t6, c0_2 = c0_2, c0_4 = c0_4, c0_8 = c0_8, c0_16 = c0_16, c2_2 = c2_2, c2_4 = c2_4, c2_8 = c2_8, c2_16 = c2_16, c2_2_o = c2_2_o, c2_4_o = c2_4_o, c2_8_o = c2_8_o, c2_16_o = c2_16_o)

boxplot(func = any, scythe = scythe, squares = squares, single = single, single_no_prune = single_no_prune, qffd = qffd_r, qffd_no_prune = qffd_r_no_prune, 't2 (3)' = t2, 't4 (5)' = t4, 't5 (4)' = t5, 't6 (6)' = t6, c0_2 = c0_2, c0_4 = c0_4, c0_8 = c0_8, c0_16 = c0_16, c1_2 = c1_2, c1_4 = c1_4, c1_8 = c1_8, c1_16 = c1_16, c2_2 = c2_2, c2_4 = c2_4, c2_8 = c2_8, c2_16 = c2_16, c2_2_o = c2_2_o, c2_4_o = c2_4_o, c2_8_o = c2_8_o, c2_16_o = c2_16_o)
boxplot(func = any, scythe = scythe, squares = squares, c3_2 = c3_2, c3_4 = c3_4, c3_8 = c3_8, c3_16 = c3_16, c4_16 = c4_16, c4_16_h = c4_16_h, c4_16_h_1h = c4_16_h_1h)
boxplot(func = any, scythe = scythe, squares = squares, c3_8 = c3_8, c3_16 = c3_16)
boxplot(func = any, scythe = scythe, squares = squares, qffd_no_prune = qffd_r_no_prune, 't6 (6)' = t6, c0_2 = c0_2, c0_4 = c0_4, c0_8 = c0_8, c0_16 = c0_16, c2_2 = c2_2, c2_4 = c2_4, c2_8 = c2_8, c2_16 = c2_16, c2_2_o = c2_2_o, c2_4_o = c2_4_o, c2_8_o = c2_8_o, c2_16_o = c2_16_o)

boxplot_ram(scythe = scythe, squares = squares, single = single, single_no_prune = single_no_prune, qffd = qffd_r, qffd_no_prune = qffd_r_no_prune, 't2 (3)' = t2, 't4 (5)' = t4, 't5 (4)' = t5, 't6 (6)' = t6, c1_2 = c1_2, c1_4 = c1_4, c1_8 = c1_8, c1_16 = c1_16, c2_2 = c2_2, c2_4 = c2_4, c2_8 = c2_8, c2_16 = c2_16, c2_16_o = c2_16_o)
boxplot_ram(scythe = scythe, squares = squares, qffd_no_prune = qffd_r_no_prune, 't6 (6)' = t6, c0_2 = c0_2, c0_4 = c0_4, c0_8 = c0_8, c0_16 = c0_16, c2_2 = c2_2, c2_4 = c2_4, c2_8 = c2_8, c2_16 = c2_16, c2_2_o = c2_2_o, c2_4_o = c2_4_o, c2_8_o = c2_8_o, c2_16_o = c2_16_o)
