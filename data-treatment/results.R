library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(readr)
library(tikzDevice)

setwd('./data-treatment')

status_levels <- rev(c(0, 3, 2, 4, 143, NA, 5, 1))
status_meanings <- rev(c('R and SQL', 'Non optimal', 'Just R', 'Just R non optimal', '?', 'Timeout', 'No solution', 'Fail'))
status_colors <- rev(c("#57853C", "#57853C", "#d79921", "#d79921", "#3c3836", "#cc241d", "#653e9c", "#3c3836"))

timelimit <- 600

my_theme <- theme_bw()

renderer <- pdf
extension <- 'pdf'

load_result_squares <- function(file) {
  t <- read_csv(file, col_types = cols(
    name = col_character(),
    timeout = col_logical(),
    real = col_double(),
    cpu = col_double(),
    ram = col_double(),
    status = col_factor(levels = status_levels)
  )) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
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
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
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
    scale_x_continuous(trans = log10_trans(), breaks = log_breaks()) +
    scale_y_continuous(trans = log10_trans(), breaks = log_breaks()) +
    geom_abline() +
    annotation_logticks() +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    geom_vline(xintercept = timelimit, linetype = "dashed") +
    labs(y = gsub("_", "-", B), x = gsub("_", "-", A)) +
    ggtitle('Real Time') +
    my_theme
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
    expand_limits(x = min(min(t$ram_A), min(t$ram_B)) * 1000, y = min(min(t$ram_A), min(t$ram_B)) * 1000) +
    my_theme
}

plot_hardness <- function(A, limit) {
  eval(parse(text = A)) %>%
    filter(timeout == T | !is.na(hard_h)) %>%
    mutate(hard_h = ifelse(is.na(hard_h), limit * 1.5, hard_h)) %>%
    ggplot(aes(x = hard_h, fill = factor(status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_histogram(bins = 40) +
    scale_x_continuous(trans = 'log10') +
    # scale_y_continuous(trans = 'log10') +
    geom_vline(xintercept = limit, linetype = "dashed") +
    scale_fill_manual(drop = F, values = status_colors, '') +
    labs(y = 'instance count', x = 'time') +
    ggtitle('Distribution of hard heuristic activation') +
    my_theme
}

plot_scores <- function(run, benchmark, instance) {
  data <- read_csv(paste0(run, '/', benchmark, '/', instance, '.csv'))
  data %>%
    ggplot(aes(x = t, y = score, group = production)) +
    geom_line(aes(color = production), size = 1) +
    facet_wrap(~line) +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Set1", n = 9))(length(unique(data$production)))) +
    my_theme
}

plot_scores2 <- function(file) {
  data <- read_csv(file)
  data %>%
    ggplot(aes(x = t, y = probability, group = prod2)) +
    geom_line(aes(color = prod2), size = 1) +
    geom_point(aes(color = prod2), size = 2) +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Set1", n = 9))(length(unique(data$prod2)))) +
    facet_wrap(~prod1) +
    my_theme
}

plot_times <- function(run) {
  eval(parse(text = run)) %>%
    ggplot(aes(x = real, fill = factor(status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_histogram(bins = 100) +
    #scale_x_log10() +
    scale_fill_manual(drop = F, values = status_colors, '') +
    geom_vline(xintercept = 600) +
    my_theme
}

plot_cumsolved <- function(...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    arrange(real) %>%
    group_by(try) %>%
    mutate(val = cumsum(status != 1 & status != 5 & timeout == F)) %>%
    ungroup() %>%
    ggplot(aes(x = val, y = real, color = try)) +
    geom_step(size = 1) +
    geom_point(shape = 4) +
    #scale_y_continuous(trans = 'log10', breaks = log_breaks(7)) +
    scale_y_continuous(breaks = pretty_breaks()) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_color_brewer(palette = 'Dark2') +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme
}

bars <- function(...) {
  tries <- list(...)
  solved <- bind_rows(tries, .id = 'try')
  results <- factor(solved$status, levels = status_levels, labels = status_meanings, exclude = NULL)
  ggplot(solved, aes(x = factor(try, levels = names(tries)), fill = results)) +
    geom_bar(position = "stack") +
    scale_y_continuous(breaks = pretty_breaks()) +
    facet_wrap(~benchmark, scales = "free") +
    scale_fill_manual(drop = F, values = status_colors) +
    labs(x = 'configuration', y = 'instances') +
    my_theme
  #theme(axis.text.x = element_text(size = 17), axis.text.y = element_text(size = 17),
  #      legend.text = element_text(size = 17), text = element_text(size = 17),
  #      panel.background = element_rect(fill = NA), panel.ontop = TRUE,
  #      panel.grid.major.x = element_line(colour = NA), panel.grid.major.y = element_line(colour = '#555555', size = .75),
  #      panel.grid.minor.y = element_line(colour = '#555555', size = .4))
}

boxplot <- function(func, ...) {
  #renderer(paste0('render.', extension), width=25, height=20)
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    group_by(name) %>%
    mutate(real = ifelse(status == 1, timelimit, real)) %>%
    filter(func(timeout != T & status != 1)) %>%
    # filter(timeout != T) %>%
    ggplot(aes(x = factor(try, levels = names(tries)), y = real)) +
    geom_boxplot(position = "dodge2", outlier.shape = NA) +
    facet_wrap(~benchmark, scales = "free") +
    scale_y_continuous(trans = log10_trans(), breaks = log_breaks()) +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    labs(x = 'configuration', y = 'time') +
    geom_jitter(aes(fill = factor(try, levels = names(tries)), col = factor(try, levels = names(tries))), show.legend = FALSE, width = .15, height = 0) +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(length(tries))) +
    my_theme
  #theme(axis.text.x = element_text(size = 12), legend.text = element_text(size = 12))
  #dev.off()
}

boxplot_ram <- function(...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    group_by(name) %>%
    ggplot(aes(x = factor(try, levels = names(tries)), y = ram * 1000)) +
    geom_boxplot(position = "dodge2", outlier.shape = NA) +
    scale_y_continuous(trans = 'log10', labels = label_bytes()) +
    labs(x = 'configuration', y = 'time') +
    geom_jitter(aes(fill = factor(try, levels = names(tries)), col = factor(try, levels = names(tries))), show.legend = FALSE, width = .15, height = 0) +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(length(tries))) +
    my_theme
}

solved_instances <- function(table) {
  table %>%
    group_by(benchmark) %>%
    mutate(total = n()) %>%
    ungroup() %>%
    filter(timeout != T & status != 1) %>%
    group_by(benchmark, total) %>%
    summarise(n = n()) %>%
    mutate(percentage = n / total)
}

test_filter <- c('scythe/demo-example', 'scythe/sqlsynthesizer', 'scythe/test-examples', 'scythe/newposts', 'scythe/dev-set')

  {
  squares <- load_result_squares('squares.csv')
  scythe <- load_result_squares('scythe.csv')

  #single <- load_result_file('single')
  single_np <- load_result_file('single_np')

  #qffd_r <- load_result_file('qffd_r')
  #qffd_r_no_prune <- load_result_file('qffd_r_no_prune')

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

  c4_16_nmcj <- load_result_file('cubes4_16_nmcj')
  c4_16_ncj <- load_result_file('cubes4_16_ncj')

  c4_2 <- load_result_file('cubes4_2')
  c4_4 <- load_result_file('cubes4_4')
  c4_8 <- load_result_file('cubes4_8')
  c4_16 <- load_result_file('cubes4_16')

  c4_2_h <- load_result_file('cubes4_2_h')
  c4_4_h <- load_result_file('cubes4_4_h')
  c4_8_h <- load_result_file('cubes4_8_h')
  c4_16_h <- load_result_file('cubes4_16_h')

  c5_16 <- load_result_file('cubes5_16')

  c6_16 <- load_result_file('cubes6_16')
  c6_16_h <- load_result_file('cubes6_16_h')

  c7_16_h <- load_result_file('cubes7_16_h')
  c7_16_h_1h <- load_result_file('cubes7_16_h_1h')

  c8_16_h <- load_result_file('cubes8_16_h')
  c9_16_h <- load_result_file('cubes9_16_h')
  c10_16_h <- load_result_file('cubes10_16_h')
  c11_16_h <- load_result_file('cubes11_16_h')

  c4_16_h_1h <- load_result_file('cubes4_16_h_1h')
}

scatter('c4_16', 'c7_16_h')

plot_hardness('c4_16_h', limit = 3600)
plot_hardness('c6_16_h', limit = 3600)
plot_hardness('c7_16_h', limit = 3600)
plot_hardness('c7_16_h_1h', limit = 3600)

plot_times('c7_16_h_1h')
plot_cumsolved('c4_16')
plot_cumsolved('c6_16_h')
plot_cumsolved('c7_16_h')
plot_cumsolved(squares = squares, single = single_np, '2P' = c4_2_h, '4P' = c4_4_h, '8P' = c4_8_h, '16P' = c4_16_h)

solved_instances(scythe)
solved_instances(squares)
solved_instances(single_np)
solved_instances(c4_16)
solved_instances(c11_16_h)

plot_scores('cubes5_16', 'scythe/top_rated_posts', '007')

plot_scores2('../detailed_logs/textbook_25.log.csv')
plot_scores2('../detailed_logs/textbook_17.log.csv')
plot_scores2('../detailed_logs/top_007.log.csv')
plot_scores2('../detailed_logs/recent_016.log.csv')
plot_scores2('../detailed_logs/recent_013.log.csv')
plot_scores2('../output.csv')

all <- inner_join(c4_16_h, c11_16_h, by = 'name')

a_slower <- inner_join(c9_16_h, c6_16, by = 'name') %>% filter(10 < real.y - real.x)
a_faster <- inner_join(c9_16_h, c6_16, by = 'name') %>% filter(10 < real.x - real.y)

b <- inner_join(c4_16, c11_16_h, by = 'name') %>% filter((timeout.y == T | status.y == 1) & (timeout.x != T & status.x != 1))
c <- inner_join(c4_16, c11_16_h, by = 'name') %>% filter((timeout.x == T | status.x == 1) & (timeout.y != T & status.y != 1))

bars(scythe = scythe, squares = squares, single = single_np, c4_2 = c4_2, c4_2_h = c4_2_h, c4_4 = c4_4, c4_4_h = c4_4_h, c4_8 = c4_8, c4_8_h = c4_8_h, c4_16 = c4_16, c4_16_h = c4_16_h)
bars(scythe = scythe, squares = squares, single = single_np, c4_16 = c4_16, c4_16_h = c4_16_h, c5_16 = c5_16, c6_16 = c6_16, c6_16_h = c6_16_h)
bars(scythe = scythe, squares = squares, single = single_np, c4_16 = c4_16, c4_16_h = c4_16_h, c6_16 = c6_16, c6_16_h = c6_16_h, c7_16_h = c7_16_h, c8_16_h = c8_16_h, c9_16_h = c9_16_h, c11_16_h = c11_16_h)
bars(scythe = scythe, squares = squares, single = single_np, c4_16_h = c4_16_h, c6_16_h = c6_16_h, c7_16_h = c7_16_h, c7_16_h_1h = c7_16_h_1h)

boxplot(func = any, scythe = scythe, squares = squares, single = single_np, c4.2 = c4_2, c4.2.h = c4_2_h, c4.4 = c4_4, c4.4.h = c4_4_h, c4.8 = c4_8, c4.8.h = c4_8_h, c4.16 = c4_16, c4.16.h = c4_16_h)
boxplot(func = any, scythe = scythe, squares = squares, single = single_np, c4_16 = c4_16, c4_16_h = c4_16_h, c6_16 = c6_16, c6_16_h = c6_16_h)
boxplot(func = any, scythe = scythe, squares = squares, single = single_np, c4_16 = c4_16, c4_16_h = c4_16_h, c6_16 = c6_16, c6_16_h = c6_16_h, c7_16_h = c7_16_h, c8_16_h = c8_16_h, c9_16_h = c9_16_h, c11_16_h = c11_16_h)
boxplot(func = any, scythe = scythe, squares = squares, single = single_np, c4_16_h = c4_16_h, c7_16_h = c7_16_h, c7_16_h_1h = c7_16_h_1h)