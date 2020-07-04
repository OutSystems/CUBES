library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(readr)
library(tikzDevice)

setwd('./data-treatment')

status_levels <- rev(c(0, 3, 2, 4, -2, -1, 5, 1))
status_meanings <- rev(c('R and SQL', 'Non optimal', 'Just R', 'Just R non optimal', 'Memout', 'Timeout', 'No solution', 'Fail'))
status_colors <- rev(c("#57853C", "#296429", "#d79921", "#B4560E", "#CC6387", "#cc241d", "#653e9c", "#3c3836"))

timelimit <- 600

my_theme <- theme_bw()

renderer <- pdf
extension <- 'pdf'

instance_info <- read_csv('instances.csv', col_types = cols(
  name = col_character(),
  loc = col_integer()
))

load_result_squares <- function(file) {
  t <- read_csv(file, col_types = cols(
    name = col_character(),
    timeout = col_logical(),
    real = col_double(),
    cpu = col_double(),
    ram = col_double(),
    status = col_character()
  )) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    filter(!(benchmark %in% test_filter))
  t$status[t$timeout == T] <- -1
  t$status[t$status == 143] <- 1
  t$status <- parse_factor(t$status, levels = status_levels)
  t %>% left_join(instance_info)
}

load_result_file <- function(file) {
  t <- read_csv(paste0(file, '.csv'), col_types = cols(.default = '?', status = col_factor(levels = status_levels))) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    mutate(log = paste0(file, '/', name, '.log')) %>%
    mutate(log_content = ifelse(file.exists(log), sapply(log, function(x) { read_file(x) }), NA)) %>%
    mutate(hard_h = parse_number(str_match(log_content, '\\[(.*)\\]\\[.*\\]\\[INFO\\] Hard problem!')[, 2])) %>%
    mutate(loc_reached = parse_integer(str_match(log_content, 'Initialising process for (.*) lines of code')[, 2])) %>%
    mutate(loc_found = parse_integer(str_match(log_content, 'Solution size: (.*)')[, 2])) %>%
    mutate(init_p = parse_number(str_match(log_content, 'Total time spent in enumerator init: (.*) \\(approx\\)')[, 2]) / cpu) %>%
    mutate(enum_p = parse_number(str_match(log_content, 'Total time spent in enumerator: (.*) \\(approx\\)')[, 2]) / cpu) %>%
    mutate(eval_p = parse_number(str_match(log_content, 'Total time spent in evaluation & testing: (.*) \\(approx\\)')[, 2]) / cpu) %>%
    mutate(block_p = parse_number(str_match(log_content, 'Total time spent blocking cubes/programs: (.*) \\(approx\\)')[, 2]) / cpu) %>%
    mutate(total_p = init_p + enum_p + eval_p + block_p) %>%
    mutate(equivalent_p = cpu / real) %>%
    filter(!(benchmark %in% test_filter)) %>%
    select(-log, -log_content)
  t$status[t$timeout == T] <- -1
  if ('memout' %in% colnames(t)) {
    t$status[t$memout == T] <- -2
  }
  t %>% left_join(instance_info)
}

plot_locs <- function(A) {
  plot <- A %>%
    group_by(loc, loc_reached) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    ggplot(aes(x = loc, y = loc_reached, fill = count)) +
    geom_tile() +
    geom_text(aes(label = count), size = 15) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks()) +
    geom_abline() +
    scale_fill_distiller(palette = 'RdYlGn') +
    labs(y = 'Lines of code reached', x = 'Solution lines of code') +
    my_theme
  list(plot, A %>%
    filter(loc_reached < loc) %>%
    select(name, loc_reached, loc))
}

plot_locs2 <- function(A) {
  plot <- A %>%
    group_by(loc, loc_found) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    ggplot(aes(x = loc, y = loc_found, fill = count)) +
    geom_tile() +
    geom_text(aes(label = count), size = 15) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks()) +
    geom_abline() +
    scale_fill_distiller(palette = 'RdYlGn') +
    labs(y = 'Lines of code found', x = 'Solution lines of code') +
    my_theme
  list(plot, A %>%
    filter(loc_found != loc) %>%
    select(name, loc_found, loc))
}

plot_locs3 <- function(A) {
  plot <- A %>%
    group_by(loc_found, loc_reached) %>%
    summarise(count = n()) %>%
    ungroup() %>%
    ggplot(aes(x = loc_found, y = loc_reached, fill = count)) +
    geom_tile() +
    geom_text(aes(label = count), size = 15) +
    scale_x_continuous(breaks = pretty_breaks()) +
    scale_y_continuous(breaks = pretty_breaks()) +
    geom_abline() +
    scale_fill_distiller(palette = 'RdYlGn') +
    labs(y = 'Lines of code reached', x = 'Lines of code found') +
    my_theme
  list(plot, A %>%
    filter(loc_reached < loc_found) %>%
    select(name, loc_reached, loc_found))
}

scatter <- function(A, B) {
  merge(eval(parse(text = A)), eval(parse(text = B)), by = 'name', suffixes = c("_A", "_B")) %>%
    filter((status_A != 1 & status_B != 1) &
             (status_A != -1 | status_B != -1) &
             (status_A != -2 & status_B != -2) &
             (status_A != 5 & status_B != 5)) %>%
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
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Set1", n = 9))(length(unique(data$prod2))), breaks = sort(unique(data$prod2))) +
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

plot_processes <- function(run) {
  eval(parse(text = run)) %>%
    ggplot(aes(x = process, fill = factor(status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_histogram(bins = 100) +
    #scale_x_log10() +
    scale_fill_manual(drop = F, values = status_colors, '') +
    geom_vline(xintercept = 600) +
    my_theme
}

plot_cumsolved <- function(use_vbs = T, full_x = T, ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  if (use_vbs) {
    data <- data %>% bind_rows(vbs(...))
  }
  data <- data %>%
    arrange(real) %>%
    group_by(try) %>%
    mutate(val = cumsum(status != 1 &
                          status != 5 &
                          status != -1 &
                          status != -2)) %>%
    ungroup()
  tmp <- ggplot(data, aes(x = val, y = real, color = try)) +
    geom_step(size = 1) +
    geom_point(shape = 4) +
    #scale_y_continuous(trans = 'log10', breaks = log_breaks(7)) +
    scale_y_continuous(breaks = pretty_breaks())
  if (full_x) {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks(), limits = c(0, n_distinct(data$name)))
  } else {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks())
  }
    tmp +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(max(length(tries) + 1, 8))[0:length(tries) + 1], breaks = c(names(tries), 'VBS'), drop=F) +
    labs(x = 'Instances solved', y = 'Time', color = 'Legend') +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme
}

vbs <- function(...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  data %>%
    group_by(name, benchmark) %>%
    summarise(real = min(ifelse(status != 1 &
                                  status != -1 &
                                  status != -2 &
                                  status != 5, real, timelimit)), status = factor(ifelse(any(status == 0),
                                                                                         0,
                                                                                         ifelse(any(status == 2),
                                                                                                2,
                                                                                                ifelse(any(status == 1),
                                                                                                       1,
                                                                                                       -1))), levels = status_levels, exclude = NULL)) %>%
    mutate(try = 'VBS')
}

bars <- function(...) {
  tries <- list(...)
  solved <- bind_rows(tries, .id = 'try') %>% bind_rows(vbs(...))
  results <- factor(solved$status, levels = status_levels, labels = status_meanings, exclude = NULL)
  ggplot(solved, aes(x = factor(try, levels = c(names(tries), 'VBS')), fill = results)) +
    geom_bar(position = "stack") +
    scale_y_continuous(breaks = pretty_breaks()) +
    facet_wrap(~benchmark, scales = "free") +
    scale_fill_manual(drop = F, values = status_colors) +
    labs(x = 'configuration', y = 'instances') +
    my_theme
}

boxplot <- function(func, ...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    bind_rows(vbs(...)) %>%
    group_by(name) %>%
    mutate(real = ifelse(status == 1 |
                           status == -2 |
                           status == -1 |
                           status == 5, timelimit, real)) %>%
    filter(func(status != 1 &
                  status != -2 &
                  status != -1 &
                  status != 5)) %>%
    ggplot(aes(x = factor(try, levels = c(names(tries), 'VBS')), y = real)) +
    geom_boxplot(position = "dodge2", outlier.shape = NA) +
    facet_wrap(~benchmark, scales = "free") +
    scale_y_continuous(trans = log10_trans(), breaks = log_breaks()) +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    labs(x = 'configuration', y = 'time') +
    geom_jitter(aes(fill = factor(try, levels = c(names(tries), 'VBS')), col = factor(try, levels = c(names(tries), 'VBS'))), show.legend = FALSE, width = .15, height = 0) +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(length(tries) + 1)) +
    my_theme
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
    filter(status != -1 & status != -2 & status != 1 & status != 5) %>%
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
  bitenum <- load_result_file('bitenum')
  #bitenum2 <- load_result_file('bitenum2')
  #bitenum3 <- load_result_file('bitenum3')

  #qffd_r <- load_result_file('qffd_r')
  #qffd_r_no_prune <- load_result_file('qffd_r_no_prune')

  # old_f_qffd_r_no_prune <- load_result_file('old_f_qffd_r_no_prune')

  # t1 <- load_result_file('try1')
  #t2 <- load_result_file('try2')
  # t3 <- load_result_file('try3')
  #t4 <- load_result_file('try4')
  #t5 <- load_result_file('try5')
  #t6 <- load_result_file('try6')
  # t7 <- load_result_file('try7')
  # t8 <- load_result_file('try8')
  # t9 <- load_result_file('try9')
  t10 <- load_result_file('try10')
  t11_c <- load_result_file('try11_c')

  #c0_2 <- load_result_file('cubes0_2')
  #c0_4 <- load_result_file('cubes0_4')
  #c0_8 <- load_result_file('cubes0_8')
  #c0_16 <- load_result_file('cubes0_16')
  #
  #c1_2 <- load_result_file('cubes1_2')
  #c1_4 <- load_result_file('cubes1_4')
  #c1_8 <- load_result_file('cubes1_8')
  #c1_16 <- load_result_file('cubes1_16')
  #
  #c2_2 <- load_result_file('cubes2_2')
  #c2_4 <- load_result_file('cubes2_4')
  #c2_8 <- load_result_file('cubes2_8')
  #c2_16 <- load_result_file('cubes2_16')
  #
  #c2_2_o <- load_result_file('cubes2_2_o')
  #c2_4_o <- load_result_file('cubes2_4_o')
  #c2_8_o <- load_result_file('cubes2_8_o')
  #c2_16_o <- load_result_file('cubes2_16_o')
  #
  #c3_2 <- load_result_file('cubes3_2')
  #c3_4 <- load_result_file('cubes3_4')
  #c3_8 <- load_result_file('cubes3_8')
  #c3_16 <- load_result_file('cubes3_16')
  #
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

  c4_16_h_1h <- load_result_file('cubes4_16_h_1h')

  #c5_16 <- load_result_file('cubes5_16')

  #c6_16 <- load_result_file('cubes6_16')
  #c6_16_h <- load_result_file('cubes6_16_h')
  #
  #c7_16_h <- load_result_file('cubes7_16_h')
  #c7_16_h_1h <- load_result_file('cubes7_16_h_1h')
  #
  #c8_16_h <- load_result_file('cubes8_16_h')
  #c9_16_h <- load_result_file('cubes9_16_h')
  #c10_16_h <- load_result_file('cubes10_16_h')
  #c11_16_h <- load_result_file('cubes11_16_h')
  #c12_16_h <- load_result_file('cubes12_16_h')
  #c14_16_h <- load_result_file('cubes14_16_h')

  c15_2 <- load_result_file('cubes15_2')
  c15_16_h <- load_result_file('cubes15_16_h')

  c16_16_h <- load_result_file('cubes16_16_h')
  c17_16_h <- load_result_file('cubes17_16_h')
  c18_16_h <- load_result_file('cubes18_16_h')
  c19_16_h <- load_result_file('cubes19_16_h')
  c20_16_h <- load_result_file('cubes20_16_h')
  c21_16_h <- load_result_file('cubes21_16_h')
  c22_16_h <- load_result_file('cubes22_16_h')
  c23_16_h <- load_result_file('cubes23_16_h')
  c24_16_h <- load_result_file('cubes24_16_h')
  c25_16_h <- load_result_file('cubes25_16_h')

  c26_2_h_0f <- load_result_file('cubes26_2_h_0f')
  c26_16_h_0f <- load_result_file('cubes26_16_h_0f')
  c26_16_h_0f_o <- load_result_file('cubes26_16_h_0f_o')
  c26_16_h_0f_oo <- load_result_file('cubes26_16_h_0f_oo')

  c27_16_h_0f <- load_result_file('cubes27_16_h_0f')
  c28_16_h_0f <- load_result_file('cubes28_16_h_0f')

  c29_16_0f <- load_result_file('cubes29_16_0f')
  c29_16_h_0f <- load_result_file('cubes29_16_h_0f')

  c30_16_0f <- load_result_file('cubes30_16_0f')
  c31_16_0f <- load_result_file('cubes31_16_0f')
  c32_16_0f <- load_result_file('cubes32_16_0f')
  c33_16_0f <- load_result_file('cubes33_16_0f')
  c34_16_0f_c <- load_result_file('cubes34_16_0f_c')
  c35_16_0f_c <- load_result_file('cubes35_16_0f_c')
  c36_16_0f_c <- load_result_file('cubes36_16_0f_c')
  c37_16_0f_c <- load_result_file('cubes37_16_0f')
  c38_16_0f_c <- load_result_file('cubes38_16_0f')
  c39_16_0f <- load_result_file('cubes39_16_0f')
  c39_16_0f_c <- load_result_file('cubes39_16_0f_c')
  c40_16_0f_c <- load_result_file('cubes40_16_0f')

  c41_4_0f_c <- load_result_file('cubes41_4_0f')
  c41_8_0f_c <- load_result_file('cubes41_8_0f')
  c41_16_0f_c <- load_result_file('cubes41_16_0f')
  c41_16_0f_c_2 <- load_result_file('cubes41_16_0f_2')
  c41_16_0f_c_3 <- load_result_file('cubes41_16_0f_3')
  c41_16_0f_c_4 <- load_result_file('cubes41_16_0f_4')
  c41_30_0f_c <- load_result_file('cubes41_30_0f')

  c42_16_0f_c <- load_result_file('cubes42_16_0f')
}

v <- vbs(bitenum, c4_16, c15_16_h, c20_16_h, c26_16_h_0f, c30_16_0f, c31_16_0f, c32_16_0f)
v1 <- vbs(c41_16_0f_c, c41_16_0f_c_2, c41_16_0f_c_3)

# last week vs condition generalization
scatter('c26_16_h_0f', 'c27_16_h_0f')

# optimization tries - little relevance
scatter('c26_16_h_0f', 'c28_16_h_0f')
scatter('c26_16_h_0f', 'c29_16_h_0f')

# restricted dsl
scatter('c26_16_h_0f', 'c30_16_0f') # best vs .
scatter('c29_16_h_0f', 'c30_16_0f') # last vs .

# bug fix and generalization transitivity addition
scatter('c26_16_h_0f', 'c32_16_0f') # best vs .
scatter('c30_16_0f', 'c32_16_0f') # last vs .

# CHECKPOINT: no generalization vs generalization
scatter('c35_16_0f_c', 'c32_16_0f')

# split cross and inner_join into seperate threads
scatter('c26_16_h_0f', 'c37_16_0f_c') # best vs .
scatter('c32_16_0f', 'c37_16_0f_c') # last vs .

# CHECKPOINT: no generalization vs generalization
scatter('c38_16_0f_c', 'c37_16_0f_c')

scatter('c37_16_0f_c', 'c39_16_0f_c')
scatter('c39_16_0f', 'c39_16_0f_c')

scatter('c37_16_0f_c', 'c40_16_0f_c')
scatter('c39_16_0f_c', 'c40_16_0f_c')

scatter('c37_16_0f_c', 'c41_16_0f_c')
scatter('c39_16_0f_c', 'c41_16_0f_c')
scatter('c40_16_0f_c', 'c41_16_0f_c')
scatter('c41_16_0f_c', 'c41_16_0f_c_2')
scatter('c37_16_0f_c', 'c42_16_0f_c')
scatter('c41_16_0f_c', 'c42_16_0f_c')

plot_locs2(c37_16_0f_c)
plot_locs(c37_16_0f_c)

plot_cumsolved(use_vbs = F, full = F, 'Original SQUARES' = squares, 'Current Sequential' = bitenum, 'Current Portoflio' = t11_c, 'Current Space Splitting' = c37_16_0f_c)
plot_cumsolved(use_vbs = F, 'Original SQUARES' = squares)
plot_cumsolved(use_vbs = F, 'Original SQUARES' = squares, 'Current Sequential' = bitenum)
plot_cumsolved(use_vbs = F, 'Original SQUARES' = squares, 'Current Sequential' = bitenum, 'Current Portoflio' = t11_c)
plot_cumsolved(use_vbs = F, 'Original SQUARES' = squares, 'Current Sequential' = bitenum, 'Current Portoflio' = t11_c, 'Current Space Splitting' = c38_16_0f_c)
plot_cumsolved(use_vbs = F, 'Original SQUARES' = squares, 'Current Sequential' = bitenum, 'Current Portoflio' = t11_c, 'Current Space Splitting' = c38_16_0f_c, '8T' = c41_8_0f_c, '16T' = c41_16_0f_c_3)
plot_cumsolved(use_vbs = F, squares = squares, bitenum = bitenum, 'Last week' = c26_16_h_0f, 'Cond. generalization I' = c27_16_h_0f, 'Cond. generalization II' = c32_16_0f, 'Split operations' = c37_16_0f_c, 'Split & no cond. gen.' = c38_16_0f_c, 'A' = c39_16_0f, 'B' = c39_16_0f_c)
plot_cumsolved(use_vbs = F,'Original SQUARES' = squares, 'Current Sequential' = bitenum, 'C_8' = c41_8_0f_c, 'C_8' = c41_8_0f_c, 'C_16_1' = c41_16_0f_c, 'C_16_2' = c41_16_0f_c_2, 'C_16_3' = c41_16_0f_c_3)

plot_scores2('../55-tests-9.log.csv')

all <- left_join(c26_16_h_0f, c32_16_0f, by = 'name') %>%  left_join(c36_16_0f_c, by = 'name') %>% select(-starts_with('benchmark'))
all <- left_join(c36_16_0f_c, c37_16_0f_c, by = 'name')

a_slower <- inner_join(c26_16_h_0f, c34_16_0f_c, by = 'name') %>% filter(10 < real.y - real.x)
a_faster <- inner_join(c26_16_h_0f, c34_16_0f_c, by = 'name') %>% filter(10 < real.x - real.y)

b <- inner_join(squares, c37_16_0f_c, by = 'name') %>% filter((status.y == -2 | status.y == -1 | status.y == 1) & (status.x != -2 & status.x != -1 & status.x != 1))
c <- inner_join(squares, c37_16_0f_c, by = 'name') %>% filter((status.x == -2 | status.x == -1 | status.x == 1) & (status.y != -2 & status.y != -1 & status.y != 1))

a <- vbs(squares = squares, c15_16_h = c15_16_h, c26_16_h_0f = c26_16_h_0f, c27_16_h_0f = c27_16_h_0f, c32_16_0f = c32_16_0f, c33_16_0f = c33_16_0f, c34_16_0f_c = c34_16_0f_c, c35_16_0f_c = c35_16_0f_c, c37_16_0f_c = c37_16_0f_c)

bars(scythe = scythe, squares = squares, bitenum = bitenum, c26_16_h_0f = c26_16_h_0f, c27_16_h_0f = c27_16_h_0f, c32_16_0f = c32_16_0f, c37_16_0f_c = c37_16_0f_c)
bars(scythe = scythe, squares = squares, bitenum = bitenum, c26 = c26_16_h_0f, c27 = c27_16_h_0f, c32 = c32_16_0f, c37 = c37_16_0f_c, c38 = c38_16_0f_c, c39_nc = c39_16_0f, c39 = c39_16_0f_c, c40 = c40_16_0f_c, c41 = c41_16_0f_c)
bars(squares = squares, bitenum = bitenum, 'C_4' = c41_4_0f_c, 'C_8' = c41_8_0f_c, 'C_16_1' = c41_16_0f_c, 'C_16_2' = c41_16_0f_c_2, 'C_16_3' = c41_16_0f_c_3, 'C_30' = c41_30_0f_c, 'N' = c42_16_0f_c, 'N1' = c41_16_0f_c_4)

boxplot(func = any, scythe = scythe, squares = squares, bitenum = bitenum, c26_16_h_0f = c26_16_h_0f, c27_16_h_0f = c27_16_h_0f, c32_16_0f = c32_16_0f, c37_16_0f_c = c37_16_0f_c)
boxplot(func = any, scythe = scythe, squares = squares, bitenum = bitenum, c26_16_h_0f = c26_16_h_0f, c27_16_h_0f = c27_16_h_0f, c32_16_0f = c32_16_0f, c37_16_0f_c = c37_16_0f_c, c38_16_0f_c = c38_16_0f_c)
boxplot(func = any, squares = squares, bitenum = bitenum, 'C_4' = c41_4_0f_c, 'C_8' = c41_8_0f_c, 'C_16_1' = c41_16_0f_c, 'C_16_2' = c41_16_0f_c_2, 'C_16_3' = c41_16_0f_c_3, 'C_30' = c41_30_0f_c, 'N' = c42_16_0f_c, 'N1' = c41_16_0f_c_4)
boxplot(func = any, 'Original SQUARES' = squares, 'Current Sequential' = bitenum, 'Current Portoflio' = t11_c, 'Current Space Splitting' = c38_16_0f_c)


squares %>% count()

squares %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% count()
squares %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = median(real))
squares %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = mean(real))
squares %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% filter(real <= 10) %>% count()

bitenum %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% count()
bitenum %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = median(real))
bitenum %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = mean(real))
bitenum %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% filter(real <= 10) %>% count()

t11_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% count()
t11_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = median(real))
t11_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = mean(real))
t11_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% filter(real <= 10) %>% count()

c37_16_0f_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% count()
c37_16_0f_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = median(real))
c37_16_0f_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% summarise(med = mean(real))
c37_16_0f_c %>% filter(status != -1 & status != -2 & status != 1 & status != 5) %>% filter(real <= 10) %>% count()