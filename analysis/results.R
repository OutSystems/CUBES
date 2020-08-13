library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(readr)
library(tikzDevice)
library(ggforce)

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

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

is_solved_status <- function(status) {
  status != -2 &
    status != -1 &
    status != 1 &
    status != 5
}

load_result_squares <- function(file) {
  read_csv(paste0(file, '.csv'), col_types = cols(.default = '?', status = col_factor(levels = status_levels))) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)),
           status = ifelse(status == 143, 1, as.character(status)),
           status = ifelse(timeout, -1, as.character(status)),
           solved = is_solved_status(status)) %>%
    #mutate(benchmark = ifelse(grepl("spider", name, fixed=TRUE), "spider", gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)))) %>%
    filter(!(benchmark %in% test_filter)) %>%
    left_join(instance_info)
}

load_result_file <- function(file) {
  read_csv(paste0(file, '.csv'), col_types = cols(.default = '?', status = col_factor(levels = status_levels))) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    #mutate(benchmark = ifelse(grepl("spider", name, fixed=TRUE), "spider", gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)))) %>%
    filter(!(benchmark %in% test_filter)) %>%
    group_by(name) %>%
    mutate(run_number = 0:(n() - 1)) %>%
    ungroup() %>%
    mutate(several_runs = any(run_number > 0),
           status = ifelse(timeout, -1, as.character(status)),  # *sighs* factors are weird
           status = ifelse(memout, -2, as.character(status)),  # *sighs* factors are weird
           solved = is_solved_status(status),
           log_suff = ifelse(several_runs, paste0('_', run_number), ''),
           log = paste0(file, '/', name, log_suff, '.log'),
           log_content = ifelse(file.exists(log), sapply(log, function(x) { read_file(x) }), NA),
           hard_h = parse_number(str_match(log_content, '\\[(.*)\\]\\[.*\\]\\[INFO\\] Hard problem!')[, 2]),
           loc_reached = parse_integer(str_match(log_content, 'Initialising process for (.*) lines of code')[, 2]),
           loc_found = parse_integer(str_match(log_content, 'Solution size: (.*)')[, 2]),
           init_p = parse_number(str_match(log_content, 'Total time spent in enumerator init: (.*) \\(approx\\)')[, 2]) / cpu,
           enum_p = parse_number(str_match(log_content, 'Total time spent in enumerator: (.*) \\(approx\\)')[, 2]) / cpu,
           eval_p = parse_number(str_match(log_content, 'Total time spent in evaluation & testing: (.*) \\(approx\\)')[, 2]) / cpu,
           block_p = parse_number(str_match(log_content, 'Total time spent blocking cubes/programs: (.*) \\(approx\\)')[, 2]) / cpu,
           solution = str_match(log_content, 'Solution found: \\[(.*)\\]')[, 2],
           total_p = init_p + enum_p + eval_p + block_p,
           equivalent_p = cpu / real) %>%
    select(-log, -log_content, -log_suff, -several_runs) %>%
    left_join(instance_info)
}

load_result_file_median <- function(file) {
  load_result_file(file) %>%
    group_by(name) %>%
    mutate(solve_count = sum(ifelse(solved, 1, 0)),
           med_solved = solve_count >= n() / 2,
           status = as.numeric(as.character(status))) %>%
    ungroup() %>%
    group_by(name, benchmark) %>%
    summarise(real = ifelse(med_solved, median(real), timelimit),
              cpu = ifelse(med_solved, median(cpu), timelimit),
              ram = median(ram),
              status = factor(ifelse(med_solved, status %>%
                .[. != -1 & . != -2 & . != 1 & . != 5] %>%
                getmode, status %>%
                                       .[. == -1 | . == -2 | . == 1 | . == 5] %>%
                                       getmode), levels = status_levels, exclude = NULL),
              timeout = status == -1,
              memout = status == -2,
              solved = is_solved_status(status),
              solutions = paste(unique(solution[!is.na(solution)]), collapse = ';;; '),
              solution_n = length(unique(solution[!is.na(solution)]))) %>%
    ungroup() %>%
    distinct()
}

load_result_file_best <- function(file) {
  load_result_file(file) %>%
    group_by(name) %>%
    mutate(solve_count = sum(ifelse(solved, 1, 0)),
           med_solved = solve_count >= n() / 2,
           status = as.numeric(as.character(status))) %>%
    ungroup() %>%
    group_by(name, benchmark) %>%
    summarise(real = min(ifelse(solved, real, timelimit)),
              status = factor(ifelse(any(status == 0),
                                     0,
                                     ifelse(any(status == 2),
                                            2,
                                            ifelse(any(status == 1),
                                                   1,
                                                   -1))), levels = status_levels, exclude = NULL),
              timeout = status == -1,
              memout = status == -2,
              solved = is_solved_status(status)) %>%
    ungroup() %>%
    distinct()
}

load_result_file_worst <- function(file) {
  tmp <- load_result_file(file)
  tmp %>%
    group_by(name) %>%
    mutate(solve_count = sum(ifelse(solved, 1, 0))) %>%
    mutate(med_solved = solve_count >= n() / 2) %>%
    mutate(status = as.numeric(as.character(status))) %>%
    ungroup() %>%
    group_by(name, benchmark) %>%
    summarise(real = max(ifelse(solved, real, timelimit)),
              status = factor(ifelse(any(status == -1),
                                     -1,
                                     ifelse(any(status == 1),
                                            1,
                                            ifelse(any(status == 2),
                                                   2,
                                                   ifelse(any(status == 5),
                                                          5,
                                                          0)))), levels = status_levels, exclude = NULL),
              timeout = status == -1,
              memout = status == -2,
              solved = is_solved_status(status)) %>%
    ungroup() %>%
    distinct()
}

#plot_locs <- function(A) {
#  plot <- A %>%
#    group_by(loc, loc_reached) %>%
#    summarise(count = n()) %>%
#    ungroup() %>%
#    ggplot(aes(x = loc, y = loc_reached, fill = count)) +
#    geom_tile() +
#    geom_text(aes(label = count), size = 15) +
#    scale_x_continuous(breaks = pretty_breaks()) +
#    scale_y_continuous(breaks = pretty_breaks()) +
#    geom_abline() +
#    scale_fill_distiller(palette = 'RdYlGn') +
#    labs(y = 'Lines of code reached', x = 'Solution lines of code') +
#    my_theme
#  list(plot, A %>%
#    filter(loc_reached < loc) %>%
#    select(name, loc_reached, loc))
#}

#plot_locs2 <- function(A) {
#  plot <- A %>%
#    group_by(loc, loc_found) %>%
#    summarise(count = n()) %>%
#    ungroup() %>%
#    ggplot(aes(x = loc, y = loc_found, fill = count)) +
#    geom_tile() +
#    geom_text(aes(label = count), size = 15) +
#    scale_x_continuous(breaks = pretty_breaks()) +
#    scale_y_continuous(breaks = pretty_breaks()) +
#    geom_abline() +
#    scale_fill_distiller(palette = 'RdYlGn') +
#    labs(y = 'Lines of code found', x = 'Solution lines of code') +
#    my_theme
#  list(plot, A %>%
#    filter(loc_found != loc) %>%
#    select(name, loc_found, loc))
#}

#plot_locs3 <- function(A) {
#  plot <- A %>%
#    group_by(loc_found, loc_reached) %>%
#    summarise(count = n()) %>%
#    ungroup() %>%
#    ggplot(aes(x = loc_found, y = loc_reached, fill = count)) +
#    geom_tile() +
#    geom_text(aes(label = count), size = 15) +
#    scale_x_continuous(breaks = pretty_breaks()) +
#    scale_y_continuous(breaks = pretty_breaks()) +
#    geom_abline() +
#    scale_fill_distiller(palette = 'RdYlGn') +
#    labs(y = 'Lines of code reached', x = 'Lines of code found') +
#    my_theme
#  list(plot, A %>%
#    filter(loc_reached < loc_found) %>%
#    select(name, loc_reached, loc_found))
#}

scatter <- function(A, B) {
  merge(eval(parse(text = A)), eval(parse(text = B)), by = 'name', suffixes = c("_A", "_B")) %>%
    filter(solved_A | solved_B) %>%
    mutate(real_A = ifelse(!solved_A, timelimit, real_A)) %>%
    mutate(real_B = ifelse(!solved_B, timelimit, real_B)) %>%
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

plot_times <- function(run, scale_x = scale_x_continuous) {
  run %>%
    ggplot(aes(x = real, fill = factor(status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_histogram(bins = 100) +
    scale_x() +
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
    mutate(val = cumsum(solved)) %>%
    ungroup()
  tmp <- ggplot(data, aes(x = val, y = real, color = try)) +
    geom_step(size = 1) +
    geom_point(shape = 4) +
    #scale_y_continuous(trans = 'log10', breaks = log_breaks(7))
    scale_y_continuous(breaks = pretty_breaks())
  if (full_x) {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks(), limits = c(0, n_distinct(data$name)))
  } else {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks())
  }
  tmp +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(max(length(tries) + 1, 8))[0:length(tries) + 1], breaks = c(names(tries), 'VBS'), drop = F) +
    labs(x = 'Instances solved', y = 'Time', color = 'Legend') +
    geom_vline(xintercept = n_distinct(data$name), linetype = "longdash") +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme
}

vbs <- function(...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  data %>%
    group_by(name, benchmark) %>%
    summarise(real = min(ifelse(solved, real, timelimit)),
              status = factor(ifelse(any(status == 0),
                                     0,
                                     ifelse(any(status == 2),
                                            2,
                                            ifelse(any(status == 1),
                                                   1,
                                                   -1))), levels = status_levels, exclude = NULL),
              solved = is_solved_status(status)) %>%
    mutate(try = 'VBS')
}

bars <- function(use_vbs = T, facet_size = 4, ...) {
  tries <- list(...)
  solved <- bind_rows(tries, .id = 'try')
  if (use_vbs) {
    solved <- solved %>% bind_rows(vbs(...))
  }
  results <- factor(solved$status, levels = status_levels, labels = status_meanings, exclude = NULL)

  pages <- ceiling(length(unique(solved$benchmark)) / (facet_size * facet_size))
  print(pages)
  plots <- vector("list", pages)
  for (i in 1:pages) {
    plots[[i]] <- ggplot(solved, aes(x = factor(try, levels = c(names(tries), 'VBS')), fill = results)) +
      geom_bar(position = "stack") +
      scale_y_continuous(breaks = pretty_breaks()) +
      facet_wrap_paginate(~benchmark, scales = "free", nrow = facet_size, ncol = facet_size, page = i) +
      scale_fill_manual(drop = F, values = status_colors) +
      labs(x = 'configuration', y = 'instances', caption = paste0('page ', toString(i), '/', toString(pages))) +
      my_theme
  }
  plots
}

boxplot <- function(func, ...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    bind_rows(vbs(...)) %>%
    group_by(name) %>%
    mutate(real = ifelse(!solved, timelimit, real)) %>%
    filter(func(solved)) %>%
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
    filter(solved) %>%
    group_by(benchmark, total) %>%
    summarise(n = n()) %>%
    mutate(percentage = n / total)
}

test_filter <- c('scythe/demo-example', 'scythe/sqlsynthesizer', 'scythe/test-examples', 'scythe/newposts', 'scythe/dev-set')

  {
  squares <- load_result_squares('squares')
  scythe <- load_result_squares('scythe')

  #single <- load_result_file('single')
  #single_np <- load_result_file('single_np')
  bitenum <- load_result_file('bitenum')
  bitenum_spider <- load_result_file('bitenum_spider')
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
  #t10 <- load_result_file('try10')
  #t11_c <- load_result_file('try11_c')

  #c0_2 <- load_result_file('cubes0_2')
  #c0_4 <- load_result_file('cubes0_4')
  #c0_8 <- load_result_file('cubes0_8')
  #c0_16 <- load_result_file('cubes0_16')

  #c1_2 <- load_result_file('cubes1_2')
  #c1_4 <- load_result_file('cubes1_4')
  #c1_8 <- load_result_file('cubes1_8')
  #c1_16 <- load_result_file('cubes1_16')

  #c2_2 <- load_result_file('cubes2_2')
  #c2_4 <- load_result_file('cubes2_4')
  #c2_8 <- load_result_file('cubes2_8')
  #c2_16 <- load_result_file('cubes2_16')

  #c2_2_o <- load_result_file('cubes2_2_o')
  #c2_4_o <- load_result_file('cubes2_4_o')
  #c2_8_o <- load_result_file('cubes2_8_o')
  #c2_16_o <- load_result_file('cubes2_16_o')

  #c3_2 <- load_result_file('cubes3_2')
  #c3_4 <- load_result_file('cubes3_4')
  #c3_8 <- load_result_file('cubes3_8')
  #c3_16 <- load_result_file('cubes3_16')

  #c4_16_nmcj <- load_result_file('cubes4_16_nmcj')
  #c4_16_ncj <- load_result_file('cubes4_16_ncj')

  #c4_2 <- load_result_file('cubes4_2')
  #c4_4 <- load_result_file('cubes4_4')
  #c4_8 <- load_result_file('cubes4_8')
  #c4_16 <- load_result_file('cubes4_16')

  #c4_2_h <- load_result_file('cubes4_2_h')
  #c4_4_h <- load_result_file('cubes4_4_h')
  #c4_8_h <- load_result_file('cubes4_8_h')
  #c4_16_h <- load_result_file('cubes4_16_h')

  #c4_16_h_1h <- load_result_file('cubes4_16_h_1h')

  #c5_16 <- load_result_file('cubes5_16')

  #c6_16 <- load_result_file('cubes6_16')
  #c6_16_h <- load_result_file('cubes6_16_h')

  #c7_16_h <- load_result_file('cubes7_16_h')
  #c7_16_h_1h <- load_result_file('cubes7_16_h_1h')

  #c8_16_h <- load_result_file('cubes8_16_h')
  #c9_16_h <- load_result_file('cubes9_16_h')
  #c10_16_h <- load_result_file('cubes10_16_h')
  #c11_16_h <- load_result_file('cubes11_16_h')
  #c12_16_h <- load_result_file('cubes12_16_h')
  #c14_16_h <- load_result_file('cubes14_16_h')

  #c15_2 <- load_result_file('cubes15_2')
  #c15_16_h <- load_result_file('cubes15_16_h')

  #c16_16_h <- load_result_file('cubes16_16_h')
  #c17_16_h <- load_result_file('cubes17_16_h')
  #c18_16_h <- load_result_file('cubes18_16_h')
  #c19_16_h <- load_result_file('cubes19_16_h')
  #c20_16_h <- load_result_file('cubes20_16_h')
  #c21_16_h <- load_result_file('cubes21_16_h')
  #c22_16_h <- load_result_file('cubes22_16_h')
  #c23_16_h <- load_result_file('cubes23_16_h')
  #c24_16_h <- load_result_file('cubes24_16_h')
  #c25_16_h <- load_result_file('cubes25_16_h')

  #c26_2_h_0f <- load_result_file('cubes26_2_h_0f')
  #c26_16_h_0f <- load_result_file('cubes26_16_h_0f')
  #c26_16_h_0f_o <- load_result_file('cubes26_16_h_0f_o')
  #c26_16_h_0f_oo <- load_result_file('cubes26_16_h_0f_oo')

  #c27_16_h_0f <- load_result_file('cubes27_16_h_0f')
  #c28_16_h_0f <- load_result_file('cubes28_16_h_0f')

  #c29_16_0f <- load_result_file('cubes29_16_0f')
  #c29_16_h_0f <- load_result_file('cubes29_16_h_0f')

  #c30_16_0f <- load_result_file('cubes30_16_0f')
  #c31_16_0f <- load_result_file('cubes31_16_0f')
  #c32_16_0f <- load_result_file('cubes32_16_0f')
  #c33_16_0f <- load_result_file('cubes33_16_0f')
  #c34_16_0f_c <- load_result_file('cubes34_16_0f_c')
  #c35_16_0f_c <- load_result_file('cubes35_16_0f_c')
  #c36_16_0f_c <- load_result_file('cubes36_16_0f_c')
  #c37_16_0f_c <- load_result_file('cubes37_16_0f')
  #c38_16_0f_c <- load_result_file('cubes38_16_0f')
  #c39_16_0f <- load_result_file('cubes39_16_0f')
  #c39_16_0f_c <- load_result_file('cubes39_16_0f_c')
  #c40_16_0f_c <- load_result_file('cubes40_16_0f')

  #c41_4_0f_c <- load_result_file('cubes41_4_0f')
  #c41_8_0f_c <- load_result_file('cubes41_8_0f')
  #c41_16_0f_c <- load_result_file('cubes41_16_0f')
  #c41_16_0f_c_5 <- load_result_file_median('cubes41_16_0f_5')
  #c41_16_0f_c_5b <- load_result_file_best('cubes41_16_0f_5')
  #c41_16_0f_c_5w <- load_result_file_worst('cubes41_16_0f_5')
  c41_30_0f_c <- load_result_file('cubes41_30_0f')

  #c42_16_0f_c <- load_result_file('cubes42_16_0f')
  #c42_16_0f_c_5 <- load_result_file_median('cubes42_16_0f_5')
  #c42_16_0f_c_5b <- load_result_file_best('cubes42_16_0f_5')
  #c42_16_0f_c_5w <- load_result_file_worst('cubes42_16_0f_5')

  #c43_16_0f_c_5 <- load_result_file_median('cubes43_16_0f_5')
  #c43_16_0f_c_5b <- load_result_file_best('cubes43_16_0f_5')
  #c43_16_0f_c_5w <- load_result_file_worst('cubes43_16_0f_5')

  #c44_16_0f_c_5 <- load_result_file_median('cubes44_16_0f_5')
  #c44_16_0f_c_5b <- load_result_file_best('cubes44_16_0f_5')
  #c44_16_0f_c_5w <- load_result_file_worst('cubes44_16_0f_5')

  c45_16_0f_c_5 <- load_result_file_median('cubes45_16_0f_5')
  c45_16_0f_c_5b <- load_result_file_best('cubes45_16_0f_5')
  c45_16_0f_c_5w <- load_result_file_worst('cubes45_16_0f_5')

  c45_16_0f_c_dsl_5 <- load_result_file_median('cubes45_16_0f_5_dsl')
  c45_16_0f_c_dsl_5b <- load_result_file_best('cubes45_16_0f_5_dsl')
  c45_16_0f_c_dsl_5w <- load_result_file_worst('cubes45_16_0f_5_dsl')

  c46_16_0f_c_cube_5 <- load_result_file_median('cubes46_16_0f_5_cube')
  c46_16_0f_c_cube_5b <- load_result_file_best('cubes46_16_0f_5_cube')
  c46_16_0f_c_cube_5w <- load_result_file_worst('cubes46_16_0f_5_cube')

  c47_16_0f_c_5 <- load_result_file_median('cubes47_16_0f_5')
  c47_16_0f_c_5b <- load_result_file_best('cubes47_16_0f_5')
  c47_16_0f_c_5w <- load_result_file_worst('cubes47_16_0f_5')

  c47_16_0f_c_spider <- load_result_file_median('cubes47_16_0f_spider')
  c48_16_0f_c_spider <- load_result_file_median('cubes48_16_0f_spider')
  c49_16_0f_c_spider <- load_result_file_median('cubes49_16_0f_spider')
}

scatter('c47_16_0f_c_spider', 'c48_16_0f_c_spider')
scatter('c47_16_0f_c_spider', 'c49_16_0f_c_spider')
scatter('c48_16_0f_c_spider', 'c49_16_0f_c_spider')
scatter('bitenum', 'c47_16_0f_c_5')
scatter('bitenum_spider', 'c49_16_0f_c_spider')

plot_cumsolved(use_vbs = F, '47' = c47_16_0f_c_spider, '48' = c48_16_0f_c_spider, '49' = c49_16_0f_c_spider)

plot_times(c47_16_0f_c_5, scale_x = scale_x_log10)
plot_times(c47_16_0f_c_spider, scale_x = scale_x_log10)
plot_times(c48_16_0f_c_spider, scale_x = scale_x_log10)

b <- inner_join(bitenum_spider, c49_16_0f_c_spider, by = 'name') %>% filter(solved.x & !solved.y)
c <- inner_join(bitenum_spider, c49_16_0f_c_spider, by = 'name') %>% filter(!solved.x & solved.y)

bars(use_vbs = F, facet_size = 5, '47' = c47_16_0f_c_spider, '48' = c48_16_0f_c_spider)

c47_16_0f_c_spider %>% filter(solved) %>% count() / c47_16_0f_c_spider %>% count()
c48_16_0f_c_spider %>% count()
c48_16_0f_c_spider %>% filter(solved) %>% count() / c48_16_0f_c_spider %>% count()
c49_16_0f_c_spider %>% count()
c49_16_0f_c_spider %>% filter(solved) %>% count() / c49_16_0f_c_spider %>% count()
solved_instances(c47_16_0f_c_spider) %>% arrange(percentage)
solved_instances(c48_16_0f_c_spider) %>% arrange(percentage)

boxplot(func = any, '43' = c43_16_0f_c_5, '45' = c45_16_0f_c_5)
boxplot(func = any, scythe = scythe, bitenum = bitenum, '43' = c43_16_0f_c_5, '45' = c45_16_0f_c_5, '47' = c47_16_0f_c_5)
boxplot(func = any, spider = c47_16_0f_c_spider)

c43_16_0f_c_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c45_16_0f_c_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c47_16_0f_c_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c45_16_0f_c_dsl_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c46_16_0f_c_cube_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')