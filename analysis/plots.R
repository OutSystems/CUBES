library(purrr)

maincolor <- '#368869'

plot_pdf <- function(filename, width, height, plot) {
  tikz(file = paste0('plots/', filename, ".tex"), width = textwidth * width, height = textwidth * height, standAlone = T)
  print(plot)
  dev.off()
  setwd('plots')
  system(paste0('xelatex ', filename, ".tex"))
  setwd('..')
}


pick <- function(condition) {
  function(d) d %>% filter_(condition)
}

solved_not_solved <- function(a, b) {
  inner_join(a, b, by = c('name', 'benchmark')) %>%
    filter(solved.x & !solved.y) %>%
    select(name, status.x, real.x, blocked.x, attempts.x, loc_reached.x, status.y, real.y, blocked.y, attempts.y, loc_reached.y)
}

solved_slower <- function(a, b) {
  inner_join(a, b, by = c('name', 'benchmark')) %>%
    filter(solved.x & solved.y & real.x >= real.y * 1.1) %>%
    select(name, status.x, real.x, blocked.x, attempts.x, loc_reached.x, status.y, real.y, blocked.y, attempts.y, loc_reached.y)
}

basic_solved_not_solved <- function(a, b) {
  inner_join(a, b, by = c('name', 'benchmark')) %>%
    filter(solved.x & !solved.y)
}

join_all <- function(a, b) {
  inner_join(a, b, by = c('name', 'benchmark')) %>%
    select(name, status.x, real.x, eval.x, block.x, blocked.x, attempts.x, loc_reached.x, status.y, real.y, eval.y, block.y, blocked.y, attempts.y, loc_reached.y)
}

scatter <- function(exclude = NULL, timelimit = 600, text_size = NULL, transparency = .2, ...) {
  args <- list(...)
  stopifnot(length(args) == 2)
  data <- inner_join(args[[1]], args[[2]], by = c('name', 'benchmark'), suffix = c("_A", "_B")) %>%
    filter(!(benchmark %in% exclude)) %>%
    filter(solved_A | solved_B) %>%
    mutate(real_A = ifelse(!solved_A, timelimit, real_A)) %>%
    mutate(real_B = ifelse(!solved_B, timelimit, real_B))
  tmp <- data %>%
    ggplot(aes(x = real_A, y = real_B)) +
    geom_point(color = maincolor, alpha = transparency, size = 1)
  if (any(data$real_A < 2) | any(data$real_B < 2)) {
    tmp <- tmp +
      scale_x_continuous(trans = log10_trans(), breaks = c(.5, 2, 10, 60, 600), limits = c(min(data$real_A, data$real_B), max(data$real_A, data$real_B)), labels = c('0.5', '2', '10', '60', '600')) +
      scale_y_continuous(trans = log10_trans(), breaks = c(.5, 2, 10, 60, 600), limits = c(min(data$real_A, data$real_B), max(data$real_A, data$real_B)), labels = c('0.5', '2', '10', '60', '600'))
  } else {
    tmp <- tmp +
      scale_x_continuous(trans = log10_trans(), breaks = c(2, 10, 60, 600), limits = c(min(data$real_A, data$real_B), max(data$real_A, data$real_B))) +
      scale_y_continuous(trans = log10_trans(), breaks = c(2, 10, 60, 600), limits = c(min(data$real_A, data$real_B), max(data$real_A, data$real_B)))
  }
  tmp <- tmp +
    geom_abline() +
    annotation_logticks() +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    geom_vline(xintercept = timelimit, linetype = "dashed") +
    labs(y = names(args)[2], x = names(args)[1]) +
    my_theme
  if (!is.null(text_size)) {
    tmp <- tmp + theme(text = element_text(size = text_size))
  }
  tmp
}

scatter_cpu <- function(exclude = NULL, timelimit = 600, transparency = .2, ...) {
  args <- list(...)
  stopifnot(length(args) == 2)
  data <- inner_join(args[[1]], args[[2]], by = c('name', 'benchmark'), suffix = c("_A", "_B")) %>%
    filter(!(benchmark %in% exclude)) %>%
    filter(solved_A | solved_B) %>%
    mutate(cpu_A = ifelse(!solved_A, timelimit, cpu_A)) %>%
    mutate(cpu_B = ifelse(!solved_B, timelimit, cpu_B))
  tmp <- data %>%
    ggplot(aes(x = cpu_A, y = cpu_B)) +
    geom_point(color = maincolor, alpha = transparency, size = 1)
  if (any(data$cpu_A < 2) | any(data$cpu_B < 2)) {
    tmp <- tmp +
      scale_x_continuous(trans = log10_trans(), breaks = c(.5, 2, 10, 60, 600), limits = c(min(data$cpu_A, data$cpu_B), max(data$cpu_A, data$cpu_B)), labels = c('0.5', '2', '10', '60', '600')) +
      scale_y_continuous(trans = log10_trans(), breaks = c(.5, 2, 10, 60, 600), limits = c(min(data$cpu_A, data$cpu_B), max(data$cpu_A, data$cpu_B)), labels = c('0.5', '2', '10', '60', '600'))
  } else {
    tmp <- tmp +
      scale_x_continuous(trans = log10_trans(), breaks = c(2, 10, 60, 600), limits = c(min(data$cpu_A, data$cpu_B), max(data$cpu_A, data$cpu_B))) +
      scale_y_continuous(trans = log10_trans(), breaks = c(2, 10, 60, 600), limits = c(min(data$cpu_A, data$cpu_B), max(data$cpu_A, data$cpu_B)))
  }
  tmp <- tmp +
    geom_abline() +
    annotation_logticks() +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    geom_vline(xintercept = timelimit, linetype = "dashed") +
    labs(y = names(args)[2], x = names(args)[1]) +
    my_theme
  tmp
}

scatter_ram <- function(exclude = NULL, ...) {
  args <- list(...)
  stopifnot(length(args) == 2)
  data <- inner_join(args[[1]], args[[2]], by = c('name', 'benchmark'), suffix = c("_A", "_B")) %>%
    filter(!(benchmark %in% exclude)) %>%
    filter(solved_A & solved_B)
  data %>%
    ggplot(aes(x = ram_A * 1000, y = ram_B * 1000)) +
    geom_point(color = maincolor, alpha = 0.2, size = 1) +
    scale_x_continuous(trans = log10_trans(), breaks = log_breaks(n = 4), labels = label_bytes(), limits = c(min(data$ram_A, data$ram_B) * 1000, max(data$ram_A, data$ram_B) * 1000)) +
    scale_y_continuous(trans = log10_trans(), breaks = log_breaks(n = 4), labels = label_bytes(), limits = c(min(data$ram_A, data$ram_B) * 1000, max(data$ram_A, data$ram_B) * 1000)) +
    geom_abline() +
    annotation_logticks() +
    labs(y = names(args)[2], x = names(args)[1]) +
    my_theme
}

scatter_enum <- function(exclude = NULL, ...) {
  args <- list(...)
  stopifnot(length(args) == 2)
  data <- inner_join(args[[1]], args[[2]], by = c('name', 'benchmark'), suffix = c("_A", "_B")) %>%
    filter(!(benchmark %in% exclude)) %>%
    filter(solved_A & solved_B)
  data %>%
    ggplot(aes(x = attempts_A, y = attempts_B)) +
    geom_point(color = maincolor, alpha = 0.2, size = 1) +
    scale_x_continuous(trans = log10_trans(), breaks = log_breaks(n = 4), limits = c(min(data$attempts_A, data$attempts_B), max(data$attempts_A, data$attempts_B))) +
    scale_y_continuous(trans = log10_trans(), breaks = log_breaks(n = 4), limits = c(min(data$attempts_A, data$attempts_B), max(data$attempts_A, data$attempts_B))) +
    geom_abline() +
    labs(y = names(args)[2], x = names(args)[1]) +
    my_theme
}

hardness <- function(A, limit) {
  eval(parse(text = A)) %>%
    filter(timeout == T | !is.na(hard_h)) %>%
    mutate(hard_h = ifelse(is.na(hard_h), limit * 1.5, hard_h)) %>%
    ggplot(aes(x = hard_h, fill = factor(status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_histogram(bins = 40) +
    scale_x_continuous(trans = 'log10') +
    #scale_y_continuous(trans = 'log10') +
    geom_vline(xintercept = limit, linetype = "dashed") +
    scale_fill_manual(drop = F, values = status_colors, '') +
    labs(y = 'instance count', x = 'time') +
    ggtitle('Distribution of hard heuristic activation') +
    my_theme
}

times <- function(run, exclude_timeouts = F) {
  run %>%
    filter(!exclude_timeouts | status != -1) %>%
    ggplot(aes(x = real, fill = factor(status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_histogram(bins = 60) +
    scale_x_continuous(trans = 'log10', breaks = log_breaks()) +
    scale_fill_manual(drop = F, values = status_colors, '') +
    geom_vline(xintercept = 600) +
    my_theme
}

processes <- function(run, n) {
  run %>%
    ggplot(aes(x = process + 1)) +
    geom_bar(fill = maincolor, aes(y = stat(count / sum(count)))) +
    scale_y_continuous(labels = label_percent(accuracy = 1, suffix = '\\%')) +
    scale_x_continuous(breaks = 1:n) +
    labs(x = 'Process', y = 'Instances Solved') +
    my_theme
}

equiv_processes <- function(run, cutoff = 5) {
  run %>%
    filter(real > cutoff) %>%
    ggplot(aes(x = equivalent_p)) +
    geom_histogram(fill = maincolor, bins = 20) +
    scale_x_continuous(breaks = c(1, 4, 8, 12, 16)) +
    labs(x = 'Equivalent Number of Processes', y = '\\# Instances') +
    my_theme
}

cumsolved <- function(use_vbs = F, full_x = F, exclude = NULL, log = T, every_other = 50, step_size = .5, point_size = .75, facet = F, legend.position = 'bottom', ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  if (use_vbs) {
    data <- data %>% bind_rows(vbs(...))
  }
  data <- data %>%
    filter(!(benchmark %in% exclude)) %>%
    arrange(real) %>%
    group_by(try) %>%
    mutate(val = cumsum(solved)) %>%
    ungroup() %>%
    filter(solved) %>%
    mutate(id = row_number())
  tmp <- ggplot(data, aes(x = val, y = real, color = try, shape = try)) +
    geom_step(size = step_size) +
    geom_point(shape = point_size, data = pick(~id %% every_other == 0))
  if (full_x) {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks(), limits = c(0, n_distinct(data$name)))
  } else {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks())
  }
  if (log) {
    if (any(data$real < 2)) {
      tmp <- tmp +
        scale_y_continuous(trans = log_trans(10), breaks = c(.5, 2, 5, 10, 60, 180, 600), labels = c('0.5', '2', '5', '10', '60', '180', '600')) +
        annotation_logticks(sides = 'l')
    } else {
      tmp <- tmp +
        scale_y_continuous(trans = log_trans(10), breaks = c(2, 5, 10, 60, 180, 600)) +
        annotation_logticks(sides = 'l')
    }
  } else {
    tmp <- tmp + facet_zoom(xy = real <= 10, zoom.size = 2 / 3)
  }
  tmp +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(max(length(tries) + 1, 8))[0:length(tries) + 1], breaks = c(names(tries), 'VBS'), drop = F) +
    labs(x = 'Instances solved', y = 'Time') +
    scale_shape(breaks = c(names(tries), 'VBS'), drop = F) +
    #geom_vline(xintercept = n_distinct(data$name), linetype = "longdash") +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme
}

invsolved <- function(use_vbs = F, full_x = F, exclude = NULL, log = T, every_other = 50, step_size = .5, point_size = .75, facet = F, legend.position = 'bottom', ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  if (use_vbs) {
    data <- data %>% bind_rows(vbs(...))
  }
  data <- data %>%
    filter(!(benchmark %in% exclude)) %>%
    arrange(real) %>%
    group_by(try) %>%
    mutate(val = cumsum(solved) / n_distinct(name)) %>%
    filter(solved) %>%
    mutate(id = row_number()) %>%
    ungroup()
  tmp <- ggplot(data, aes(y = val, x = real, color = try, group = try, shape = try)) +
    geom_step(size = step_size) +
    geom_point(size = point_size, data = pick(~id %% every_other == 0))
  if (full_x) {
    tmp <- tmp + scale_y_continuous(breaks = extended_breaks(n = 6), limits = c(0, 1), labels = label_percent(accuracy = 1, suffix = '\\%'))
  } else {
    tmp <- tmp + scale_y_continuous(breaks = extended_breaks(n = 6), labels = label_percent(accuracy = 1, suffix = '\\%'))
  }
  if (log) {
    if (any(data$real < 2)) {
      tmp <- tmp +
        scale_x_continuous(trans = log_trans(10), breaks = c(.5, 2, 5, 10, 60, 180, 600), labels = c('0.5', '2', '5', '10', '60', '180', '600')) +
        annotation_logticks(sides = 'b')
    } else {
      tmp <- tmp +
        scale_x_continuous(trans = log_trans(10), breaks = c(2, 5, 10, 60, 180, 600)) +
        annotation_logticks(sides = 'b')
    }
  } else {
    tmp <- tmp + facet_zoom(xy = real <= 10, zoom.size = 2 / 3)
  }
  if (facet) {
    tmp <- tmp + facet_wrap(~benchmark)
  }
  tmp +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(max(length(tries) + 1, 8))[0:length(tries) + 1], breaks = c(names(tries), 'VBS'), drop = F) +
    scale_shape(breaks = c(names(tries), 'VBS'), drop = F) +
    labs(y = 'Instances Solved', x = 'Time (s)') +
    #geom_vline(xintercept = n_distinct(data$name), linetype = "longdash") +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme +
    theme(legend.position = legend.position, legend.background = element_rect(fill = F), legend.key = element_rect(fill = F), legend.key.height = unit(.75, 'lines'))
}

invsolved_cpu <- function(use_vbs = F, full_x = F, exclude = NULL, log = T, every_other = 50, step_size = .5, point_size = .75, facet = F, legend.position = 'bottom', ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  if (use_vbs) {
    data <- data %>% bind_rows(vbs(...))
  }
  data <- data %>%
    filter(!(benchmark %in% exclude)) %>%
    arrange(cpu) %>%
    group_by(try) %>%
    mutate(val = cumsum(solved) / n_distinct(name)) %>%
    filter(solved) %>%
    mutate(id = row_number()) %>%
    ungroup()
  tmp <- ggplot(data, aes(y = val, x = cpu, color = try, group = try, shape = try)) +
    geom_step(size = step_size) +
    geom_point(size = point_size, data = pick(~id %% every_other == 0))
  if (full_x) {
    tmp <- tmp + scale_y_continuous(breaks = extended_breaks(n = 6), limits = c(0, 1), labels = label_percent(accuracy = 1, suffix = '\\%'))
  } else {
    tmp <- tmp + scale_y_continuous(breaks = extended_breaks(n = 6), labels = label_percent(accuracy = 1, suffix = '\\%'))
  }
  if (log) {
    if (any(data$cpu < 2)) {
      tmp <- tmp +
        scale_x_continuous(trans = log_trans(10), breaks = c(.5, 2, 5, 10, 60, 180, 600), labels = c('0.5', '2', '5', '10', '60', '180', '600')) +
        annotation_logticks(sides = 'b')
    } else {
      tmp <- tmp +
        scale_x_continuous(trans = log_trans(10), breaks = c(2, 5, 10, 60, 180, 600)) +
        annotation_logticks(sides = 'b')
    }
  } else {
    tmp <- tmp + facet_zoom(xy = cpu <= 10, zoom.size = 2 / 3)
  }
  if (facet) {
    tmp <- tmp + facet_wrap(~benchmark)
  }
  tmp +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(max(length(tries) + 1, 8))[0:length(tries) + 1], breaks = c(names(tries), 'VBS'), drop = F) +
    scale_shape(breaks = c(names(tries), 'VBS'), drop = F) +
    labs(y = 'Instances Solved', x = 'CPU Time (s)') +
    #geom_vline(xintercept = n_distinct(data$name), linetype = "longdash") +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme +
    theme(legend.position = legend.position, legend.background = element_rect(fill = F), legend.key = element_rect(fill = F), legend.key.height = unit(.75, 'lines'))
}

vbs <- function(..., timelimit = 600) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  data %>%
    group_by(name, benchmark) %>%
    summarise(real = min(ifelse(solved, real, timelimit)),
              cpu = min(ifelse(solved, cpu, timelimit)),
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
    tmp <- ggplot(solved, aes(x = factor(try, levels = c(names(tries), 'VBS')), fill = results)) +
      geom_bar(position = "stack") +
      scale_y_continuous(breaks = pretty_breaks())
    if (pages != 1) {
      tmp <- tmp + facet_wrap_paginate(~benchmark, scales = "free", nrow = facet_size, ncol = facet_size, page = i)
    } else {
      tmp <- tmp + facet_wrap(~benchmark, scales = "free")
    }
    tmp <- tmp +
      scale_fill_manual(drop = T, values = map2(status_levels, status_colors, c) %>%
        keep(function(x) { any(x[1] == as.character(solved$status)) }) %>%
        map(function(x) { x[2] })) +
      labs(x = element_blank(), y = 'Number of Instances')
    if (pages != 1) {
      tmp <- tmp + labs(caption = paste0('page ', toString(i), '/', toString(pages)))
    }
    plots[[i]] <- tmp + my_theme
  }
  plots
}

boxplot <- function(func, timelimit = 600, ...) {
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
    geom_sina(aes(fill = factor(try, levels = c(names(tries), 'VBS')), col = factor(try, levels = c(names(tries), 'VBS'))), show.legend = FALSE, width = .15, height = 0) +
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

boxplot_fails <- function(func, ...) {
  tries <- list(...)
  bind_rows(tries, .id = 'try') %>%
    group_by(name) %>%
    filter(func(solved)) %>%
    ggplot(aes(fill = factor(try, levels = c(names(tries), 'VBS')), x = fails)) +
    geom_histogram(position = "identity", alpha = 0.5, bins = 60) +
    scale_y_continuous(trans = pseudo_log_trans(sigma = 1, base = 10)) +
    scale_fill_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(length(tries) + 1)) +
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

time_dist <- function(filter = NULL, wrap = F, ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  if (!is.null(filter)) {
    data <- right_join(data, filter %>% select(name))
  }
  tmp <- ggplot(data, aes(x = real, group = try, fill = try))
  for (n in names(tries)) {
    tmp <- tmp + geom_histogram(data = subset(data, try == n), aes(y = stat(count / sum(count))), alpha = 0.6, bins = 20)
  }
  if (wrap) {
    tmp <- tmp + facet_wrap(~try)
  }
  tmp + labs(y = 'Frequency', x = 'Time')
}

time_part_dist <- function(filter = NULL, col = NULL, wrap = F, ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  if (!is.null(filter)) {
    data <- right_join(data, filter %>% select(name))
  }
  tmp <- ggplot(data, aes(x = !!as.name(col) / real, group = try, fill = try))
  for (n in names(tries)) {
    tmp <- tmp + geom_histogram(data = subset(data, try == n), alpha = 0.4, bins = 20)
  }
  if (wrap) {
    tmp <- tmp + facet_wrap(~try)
  }
  tmp +
    scale_fill_brewer(palette = 'Dark2') +
    labs(y = 'Number of Instances', x = paste0('Fraction of Time Spent in ', ifelse(col == 'eval', 'Evaluation', col)), legend = 'Configuration') +
    my_theme
}

plot_cells_time <- function(...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try') %>% mutate(real = ifelse(status == -2, 600, real), try = factor(try, levels = names(tries))) %>%
    mutate(status = ifelse(status == 0 | status == 2, 0, ifelse(status == -1, 1, 2)),
           real = ifelse(status == 2, 1000, real))
  data %>% ggplot(aes(x = total_cells, y = real, color = factor(data$status, levels = c(2,1,0), labels = c('Other', 'Timeout', 'Solved'), exclude = NULL))) +
    geom_point(alpha = 0.3, size = 0.9) +
    scale_x_log10(breaks = log_breaks()) +
    scale_y_log10(breaks = c(.1, 1, 10, 60, 600)) +
    # annotation_logticks(sides='lb') +
    facet_wrap(~try) +
    scale_color_manual(drop = T, values = map2(c(0,1,2), c('#000000', '#cc241d', maincolor), c) %>%
      keep(function(x) { any(x[1] == as.character(data$status)) }) %>%
      map(function(x) { x[2] })) +
    labs(y = 'Time (s)', x = '\\# Total Cells') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    my_theme
}

plot_cells_ram <- function(...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  data %>% ggplot(aes(x = total_cells, y = ram * 1000, color = factor(data$status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_point(alpha = 0.4, size = 1) +
    scale_x_log10(breaks = log_breaks()) +
    scale_y_log10(breaks = log_breaks(), labels = label_bytes()) +
    annotation_logticks(sides = 'lb') +
    facet_wrap(~try) +
    scale_color_manual(drop = T, values = map2(status_levels, status_colors, c) %>%
      keep(function(x) { any(x[1] == as.character(data$status)) }) %>%
      map(function(x) { x[2] })) +
    labs(y = 'Time (s)', x = '# Total Cells') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    my_theme
}

scatter_real_cpu <- function(...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  data %>% ggplot(aes(x = real, y = cpu, color = factor(data$status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_point(alpha = 0.4, size = 1) +
    geom_abline(size = .25) +
    annotate(x = 0.5, y = 0.8, "text", label = 'slope=1', angle = 30, size = 2.5) +
    geom_abline(intercept = log10(4), size = .25) +
    annotate(x = 0.5, y = 3.5, "text", label = 'slope=4', angle = 30, size = 2.5) +
    geom_abline(intercept = log10(16), size = .25) +
    annotate(x = 0.5, y = 15, "text", label = 'slope=16', angle = 30, size = 2.5) +
    scale_x_continuous(trans = 'log10', breaks = log_breaks()) +
    scale_y_log10(breaks = log_breaks()) +
    annotation_logticks(sides = 'lb') +
    facet_wrap(~try) +
    scale_color_manual(drop = T, values = map2(status_levels, status_colors, c) %>%
      keep(function(x) { any(x[1] == as.character(data$status)) }) %>%
      map(function(x) { x[2] })) +
    labs(y = 'CPU Time', x = 'Real Time') +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    my_theme
}

scatter_equiv <- function(x, ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  data %>% ggplot(aes(x = !!as.name(x), y = cpu / real, color = factor(data$status, levels = status_levels, labels = status_meanings, exclude = NULL))) +
    geom_point(alpha = 0.4, size = 1) +
    scale_x_log10(breaks = log_breaks()) +
    # scale_y_log10(breaks = log_breaks()) +
    annotation_logticks(sides = 'b') +
    facet_wrap(~try) +
    scale_color_manual(drop = T, values = map2(status_levels, status_colors, c) %>%
      keep(function(x) { any(x[1] == as.character(data$status)) }) %>%
      map(function(x) { x[2] })) +
    labs(y = 'cpu/real', x = x) +
    guides(colour = guide_legend(override.aes = list(alpha = 1))) +
    my_theme
}

plot_sql_size <- function(...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  data %>%
    filter(solved) %>%
    ggplot(aes(x = sql_size)) +
    geom_histogram(bins=10) +
    facet_wrap(~try)
}

plot_fuzzy <- function(drop_error = F, fill_bars = F, filter_all = F, facet = F, refactor=F, ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try') %>%
    filter(status == 0 & benchmark != '55-tests') %>%
    mutate(try = factor(try, levels = names(tries)))
  if (refactor) {
    data <- data %>% mutate(fuzzy = fct_recode(fuzzy, "Fuzzing Accepted" = 'Possibly Correct', "Incorrect by Fuzzing" = 'Possibly Correct Top 3', "Incorrect by Fuzzing" = 'Possibly Correct Top 5',  Unknown = "Error", Unknown = "Timeout", Unknown = "Fuzzer Error", Unknown = "GT Mismatch"))
    print(data$fuzzy)
  }
  if (filter_all) {
    data <- data %>%
      group_by(name) %>%
      filter(all(fuzzy != 'Error')) %>%
      ungroup()
  }
  if (drop_error) {
    data <- data %>% filter(fuzzy != 'Error')
  }
  tmp <- data %>% ggplot(aes(x = try, fill = fuzzy))
  if (fill_bars) {
    tmp <- tmp + geom_bar(position = 'fill') + scale_y_continuous(labels = label_percent(accuracy = 1, suffix = '\\%'))
  } else {
    tmp <- tmp + geom_bar(position = 'stack')
  }
  if (facet) {
    tmp <- tmp + facet_wrap(~benchmark, scales = 'free_y')
  }
  if (!refactor) {
    tmp <- tmp + scale_fill_manual(values = map2(fuzzy_levels, fuzzy_colors, c) %>% map(function(x) { x[2] }), drop = F)
  } else {
    tmp <- tmp + scale_fill_manual(values = map2(rev(c(0, 1, 2, -1)), rev(c("#57853C", "#B4560E", "#d79921", "#999999")), c) %>%
      map(function(x) { x[2] }), drop = F)
  }
  tmp  +
    labs(y = 'Percentage of Instances', x = NULL, fill = 'Fuzzing Status') +
    my_theme +
    theme(legend.position = "right", , legend.title = element_text())
}

different_solutions <- function(table) {
  table %>% ggplot(aes(x = solution_n)) +
    geom_bar()
}

n_solveds <- function(table) {
  table %>% ggplot(aes(x = solveds)) +
    geom_bar()
}

non_determinism_plot <- function(data) {
  data %>% group_by(solution_n, solveds) %>% summarise(n = n()) %>% ggplot(aes(x = solveds, y = solution_n, fill=n)) +
    geom_tile() +
    geom_text(aes(label=n), size=2.5) +
    scale_fill_gradient(low = 'white', high = maincolor) +
    scale_x_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
    scale_y_continuous(breaks = c(0,1,2,3,4,5,6,7,8,9,10)) +
    labs(x = 'Times Solved', y = 'Different Solutions') +
    my_theme +
    theme(panel.grid.major = element_blank())
}

speedup <- function(data1, data2) {
  data <- data1 %>% inner_join(data2, by='name') %>%
    filter(solved.x & solved.y & real.x > 60) %>%
    mutate(speedup = real.x / real.y)
  p25 <- quantile(data$speedup, probs=.25)
  p50 <- quantile(data$speedup, probs=.5)
  p75 <- quantile(data$speedup, probs=.75)
  data %>%
    # group_by(hard) %>%
    # summarise(speedup = gm_mean(speedup)) %>%
    # ungroup() %>%
    ggplot(aes(x=speedup)) + geom_histogram(fill=maincolor, bins=15) +
    scale_x_log10() +
    annotation_logticks(sides='b') +
    geom_vline(xintercept = p25, color='#555555') +
    annotate(geom = "text", x=10^(log10(p25)-.075), label="25\\%", y=100, angle=90, size=2.5) +
    geom_vline(xintercept = p50, color='#555555') +
    annotate(geom = "text", x=10^(log10(p50)-.075), label="50\\%", y=100, angle=90, size=2.5) +
    geom_vline(xintercept = p75, color='#555555') +
    annotate(geom = "text", x=10^(log10(p75)-.075), label="75\\%", y=100, angle=90, size=2.5) +
    labs(x = 'Speedup', y = 'Number of Instances') +
    my_theme
}

speedup_data <- function(data1, data2) {
  data <- data1 %>% inner_join(data2, by='name') %>%
    filter(solved.x & solved.y & real.x > 60) %>%
    mutate(speedup = real.x / real.y)
  print(sprintf('Mean: %f', mean(data$speedup)))
  print(sprintf('Std Dev: %f', sd(data$speedup)))
  print(sprintf('Geom Mean: %f', gm_mean(data$speedup)))
  print(sprintf('Median: %f', median(data$speedup)))
}