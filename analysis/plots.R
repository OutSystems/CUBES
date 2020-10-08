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


pick <- function(condition){
  function(d) d %>% filter_(condition)
}

scatter <- function(exclude = NULL, timelimit = 600, text_size=NULL, ...) {
  args <- list(...)
  stopifnot(length(args) == 2)
  data <- inner_join(args[[1]], args[[2]], by = c('name', 'benchmark'), suffix = c("_A", "_B")) %>%
    filter(!(benchmark %in% exclude)) %>%
    filter(solved_A | solved_B) %>%
    mutate(real_A = ifelse(!solved_A, timelimit, real_A)) %>%
    mutate(real_B = ifelse(!solved_B, timelimit, real_B))
  tmp <- data %>%
    ggplot(aes(x = real_A, y = real_B)) +
    geom_point(color = maincolor, alpha = 0.2, size = .75) +
    scale_x_continuous(trans = log10_trans(), breaks = c(2, 10, 60, 600), limits = c(min(data$real_A, data$real_B), max(data$real_A, data$real_B))) +
    scale_y_continuous(trans = log10_trans(), breaks = c(2, 10, 60, 600), limits = c(min(data$real_A, data$real_B), max(data$real_A, data$real_B))) +
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

scatter_cpu <- function(exclude = NULL, ...) {
  args <- list(...)
  stopifnot(length(args) == 2)
  data <- inner_join(args[[1]], args[[2]], by = c('name', 'benchmark'), suffix = c("_A", "_B")) %>%
    filter(!(benchmark %in% exclude)) %>%
    filter(solved_A | solved_B) %>%
    mutate(real_A = ifelse(!solved_A, timelimit, real_A)) %>%
    mutate(real_B = ifelse(!solved_B, timelimit, real_B))
  data %>%
    ggplot(aes(x = cpu_A, y = cpu_B)) +
    geom_point(color = maincolor, alpha = 0.2, size = 1.5) +
    scale_x_continuous(trans = log10_trans(), breaks = log_breaks(), limits = c(min(data$cpu_A, data$cpu_B), max(data$cpu_A, data$cpu_B))) +
    scale_y_continuous(trans = log10_trans(), breaks = log_breaks(), limits = c(min(data$cpu_A, data$cpu_B), max(data$cpu_A, data$cpu_B))) +
    geom_abline() +
    annotation_logticks() +
    geom_hline(yintercept = timelimit, linetype = "dashed") +
    geom_vline(xintercept = timelimit, linetype = "dashed") +
    labs(y = names(args)[2], x = names(args)[1]) +
    my_theme
}

scatter_ram <- function(exclude = NULL, ...) {
  args <- list(...)
  stopifnot(length(args) == 2)
  data <- inner_join(args[[1]], args[[2]], by = c('name', 'benchmark'), suffix = c("_A", "_B")) %>%
    filter(!(benchmark %in% exclude)) %>%
    filter(solved_A & solved_B)
  data %>%
    ggplot(aes(x = ram_A * 1000, y = ram_B * 1000)) +
    geom_point(color = maincolor, alpha = 0.2, size = 1.5) +
    scale_x_continuous(trans = log10_trans(), breaks = log_breaks(), labels = label_bytes(), limits = c(min(data$ram_A, data$ram_B) * 1000, max(data$ram_A, data$ram_B) * 1000)) +
    scale_y_continuous(trans = log10_trans(), breaks = log_breaks(), labels = label_bytes(), limits = c(min(data$ram_A, data$ram_B) * 1000, max(data$ram_A, data$ram_B) * 1000)) +
    geom_abline() +
    annotation_logticks() +
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
    geom_histogram(bins = 100) +
    scale_x_continuous(trans = 'log10', breaks = log_breaks()) +
    scale_fill_manual(drop = F, values = status_colors, '') +
    geom_vline(xintercept = 600) +
    my_theme
}

processes <- function(run) {
  run %>%
    ggplot(aes(x = process)) +
    geom_bar() +
    my_theme
}

cumsolved <- function(use_vbs = T, full_x = F, exclude = NULL, ...) {
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
    filter(solved)
  tmp <- ggplot(data, aes(x = val, y = real, color = try)) +
    geom_step(size = 1) +
    geom_point(shape = 4)
  if (full_x) {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks(), limits = c(0, n_distinct(data$name)))
  } else {
    tmp <- tmp + scale_x_continuous(breaks = pretty_breaks())
  }
  tmp +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(max(length(tries) + 1, 8))[0:length(tries) + 1], breaks = c(names(tries), 'VBS'), drop = F) +
    labs(x = 'Instances solved', y = 'Time') +
    facet_zoom(xy = real <= 10, zoom.size = 2 / 3) +
    #geom_vline(xintercept = n_distinct(data$name), linetype = "longdash") +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme
}

invsolved <- function(use_vbs = T, full_x = F, exclude = NULL, log = F, every_other=50, legend.position='bottom', ...) {
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
  tmp <- ggplot(data, aes(y = val, x = real, color = try, group=try, shape=try)) +
    geom_step(size = .5) +
    geom_point(size=.75, data = pick(~id %% every_other == 0))
  if (full_x) {
    tmp <- tmp + scale_y_continuous(breaks = extended_breaks(n = 6), limits = c(0, 1), labels=label_percent(accuracy = 1, suffix = '\\%'))
  } else {
    tmp <- tmp + scale_y_continuous(breaks = extended_breaks(n = 6), labels=label_percent(accuracy = 1, suffix = '\\%'))
  }
  if (log) {
    if(any(data$real < 2)) {
      tmp <- tmp + scale_x_continuous(trans=log_trans(10), breaks = c(.5, 2, 5, 10, 60, 180, 600), labels = c('0.5', '2', '5', '10', '60', '180', '600')) +
        annotation_logticks(sides='b')
    } else {
      tmp <- tmp + scale_x_continuous(trans=log_trans(10), breaks = c(2, 5, 10, 60, 180, 600)) +
        annotation_logticks(sides='b')
    }
  } else {
    tmp <- tmp + facet_zoom(xy = real <= 10, zoom.size = 2 / 3)
  }
  tmp +
    scale_color_manual(values = colorRampPalette(brewer.pal(name = "Dark2", n = 8))(max(length(tries) + 1, 8))[0:length(tries) + 1], breaks = c(names(tries), 'VBS'), drop = F) +
    scale_shape_discrete(breaks = c(names(tries), 'VBS'), drop = F) +
    labs(y = 'Instances solved', x = 'Time (s)') +
    #geom_vline(xintercept = n_distinct(data$name), linetype = "longdash") +
    #annotation_logticks(sides = 'l') +
    #geom_vline(xintercept = 600) +
    my_theme + theme(legend.position = legend.position, legend.background = element_rect(fill=F), legend.key = element_rect(fill=F), legend.key.height = unit(.75, 'lines'))
}

vbs <- function(..., timelimit = 600) {
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
      labs(x = element_blank(), y = 'Number of instances')
    if (pages != 1) {
      tmp <- tmp + labs(caption = paste0('page ', toString(i), '/', toString(pages)))
    }
    plots[[i]] <- tmp + my_theme
  }
  plots
}

boxplot <- function(func, timelimit=600, ...) {
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