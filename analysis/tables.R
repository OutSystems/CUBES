
benchmark_summary <- function(filename, ...) {
  tries <- rev(list(...))
  solved <- bind_rows(tries, .id = 'try')
  table <- solved %>%
    bind_rows(mutate(., benchmark = "All")) %>%
    group_by(try, benchmark) %>%
    summarise(n_inst = n(),
              p_tmp = sum(ifelse(solved, 1L, 0L)),
              p = p_tmp / n_inst,
              p2_tmp = sum(ifelse(real <= 10 & solved, 1L, 0L)),
              p2 = p2_tmp / n_inst) %>%
    ungroup() %>% group_by(try) %>%
    select(-n_inst, -p_tmp, -p2_tmp) %>%
    mutate(benchmark = ifelse(benchmark != 'All', paste0('\\begin{sideways}\\texttt{', benchmark, '}\\end{sideways}'), benchmark)) %>%
    rename('10 min' = p, '10 sec' = p2) %>%
    gather('10 min', '10 sec', key = 'info', value = 'val') %>%
    spread(benchmark, val) %>%
    arrange(desc(info), match(try, rev(names(tries)))) %>%
    ungroup() %>%
    group_by(info) %>%
    mutate_at(vars(-info,-try), function(x) {ifelse(!is.na(x), ifelse(x == max(x,na.rm=T), paste0('\\textbf{', percent(x, accuracy=.1, suffix='\\%'), '}'), percent(x, accuracy=.1, suffix='\\%')), 'NA')}) %>%
    mutate(info = ifelse(row_number() == 1, paste0('\\multirow{', length(tries) ,'}{*}{\\STAB{\\rotatebox[origin=c]{90}{', info, '}}}'), '')) %>%
    ungroup() %>%
    relocate(info, .before = try) %>%
    rename('Run' = try) %>%
    rename('~' = info) %>%
    xtable()
  align(table) <- "lllcccccl"
    print(table, include.rownames = FALSE, file = paste0('tables/', filename, ".tex"), sanitize.text.function = function(x) { x }, booktabs = TRUE, hline.after = c(0,length(tries),length(tries)*2))
}
