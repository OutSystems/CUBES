status_levels <- rev(c(0, 3, 2, 4, -2, -1, 5, 1, 143))
status_meanings <- rev(c('R and SQL', 'Non-optimal', 'Just R', 'Just R Non-optimal', 'Memout', 'Timeout', 'No solution', 'Fail', 'Scythe ERR'))
status_colors <- rev(c("#57853C", "#296429", "#d79921", "#B4560E", "#4B44CC", "#cc241d", "#653e9c", "#3c3836", '#000000'))

fuzzy_levels <- rev(c(0, 1, 2, 3, -1))
fuzzy_meanings <- rev(c('Correct', 'Incorrect', 'Fuzzying Incorrect', 'Timeout', 'Error'))
fuzzy_colors <- rev(c("#57853C", "#B4560E", "#d79921", "#4B44CC", "#cc241d"))

instance_info <- read_csv('instances.csv', col_types = cols(
  name = col_character(),
  .default = '?'
)) %>% mutate(benchmark = ifelse(grepl("spider", name, fixed = TRUE), "spider", gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))))

prr <- function(x) {
  print(x)
  x
}

make_true_na <- function(x) {
  unlist(x)
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

max_non_inf <- function(...) {
  tmp <- max(...)
  if (is.finite(tmp)) {
    tmp
  } else {
    NA
  }
}

is_solved_status <- function(status) {
  status != -2 &
    status != -1 &
    status != 1 &
    status != 5
}

load_result_squares <- function(file) {
  result <- read_csv(paste0('data/', file, '.csv'), col_types = cols(.default = '?', status = col_factor(levels = status_levels))) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)),
           status = ifelse(status == 143, 1, as.character(status)),
           status = ifelse(timeout, -1, as.character(status)))
  if ('memout' %in% names(result)) {
    result <- result %>% mutate(status = ifelse(memout, -2, as.character(status)))
  }
  result <- result %>%
    mutate(solved = is_solved_status(status)) %>%
    mutate(benchmark = ifelse(grepl("spider", name, fixed = TRUE), "spider", gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)))) %>%
    filter(!(benchmark %in% test_filter)) %>%
    left_join(instance_info)
  if (file.exists(paste0('fuzzy/', file, '.csv'))) {
    result_fuzzy <- read_csv(paste0('fuzzy/', file, '.csv'), col_types = cols(.default='?', base_eq='c'))
    result <- left_join(result, result_fuzzy) %>%
      mutate(fuzzy = ifelse(is.na(base_eq), -1, ifelse(base_eq == '-1', 3, ifelse(base_eq == 'False', 1, ifelse(fuzzy_eq == fuzzies, 0, 2))))) %>%
      mutate(fuzzy = factor(fuzzy, fuzzy_levels, fuzzy_meanings))
  }
  gc()
  result
}

load_result_file <- function(file) {
  result <- read_csv(paste0('data/', file, '.csv'), col_types = cols(.default = '?', status = col_factor(levels = status_levels))) %>%
    #mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    mutate(benchmark = ifelse(grepl("spider", name, fixed = TRUE), "spider", gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)))) %>%
    filter(!(benchmark %in% test_filter)) %>%
    group_by(name) %>%
    mutate(run_number = 0:(n() - 1)) %>%
    ungroup() %>%
    mutate(several_runs = any(run_number > 0),
           status = ifelse(is.na(status), 1, as.character(status)),  # *sighs* factors are weird
           status = ifelse(timeout, ifelse(status == 3 | status == 4, as.character(status), -1), as.character(status)),  # *sighs* factors are weird
           status = ifelse(memout, -2, as.character(status)),  # *sighs* factors are weird
           solved = is_solved_status(status),
           log_suff = paste0('_', run_number),
           log = paste0('data/', file, '/', name, log_suff, '.log'),
           log_content = map(log, function(x) { ifelse(file.exists(x), read_file(x), NA) }),
           skewed = unlist(map(log_content, function(x) { str_detect(x, 'ypcall') })),
           hard_h = unlist(map(log_content, function(x) { str_match(x, '\\[(.*)\\]\\[.*\\]\\[INFO\\] Hard problem!')[, 2] })),
           loc_reached = unlist(map(log_content, function(x) { max_non_inf(parse_integer(str_match_all(x, 'Enumerator for loc (.*) constructed')[[1]][,2])) })),
           loc_found = unlist(map(log_content, function(x) { str_match(x, 'Solution size: (.*)')[, 2] })),
           fails = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Failed: (.*) \\(approx\\)')[, 2] }))),
           init = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent in enumerator init: (.*) \\(approx\\)')[, 2] }))),
           enum = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent in enumerator: (.*) \\(approx\\)')[, 2] }))),
           eval = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent in evaluation & testing: (.*) \\(approx\\)')[, 2] }))),
           block = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent blocking cubes/programs: (.*) \\(approx\\)')[, 2] }))),
           blocked = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Blocked programs: (.*) \\(.*\\) \\(approx\\)')[, 2] }))),
           cubes = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Generated cubes: (.*)')[, 2] }))),
           blocked_cubes = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Blocked cubes: (.*) \\(.*\\)')[, 2] }))),
           attempts = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Attempted programs: (.*) \\(approx\\)')[, 2] }))),
           solution = map(log_content, function(x) { str_match(x, 'Solution found: \\[(.*)\\]')[, 2] }),
           #status = ifelse(is.na(solution), as.character(status), ifelse(status == -2, 2, as.character(status))),  # *sighs* factors are weird
           #solved = ifelse(is.na(solution), solved, ifelse(status == -2, T, solved)),  # *sighs* factors are weird
           equivalent_p = cpu / real) %>%
    filter(!skewed) %>%
    select(-log, -log_content, -log_suff, -several_runs, -skewed) %>%
    left_join(instance_info)
  if (file.exists(paste0('fuzzy/', file, '.csv'))) {
    result_fuzzy <- read_csv(paste0('fuzzy/', file, '.csv'), col_types = cols(.default='?', base_eq = 'c'))
    result <- left_join(result, result_fuzzy) %>%
      mutate(fuzzy = ifelse(is.na(base_eq), -1, ifelse(base_eq == '-1', 3, ifelse(base_eq == 'False', 1, ifelse(fuzzy_eq == fuzzies, 0, 2))))) %>%
      mutate(fuzzy = factor(fuzzy, fuzzy_levels, fuzzy_meanings))
  }
  gc()
  result
}

load_result_file_median <- function(file, timelimit = 600) {
  load_result_file(file) %>%
    group_by(name) %>%
    mutate(solve_count = sum(ifelse(solved, 1, 0)),
           med_solved = solve_count >= n() / 2,
           status = as.numeric(as.character(status))) %>%
    ungroup() %>%
    group_by(name, benchmark) %>%
    summarise(real_sd = sd(real),
              real_min = min(real),
              real_max = max(real),
              real = ifelse(med_solved, median(real), timelimit),
              cpu = ifelse(med_solved, median(cpu), timelimit),
              ram = median(ram),
              solveds = sum(ifelse(solved, 1L, 0L)),
              status = factor(ifelse(med_solved, status %>%
                .[. != -1 & . != -2 & . != 1 & . != 5] %>%
                getmode, status %>%
                                       .[. == -1 | . == -2 | . == 1 | . == 5] %>%
                                       getmode), levels = status_levels, exclude = NULL),
              timeout = status == -1,
              memout = status == -2,
              fails = median(fails),
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

test_filter <- c('scythe/demo-example', 'scythe/sqlsynthesizer', 'scythe/test-examples', 'scythe/newposts', 'scythe/dev-set', 'outsystems', 'leetcode')
{
  squares <- load_result_squares('squares')
  squares_2 <- load_result_squares('squares_2')
  scythe <- load_result_squares('scythe')
  scythe_2 <- load_result_squares('scythe_2')
  scythe_3 <- load_result_squares('scythe_3')
  patsql <- load_result_squares('patsql')

  #single <- load_result_file('single')
  #single_np <- load_result_file('single_np')
  #bitenum <- load_result_file('bitenum')
  #bitenum_nobit <- load_result_file('bitenum_nobit')
  #bitenum_nosub <- load_result_file('bitenum_nosub')
  #bitenum_nofd <- load_result_file('bitenum_nofd')

  #sequential1 <- load_result_file('sequential')
  sequential <- load_result_file('sequential_2')
  sequential_3 <- load_result_file('sequential_3')
  sequential_subsume <- load_result_file('sequential_subsume')
  sequential_no_qffd <- load_result_file('sequential_no_qffd')
  sequential_simple_dsl <- load_result_file('sequential_simple_dsl')
  sequential_no_bitvec <- load_result_file('sequential_no_bitvec')

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
  #portfolio1 <- load_result_file('portfolio_1')
  #portfolio2 <- load_result_file('portfolio_2')
  #portfolio3 <- load_result_file('portfolio_3')

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
  #c41_30_0f_c <- load_result_file('cubes41_30_0f')

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

  #c45_16_0f_c_5 <- load_result_file_median('cubes45_16_0f_5')
  #c45_16_0f_c_5b <- load_result_file_best('cubes45_16_0f_5')
  #c45_16_0f_c_5w <- load_result_file_worst('cubes45_16_0f_5')

  #c45_16_0f_c_dsl_5 <- load_result_file_median('cubes45_16_0f_5_dsl')
  #c45_16_0f_c_dsl_5b <- load_result_file_best('cubes45_16_0f_5_dsl')
  #c45_16_0f_c_dsl_5w <- load_result_file_worst('cubes45_16_0f_5_dsl')

  #c46_16_0f_c_cube_5 <- load_result_file_median('cubes46_16_0f_5_cube')
  #c46_16_0f_c_cube_5b <- load_result_file_best('cubes46_16_0f_5_cube')
  #c46_16_0f_c_cube_5w <- load_result_file_worst('cubes46_16_0f_5_cube')

  #c47_16_0f_c_5 <- load_result_file_median('cubes47_16_0f_5')
  #c47_16_0f_c_5b <- load_result_file_best('cubes47_16_0f_5')
  #c47_16_0f_c_5w <- load_result_file_worst('cubes47_16_0f_5')

  #c47_16_0f_c_spider <- load_result_file_median('cubes47_16_0f_spider')
  #c48_16_0f_c_spider <- load_result_file_median('cubes48_16_0f_spider')
  #c49_16_0f_c_spider <- load_result_file_median('cubes49_16_0f_spider')

  #c49_4_0f_c <- load_result_file_median('cubes49_4_0f')
  #c49_8_0f_c <- load_result_file_median('cubes49_8_0f')
  #c49_16_0f_c <- load_result_file_median('cubes49_16_0f_1')

  #c49_16_0f_c_static <- load_result_file_median('cubes49_16_0f_static')
  #c49_16_0f_c_no_split <- load_result_file_median('cubes49_16_0f_no_split')
  #c49_16_0f_c_optimal <- load_result_file_median('cubes49_16_0f_optimal')
  #c49_16_0f_c_no_unsat <- load_result_file_median('cubes49_16_0f_no_unsat')

  #portfolio1 <- load_result_file('portfolio_1')
  portfolio5_4 <- load_result_file('portfolio_5_4')
  portfolio5_8 <- load_result_file('portfolio_5_8')
  portfolio5_16 <- load_result_file('portfolio_5_16')

  c50_4 <- load_result_file('c50_4')
  c50_8 <- load_result_file('c50_8')
  #c50_16_1 <- load_result_file('c50_16')
  c50_16 <- load_result_file('c50_16_2')
  #c50_16_optimal <- load_result_file('c50_16_optimal')
  c50_16_optimal <- load_result_file('c50_16_optimal_2')
  c50_16_static <- load_result_file('c50_16_static')
  c50_16_no_dedu <- load_result_file('c50_16_no_dedu')
  c50_16_no_split <- load_result_file('c50_16_no_split')

  determ <- load_result_file_median('determinism')
  determ2 <- load_result_file_median('determinism_2')
}