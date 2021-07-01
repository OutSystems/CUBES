status_levels <- rev(c(0, 3, 2, 4, 6, -2, -1, 5, 1, 143))
status_meanings <- rev(c('Solved', 'Non-optimal', 'Solved (Just R)', 'Just R Non-optimal', 'Empty output', 'Memout', 'Timeout', 'No solution', 'Fail', 'Scythe ERR'))
status_colors <- rev(c("#57853C", "#296429", "#d79921", "#B4560E", "#59235F", "#4B44CC", "#cc241d", "#653e9c", "#3c3836", '#000000'))

fuzzy_levels <- rev(c(1, 2, 3, 0, 4, -1, -4, -12, -9, -10, -8, -5, -2, -3, -6))
fuzzy_meanings <- rev(c('Possibly Correct', 'Possibly Correct Top 5', 'Possibly Correct Any', 'Incorrect', 'Incorrect by Fuzzing', 'Timeout', 'Fuzzer Error', 'Exec. Error Base', 'Exec. Error Fuzzied', 'No GT', 'Exec. Error GT', 'GT Mismatch', 'No solution', 'No log file', 'No database'))
fuzzy_colors <- rev(c("#57853C", "#085F05", "#052C03", "#B4560E", "#d79921", "#4B44CC", '#5F89B3', '#2D3A87', '#02034F', '#AC4240', '#6E1413', '#400307', '#000000', '#AAAAAA', '#EEEEEE'))

test_filter <- c('scythe/demo-example', 'scythe/sqlsynthesizer', 'scythe/test-examples', 'scythe/newposts', 'scythe/dev-set', 'outsystems', 'leetcode')

instance_info <- read_csv('instances.csv', col_types = cols(
  name = col_character(),
  loc = col_number(),
  .default = '?'
)) %>% mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)))

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

min_non_inf <- function(...) {
  tmp <- min(...)
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
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    filter(!(benchmark %in% test_filter)) %>%
    left_join(instance_info)
  if (file.exists(paste0('fuzzy/', file, '.csv'))) {
    result_fuzzy <- read_csv(paste0('fuzzy/', file, '.csv'), col_types = cols(.default = '?', base_eq = 'c', top_i = 'i'))
    result <- left_join(result, result_fuzzy, by = 'name')
    result <- result %>% mutate(fuzzy = base_eq, fuzzy = base_eq, fuzzy = ifelse(is.na(fuzzy), -1, ifelse(fuzzy == 1, ifelse(fuzzy_eq < fuzzies, 4, 1), fuzzy)),)
    if ('top_i' %in% names(result)) {
      result <- result %>% mutate(fuzzy = ifelse(!is.na(top_i) &
                                                   fuzzy == 1 &
                                                   top_i > 1 &
                                                   top_i <= 5, 2, fuzzy),
                                  fuzzy = ifelse(!is.na(top_i) & fuzzy == 1 & top_i > 5, 3, fuzzy))
    }
    result <- result %>% mutate(fuzzy = factor(fuzzy, fuzzy_levels, fuzzy_meanings))
  }
  gc()
  result
}

load_result_file <- function(file, top_n = F, fuzzies = NULL) {
  result <- read_csv(paste0('data/', file, '.csv'), col_types = cols(.default = '?')) %>%
    mutate(status = factor(status, levels = status_levels)) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    filter(!(benchmark %in% test_filter)) %>%
    group_by(name) %>%
    mutate(run_number = 0:(n() - 1)) %>%
    ungroup() %>%
    mutate(several_runs = any(run_number > 0),
           status = ifelse(is.na(status), 1, as.character(status)),  # *sighs* factors are weird
           status = ifelse(timeout, ifelse(status == 3 | status == 4, as.character(status), -1), as.character(status)),  # *sighs* factors are weird
           status = ifelse(memout, -2, as.character(status)),  # *sighs* factors are weird
           log_suff = paste0('_', run_number),
           log = paste0('data/', file, '/', name, log_suff, '.log'),
           log_content = map(log, function(x) { ifelse(file.exists(x), read_file(x), NA) }),
           # hard_h = unlist(map(log_content, function(x) { str_match(x, '\\[(.*)\\]\\[.*\\]\\[INFO\\] Hard problem!')[, 2] })),
           # loc_reached = unlist(map(log_content, function(x) { max_non_inf(parse_integer(str_match_all(x, 'Enumerator for loc (.*) constructed')[[1]][, 2])) })),
           loc_found = unlist(map(log_content, function(x) { str_match(x, 'Solution size: (.*)')[, 2] })),
           fails = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Failed: (.*) \\(approx\\)')[, 2] }))),
           init = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent in enumerator init: (.*) \\(approx\\)')[, 2] }))),
           enum = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent in enumerator: (.*) \\(approx\\)')[, 2] }))),
           eval = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent in evaluation & testing: (.*) \\(approx\\)')[, 2] }))),
           block = parse_number(unlist(map(log_content, function(x) { str_match(x, 'Total time spent blocking cubes/programs: (.*) \\(approx\\)')[, 2] }))),
           eval_p = eval / (init + enum + eval + block),
           blocked = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Blocked programs: (.*) \\(.*\\) \\(approx\\)')[, 2] }))),
           cubes = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Generated cubes: (.*)')[, 2] }))),
           blocked_cubes = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Blocked cubes: (.*) \\(.*\\)')[, 2] }))),
           attempts = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Attempted programs: (.*) \\(approx\\)')[, 2] }))),
           cache_hits = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Cache hits: (.*) \\(approx\\)')[, 2] }))),
           cache_misses = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Cache misses: (.*) \\(approx\\)')[, 2] }))),
           cache_hit_ratio = cache_hits / (cache_hits + cache_misses),
           solution = map(log_content, function(x) { map(str_match_all(x, '\\[MainProcess\\]\\[INFO\\] Solution found: \\[(.*)\\]'), function(x) { x[, 2] }) }),
           solution = ifelse(solution != '', solution, NA),
           sol_sizes = map(log_content, function(x) { unlist(parse_integer(str_match_all(x, 'Solution size: (.*)')[[1]][, 2])) }),
           min_sol_loc = unlist(map(sol_sizes, function(x) { min_non_inf(x) })),
           max_sol_loc = unlist(map(sol_sizes, function(x) { max_non_inf(x) })),
           solutions = unlist(map(log_content, function(x) { str_count(x, fixed('------------------------------------- R Solution ---------------------------------------')) })),
           status = ifelse(solutions > 0, 0, as.character(status)),  # *sighs* factors are weird
           solution = unlist(map(solution, function(s) { ifelse(length(s) == 0, NA, s[[1]]) })),
           status = ifelse(is.na(solution), as.character(status), ifelse(status == -2, 2, as.character(status))),  # *sighs* factors are weird
           # solved = ifelse(is.na(solution), solved, ifelse(status == -2, T, solved)),  # *sighs* factors are weird
           solved = is_solved_status(status),
           equivalent_p = cpu / real)
  result <- result %>%
    select(-log_content, -log_suff, -several_runs) %>%
    left_join(instance_info)
  if (file.exists(paste0('fuzzy/', file, '.csv'))) {
    result_fuzzy <- read_csv(paste0('fuzzy/', file, '.csv'), col_types = cols(.default = '?', base_eq = 'c', top_i = 'i'))
    result <- left_join(result, result_fuzzy, by = 'name')
  }
  for (f in fuzzies) {
    if (file.exists(paste0('fuzzy/', f, '.csv'))) {
      result_fuzzy <- read_csv(paste0('fuzzy/', f, '.csv'), col_types = cols(.default = '?', base_eq = 'c'))
      result <- left_join(result, result_fuzzy, suffix = c('.o', ''), by = 'name') %>%
        mutate(fuzzies = coalesce(fuzzies, fuzzies.o),
               base_eq = coalesce(base_eq, base_eq.o),
               fuzzy_eq = coalesce(fuzzy_eq, fuzzy_eq.o),
               fuzzy_neq = coalesce(fuzzy_neq, fuzzy_neq.o),
               fuzzy_err = coalesce(fuzzy_err, fuzzy_err.o)) %>%
        select(-fuzzies.o, -base_eq.o, -fuzzy_eq.o, -fuzzy_neq.o, -fuzzy_err.o)
    }
  }
  if ('base_eq' %in% names(result)) {
    result <- result %>% mutate(fuzzy = base_eq, fuzzy = base_eq, fuzzy = ifelse(is.na(fuzzy), -1, ifelse(fuzzy == 1, ifelse(fuzzy_eq < fuzzies, 4, 1), fuzzy)),)
    if ('top_i' %in% names(result)) {
      result <- result %>% mutate(top_i_sol_loc = unlist(map2(sol_sizes, top_i, function(x, y) { ifelse(y >= 1, x[y], NA) })),
                                  fuzzy = ifelse(!is.na(top_i) &
                                                   fuzzy == 1 &
                                                   top_i > 1 &
                                                   top_i <= 5, 2, fuzzy),
                                  fuzzy = ifelse(!is.na(top_i) & fuzzy == 1 & top_i > 5, 3, fuzzy))
    }
    result <- result %>% mutate(fuzzy = factor(fuzzy, fuzzy_levels, fuzzy_meanings))
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

load_fuzzy_results <- function(run) {
  read_csv(paste0('fuzzy/', run, '.csv'), col_types = cols(.default = '?', base_eq = 'c', top_i = 'i')) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)),
           solved = T, status = '0',
           fuzzy = base_eq, fuzzy = ifelse(is.na(fuzzy), -1, ifelse(fuzzy == 1, ifelse(fuzzy_eq < fuzzies, 4, 1), fuzzy)),
           fuzzy = ifelse(!is.na(top_i) &
                            fuzzy == 1 &
                            top_i > 1 &
                            top_i <= 5, 2, fuzzy),
           fuzzy = ifelse(!is.na(top_i) & fuzzy == 1 & top_i > 5, 3, fuzzy),
           fuzzy = factor(fuzzy, fuzzy_levels, fuzzy_meanings),
           status = as.character(ifelse(fuzzy != 'No solution', 0, -1))) %>%
    filter(fuzzy != 'No log file')
}

{
  # squares <- load_result_squares('squares')
  # squares <- load_result_squares('squares_2')
  # scythe <- load_result_squares('scythe')
  # scythe_2 <- load_result_squares('scythe_2')
  # scythe <- load_result_squares('scythe_3')
  # patsql <- load_result_squares('patsql')
  # patsql_2 <- load_result_squares('patsql_2')
  patsql_3 <- load_result_squares('patsql_3')
  patsql_3_500 <- semi_join(patsql_3, c59_16, by = 'name')
  patsql_4 <- load_result_squares('patsql_4')
  patsql_4_500 <- semi_join(patsql_4, c59_16, by = 'name')


  #single <- load_result_file('single')
  #single_np <- load_result_file('single_np')
  #bitenum <- load_result_file('bitenum')
  #bitenum_nobit <- load_result_file('bitenum_nobit')
  #bitenum_nosub <- load_result_file('bitenum_nosub')
  #bitenum_nofd <- load_result_file('bitenum_nofd')

  #sequential1 <- load_result_file('sequential')
  # sequential <- load_result_file('sequential_2')
  # sequential_3 <- load_result_file('sequential_3')
  # sequential <- load_result_file('sequential_semantics')
  # sequential_top5 <- load_result_file('sequential_sem_top_5_2')
  # sequential_subsume <- load_result_file('sequential_subsume')
  # sequential_no_qffd <- load_result_file('sequential_no_qffd')
  # sequential_simple_dsl <- load_result_file('sequential_simple_dsl')
  # sequential_no_bitvec <- load_result_file('sequential_no_bitvec')

  ratsql <- load_fuzzy_results('ratsql')
  smbop <- load_fuzzy_results('smbop')

  db2csv_1 <- load_result_file('db2csv_dev_basic_1')
  db2csv_2 <- load_result_file('db2csv_dev_basic_2')
  db2csv_3 <- load_result_file('db2csv_dev_basic_3')
  db2csv_4 <- load_result_file('db2csv_dev_basic_4')
  db2csv_5 <- load_result_file('db2csv_dev_basic_5')
  db2csv_6 <- load_result_file('db2csv_dev_basic_6')
  db2csv_7 <- load_result_file('db2csv_dev_basic_7')
  db2csv_8 <- load_result_file('db2csv_dev_basic_8')
  db2csv_9 <- load_result_file('db2csv_dev_basic_9')
  db2csv_10 <- load_result_file('db2csv_dev_basic_10')
  db2csv_2_c20 <- load_result_file('db2csv_dev_basic_2_c20')
  db2csv_3_c20 <- load_result_file('db2csv_dev_basic_3_c20')
  db2csv_9_c20 <- load_result_file('db2csv_dev_basic_9_c20')
  db2csv_beam_1 <- load_result_file('db2csv_dev_beam_1')
  db2csv_beam_2 <- load_result_file('db2csv_dev_beam_2')
  db2csv_beam_3 <- load_result_file('db2csv_dev_beam_3')
  db2csv_beam_10 <- load_result_file('db2csv_dev_beam_10')
  db2csv_beam_11 <- load_result_file('db2csv_dev_beam_11')
  db2csv_beam_12_p51 <- load_result_file('db2csv_dev_beam_12_p51')
  db2csv_beam_12_p75 <- load_result_file('db2csv_dev_beam_12')
  db2csv_beam_12_p9 <- load_result_file('db2csv_dev_beam_12_p9')
  # db2csv_beam_12_p9_max_filter1 <- load_result_file('db2csv_dev_beam_12_p9_max_filter-1')
  # db2csv_beam_12_p9_max_join1 <- load_result_file('db2csv_dev_beam_12_p9_max_join-1')
  # db2csv_beam_12_p9_no_antijoin <- load_result_file('db2csv_dev_beam_12_p9_no_anti-join')
  # db2csv_beam_12_p9_no_crossjoin <- load_result_file('db2csv_dev_beam_12_p9_no_cross-join')
  # db2csv_beam_12_p9_no_leftjoin <- load_result_file('db2csv_dev_beam_12_p9_no_left-join')
  # db2csv_beam_12_p9_no_mutate <- load_result_file('db2csv_dev_beam_12_p9_no_mutate')
  # db2csv_beam_12_p9_no_naturaljoin4 <- load_result_file('db2csv_dev_beam_12_p9_no_natural-join4')
  # db2csv_beam_12_p9_no_semijoin <- load_result_file('db2csv_dev_beam_12_p9_no_semi-join')
  # db2csv_beam_12_p9_no_union <- load_result_file('db2csv_dev_beam_12_p9_no_union')
  # db2csv_beam_12_p9_no_intersect <- load_result_file('db2csv_dev_beam_12_p9_no_intersect')
  db2csv_beam_12_p9_combo <- load_result_file('db2csv_dev_beam_12_p9_max_filter1_no_naturaljoin4_leftjoin_crossjoin')
  db2csv_beam_13_combo <- load_result_file('db2csv_dev_beam_13_p9_max_filter1_no_naturaljoin4_leftjoin_crossjoin')
  db2csv_beam_15_combo <- load_result_file('db2csv_dev_beam_15_combo')
  db2csv_beam_16_combo <- load_result_file('db2csv_dev_beam_16_combo')
  db2csv_beam_2_c20 <- load_result_file('db2csv_dev_beam_2_c20')
  db2csv_beam_3_c20 <- load_result_file('db2csv_dev_beam_3_c20')
  db2csv_beam_12_p9_combo_c20 <- load_result_file('db2csv_dev_beam_12_p9_max_filter1_no_naturaljoin4_leftjoin_crossjoin_c20')
  db2csv_beam_12_combo_c20 <- load_result_file('db2csv_dev_beam_12_p9_max_filter1_no_naturaljoin4_leftjoin_crossjoin_c20_no_split')
  db2csv_beam_14_combo_c20 <- load_result_file('db2csv_dev_beam_14_combo_c20')
  db2csv_ratsql_17_combo_c20 <- load_result_file('db2csv_beam_ratsql_17_c20')
  db2csv_ratsql_17_combo_c20_all300 <- load_result_file('db2csv_beam_ratsql_17_c20_all300')
  db2csv_smbop_17_combo_c20 <- load_result_file('db2csv_beam_smbop_17_c20')
  db2csv_smbop_17_combo_c20_all300 <- load_result_file('db2csv_beam_smbop_17_c20_all300')
  db2csv_smbop_18_combo_c20_all300 <- load_result_file('db2csv_beam_smbop_18_c20_all300')
  db2csv_beam_no_conserv <- load_result_file('db2csv_dev_beam_1_no_conserv')


  # c50_4 <- load_result_file('c50_4')
  # c50_8 <- load_result_file('c50_8')
  #c50_16_1 <- load_result_file('c50_16')
  c50_16 <- load_result_file('c50_16_2')
  c51_16 <- load_result_file('c51_16')
  c52_16 <- load_result_file('c52_16_2')
  c52_16_all600 <- load_result_file('c52_16')
  c53_16 <- load_result_file('c53_16')
  # c53_16_all600 <- load_result_file('c53_16_all600')
  c54_16 <- load_result_file('c54_16')
  c55_16 <- load_result_file('c55_16')
  c56_16 <- load_result_file('c56_16')
  c57_16 <- load_result_file('c57_16')
  c57_16_no_cache <- load_result_file('c57_16_no-cache')
  c58_16 <- load_result_file('c58_16')
  c59_16 <- load_result_file('c59_16')
  c59_16_all600 <- load_result_file('c59_16_all600')
  c60_16 <- load_result_file('c60_16')
  c61_16 <- load_result_file('c61_16')
  c62_16 <- load_result_file('c62_16')
  # c50_16_top5 <- load_result_file('c50_16_top5')
  #c50_16_optimal <- load_result_file('c50_16_optimal')
  # c50_16_optimal <- load_result_file('c50_16_optimal_2')
  # c50_16_static <- load_result_file('c50_16_static')
  # c50_16_no_dedu <- load_result_file('c50_16_no_dedu')
  # c50_16_no_split <- load_result_file('c50_16_no_split')

  # determ <- load_result_file_median('determinism')
  # determ2 <- load_result_file_median('determinism_2')

  # determ5 <- load_result_file('determinism_5')
  # determ5 <- load_result_file_median('determinism_5')
}