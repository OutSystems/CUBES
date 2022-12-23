status_levels <- rev(c(0, 3, 2, 4, 6, -2, -1, 5, 1, 143))
status_meanings <- rev(c('Solved', 'Non-optimal', 'Solved (Just R)', 'Just R Non-optimal', 'Empty output', 'Memout', 'Timeout', 'No solution', 'Fail', 'Scythe ERR'))
# status_colors <- rev(c("#57853C", "#296429", "#d79921", "#B4560E", "#59235F", "#4B44CC", "#cc241d", "#653e9c", "#3c3836", '#000000'))
status_colors <- rev(c("#66a61e", "#296429", "#d79921", "#B4560E", "#59235F", "#7570B3", "#D95F02", "#653e9c", "#666666", '#000000'))

fuzzy_combo <- rev(list(
  list(5, 'Pre Possibly Correct', "#21afc2"),
  list(6, 'Pre Possibly Correct Top 5', "#2182c2"),
  list(7, 'Pre Possibly Correct Any', "#215fc2"),
  list(1, 'Possibly Correct', "#57853C"),
  list(2, 'Possibly Correct Top 5', "#085F05"),
  list(3, 'Possibly Correct Any', "#052C03"),
  list(8, 'Pre Incorrect by Fuzzing', "#b221c2"),
  list(4, 'Incorrect by Fuzzing', "#d79921"),
  list(0, 'Incorrect', "#B4560E"),
  list(-1, 'Timeout', "#4B44CC"),
  list(-4, 'Fuzzer Error', '#5F89B3'),
  list(-12, 'Exec. Error Base', '#2D3A87'),
  list(-9, 'Exec. Error Fuzzied', '#02034F'),
  list(-10, 'No GT', '#AC4240'),
  list(-8, 'Exec. Error GT', '#6E1413'),
  list(-5, 'GT Mismatch', '#400307'),
  list(-2, 'No solution', '#000000'),
  list(-3, 'No log file', '#AAAAAA'),
  list(-6, 'No database', '#EEEEEE')
))

fuzzy_levels <- unlist(map(fuzzy_combo, function(x) { x[1] }))
fuzzy_meanings <- unlist(map(fuzzy_combo, function(x) { x[2] }))
fuzzy_colors <- unlist(map(fuzzy_combo, function(x) { x[3] }))

test_filter <- c('scythe/demo-example', 'scythe/sqlsynthesizer', 'scythe/test-examples', 'scythe/newposts', 'scythe/dev-set', 'outsystems', 'leetcode', '55-tests')
instance_blocklist <- c('textbook/30')

instance_info <- read_csv('instances.csv', col_types = cols(
  name = col_character(),
  loc = col_number(),
  .default = '?'
)) %>% mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)),
              benchmark = ifelse(str_detect(benchmark, 'spider'), 'spider', benchmark),
              sql_hardness = factor(sql_hardness, levels = c('easy', 'medium', 'hard', 'extra', NA)))

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
  tmp <- suppressWarnings(max(...))
  if (is.finite(tmp)) {
    tmp
  } else {
    NA
  }
}

min_non_inf <- function(...) {
  tmp <- suppressWarnings(min(...))
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


load_result_squares <- function(file, dis_fuzz = F, remove_empties=F) {
  result <- read_csv(paste0('data/', file, '.csv'), col_types = cols(.default = '?')) %>%
    mutate(status = factor(status, levels = status_levels)) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2)),
           status = ifelse(status == 143, 1, as.character(status)),
           status = ifelse(timeout, -1, as.character(status)))
  if ('memout' %in% names(result)) {
    result <- result %>% mutate(status = ifelse(memout, -2, as.character(status)))
  }
  result <- result %>%
    mutate(solved = is_solved_status(status)) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    mutate(benchmark = ifelse(str_detect(benchmark, 'spider'), 'spider', benchmark)) %>%
    filter(!(benchmark %in% test_filter)) %>%
    filter(!(name %in% instance_blocklist)) %>%
    left_join(instance_info) %>%
    mutate(log = paste0('data/', file, '/', name, '.log'),
           log_content = map(log, function(x) { ifelse(file.exists(x), read_file(x), NA) }))

  if (remove_empties) {
    result <- result %>%
      mutate(status = ifelse(log_content == '' & solved, 5, status),
             solved = is_solved_status(status))
  }

  result <- result %>%
    select(-log_content)
  if (file.exists(paste0('fuzzy/', file, ifelse(dis_fuzz, '_dis_fuzz', ''), '.csv'))) {
    result_fuzzy <- read_csv(paste0('fuzzy/', file, ifelse(dis_fuzz, '_dis_fuzz', ''), '.csv'), col_types = cols(.default = '?', base_eq = 'c', top_i = 'i'))
    result <- left_join(result, result_fuzzy, by = 'name')
  }
  if ('base_eq' %in% names(result)) {
    result <- result %>% mutate(fuzzy = base_eq, fuzzy = base_eq, fuzzy = ifelse(is.na(fuzzy), -1, ifelse(fuzzy == 1, ifelse(fuzzy_eq < fuzzies, 4, 1), ifelse(fuzzy == 2, ifelse(fuzzy_eq < fuzzies, 8, 5), fuzzy))),)
    if ('top_i' %in% names(result)) {
      result <- result %>% mutate(fuzzy = ifelse(!is.na(top_i) &
                                                   fuzzy == 1 &
                                                   top_i > 1 &
                                                   top_i <= 5, 2, ifelse(!is.na(top_i) &
                                                                           fuzzy == 5 &
                                                                           top_i > 1 &
                                                                           top_i <= 5, 6, fuzzy)),
                                  fuzzy = ifelse(!is.na(top_i) & fuzzy == 1 & top_i > 5, 3, ifelse(!is.na(top_i) & fuzzy == 5 & top_i > 5, 7, fuzzy)))
    }
    result <- result %>% mutate(fuzzy = factor(fuzzy, fuzzy_levels, fuzzy_meanings))
  }
  gc()
  result
}

load_result_file <- function(file, top_n = F, fuzzies = NULL, dis_fuzz = F, use_log_suff = T) {
  result <- read_csv(paste0('data/', file, '.csv'), col_types = cols(.default = '?')) %>%
    mutate(status = factor(status, levels = status_levels)) %>%
    mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
    mutate(benchmark = ifelse(str_detect(benchmark, 'spider'), 'spider', benchmark)) %>%
    filter(!(benchmark %in% test_filter)) %>%
    filter(!(name %in% instance_blocklist)) %>%
    group_by(name) %>%
    mutate(run_number = 0:(n() - 1)) %>%
    ungroup() %>%
    mutate(several_runs = any(run_number > 0),
           status = ifelse(is.na(status), 1, as.character(status)),  # *sighs* factors are weird
           status = ifelse(timeout, ifelse(status == 3 | status == 4, as.character(status), -1), as.character(status)),  # *sighs* factors are weird
           status = ifelse(memout, -2, as.character(status)),  # *sighs* factors are weird,
           use_log_suff = use_log_suff,
           log_suff = ifelse(use_log_suff, paste0('_', run_number), ''),
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
           init_p = init / cpu,
           eval_p = eval / cpu,
           enum_p = enum / cpu,
           block_p = block / cpu,
           blocked = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Blocked programs: (.*) \\(.*\\) \\(approx\\)')[, 2] }))),
           cubes = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Generated cubes: (.*)')[, 2] }))),
           blocked_cubes = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Blocked cubes: (.*) \\(.*\\)')[, 2] }))),
           attempts = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Attempted programs: (.*) \\(approx\\)')[, 2] }))),
           cache_hits = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Cache hits: (.*) \\(approx\\)')[, 2] }))),
           cache_misses = parse_integer(unlist(map(log_content, function(x) { str_match(x, 'Cache misses: (.*) \\(approx\\)')[, 2] }))),
           empties_percentage = parse_double(unlist(map(log_content, function(x) { str_match(x, 'Empty outputs: \\d+ \\((.*)%\\) \\(approx\\)')[, 2] }))),
           cache_hit_ratio = cache_hits / (cache_hits + cache_misses),
           solution = map(log_content, function(x) { map(str_match_all(x, '\\[MainProcess\\]\\[INFO\\] Solution found: (.*)'), function(x) { x[, 2] }) }),
           solution = ifelse(solution != '', solution, NA),
           sol_sizes = map(log_content, function(x) { unlist(parse_integer(str_match_all(x, 'Solution size: (.*)')[[1]][, 2])) }),
           min_sol_loc = unlist(map(sol_sizes, function(x) { min_non_inf(x) })),
           max_sol_loc = unlist(map(sol_sizes, function(x) { max_non_inf(x) })),
           solutions = unlist(map(log_content, function(x) { str_count(x, fixed('------------------------------------- R Solution ---------------------------------------')) })),
           sql_solutions = unlist(map(log_content, function(x) { str_count(x, fixed('+++++++++++++++++++++++++++++++++++++ SQL Solution +++++++++++++++++++++++++++++++++++++')) })),
           timeout_reached = unlist(map(log_content, function(x) { str_detect(x, fixed('Timeout reached')) })),
           status = ifelse(solutions == 0 & timeout_reached, -1, as.character(status)),  # *sighs* factors are weird
           status = ifelse(solutions > 0, 0, as.character(status)),  # *sighs* factors are weird
           solution = unlist(map(solution, function(s) { ifelse(length(s) == 0, NA, s[[1]]) })),
           status = ifelse(is.na(solution), as.character(status), ifelse(status == -2, 2, as.character(status))),  # *sighs* factors are weird
           # solved = ifelse(is.na(solution), solved, ifelse(status == -2, T, solved)),  # *sighs* factors are weird
           solved = is_solved_status(status),
           equivalent_p = cpu / real)
  result <- result %>%
    select(-log_content, -log_suff, -several_runs) %>%
    left_join(instance_info)
  if (file.exists(paste0('fuzzy/', file, ifelse(dis_fuzz, '_dis_fuzz', ''), '.csv'))) {
    print("A")
    result_fuzzy <- read_csv(paste0('fuzzy/', file, ifelse(dis_fuzz, '_dis_fuzz', ''), '.csv'), col_types = cols(.default = '?', base_eq = 'c', top_i = 'i'))
    print("B")
    result <- left_join(result, result_fuzzy, by = 'name')
    print("C")
  }
  for (f in fuzzies) {
    if (file.exists(paste0('fuzzy/', f, '.csv'))) {
      result_fuzzy <- read_csv(paste0('fuzzy/', f, '.csv'), col_types = cols(.default = '?', base_eq = 'c'))
      result <- left_join(result, result_fuzzy, suffix = c('.o', ''), by = 'name') %>%
        mutate(fuzzies = coalesce(fuzzies, fuzzies.o),
               base_eq = coalesce(base_eq, base_eq.o),
               fuzzy_eq = coalesce(fuzzy_eq, fuzzy_eq.o),
               fuzzy_neq = coalesce(fuzzy_neq, fuzzy_neq.o),
               fuzzy_err = coalesce(fuzzy_err, fuzzy_err.o)
        ) %>%
        select(-fuzzies.o, -base_eq.o, -fuzzy_eq.o, -fuzzy_neq.o, -fuzzy_err.o)
    }
  }
  if ('base_eq' %in% names(result)) {
    result <- result %>% mutate(fuzzy = base_eq, fuzzy = base_eq, fuzzy = ifelse(is.na(fuzzy), -1, ifelse(fuzzy == 1, ifelse(fuzzy_eq < fuzzies, 4, 1), ifelse(fuzzy == 2, ifelse(fuzzy_eq < fuzzies, 8, 5), fuzzy))),)
    if ('top_i' %in% names(result)) {
      result <- result %>% mutate(top_i_sol_loc = unlist(map2(sol_sizes, top_i, function(x, y) { ifelse(y >= 1, x[y], NA) })),
                                  fuzzy = ifelse(!is.na(top_i) &
                                                   fuzzy == 1 &
                                                   top_i > 1 &
                                                   top_i <= 5, 2, ifelse(!is.na(top_i) &
                                                                           fuzzy == 5 &
                                                                           top_i > 1 &
                                                                           top_i <= 5, 6, fuzzy)),
                                  fuzzy = ifelse(!is.na(top_i) & fuzzy == 1 & top_i > 5, 3, ifelse(!is.na(top_i) & fuzzy == 5 & top_i > 5, 7, fuzzy)))
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
           fuzzy = base_eq, fuzzy = base_eq, fuzzy = ifelse(is.na(fuzzy), -1, ifelse(fuzzy == 1, ifelse(fuzzy_eq < fuzzies, 4, 1), ifelse(fuzzy == 2, ifelse(fuzzy_eq < fuzzies, 8, 5), fuzzy))),
           fuzzy = ifelse(!is.na(top_i) &
                            fuzzy == 1 &
                            top_i > 1 &
                            top_i <= 5, 2, ifelse(!is.na(top_i) &
                                                    fuzzy == 5 &
                                                    top_i > 1 &
                                                    top_i <= 5, 6, fuzzy)),
           fuzzy = ifelse(!is.na(top_i) & fuzzy == 1 & top_i > 5, 3, ifelse(!is.na(top_i) & fuzzy == 5 & top_i > 5, 7, fuzzy)),
           fuzzy = factor(fuzzy, fuzzy_levels, fuzzy_meanings),
           status = as.character(ifelse(fuzzy != 'No solution', 0, -1))) %>%
    filter(fuzzy != 'No log file')
}
