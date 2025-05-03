source("loading.R")

scythe <- load_result_squares('scythe_4')
scythe_60 <- load_result_squares('scythe_5_60')
scythe_top100 <- load_result_squares('scythe_5_top100')
patsql <- load_result_squares('patsql_8', remove_empties = T)
patsql_5 <- load_result_squares('patsql_8_5', remove_empties = T)
squares <- load_result_squares('squares_3') %>% semi_join(scythe, by = 'name')
libra <- load_result_squares('libra_1', is_libra=T)
seq <- load_result_file('sequential_5', use_log_suff = F) %>% semi_join(scythe, by = 'name')

seq_600 <- load_result_file('sequential_6_all600', use_log_suff = F) %>% semi_join(scythe, by = 'name')
seq_600_dis <- load_result_file('sequential_6_all600', use_log_suff = F, dis_fuzz = T) %>% semi_join(scythe, by = 'name')
seq_600_dis_data <- read_csv("fuzzy/sequential_6_all600_dis.csv")  %>% semi_join(scythe, by = 'name') %>% mutate(final_queries_n = str_count(final_queries, '", "') +
  str_count(final_queries, "', '") +
  1)

cubes_4 <- load_result_file('c62_4_full') %>% semi_join(scythe, by = 'name')
cubes_8 <- load_result_file('c62_8_full') %>% semi_join(scythe, by = 'name')
cubes_16 <- load_result_file('c62_16_full') %>%
  filter(!str_detect(benchmark, 'db2csv')) %>%
  semi_join(scythe, by = 'name')
cubes_32 <- load_result_file('major_rev_j32') %>%
  filter(!str_detect(benchmark, 'db2csv')) %>%
  semi_join(scythe, by = 'name')

cubes_16_no_split <- load_result_file('c62_16_full_no_split') %>% semi_join(scythe, by = 'name')
cubes_16_no_bitvec <- load_result_file('c62_16_full_no_bitvec') %>% semi_join(scythe, by = 'name')
cubes_16_random <- load_result_file('c62_16_full_random') %>% semi_join(scythe, by = 'name')
cubes_16_no_extra_score_update <- load_result_file('major_rev_no_extra_score_update') %>% semi_join(scythe, by = 'name')

cubes_32_no_split <- load_result_file('major_rev_j32_no_split') %>% semi_join(scythe, by = 'name')
cubes_32_no_bitvec <- load_result_file('major_rev_j32_no_bitvec') %>% semi_join(scythe, by = 'name')
cubes_32_random <- load_result_file('major_rev_j32_random') %>% semi_join(scythe, by = 'name')


cubes_16_600 <- load_result_file('c62_16_all600_full') %>% semi_join(scythe, by = 'name')
cubes_16_600_old <- cubes_16_600
cubes_16_600_dis <- load_result_file('c62_16_all600_full', dis_fuzz = T) %>% semi_join(scythe, by = 'name')
cubes_16_600_dis_old <- cubes_16_600_dis
cubes_16_600_dis_data <- read_csv('fuzzy/c62_16_all600_full_dis.csv')%>% semi_join(scythe, by = 'name') %>% mutate(final_queries_n = str_count(final_queries, '", "') +
  str_count(final_queries, "', '") +
  1)
cubes_16_600_dis_data_old <- cubes_16_600_dis_data

cubes_16_n10 <- load_result_file_median('c62_16_n10')

# cubes_60_o <- load_result_file('sequential_5_all60', dis_fuzz = T) %>% semi_join(scythe, by = 'name')
# cubes_600_o <- load_result_file('sequential_5_all600', use_log_stuff = F) %>% semi_join(scythe, by = 'name')

# s62 <- load_result_file('s62', use_log_stuff = F) %>% semi_join(scythe, by = 'name')
#
#

#   c62_16_full_static <- load_result_file('c62_16_full_static') %>% semi_join(scythe, by = 'name')

# #   c62_16_n10 <- load_result_file_median('c62_16_n10')

# # c50_16_top5 <- load_result_file('c50_16_top5')
# # c50_16_optimal <- load_result_file('c50_16_optimal')
# # c50_16_optimal <- load_result_file('c50_16_optimal_2')
# # c50_16_static <- load_result_file('c50_16_static')
# # c50_16_no_dedu <- load_result_file('c50_16_no_dedu')
# # c50_16_no_split <- load_result_file('c50_16_no_split')
#
# # determ <- load_result_file_median('determinism')
# # determ2 <- load_result_file_median('determinism_2')
#
# # determ5 <- load_result_file('determinism_5')
# # determ5 <- load_result_file_median('determinism_5')
#
# squares_500 <- semi_join(squares, c62_16, by = 'name')
# scythe_500 <- semi_join(scythe, c62_16, by = 'name')
# patsql_500 <- semi_join(patsql, c62_16, by = 'name')
