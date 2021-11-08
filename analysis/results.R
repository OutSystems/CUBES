library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)
library(scales)
library(RColorBrewer)
library(readr)
library(tikzDevice)
library(ggforce)
library(purrr)
library(xtable)
library(forcats)
# library(ggpattern)

setwd('analysis')

arial <- F

if (arial) {
  options(tikzDefaultEngine = 'xetex',
          tikzXelatexPackages = c(
            "\\usepackage{tikz}\n",
            "\\usepackage[active,tightpage,xetex]{preview}\n",
            "\\usepackage{fontspec,xunicode}\n",
            "\\PreviewEnvironment{pgfpicture}\n",
            "\\setlength\\PreviewBorder{0pt}\n",
            "\\setsansfont{Arial}\n",
            "\\renewcommand{\\familydefault}{\\sfdefault}\n"
          ),
          tikzMetricsDictionary = './metrics_cache_arial',
          standAlone = T)
  textwidth <- 6.30045
  my_theme <- theme_bw() + theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 10), strip.text.x = element_text(size = 9))
} else {
  options(tikzDefaultEngine = 'xetex',
          tikzXelatexPackages = c(
            "\\usepackage{tikz}\n",
            "\\usepackage[active,tightpage,xetex]{preview}\n",
            "\\usepackage{fontspec,xunicode}\n",
            "\\PreviewEnvironment{pgfpicture}\n",
            "\\setlength\\PreviewBorder{0pt}\n"
          ),
          tikzMetricsDictionary = './metrics_cache',
          standAlone = T)
  textwidth <- 3.34
  my_theme <- theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank(), text = element_text(size = 9), strip.text.x = element_text(size = 8))
}

source('loading.R')
source('plots.R')
source('tables.R')

bars(use_vbs = F, ratsql = ratsql, smbop = smbop,
     smbop17_combo_c20_all300 = db2csv_smbop_17_combo_c20_all300,
     smbop18_combo_c20_all300 = db2csv_smbop_18_combo_c20_all300,
     smbop20_combo_c20_all300 = db2csv_smbop_20_combo_c20_all300,
     smbop21_combo_c21_all300 = db2csv_smbop_21_combo_c20_all300)

invsolved(ratsql = ratsql, smbop = smbop,
          smbop17_combo_c20_all300 = db2csv_smbop_17_combo_c20_all300,
          smbop18_combo_c20_all300 = db2csv_smbop_18_combo_c20_all300,
          smbop20_combo_c20_all300 = db2csv_smbop_20_combo_c20_all300,
          smbop21_combo_c21_all300 = db2csv_smbop_21_combo_c20_all300)

scatter(smbop20_combo_c20_all300 = db2csv_smbop_20_combo_c20_all300,
        smbop21_combo_c20_all300 = db2csv_smbop_21_combo_c20_all300)

plot_fuzzy(drop_empties = F, refactor = T, drop_nones = F, fill_bars = F,
           'RATSQL' = ratsql,
           'CUBES (RATSQL)' = db2csv_ratsql_21_combo_c20_all300,
           'SmBoP' = smbop,
           'CUBES (SmBoP)' = db2csv_smbop_21_combo_c20_all300)

plot_fuzzy(drop_empties = F, refactor = T, drop_nones = F, fill_bars = F,
           'RATSQL' = ratsql,
           'CUBES (RATSQL)' = db2csv_ratsql_21_combo_c20_all300,
           'MUATATE (RATSQL)' = mutate_ratsql,
           'SmBoP' = smbop,
           'CUBES (SmBoP)' = db2csv_smbop_21_combo_c20_all300,
           'MUTATE (SmBoP)' = mutate_smbop)

plot_fuzzy(drop_empties = F, refactor = F, drop_nones = F, fill_bars = F,
           'RATSQL' = ratsql,
           'SmBoP' = smbop)

ratsql %>% group_by(fuzzy) %>% count()

ratsql %>% group_by(fuzzy) %>% count()
smbop %>% group_by(fuzzy) %>% count()

a <- inner_join(db2csv_ratsql_21_combo_c20_all300, mutate_smbop, by='name') %>% filter(str_starts(fuzzy.y, 'Pre') & !str_starts(fuzzy.x, 'Pre'))


a <- inner_join(db2csv_smbop_20_combo_c20_all300, db2csv_smbop_21_combo_c20_all300, by = 'name') %>% filter((fuzzy.x == 'Possibly Correct' |
  fuzzy.x == 'Possibly Correct Top 5' |
  fuzzy.x == 'Possibly Correct Any') &
                                                                                                              fuzzy.y != 'Possibly Correct' &
                                                                                                              fuzzy.y != 'Possibly Correct Top 5' &
                                                                                                              fuzzy.y != 'Possibly Correct Any')
a <- inner_join(smbop, db2csv_smbop_20_combo_c20_all300, by = 'name') %>% filter((fuzzy.x == 'Possibly Correct' |
  fuzzy.x == 'Possibly Correct Top 5' |
  fuzzy.x == 'Possibly Correct Any') &
                                                                                   fuzzy.y != 'Possibly Correct' &
                                                                                   fuzzy.y != 'Possibly Correct Top 5' &
                                                                                   fuzzy.y != 'Possibly Correct Any')
b <- a %>%
  group_by(fuzzy.x, status.y, fuzzy.y) %>%
  summarise(n = n())

plot_pdf("d_fuzzy", 1.5, 1, plot_fuzzy(drop_empties = T, refactor = T, drop_nones = T, 'RATSQL' = ratsql, 'SmBoP' = smbop,
                                       # 'CUBES (RATSQL)' = db2csv_ratsql_21_combo_c20_all300,
                                       # 'CUBES (SmBoP)' = db2csv_smbop_21_combo_c20_all300,
                                       'RATSQL + CUBES' = vbs(ratsql = ratsql, ratsql21_combo_c21_all300 = db2csv_ratsql_21_combo_c20_all300),
                                       'SmBoP + CUBES' = vbs(smbop = smbop, smbop21_combo_c21_all300 = db2csv_smbop_21_combo_c20_all300)))


beam_acc <- function(use_vbs = F, exclude = NULL, ...) {
  tries <- list(...)
  data <- bind_rows(tries, .id = 'try')
  if (use_vbs) {
    data <- data %>% bind_rows(vbs(...))
  }
  data <- data %>%
    filter(!(benchmark %in% exclude)) %>%
    arrange(top_i) %>%
    group_by(try) %>%
    mutate(tmp = 1,
           val = cumsum(tmp) / n_distinct(name)) %>%
    ungroup()
  ggplot(data, aes(y = val, x = top_i, color = try, group = try, shape = try)) +
    geom_step() +
    my_theme
}

beam_acc('RATSQL' = ratsql, 'SmBoP' = smbop,
         'CUBES (RATSQL)' = db2csv_ratsql_21_combo_c20_all300,
         'CUBES (SmBoP)' = db2csv_smbop_21_combo_c20_all300,
         'RATSQL + CUBES (RATSQL)' = vbs(ratsql = ratsql, ratsql21_combo_c21_all300 = db2csv_ratsql_21_combo_c20_all300),
         'SmBoP + CUBES (SmBoP)' = vbs(smbop = smbop, smbop21_combo_c21_all300 = db2csv_smbop_21_combo_c20_all300)
)

c <- vbs(ratsql = ratsql, ratsql21_combo_c21_all300 = db2csv_ratsql_21_combo_c20_all300)

tmp <- db2csv_ratsql_21_combo_c20_all300$top_i
db2csv_ratsql_21_combo_c20_all300 %>%
  arrange(top_i) %>%
  mutate(tmp = 1, val = cumsum(tmp) / n_distinct(name)) %>%
  ggplot(aes(x = top_i, y = val)) + geom_step()

a <- db2csv_ratsql_21_combo_c20_all300 %>%
  arrange(top_i) %>%
  mutate(tmp = 1, val = cumsum(tmp))

sum(2 >= tmp, na.rm = T)

### ORIGNAL INSTANCES

a <- patsql %>% filter(fuzzy == 'Incorrect')

bars(use_vbs = F, s62 = s62, c62 = c62_16, c62_full = c62_16_full, c62_full_static = c62_16_full_static)

invsolved(every_other = 150, use_vbs = F, 'Scythe' = scythe, 'SQUARES' = squares, 'PatSQL' = patsql, 'CUBES-SEQ' = s62, 'CUBES-DC (16)' = c62_16_full)

plot_fuzzy(refactor = T, drop_nones = T, fill_bars = T,
                     'Scythe' = scythe_500, 'SQUARES' = squares_500, 'PatSQL' = patsql_500, 'CUBES-DC (16)' = c62_16_all600, 'CUBES-DC (16) + Dis.' = c62_16_all600_dis)[[1]]


invsolved(every_other = 150, 'SEQ' = s62, 'CUBES-4' = c62_4_full, 'CUBES-8' = c62_8_full, 'CUBES-16' = c62_16_full)
invsolved(every_other = 150, 'CUBES-16-DYNAMIC' = c62_16_full, 'CUBES-16-STATIC' = c62_16_full_static, 'CUBES-16-RANDOM' = c62_16_full_random)

plot_pdf('seq_solved', 0.9, 0.65,
         invsolved(every_other = 150, point_size = 1, step_size = .75, use_vbs = T,
                   '\\textsc{Squares}' = squares,
                   '\\textsc{Scythe}' = scythe,
                   '\\textsc{PatSQL}' = patsql,
                   '\\textsc{Cubes-Seq}' = s62) + guides(color = guide_legend(nrow = 2)))

invsolved(every_other = 150, point_size = 1, step_size = .75, use_vbs = T,
                   'DC' = c62_16_full,
'static' = c62_16_full_static,
          'random' = c62_16_full_random)


## BEGIN A

plot_pdf('weird_plot', 1.9, 0.85,
         plot_cells_time('\\textsc{Scythe}' = scythe,
                         '\\textsc{Squares}' = squares,
                         '\\textsc{PatSQL}' = patsql,
                         '\\textsc{Cubes-Seq}' = s62,
                         '\\textsc{Cubes-DC16}' = c62_16_full) + theme(legend.position = c(0.8, 0.2)))

s62 %>% filter(total_cells > 1000 & solved) %>% count()
c62_16_full %>% filter(total_cells > 1000 & solved) %>% count()
patsql %>% filter(total_cells > 1000 & solved) %>% count()
squares %>% filter(total_cells > 1000 & solved) %>% count()
scythe %>% filter(total_cells > 1000 & solved) %>% count()

a <- patsql %>% filter(fuzzy == 'Incorrect')

## END A

plot_pdf('dc_solved', 0.9, 0.65,
         invsolved(every_other = 150, point_size = 1, step_size = .75, 'SEQ' = s62, 'CUBES-4' = c62_4_full, 'CUBES-8' = c62_8_full, 'CUBES-16' = c62_16_full))

plot_pdf('non_determ', 0.75, 0.75,
         non_determinism_plot(c62_16_n10) + theme(legend.position = "none"))

a <- c62_16_n10 %>% filter(solution_n == 0, solveds == 10)

plot_fuzzy(c60 = c60_16, c61 = c61_16, c62_all600 = c62_16_all600, c62_all600_dis = c62_16_all600_dis)

a <- inner_join(c62_16_all600, c62_16_all600_dis, by = 'name') %>% filter((fuzzy.x == 'Possibly Correct' |
  fuzzy.x == 'Possibly Correct Top 5' |
  fuzzy.x == 'Possibly Correct Any') &
                                                                            fuzzy.y != 'Possibly Correct' &
                                                                            fuzzy.y != 'Possibly Correct Top 5' &
                                                                            fuzzy.y != 'Possibly Correct Any')
a <- inner_join(c62_16_all600_dis, c62_16_all600, by = 'name') %>% filter((fuzzy.x == 'Possibly Correct' |
  fuzzy.x == 'Possibly Correct Top 5' |
  fuzzy.x == 'Possibly Correct Any') &
                                                                            fuzzy.y != 'Possibly Correct' &
                                                                            fuzzy.y != 'Possibly Correct Top 5' &
                                                                            fuzzy.y != 'Possibly Correct Any')

plot_pdf("fuzzy", 1.85, 0.6,
         plot_fuzzy(refactor = T, drop_nones = T, fill_bars = T,
                    '\\textsc{Squares}' = squares_500,
                    '\\textsc{Scythe}' = scythe_500,
                    '\\textsc{PatSQL}' = patsql_500,
                    '\\textsc{Cubes-DC16}' = c62_16_all600,
                    '\\textsc{Cubes-DC16}+Dis.' = c62_16_all600_dis)[[1]] + theme(legend.position = "right"))

c62_16_all600_dis %>% filter(fuzzy == 'Possibly Correct') %>% count()
c62_16_all600_dis %>% filter(if_fuzzy_ok(fuzzy)) %>% count()

c62_16_all600_dis_simple %>% filter(total_queries >= 11 & total_queries < 100) %>% summarise(mean(n_questions))

plot_fuzzy(refactor = T, drop_nones = T, fill_bars = T, 'SQUARES' = squares_500, 'Scythe' = scythe_500, 'PatSQL' = patsql_500, 'CUBES' = c62_16_all600, 'Dis' = c62_16_all600_dis)[[1]] + theme(legend.position = "right")
plot_fuzzy(refactor = F, drop_nones = T, fill_bars = F, 'SQUARES' = squares_500, 'Scythe' = scythe_500, 'PatSQL' = patsql_500, 'CUBES' = c62_16_all600, 'Dis' = c62_16_all600_dis)[[1]] + theme(legend.position = "right")
plot_fuzzy(refactor = F, drop_nones = T, fill_bars = T, 'SQUARES' = squares_500, 'Scythe' = scythe_500, 'PatSQL' = patsql_500, 'CUBES' = c62_16_all600, 'Dis' = c62_16_all600_dis)[[1]] + theme(legend.position = "right")

plot_pdf('speedup', .9, .5,
         speedup(s62, c62_16_full))

speedup_data(s62, c62_4_full)
speedup_data(s62, c62_8_full)
speedup_data(s62, c62_16_full)

questions_by_queries_plot <- function(data) {
  data %>%
    filter(n_questions > 0) %>%
    mutate(total_queries_cat = cut(total_queries, c(0, 10, 100, 1000, 10000), c('1 to 10', '11 to 100', '101 to 1000', '>1000'))) %>%
    ggplot(aes(x = total_queries_cat, y = n_questions)) +
    geom_boxplot(outlier.size = 0.5) +
    stat_boxplot(geom = 'errorbar', width = 0.25) +
    labs(x = 'Number of queries pre-disambiguation', y = 'Questions asked') +
    my_theme
}

plot_pdf('n_questions', .9, .5,
         questions_by_queries_plot(c62_16_all600_dis_simple))

a %>%
  filter(status == 0) %>%
  ggplot(aes(x = n_questions)) +
  geom_bar() +
  my_theme
a %>% ggplot(aes(x = status)) +
  geom_bar() +
  my_theme
a %>%
  filter(status == 0) %>%
  ggplot(aes(x = total_queries)) + geom_histogram()
a %>%
  filter(status == 0) %>%
  ggplot(aes(x = total_queries, y = n_questions)) +
  geom_point() +
  scale_x_log10()

b <- a %>% filter(n_questions == 1)

inner_join(c62_16_)

benchmark_summary('extended_abstract_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{PatSQL}' = patsql,
                  '\\textsc{Cubes-Seq}' = s62,
                  '\\textsc{Cubes-DC4}' = c62_4_full,
                  '\\textsc{Cubes-DC8}' = c62_8_full,
                  '\\textsc{Cubes-DC16}' = c62_16_full
)

benchmark_summary('extended_abstract_summary_2',
                  '\\textsc{Cubes-DC16} (Default)' = c62_16_full,
                  'No Inv. Prog. Deduct.' = c62_16_full_no_bitvec,
                  'No Split Prog. Space' = c62_16_full_no_split,
                  'Random Cube Gen.' = c62_16_full_random
)

unique(squares$benchmark)

stop
# sequential %>% filter(fuzzy == 'Exec. Error')
# a <- scythe %>%
#   inner_join(c50_16, by = 'name') %>%
#   select(name, status_scythe = status.x, real_scythe = real.x, status_cubes = status.y) %>%
#   filter(status_scythe == 1)
#
# determ_instances <- sequential_sem %>% select(name) %>% sample_frac(.2)
# determ_instances_split <- determ_instances %>% group_split(row_number() %% 4, .keep=F)
#
# different_solutions(determ5)
# n_solveds(determ5)
# non_determinism_plot(determ5)
#
# scythe_3 %>% filter(solved) %>% filter(fuzzy == 'Error') %>% count()
# sequential_3 %>% filter(solved) %>% filter(fuzzy == 'Error') %>% count()
# sequential_3 %>% filter(status == 0) %>% filter(fuzzy == 'Error') %>% count()
# sequential_3 %>% filter(status == 0) %>% filter(fuzzy == 'Error') %>% sample_n(1) %>% select(name, status, fuzzy)
# sequential_3 %>% filter(solved) %>% count()
#
#
# a <- squares %>% filter(status == 1) %>% select(name) %>% arrange(name)
#
# invsolved('Scythe' = scythe, 'Scythe3' = scythe_3, 'SQUARES' = squares, 'SQUARES2' = squares_2, 'CUBES-SEQ' = sequential)
# invsolved('CUBES-SEQ' = sequential, 'CUBES-SEQ-TOP5' = sequential_top5)
# invsolved('CUBES-DC16' = c50_16, 'CUBES-DC16-TOP5' = c50_16_top5)
# a <- sequential %>% inner_join(sequential_top5, by='name') %>% filter(fuzzy.x == 'Possibly Correct' & fuzzy.y != 'Possibly Correct')
#
#
# scatter('CUBES-SEQ3' = sequential_3, 'CUBES-SEQ-SEM' = sequential_sem)
#
# plot_cells_time('Scythe' = scythe_3, 'SQUARES' = squares_2, 'CUBES-SEQ' = sequential_sem, 'PatSQL' = patsql_3)
# plot_cells_ram('Scythe3' = scythe_3, 'SQUARES2' = squares_2, 'CUBES-SEQ3' = sequential_3, 'PatSQL2' = patsql_3)
# scatter_real_cpu('Scythe3' = scythe_3, 'SQUARES2' = squares_2, 'CUBES-SEQ3' = sequential_3, 'PatSQL2' = patsql_3)
# scatter_equiv(x = 'real', seq = sequential_3, sem = sequential_sem)
# scatter_equiv(x = 'cpu', seq = sequential_3, sem = sequential_sem)
#
# plot_sql_size('\\textsc{Scythe}' = scythe_3,
#               '\\textsc{Squares}' = squares_2,
#               '\\textsc{PatSQL}' = patsql_3,
#               '\\textsc{Cubes-Seq}' = sequential_sem,
#               '\\textsc{Cubes-DC16}' = c50_16)
#
# sequential_sem %>%
#   ggplot(aes(x = sql_size)) +
#   geom_histogram(bins=15)
#
# plot_fuzzy('Scythe' = scythe, 'SQUARES' = squares, 'CUBES-SEQ' = sequential, 'CUBES-SEQ-T5' = sequential_top5, 'PatSQL' = patsql, 'CUBES-DC16' = c50_16)
# plot_fuzzy(refactor = T, 'Scythe' = scythe, 'SQUARES' = squares, 'CUBES-SEQ' = sequential, 'CUBES-SEQ-T5' = sequential_top5, 'PatSQL' = patsql, 'CUBES-DC16' = c50_16)
# plot_fuzzy(refactor = T, fill_bars = T, 'Scythe' = scythe, 'SQUARES' = squares, 'CUBES-SEQ' = sequential, 'CUBES-SEQ-T5' = sequential_top5, 'PatSQL' = patsql)
# plot_fuzzy('Scythe' = scythe_3, 'SQUARES' = squares_2, 'CUBES-SEQ' = sequential_sem, 'CUBES-SEQ-T5' = sequential_sem_top5, 'PatSQL' = patsql_3)
# plot_fuzzy(fill_bars = T, 'Scythe' = scythe_3, 'SQUARES' = squares_2, 'CUBES-SEQ' = sequential_sem, 'CUBES-SEQ-T5' = sequential_sem_top5, 'PatSQL' = patsql_3)
#
# scatter(seq = sequential, seq3 = sequential_3)
# scatter(seq = sequential, sem = sequential_sem)
#
# sequential_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% filter(fuzzy == 'Correct') %>% count() / sequential_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% count()
# scythe_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% filter(fuzzy == 'Correct') %>% count() / scythe_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% count()
#
# sequential_3 %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / sequential_3 %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% count()
# patsql %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / patsql %>% filter(benchmark == 'spider' & solved& fuzzy != 'Error') %>% count()
# scythe %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / scythe %>% filter(benchmark == 'spider' & solved& fuzzy != 'Error') %>% count()
# sequential_sem %>% filter(benchmark == 'textbook' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / sequential_sem %>% filter(benchmark == 'textbook' & solved& fuzzy != 'Error') %>% count()
#
# sequential_3 %>% filter(fuzzy == 'Correct') %>% count()
# scythe_3 %>% filter(fuzzy == 'Correct') %>% count()
# patsql %>% filter(fuzzy == 'Correct') %>% count()
#
# scythe %>%
#   filter(solved) %>%
#   summarise(m = mean(sql_size, na.rm = T))
# squares %>%
#   filter(solved) %>%
#   summarise(m = mean(sql_size, na.rm = T))
# sequential %>%
#   filter(solved) %>%
#   summarise(m = mean(sql_size, na.rm = T))
#
# instance_info %>% ggplot(aes(x = sql_size)) +
#   geom_histogram(bins = 15) +
#   facet_wrap(~benchmark, scales = 'free_y')
#
# scatter(seq = sequential_3, sem = sequential_sem)
#
# a <- sequential_sem %>% filter(solved & (fuzzy == 'Incorrect' | fuzzy == 'Fuzzying Incorrect')) %>% select(name) %>% mutate(name = paste0('tests/', name))
#
# a <- sequential_3 %>%
#   filter(fuzzy == 'Incorrect') %>%
#   select(name) %>% arrange(name)
#
# intent <- bitenum %>%
#   inner_join(squares, by = c('name', 'benchmark')) %>%
#   inner_join(scythe, by = c('name', 'benchmark')) %>%
#   filter(solved.x & solved.y & solved) %>%
#   group_by(benchmark) %>%
#   sample_frac(.15) %>%
#   ungroup() %>%
#   select(name) %>%
#   mutate(intent = NA)
#
# a <- determ5 %>% filter(solution_n == 10) %>% select(solutions)
# determ5 %>% filter(solution_n == 10) %>% select(name)

####### PAPER PLOTS ######

plot_pdf('seq_solved', 0.9, 0.65,
         invsolved(every_other = 150, point_size = 1, step_size = .75, use_vbs = T,
                   '\\textsc{Squares}' = squares,
                   '\\textsc{Scythe}' = scythe,
                   '\\textsc{PatSQL}' = patsql,
                   '\\textsc{Cubes-Seq}' = sequential) + guides(color = guide_legend(nrow = 2)))


plot_pdf('weird_plot', 1.9, 0.8,
         plot_cells_time('\\textsc{Scythe}' = scythe,
                         '\\textsc{Squares}' = squares,
                         '\\textsc{PatSQL}' = patsql,
                         '\\textsc{Cubes-Seq}' = sequential,
                         '\\textsc{Cubes-DC16}' = c50_16) + theme(legend.position = c(0.8, 0.2)))

# plot_pdf('seq_scythe_scatter', .65, .65,
#          scatter('\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = sequential))

plot_pdf('fuzzy', 1.5, 0.6,
         plot_fuzzy(refactor = T, fill_bars = T, '\\textsc{Scythe}' = scythe,
                    '\\textsc{Squares}' = squares,
                    '\\textsc{PatSQL}' = patsql,
                    '\\textsc{Cubes-Seq}' = sequential))

sequential_sem %>% filter(fuzzy == 'Possibly Correct' | fuzzy == '')

plot_pdf('non_determ', 0.75, 0.75,
         non_determinism_plot(determ5) + theme(legend.position = "none"))

scythe_3 %>%
  filter(solved) %>%
  ggplot(aes(x = total_cells)) +
  geom_histogram() +
  scale_x_log10()
squares %>%
  filter(solved) %>%
  ggplot(aes(x = total_cells)) +
  geom_histogram() +
  scale_x_log10()
sequential_3 %>%
  filter(solved) %>%
  ggplot(aes(x = total_cells)) +
  geom_histogram() +
  scale_x_log10()


basic_solved_not_solved(scythe, sequential) %>% count()

plot_pdf('seq_ablation', 0.9, 0.6,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   'No Inv. Prog. Deduction' = sequential_no_bitvec) + guides(color = guide_legend(nrow = 3)))

solved_not_solved(c50_16, sequential) %>% count() - solved_not_solved(sequential, c50_16) %>% count()

plot_pdf('parallel_solved', .9, .65,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential_sem,
                   '\\textsc{Cubes-Port8}' = portfolio5_8,
                   '\\textsc{Cubes-DC16}' = c50_16) +
           guides(color = guide_legend(nrow = 1)) +
           theme(legend.spacing.x = unit(0, 'cm')))

gm_mean <- function(x, na.rm = TRUE) {
  exp(sum(log(x[x > 0]), na.rm = na.rm) / length(x))
}

sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  select(speedup) %>%
  ggplot(aes(x = speedup)) +
  geom_histogram() +
  scale_x_log10()

# SPEEDUP EASY VS HARD
# FAZER TABELA CONFIGURAÇÕES
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 60 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = gm_mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 60 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = median(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 60 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 60 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = sd(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 60 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  filter(speedup > 10) %>%
  count() /
  sequential %>%
    inner_join(c50_16, by = 'name') %>%
    filter(real.x > 60 & solved.x & solved.y) %>%
    mutate(speedup = real.x / real.y) %>%
    count()
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 1 & real.x < 60)


speedup(sequential, c50_4)
speedup(sequential, c50_8)
speedup(sequential, c50_16)


sequential %>%
  inner_join(c50_4, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = mean(speedup))
sequential %>%
  inner_join(c50_8, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  select(speedup) %>%
  ggplot(aes(x = speedup)) + geom_histogram()
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 10 & real.y > 10) %>%
  mutate(speedup = real.x / real.y) %>%
  select(speedup) %>%
  ggplot(aes(x = speedup)) + geom_histogram()
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 10 & real.y > 10 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  select(speedup)
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 60 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  select(speedup) %>%
  ggplot(aes(x = speedup)) +
  geom_histogram() +
  scale_x_log10()
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 60 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 10 & real.y > 10 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 10 & real.y > 10 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = gm_mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  filter(real.x > 10 & real.y > 10 & solved.x & solved.y) %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = median(speedup))
sequential %>%
  inner_join(c50_4, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = gm_mean(speedup))
sequential %>%
  inner_join(c50_8, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = gm_mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = gm_mean(speedup))
sequential %>%
  inner_join(c50_16, by = 'name') %>%
  mutate(speedup = real.x / real.y) %>%
  summarise(speedup = g(speedup))

# plot_pdf('dc_solved', .9, .7,
#          invsolved(every_other = 150, point_size = 1, step_size = .75,
#                    '\\textsc{Cubes-Seq}' = sequential,
#                    '\\textsc{Cubes-DC4}' = c50_4,
#                    '\\textsc{Cubes-DC8}' = c50_8,
#                    '\\textsc{Cubes-DC16}' = c50_16) + guides(color = guide_legend(nrow = 2)))

# plot_pdf('dc_ablation', .9, .7,
#          invsolved(every_other = 150, point_size = 1, step_size = .75,
#                    '\\textsc{Cubes-DC16}' = c50_16,
#                    'Optimal' = c50_16_optimal,
#                    'Static Cube Gen.' = c50_16_static,
#                    'No Learning from Cubes' = c50_16_no_dedu,
#                    'No DSL Split' = c50_16_no_split,
#                    '\\textsc{Cubes-Seq}' = sequential) + guides(color = guide_legend(nrow = 3)))

plot_pdf('work_distribution', .9, .5,
         equiv_processes(c50_16, cutoff = 20)
)

plot_pdf('port_solved', .9, .7,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-Port4}' = portfolio5_4,
                   '\\textsc{Cubes-Port8}' = portfolio5_8,
                   '\\textsc{Cubes-Port16}' = portfolio5_16) + guides(color = guide_legend(nrow = 2)))

plot_pdf('port_scythe', .9, .7,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-Port4}' = portfolio5_4,
                   '\\textsc{Scythe}+\\textsc{Cubes-Seq}' = vbs(scythe, sequential),
                   '\\textsc{Scythe}+\\textsc{Cubes-Port4}' = vbs(scythe, portfolio5_4)) + guides(color = guide_legend(nrow = 2)))

plot_pdf('speedup', .9, .5,
         speedup(sequential, c50_16))

speedup_data(sequential, portfolio5_4)
speedup_data(sequential, portfolio5_8)
speedup_data(sequential, portfolio5_16)
speedup_data(sequential, c50_4)
speedup_data(sequential, c50_8)
speedup_data(sequential, c50_16)

sequential %>%
  filter(solved & real > 60) %>%
  count()

benchmark_summary('extended_abstract_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{PatSQL}' = patsql,
                  '\\textsc{Cubes-Seq}' = sequential,
                  '\\hspace{1em}\\textsc{No pruning}' = sequential_no_bitvec,
                  '\\textsc{Cubes-Port4}' = portfolio5_4,
                  '\\textsc{Cubes-Port8}' = portfolio5_8,
                  '\\textsc{Cubes-Port16}' = portfolio5_16,
                  '\\textsc{Cubes-DC4}' = c50_4,
                  '\\textsc{Cubes-DC8}' = c50_8,
                  '\\textsc{Cubes-DC16}' = c50_16)

####### PRESENTATION PLOTS ######

plot_pdf('results_slides', 1.2, .675,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Squares}' = squares,
                   '\\textsc{Scythe}' = scythe,
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-Port8}' = portfolio5_8,
                   '\\textsc{Cubes-DC16}' = c50_16))

plot_pdf('seq_ablation_results', 1.2, .675,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   'Learning from Inc. Prog.' = sequential_subsume,
                   'No QF-FD Theory' = sequential_no_qffd,
                   'Simple DSL' = sequential_simple_dsl,
                   'No Inv. Prog. Deduction' = sequential_no_bitvec) + guides(color = guide_legend(nrow = 3)))

plot_pdf('dc_solved_results', 1.2, .675,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-DC4}' = c50_4,
                   '\\textsc{Cubes-DC8}' = c50_8,
                   '\\textsc{Cubes-DC16}' = c50_16))

plot_pdf('dc_ablation_results', 1.2, .675,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-DC16}' = c50_16,
                   'Optimal' = c50_16_optimal,
                   'Static Cube Gen.' = c50_16_static,
                   'No Learning from Cubes' = c50_16_no_dedu,
                   'No DSL Split' = c50_16_no_split,
                   '\\textsc{Cubes-Seq}' = sequential))

plot_pdf('port_solved_results', 1.2, .675,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-Port4}' = portfolio5_4,
                   '\\textsc{Cubes-Port8}' = portfolio5_8,
                   '\\textsc{Cubes-Port16}' = portfolio5_16))

plot_pdf('fuzzy_slides', 1.2, .675,
         plot_fuzzy('Scythe' = scythe_3, 'SQUARES' = squares_2, 'CUBES-SEQ' = sequential_sem, 'CUBES-SEQ-T5' = sequential_sem_top5, 'PatSQL' = patsql_2))

plot_pdf('nondeterm_slides', 1.1, 1,
         non_determinism_plot(determ5))
