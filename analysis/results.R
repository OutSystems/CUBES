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

sequential %>% filter(fuzzy == 'Exec. Error')
a <- scythe %>%
  inner_join(c50_16, by = 'name') %>%
  select(name, status_scythe = status.x, real_scythe = real.x, status_cubes = status.y) %>%
  filter(status_scythe == 1)


scythe_3 %>% filter(solved) %>% filter(fuzzy == 'Error') %>% count()
sequential_3 %>% filter(solved) %>% filter(fuzzy == 'Error') %>% count()
sequential_3 %>% filter(status == 0) %>% filter(fuzzy == 'Error') %>% count()
sequential_3 %>% filter(status == 0) %>% filter(fuzzy == 'Error') %>% sample_n(1) %>% select(name, status, fuzzy)
sequential_3 %>% filter(solved) %>% count()


a <- squares %>% filter(status == 1) %>% select(name) %>% arrange(name)

invsolved('Scythe' = scythe, 'Scythe3' = scythe_3, 'SQUARES' = squares, 'SQUARES2' = squares_2, 'CUBES-SEQ' = sequential, 'PatSQL' = patsql, 'CUBES-DC16' = c50_16)

plot_cells_time('Scythe' = scythe, 'Scythe3' = scythe_3, 'SQUARES' = squares, 'SQUARES2' = squares_2, 'CUBES-SEQ' = sequential_3, 'CUBES-DC' = c50_16, 'PatSQL' = patsql)
plot_cells_ram('Scythe' = scythe, 'Scythe3' = scythe_3, 'SQUARES' = squares, 'CUBES-SEQ' = sequential_3, 'CUBES-DC' = c50_16, 'PatSQL' = patsql)
scatter_real_cpu('Scythe' = scythe, 'Scythe3' = scythe_3, 'SQUARES' = squares, 'CUBES-SEQ' = sequential_3, 'CUBES-DC' = c50_16, 'PatSQL' = patsql)
scatter_equiv(x = 'real', 'Scythe' = scythe, 'Scythe3' = scythe_3, 'SQUARES' = squares, 'CUBES-SEQ' = sequential_3, 'CUBES-DC' = c50_16, 'PatSQL' = patsql)

scatter(patsql = patsql, seq = sequential)
scatter(squares = squares, seq = sequential)
scatter(patsql = patsql, dc = c50_16)
scatter(patsql = patsql, scythe = scythe)

scatter(seq2 = sequential, seq3 = sequential_3)
scatter('Scythe' = scythe, 'Scythe3' = scythe_3)

plot_sql_size('Scythe' = scythe, 'SQUARES' = squares, 'CUBES-SEQ' = sequential, 'CUBES-DC' = c50_16, 'PatSQL' = patsql)

scythe %>%
  filter(solved) %>%
  summarise(m = mean(sql_size, na.rm = T))
squares %>%
  filter(solved) %>%
  summarise(m = mean(sql_size, na.rm = T))
sequential %>%
  filter(solved) %>%
  summarise(m = mean(sql_size, na.rm = T))

instance_info %>% ggplot(aes(x = sql_size)) +
  geom_histogram(bins = 15) +
  facet_wrap(~benchmark, scales = 'free_y')

plot_fuzzy(scythe = scythe_3, squares = squares, squares2 = squares_2, patsql = patsql, seq = sequential_3, dc = c50_16)
plot_fuzzy(facet=T,scythe = scythe_3, squares = squares, patsql = patsql, seq = sequential_3, dc = c50_16)
plot_fuzzy(scythe = scythe_3)
plot_fuzzy(filter_all = T, scythe = scythe_3, squares = squares, patsql = patsql, seq = sequential_3, dc = c50_16)
plot_fuzzy(filter_all = T, fill_bars = T, scythe = scythe_3, squares = squares, patsql = patsql, seq = sequential_3, dc = c50_16)
plot_fuzzy(fill_bars = T, scythe = scythe_3, squares = squares, patsql = patsql, seq = sequential_3, dc = c50_16)
plot_fuzzy(drop_error = T, fill_bars = T, scythe = scythe_3, squares = squares, patsql = patsql, seq = sequential_3, dc = c50_16)

sequential_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% filter(fuzzy == 'Correct') %>% count() / sequential_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% count()
scythe_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% filter(fuzzy == 'Correct') %>% count() / scythe_3 %>% filter(benchmark == 'scythe/recent-posts' | benchmark == 'scythe/top-rated-posts' & solved) %>% count()

sequential_3 %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / sequential_3 %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% count()
patsql %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / patsql %>% filter(benchmark == 'spider' & solved& fuzzy != 'Error') %>% count()
scythe %>% filter(benchmark == 'spider' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / scythe %>% filter(benchmark == 'spider' & solved& fuzzy != 'Error') %>% count()
sequential_3 %>% filter(benchmark == 'textbook' & solved & fuzzy != 'Error') %>% filter(fuzzy == 'Correct') %>% count() / sequential_3 %>% filter(benchmark == 'textbook' & solved& fuzzy != 'Error') %>% count()



sequential_3 %>% filter(fuzzy == 'Correct') %>% count()
scythe_3 %>% filter(fuzzy == 'Correct') %>% count()
patsql %>% filter(fuzzy == 'Correct') %>% count()

a <- sequential_3 %>%
  filter(fuzzy == 'Incorrect') %>%
  select(name) %>% arrange(name)

intent <- bitenum %>%
  inner_join(squares, by = c('name', 'benchmark')) %>%
  inner_join(scythe, by = c('name', 'benchmark')) %>%
  filter(solved.x & solved.y & solved) %>%
  group_by(benchmark) %>%
  sample_frac(.15) %>%
  ungroup() %>%
  select(name) %>%
  mutate(intent = NA)

#######   THESIS   ######

plot_pdf('squares_scatter', .35, .35,
         scatter('\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe))

plot_pdf('squares_scatter_ram', .39, .35,
         scatter_ram('\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe))

plot_pdf('seq_scythe_scatter', .35, .35,
         scatter('\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = sequential))

squares %>%
  filter(solved & real <= 10) %>%
  count() / squares %>% filter(solved) %>% count()
scythe %>%
  filter(solved & real <= 10) %>%
  count() / scythe %>% filter(solved) %>% count()

plot_pdf('seq_bars', 1, .6,
         bars(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = sequential))

plot_pdf('seq_solved', .9, .4,
         invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'right',
                   '\\textsc{Squares}' = squares,
                   '\\textsc{Scythe}' = scythe,
                   '\\textsc{Cubes-Seq}' = sequential))


plot_pdf('seq_ablation', 1, .45,
         invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'right',
                   '\\textsc{Cubes-Seq}' = sequential,
                   'Learning from Inc. Prog.' = sequential_subsume,
                   'No QF-FD Theory' = sequential_no_qffd,
                   'Simple DSL' = sequential_simple_dsl,
                   'No Inv. Prog. Deduction' = sequential_no_bitvec))

solved_not_solved(sequential_no_bitvec, sequential) %>% count()
solved_not_solved(sequential, sequential_simple_dsl) %>% count()

plot_pdf('seq_learning', .7, .45,
         time_part_dist(col = 'eval',
                        'No Learning from Programs' = sequential,
                        'Learning from Programs' = sequential_subsume))

sequential_subsume %>%
  mutate(a = eval / real) %>%
  filter(a > .75) %>%
  count()
sequential %>%
  mutate(a = eval / real) %>%
  filter(a > .75) %>%
  count()


plot_pdf('port_solved', .48, .55,
         invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'bottom',
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-Port4}' = portfolio5_4,
                   '\\textsc{Cubes-Port8}' = portfolio5_8,
                   '\\textsc{Cubes-Port16}' = portfolio5_16) + guides(color = guide_legend(nrow = 2)))

plot_pdf('port_scythe', .48, .55,
         invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'bottom',
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-Port4}' = portfolio5_4,
                   '\\textsc{Scythe}+\\textsc{Cubes-Seq}' = vbs(scythe, sequential),
                   '\\textsc{Scythe}+\\textsc{Cubes-Port4}' = vbs(scythe, portfolio5_4)) + guides(color = guide_legend(nrow = 2)))

plot_pdf('port_1', width = .33, height = .35, processes(portfolio5_4, 4))
plot_pdf('port_2', width = .48, height = .35, processes(portfolio5_8, 8))
plot_pdf('port_3', width = .68, height = .35, processes(portfolio5_16, 16))


scatter('sequential' = sequential, 'portfolio4' = portfolio5_4)
scatter('portfolio4' = portfolio5_4, 'portfolio8' = portfolio5_8)
scatter('portfolio8' = portfolio5_8, 'portfolio16' = portfolio5_16)

plot_pdf('dc_solved', 1, .45,
         invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'right',
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-DC4}' = c50_4,
                   '\\textsc{Cubes-DC8}' = c50_8,
                   '\\textsc{Cubes-DC16}' = c50_16))

plot_pdf('parallel_solved', 1, .45,
         invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'right',
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-Port4}' = portfolio5_4,
                   '\\textsc{Cubes-Port8}' = portfolio5_8,
                                                               #'\\textsc{Cubes-Port16}' = portfolio1,
                   '\\textsc{Cubes-Port16}' = portfolio5_16,
                   '\\textsc{Cubes-DC4}' = c50_4,
                   '\\textsc{Cubes-DC8}' = c50_8,
                   '\\textsc{Cubes-DC16}' = c50_16))

scatter('\\textsc{Cubes-Port4}' = portfolio5_4, '\\textsc{Cubes-DC4}' = c50_4)
scatter_ram('\\textsc{Cubes-Port4}' = portfolio5_4, '\\textsc{Cubes-DC4}' = c50_4)

plot_pdf('work_distribution', .5, .35,
         equiv_processes(c50_16, cutoff = 20)
)
c50_16 %>%
  filter(real > 20 & equivalent_p > 15) %>%
  count() / c50_16 %>% filter(real > 20) %>% count()


plot_pdf('dc_ablation', 1, .45,
         invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'right',
                   '\\textsc{Cubes-DC16}' = c50_16,
                   'Optimal' = c50_16_optimal,
                   'Static Cube Gen.' = c50_16_static,
                   'No Learning from Cubes' = c50_16_no_dedu,
                   'No DSL Split' = c50_16_no_split,
                   '\\textsc{Cubes-Seq}' = sequential))

c50_16 %>% summarise(a = mean(blocked_cubes / cubes, na.rm = T))

plot_pdf('dc_optimality', .9, .7,
         bars(use_vbs = F, '\\textsc{Cubes-DC16}' = c50_16, 'Optimal' = c50_16_optimal))

c50_16_optimal %>%
  filter(status == 3 | status == 4) %>%
  count() / c50_16_optimal %>% filter(solved) %>% count()

benchmark_summary('thesis_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{Cubes-Seq}' = sequential,
                  '\\hspace{1em}Learning from Programs' = sequential_subsume,
                  '\\hspace{1em}No QF-FD Theory' = sequential_no_qffd,
                  '\\hspace{1em}Simple DSL' = sequential_simple_dsl,
                  '\\hspace{1em}No Inc. Prog. Deduction' = sequential_no_bitvec,
                  '\\textsc{Cubes-Port4}' = portfolio5_4,
                  '\\textsc{Cubes-Port8}' = portfolio5_8,
                  '\\textsc{Cubes-Port16}' = portfolio5_16,
                  '\\textsc{Cubes-DC4}' = c50_4,
                  '\\textsc{Cubes-DC8}' = c50_8,
                  '\\textsc{Cubes-DC16}' = c50_16,
                  '\\hspace{1em}Optimal' = c50_16_optimal,
                  '\\hspace{1em}Static Cube Gen.' = c50_16_static,
                  '\\hspace{1em}No Learning from Cubes' = c50_16_no_dedu,
                  '\\hspace{1em}No DSL Split' = c50_16_no_split)

invsolved(every_other = 150, point_size = 1, step_size = .75, legend.position = 'right', '\\textsc{Cubes-DC16}' = c50_16, '22T' = vbs(c50_16, portfolio5_4, scythe, squares))
benchmark_summary('test', '\\textsc{Cubes-DC16}' = c50_16, '22T' = vbs(c50_16, portfolio5_4, scythe, squares))

determ2 %>% summarise(a = median(solution_n, na.rm = T))
determ2 %>% summarise(a = mean(solution_n, na.rm = T))
determ2 %>% summarise(a = max(solution_n, na.rm = T))
determ2 %>% summarise(a = getmode(solution_n))

####### PAPER PLOTS ######

plot_pdf('weird_plot', 2, 1.1,
         plot_cells_time('\\textsc{Scythe}' = scythe_3,
                         '\\textsc{Squares}' = squares,
                         '\\textsc{PatSQL}' = patsql,
                         '\\textsc{Cubes-Seq}' = sequential_3,
                         '\\textsc{Cubes-DC16}' = c50_16))

plot_pdf('fuzzy', 1.5, 0.6,
         plot_fuzzy('\\textsc{Scythe}' = scythe_3,
                    '\\textsc{Squares}' = squares,
                    '\\textsc{PatSQL}' = patsql,
                    '\\textsc{Cubes-Seq}' = sequential_3,
                    '\\textsc{Cubes-DC16}' = c50_16))

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

plot_pdf('squares_scatter', .65, .65,
         scatter('\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe))

plot_pdf('seq_scythe_scatter', .65, .65,
         scatter('\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = sequential))

basic_solved_not_solved(scythe, sequential) %>% count()

plot_pdf('seq_solved', 0.9, 0.7,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Squares}' = squares,
                   '\\textsc{Scythe}' = scythe,
                   '\\textsc{Cubes-Seq}' = sequential))


plot_pdf('seq_ablation', 0.9, 0.7,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   'Learning from Inc. Prog.' = sequential_subsume,
                   'No QF-FD Theory' = sequential_no_qffd,
                   'Simple DSL' = sequential_simple_dsl,
                   'No Inv. Prog. Deduction' = sequential_no_bitvec) + guides(color = guide_legend(nrow = 3)))

plot_pdf('dc_solved', .9, .7,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-Seq}' = sequential,
                   '\\textsc{Cubes-DC4}' = c50_4,
                   '\\textsc{Cubes-DC8}' = c50_8,
                   '\\textsc{Cubes-DC16}' = c50_16) + guides(color = guide_legend(nrow = 2)))

plot_pdf('dc_ablation', .9, .7,
         invsolved(every_other = 150, point_size = 1, step_size = .75,
                   '\\textsc{Cubes-DC16}' = c50_16,
                   'Optimal' = c50_16_optimal,
                   'Static Cube Gen.' = c50_16_static,
                   'No Learning from Cubes' = c50_16_no_dedu,
                   'No DSL Split' = c50_16_no_split,
                   '\\textsc{Cubes-Seq}' = sequential) + guides(color = guide_legend(nrow = 3)))

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

benchmark_summary('extended_abstract_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{Cubes-Seq}' = sequential,
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