library(psych)
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
library(ggpattern)
library(ggridges)
library(ggthemes)
library(elementalist)
library(devEMF)
library(ggpubr)

setwd('analysis')

arial <- T

if (arial) {
  options(tikzDefaultEngine = 'xetex',
          tikzXelatexPackages = c(
            "\\usepackage{tikz}\n",
            "\\usepackage[active,tightpage,xetex]{preview}\n",
            "\\usepackage{fontspec}\n",
            "\\PreviewEnvironment{pgfpicture}\n",
            "\\setlength\\PreviewBorder{0pt}\n",
            "\\usepackage[sfdefault]{notomath}\n",
            "\\usepackage{sourcecodepro}\n"
          ),
          tikzMetricsDictionary = './metrics_cache_thesis',
          standAlone = T)
  textwidth <- 6.30045

  my_theme_f <- function(base_size = 9, base_family = "sans") {
    ltgray <- "#cccccc"
    dkgray <- "#111111"
    dkgray2 <- "#000000"
    theme_foundation(base_size = base_size, base_family = base_family) + theme(
      rect = element_rect(colour = "black", fill = "white"),
      line = element_line(colour = "black"),
      text = element_text(colour = dkgray),
      plot.title = element_text(face = "plain", size = rel(20 / 12), hjust = 0, colour = dkgray),
      plot.subtitle = element_text(hjust = 0, size = rel(1), face = "plain", colour = dkgray),
      plot.caption = element_text(hjust = 0, size = rel(1), face = "plain", colour = dkgray),
      panel.background = element_rect(fill = NA, colour = NA),
      panel.border = element_rect(fill = NA, colour = NA),
      strip.text = element_text(hjust = 0, size = rel(1), colour = dkgray2, face = "plain"),
      strip.background = element_rect(colour = NA, fill = NA),
      axis.title = element_text(face = "plain", colour = dkgray2, size = rel(1)),
      axis.text = element_text(face = "plain", colour = dkgray, size = rel(.9)),
      axis.line = element_line(colour = "black"), axis.line.y = element_blank(),
      axis.ticks = element_blank(), panel.grid.major = element_line(colour = ltgray),
      panel.grid.minor = element_blank(), legend.background = element_rect(colour = NA),
      legend.text = element_text(size = rel(1), colour = dkgray),
      # legend.title = element_text(size = rel(1), colour = dkgray2, face = "plain"),
      legend.key = element_rect_round(color = NA, radius = unit(0.4, "snpc")),
      # legend.position = "right",
      # legend.direction = "vertical",
      legend.position = "bottom",
      legend.title = element_blank(),
      plot.background = element_rect(colour = NA))
  }

  my_theme <- my_theme_f()

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

source('loading_esem.R')  # DONT FORGET TO CHECK IF 55-TESTS IS INCLUDED OR NOT
source('plots.R')
source('tables.R')

seq_dis_timeouts <- left_join(seq_600_dis, seq_600_dis_data, by = 'name', suffix = c('', '.dis')) %>%
  filter((status.dis == 2 | is.na(status.dis)) & sql_solutions != 0) %>%
  select(name)
c16_dis_timeouts <- left_join(cubes_16_600_dis, cubes_16_600_dis_data, by = 'name', suffix = c('', '.dis')) %>%
  filter((status.dis == 2 | is.na(status.dis)) & sql_solutions != 0) %>%
  select(name)


groups <- cubes %>%
  select(name) %>%
  mutate(group = sample.int(3, n(), replace = T))

groups_a <- groups %>% filter(group == 1) %>% select(name)
groups_b <- groups %>% filter(group == 2) %>% select(name)
groups_c <- groups %>% filter(group == 3) %>% select(name)

invsolved(prev = s62, post = cubes)

cubes %>% group_by(benchmark) %>% count()

plot_emf <- function(filename, width, height, plot) {
  emf(file = paste0('plots/', filename, ".emf"), width = width, height = height, emfPlus = F)
  print(plot)
  dev.off()
}

plot_pdf('seq_solved', 1.3, 0.55,
         invsolved(every_other = 150, point_size = 1, step_size = .75, use_vbs = T,
                   '\\textsc{Squares}' = squares,
                   '\\textsc{Scythe}' = scythe,
                   '\\textsc{PatSQL}' = patsql,
                   '\\textsc{Libra}' = libra,
                   '\\textsc{Cubes-Seq}' = seq) + theme(legend.position = "right"))

plot_emf("solved", 6, 3.375,
         invsolved(every_other = 150, point_size = 1, step_size = .5, use_vbs = F,
                   'Squares' = squares,
                   'Scythe' = scythe,
                   'PatSQL' = patsql,
                   'Cubes-Seq' = seq,
                   'Cubes-DC16' = cubes_16, perecentage_suffix = "%"))

dev.off()

invsolved(every_other = 150, point_size = 1, step_size = .5, use_vbs = F,
          'Cubes-Seq' = seq,
          'Cubes-DC4' = cubes_4,
          'Cubes-DC8' = cubes_8,
          'Cubes-DC16' = cubes_16,
          'Cubes-DC32' = cubes_32, perecentage_suffix = "%")

cubes_16_no_bitvec %>% summarise(a = max(eval_p, na.rm = T))
cubes_16 %>% summarise(a = max(eval_p, na.rm = T))
seq %>% summarise(a = mean(eval_p, na.rm = T))
seq %>% summarise(a = median(eval_p, na.rm = T))

# plot_pdf('weird_plot', 1.9, 0.85,
#          plot_cells_time('\\textsc{Scythe}' = scythe,
#                          '\\textsc{Squares}' = squares,
#                          '\\textsc{PatSQL}' = patsql,
#                          '\\textsc{Cubes-Seq}' = s62,
#                          '\\textsc{Cubes-DC16}' = c62_16_full) + theme(legend.position = c(0.8, 0.2)))

plot_pdf('dc_solved', 1, 0.65,
         invsolved(every_other = 150, point_size = 1, step_size = .75, 'SEQ' = seq, 'CUBES-4' = cubes_4, 'CUBES-8' = cubes_8, 'CUBES-16' = cubes_16))

invsolved(every_other = 150, point_size = 1, step_size = .75, '\\textsc{Cubes-DC16} (Default)' = cubes_16,
          'No Invalid Program Pruning' = cubes_16_no_bitvec,
          'No Split Program Space' = cubes_16_no_split,
          'Random Cube Generation' = cubes_16_random)


plot_pdf('non_determ', 0.75, 0.75,
         non_determinism_plot(cubes_16_n10) + theme(legend.position = "none"))

plot_fuzzy_new('\\textsc{Squares}' = squares,
               '\\textsc{Scythe}' = scythe,
               '\\textsc{PatSQL}' = patsql,
               '\\textsc{Cubes-Seq}' = seq,
               '\\textsc{Cubes-DC16}' = cubes_16,
               '\\textsc{Cubes-Seq}\nAll Solutions' = seq_600,
               '\\textsc{Cubes-DC16}\nAll Solutions' = cubes_16_600,
               '\\textsc{Cubes-DC16}\nAll Solutions_old' = cubes_16_600_old)

A <- read_csv('fuzzy/c62_16_all600_A_dis_fuzz_180s.csv') %>% filter(base_eq != -3)
B <- read_csv('fuzzy/c62_16_all600_B_dis_fuzz_180s.csv') %>% filter(base_eq != -3)
C <- read_csv('fuzzy/c62_16_all600_C_dis_fuzz_180s.csv') %>% filter(base_eq != -3)

all <- bind_rows(A, B, C)

all %>% filter(stat)

write_csv(all, 'fuzzy/c62_16_all600_full.csv')

plot_fuzzy_new('\\textsc{Cubes-Seq}\nAll Solutions' = seq_600 %>% mutate(status.dis = 0),
               '\\textsc{Cubes-Seq}\nAll Solutions\n+ Disambiguation' = left_join(seq_600_dis, seq_600_dis_data, by = 'name', suffix = c('', '.dis')),
               '\\textsc{Cubes-DC16}\nAll Solutions' = cubes_16_600 %>% mutate(status.dis = 0),
               '\\textsc{Cubes-DC16}\nAll Solutions\n+ Disambiguation' = left_join(cubes_16_600_dis, cubes_16_600_dis_data, by = 'name', suffix = c('', '.dis')), has_dis_data = T) + theme(legend.position = "right")


a <- plot_fuzzy_new_data('\\textsc{Cubes-Seq}\nAll Solutions' = seq_600 %>% mutate(status.dis = 0),
                         '\\textsc{Cubes-Seq}\nAll Solutions\n+ Disambiguation' = left_join(seq_600_dis, seq_600_dis_data, by = 'name', suffix = c('', '.dis')), has_dis_data = T)

a_1 <- a %>% filter(try == "\\textsc{Cubes-Seq}\nAll Solutions")
a_2 <- a %>% filter(try == "\\textsc{Cubes-Seq}\nAll Solutions\n+ Disambiguation")

a_combo <- inner_join(a_1, a_2, by = 'name')
a_filter <- a_combo %>% filter(fuzzy.x != "No solution" & fuzzy.y == "No solution")

a <- inner_join(seq_600 %>% mutate(status.dis = 0), left_join(seq_600_dis, seq_600_dis_data, by = 'name', suffix = c('', '.dis')), by = 'name') %>%
  filter(name == 'kaggle/1_5_T_1' | name == 'kaggle/1_6_X_1') %>%
  select(name, solved.x, solved.y, status.x, status.y, fuzzy.x, fuzzy.y, status.dis.x, status.dis.y)


plot_pdf("fuzzy", 1.7, 0.75,
         plot_fuzzy_new('\\textsc{Squares}' = squares,
                        '\\textsc{Scythe}' = scythe,
                        '\\textsc{PatSQL}' = patsql,
                        '\\textsc{Cubes-Seq}' = seq,
                        '\\textsc{Cubes-DC16}\\textsuperscript{\\textdagger}' = cubes_16,
                        '\\textsc{Cubes-Seq}\nAll Solutions' = seq_600,
                        '\\textsc{Cubes-DC16}\\textsuperscript{\\textdagger}\nAll Solutions' = cubes_16_600))

plot_emf("fuzzy", 6, 3.375,
         plot_fuzzy_new('Squares' = squares,
                        'Scythe' = scythe,
                        'PatSQL' = patsql,
                        'Cubes-Seq' = seq_600,
                        'Cubes-DC16' = cubes_16_600, bar_width = 0.8, bar_position = "fill", use_percentage = T))


a <- compute_fuzzy_table('\\textsc{Squares}' = squares,
                         '\\textsc{Scythe}' = scythe,
                         '\\textsc{PatSQL}' = patsql,
                         '\\textsc{Cubes-Seq}' = seq,
                         '\\textsc{Cubes-DC16}\\textsuperscript{\\textdagger}' = cubes_16,
                         '\\textsc{Cubes-Seq}\nAll Solutions' = seq_600,
                         '\\textsc{Cubes-DC16}\\textsuperscript{\\textdagger}\nAll Solutions' = cubes_16_600)

a <- compute_fuzzy_table(
  'cubes seq' = seq,
  'cubes seq all600 dis' = seq_600_dis,
  'cubes dc16' = cubes_16,
  'cubes dc16 all600 dis' = cubes_16_600_dis
)

plot_pdf("disambiguation", 1.6, 0.75,
         plot_fuzzy_new('\\textsc{Cubes-Seq}\nAll Solutions' = seq_600 %>% mutate(status.dis = 0),
                        '\\textsc{Cubes-Seq}\nAll Solutions\n+ Disambiguation' = left_join(seq_600_dis, seq_600_dis_data, by = 'name', suffix = c('', '.dis')),
                        '\\textsc{Cubes-DC16}\nAll Solutions' = cubes_16_600 %>% mutate(status.dis = 0),
                        '\\textsc{Cubes-DC16}\nAll Solutions\n+ Disambiguation' = left_join(cubes_16_600_dis, cubes_16_600_dis_data, by = 'name', suffix = c('', '.dis')), has_dis_data = T) + theme(legend.position = "right"))

plot_emf("disambiguation", 6, 3.375,
         plot_fuzzy_new('Cubes-Seq' = seq_600 %>% mutate(status.dis = 0),
                        'Cubes-Seq\n+ Disambiguation' = left_join(seq_600_dis, seq_600_dis_data, by = 'name', suffix = c('', '.dis')),
                        'Cubes-DC16' = cubes_16_600 %>% mutate(status.dis = 0),
                        'Cubes-DC16\n+ Disambiguation' = left_join(cubes_16_600_dis, cubes_16_600_dis_data, by = 'name', suffix = c('', '.dis')),
                        has_dis_data = T, bar_width = 0.8, bar_position = "fill", use_percentage = T))


plot_pdf('speedup', 1.2, .55,
         speedup(seq, cubes_16))


c62_16_all600_dis_simple %>% ggplot(aes(x = n_questions)) + geom_histogram()
cubes_600_dis_data %>% ggplot(aes(x = total_queries)) + geom_histogram()
c62_16_all600_dis_simple %>% ggplot(aes(x = total_queries)) + geom_histogram()

questions_by_queries_plot('\\textsc{Cubes-Seq}' = seq_600_dis_data, '\\textsc{Cubes-DC16}' = cubes_16_600_dis_data)
questions_by_queries_plot(cubes_16_600_dis_data)

p1 <- questions_by_queries_plot('\\textsc{Cubes-Seq}' = seq_600_dis_data,
                                '\\textsc{Cubes-DC16}' = cubes_16_600_dis_data) +
  labs(x = NULL)

p2 <- questions_by_queries_plot_no_cats('\\textsc{Cubes-Seq}' = seq_600_dis_data,
                                        '\\textsc{Cubes-DC16}' = cubes_16_600_dis_data) +
  labs(x = NULL, y = NULL)

fig <- ggarrange(p1, p2, common.legend = T, legend = "right", widths = c(3, 1))


plot_pdf('n_questions', 1.5, .6,
         annotate_figure(fig, bottom = text_grob("Number of queries pre-disambiguation", size = 9)))

plot_emf("n_questions", 6, 3.375,
         questions_by_queries_plot('Cubes-Seq' = seq_600_dis_data,
                                   'Cubes-DC16' = cubes_16_600_dis_data))


benchmark_summary('extended_abstract_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{PatSQL}' = patsql,
                  '\\textsc{Libra}' = libra,
                  '\\textsc{Cubes-Seq}' = seq,
                  '\\textsc{Cubes-DC4}' = cubes_4,
                  '\\textsc{Cubes-DC8}' = cubes_8,
                  '\\textsc{Cubes-DC16}' = cubes_16,
                  '\\textsc{Cubes-DC32}' = cubes_32,
                  'vbs seq' = vbs(squares, scythe, patsql, libra, seq)
)

benchmark_summary('extended_abstract_summary_4',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{PatSQL}' = patsql,
                  '\\textsc{Libra}' = libra,
                  '\\textsc{Cubes-Seq}' = seq,
                  '\\textsc{Cubes-DC4}' = cubes_4,
                  '\\textsc{Cubes-DC8}' = cubes_8,
                  '\\textsc{Cubes-DC16}' = cubes_16
)

benchmark_summary('extended_abstract_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{PatSQL}' = patsql,
                  '\\textsc{Cubes-Seq}' = cubes,
                  'vbs' = vbs(squares, scythe, patsql, cubes),
                  'vbs_2' = vbs(patsql, cubes),
                  '\\textsc{Cubes-DC4}' = c62_4_full,
                  '\\textsc{Cubes-DC8}' = c62_8_full,
                  '\\textsc{Cubes-DC16}' = c62_16_full
)

benchmark_summary('extended_abstract_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{PatSQL}' = patsql,
                  '\\textsc{Cubes-Seq}' = cubes,
                  'vbs' = vbs(squares, scythe, patsql, cubes),
                  'vbs_2' = vbs(patsql, cubes),
                  '\\textsc{Cubes-DC4}' = c62_4_full,
                  '\\textsc{Cubes-DC8}' = c62_8_full,
                  '\\textsc{Cubes-DC16}' = c62_16_full
)

invsolved('\\textsc{Cubes-DC16} (Default)' = cubes_16,
          'No Invalid Program Pruning' = cubes_16_no_bitvec,
          'No Split Program Space' = cubes_16_no_split,
          'Random Cube Generation' = cubes_16_random,
          "No extra score update" = cubes_16_no_extra_score_update
)

benchmark_summary('extended_abstract_summary_2',
                  '\\textsc{Cubes-DC16} (Default)' = cubes_16,
                  'No Invalid Program Pruning' = cubes_16_no_bitvec,
                  'No Split Program Space' = cubes_16_no_split,
                  'Random Cube Generation' = cubes_16_random,
                  "No extra score update" = cubes_16_no_extra_score_update
)

benchmark_summary('extended_abstract_summary_2',
                  '\\textsc{Cubes-DC16} (Default)' = cubes_16,
                  'No Invalid Program Pruning' = cubes_16_no_bitvec,
                  'No Split Program Space' = cubes_16_no_split,
                  'Random Cube Generation' = cubes_16_random
)

benchmark_summary('extended_abstract_summary_3',
                  '\\textsc{Cubes-DC32} (Default)' = cubes_32,
                  'No Invalid Program Pruning' = cubes_32_no_bitvec,
                  'No Split Program Space' = cubes_32_no_split,
                  'Random Cube Generation' = cubes_32_random
)

# number of instances per benchmark
c62_16_full %>% group_by(benchmark) %>% count()

# solved_not_solveds
solved_not_solved(patsql, s62)
solved_not_solved(s62, patsql)

#n_questions
mean(c62_16_all600$solutions)
median(c62_16_all600$solutions)

c62_16_all600_dis_data <- read_csv('fuzzy/c62_16_all600_dis.csv')

c62_16_all600_dis_data %>%
  filter(n_questions > 0) %>%
  summarise(a = mean(n_questions))
c62_16_all600_dis_data %>%
  filter(n_questions > 0) %>%
  summarise(a = median(n_questions))


plot_pdf("solved_cells", 1.5, 1,
         plot_solved(x_expr = input_cells,
                     "\\textsc{Squares}" = squares,
                     '\\textsc{Scythe}' = scythe,
                     "\\textsc{PatSQL}" = patsql,
                     # "\\textsc{Libra}" = libra,
                     "\\textsc{Cubes-Seq}" = seq,
                     "\\textsc{Cubes-DC16}" = cubes_16,
                     x_lab = "Number of Cells in Input Tables",
                     y_lab = "Percentage of Instances"))

plot_pdf("solved_output_cells", 1.5, 1,
         plot_solved(x_expr = output_cells,
                     "\\textsc{Squares}" = squares,
                     '\\textsc{Scythe}' = scythe,
                     "\\textsc{PatSQL}" = patsql,
                     # "\\textsc{Libra}" = libra,
                     "\\textsc{Cubes-Seq}" = seq,
                     "\\textsc{Cubes-DC16}" = cubes_16,
                     x_lab = "Number of Cells in Output Table",
                     y_lab = "Percentage of Instances"))

plot_pdf("solved_cols_per_table", 1.5, 1,
         plot_solved(x_expr = input_cols / tables,
                     "\\textsc{Squares}" = squares,
                     '\\textsc{Scythe}' = scythe,
                     "\\textsc{PatSQL}" = patsql,
                     # "\\textsc{Libra}" = libra,
                     "\\textsc{Cubes-Seq}" = seq,
                     "\\textsc{Cubes-DC16}" = cubes_16,
                     x_lab = "Average Columns per Table",
                     y_lab = "Percentage of Instances"))


# ORIGINAL PAPER

c62_16_full %>% count(benchmark)

invsolved(every_other = 150, point_size = 1, step_size = .75, use_vbs = T,
          '\\textsc{Squares}' = squares,
          '\\textsc{Scythe}' = scythe,
          '\\textsc{PatSQL}' = patsql,
          '\\textsc{Cubes-Seq}' = s62) + theme(legend.position = "right")

benchmark_summary('vldv_summary',
                  "\\textsc{Squares}" = squares,
                  '\\textsc{Scythe}' = scythe,
                  '\\textsc{PatSQL}' = patsql,
                  '\\textsc{Cubes-Seq}' = s62,
                  '\\textsc{Cubes-DC4}' = c62_4_full,
                  '\\textsc{Cubes-DC8}' = c62_8_full,
                  '\\textsc{Cubes-DC16}' = c62_16_full
)

speedup(seq, cubes_4)
speedup(seq, cubes_8)
speedup(seq, cubes_16)
speedup(seq, cubes_32)
speedup_data(seq, cubes_4)
speedup_data(seq, cubes_8)
speedup_data(seq, cubes_16)
speedup_data(seq, cubes_32)

benchmark_summary('vldv_summary_ablation',
                  'default' = c62_16_full,
                  'inv prog' = c62_16_full_no_bitvec,
                  'no split' = c62_16_full_no_split,
                  'random' = c62_16_full_random
)

cubes_16_n10 %>% group_by(benchmark) %>% count()
cubes_16 %>%
  mutate(a = n() / 500) %>%
  group_by(benchmark) %>%
  summarise(a = n() / first(a))

cubes_n10_500 <- cubes_16_n10 %>% select(name)

load_result_file(file)

non_determinism_plot(cubes_16_n10)

kaggle_missing <- cubes_16 %>%
  anti_join(cubes_16_n10, by = 'name') %>%
  filter(benchmark == 'kaggle') %>%
  sample_n(4)
trp_missing <- cubes_16 %>%
  anti_join(cubes_16_n10, by = 'name') %>%
  filter(benchmark == 'scythe/top-rated-posts') %>%
  sample_n(1)
spider_missing <- cubes_16 %>%
  anti_join(cubes_16_n10, by = 'name') %>%
  filter(benchmark == 'spider') %>%
  sample_n(6)

missings <- bind_rows(kaggle_missing, trp_missing, spider_missing) %>% select(name)

cubes_16_600_dis_data %>%
  filter(status == 0) %>%
  summarise(n = mean(n_questions))
cubes_16_600_dis_data %>%
  filter(status == 0) %>%
  summarise(n = median(n_questions))
cubes_16_600_dis_data %>%
  filter(status == 0) %>%
  summarise(n = max(n_questions))
cubes_16_600_dis_data %>%
  filter(status == 0) %>%
  summarise(n = min(n_questions))

mean(c62_16_all600_dis_simple$n_questions)
median(c62_16_all600_dis_simple$n_questions)
mean(c62_16_all600_dis_simple$n_questions)

instance_info %>%
  group_by(benchmark) %>%
  summarise(m = mean(input_cells))
instance_info %>%
  group_by(benchmark) %>%
  summarise(m = median(input_cells))
instance_info %>%
  group_by(benchmark) %>%
  summarise(m = mean(tables))
instance_info %>%
  group_by(benchmark) %>%
  summarise(m = min(tables))
instance_info %>%
  group_by(benchmark) %>%
  summarise(m = max(tables))

cubes_16_600 %>%
  filter(fuzzy == 'Possibly Correct' |
           fuzzy == 'Possibly Correct Top 5' |
           fuzzy == 'Possibly Correct Any') %>%
  count() / cubes_16_600 %>% count()
seq %>%
  filter(fuzzy == 'Possibly Correct' |
           fuzzy == 'Possibly Correct Top 5' |
           fuzzy == 'Possibly Correct Any') %>%
  count() / seq %>% count()

# CAT PLOTS

textwidth <- 6.30045

ibm_colors <- c("#fe6100", "#dc267f", "#785ef0", "#648fff", "#ffb000")

ibm_palette <- function(n) {
  if (missing(n)) {
    n <- length(ibm_colors)
  }
  if (n <= length(ibm_colors)) {
    colors <- ibm_colors[1:n]
  } else {
    colors <- grDevices::colorRampPalette(ibm_colors)(n)
  }

  structure(colors, name = "ibm_colors", class = "palette")
}

scale_fill_ibm <- function(...) {
  discrete_scale(aesthetics = 'fill', scale_name = 'ibm_scale', palette = ibm_palette, na.value = '#000000')
}

scale_color_ibm <- function(...) {
  discrete_scale(aesthetics = 'color', scale_name = 'ibm_scale', palette = ibm_palette, na.value = '#000000')
}


plot_pdf('seq_solved', 0.8, 0.35,
         invsolved(every_other = 150, point_size = 1, step_size = .75, use_vbs = T,
                   '\\textsc{Squares}' = squares,
                   '\\textsc{Scythe}' = scythe,
                   '\\textsc{PatSQL}' = patsql,
                   '\\textsc{Cubes-Seq}' = seq, use_ibm = T) +
           theme(legend.position = "right"))

plot_pdf('speedup', 0.7, .3,
         speedup(seq, cubes_16) + scale_fill_ibm())

plot_pdf('n_questions', 0.7, .3,
         questions_by_queries_plot('\\textsc{Cubes-Seq}' = seq_600_dis_data,
                                   '\\textsc{Cubes-DC16}' = cubes_16_600_dis_data) +
           scale_fill_ibm() +
           theme(legend.position = "right"))

plot_pdf("fuzzy", 1, 0.45,
         plot_fuzzy_new('\\textsc{Squares}' = squares,
                        '\\textsc{Scythe}' = scythe,
                        '\\textsc{PatSQL}' = patsql,
                        '\\textsc{Cubes-Seq}' = seq,
                        '\\textsc{Cubes-DC16}\\textsuperscript{\\textdagger}' = cubes_16,
                        '\\textsc{Cubes-Seq}\nAll Solutions' = seq_600,
                        '\\textsc{Cubes-DC16}\\textsuperscript{\\textdagger}\nAll Solutions' = cubes_16_600, use_ibm = T))

plot_pdf("disambiguation", 0.95, 0.35,
         plot_fuzzy_new('\\textsc{Cubes-Seq}\nAll Solutions' = seq_600 %>% mutate(status.dis = 0),
                        '\\textsc{Cubes-Seq}\nAll Solutions\n+ Disambiguation' = left_join(seq_600_dis, seq_600_dis_data, by = 'name', suffix = c('', '.dis')),
                        '\\textsc{Cubes-DC16}\nAll Solutions' = cubes_16_600 %>% mutate(status.dis = 0),
                        '\\textsc{Cubes-DC16}\nAll Solutions\n+ Disambiguation' = left_join(cubes_16_600_dis, cubes_16_600_dis_data, by = 'name', suffix = c('', '.dis')), has_dis_data = T, use_ibm = T) + theme(legend.position = "right"))


cubes_4 %>% ggplot(aes(x = ram / 1024)) + geom_histogram()

cubes_4 %>% count()
cubes_4 %>%
  filter(ram > 8 * 1024 * 1024) %>%
  count()
cubes_4 %>%
  filter(ram > 8 * 1024 * 1024) %>%
  count() / cubes_4 %>% count()
cubes_16 %>%
  filter(ram > 16 * 1024 * 1024) %>%
  count()

set1 <- c("textbook/1", "textbook/2", "textbook/3")
set2 <- (cubes_16 %>% select(name) %>% sample_n(50))$name
seq %>%
  filter(name %in% set2) %>%
  summarise(t = sum(real) / 60)
cubes_4 %>%
  filter(name %in% set2) %>%
  summarise(t = sum(real) / 60)
length(set2) * 600 / 60