library(rlang)
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
library(ggbeeswarm)
# library(ggpattern)

# library(fitdistrplus)
# library(logspline)

setwd('analysis')

options(tikzDefaultEngine = 'pdftex',
        tikzLatexPackages  = c(
          "\\usepackage{tikz}",
          "\\usepackage[active,tightpage]{preview}",
          "\\PreviewEnvironment{pgfpicture}",
          "\\setlength\\PreviewBorder{0pt}",
          "\\RequirePackage[T1]{fontenc}",
          "\\RequirePackage[tt=false, type1=true]{libertine}",
          "\\RequirePackage[varqu]{zi4}",
          "\\RequirePackage[libertine]{newtxmath}"
        ),
        tikzMetricsDictionary = './metrics_cache_acm',
        standAlone = T)
textwidth <- 3.3374
my_theme <- theme_bw(base_size = 9) +
  theme(legend.position = "bottom",
        legend.title = element_blank(),
        legend.margin = margin(0, 0, 0, 0))

source('loading_esem.R')  # DONT FORGET TO CHECK IF 55-TESTS IS INCLUDED OR NOT
source('plots.R')
source('tables.R')

patsql_8 %>% filter(solved & log_content == '')


scatter(patsql = patsql, patsql_8 = patsql_8)
scatter_ram(patsql = patsql, patsql_8 = patsql_8)
bars(facet = T, patsql = patsql, patsql_8 = patsql_8)
invsolved(patsql = patsql, patsql_8 = patsql_8)

bars(facet = T, use_vbs = F, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
invsolved(use_vbs = F, squares = squares, scythe = scythe, patsql = patsql, patsql_5 = patsql_5, cubes = cubes, cubes600 = cubes_600)

solved_not_solved(patsql, patsql_8)
solved_not_solved(patsql_8, patsql)

instance_info_dist(sql_length)
instance_info_dist(sql_size)
instance_info_dist(tables)
instance_info_dist(total_cols, log_x = T)
instance_info_dist(total_rows, log_x = T)
instance_info_dist(total_cells, log_x = T)
instance_info_dist(sql_hardness, continuous = F, facet = T)
instance_info_dist(output_cells / total_cells)
instance_info_dist(total_cells / tables, log_x = T)
instance_info_dist(log(total_cols / tables))
instance_info_dist(total_cols / tables, log_x = T)
instance_info_dist(total_cols, log_x = T)
instance_info_dist(unique_cols / tables, log_x = T)
instance_info_dist(total_cols / tables)

instance_info_dist(input_cols - unique_cols, log_x = T)
instance_info_dist(solutions, data = cubes_600, log_x = T, density_adjust = .75)
instance_info_dist(total_cols / tables, data = cubes, group_status = T, log_x = T)
instance_info_dist(total_cols / tables, data = scythe, group_status = T, log_x = T)
instance_info_dist(total_cols / tables, data = patsql, group_status = T, log_x = T)
instance_info_dist(total_cols / tables, data = patsql_7, group_status = T, log_x = T)
instance_info_dist(tables, data = cubes, group_status = T)

instance_info %>% ggplot(aes(x = input_cells, y = output_cells)) +
  geom_density2d() +
  geom_point(alpha = .1) +
  scale_y_log10() +
  scale_x_log10()

cubes %>% ggplot(aes(x = empties_percentage, fill = benchmark, color = benchmark)) +
  geom_density(alpha = .1) +
  facet_wrap(~status)

plot_cells(x_expr = total_cells, y_expr = total_bytes, log_y = T, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)

plot_cells(x_expr = real, y_expr = cpu, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes) + geom_abline()

plot_cells(x_expr = total_cells, y_expr = ram, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes) +
  geom_smooth(aes(color = NULL), level = .9999, formula = y ~ x)


plot_cells(x_expr = cpu, y_expr = ram, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)

plot_cells(x_expr = ram, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, patsql_8 = patsql_8, cubes = cubes, cubes600 = cubes_600)

plot_cells(x_expr = empties_percentage, y_expr = real, cubes = cubes)

plot_cells(x_expr = total_cells, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = total_cols / tables, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = unique_cols / total_cols, y_expr = real, log_x = F, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)


plot_cells(x_expr = input_cells, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = output_cells, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = output_cells / total_cells, y_expr = real, log_x = F, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = tables, y_expr = real, log_x = F, beeswarm = T, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = total_cols, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = input_cols, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = output_cols, y_expr = real, log_x = F, beeswarm = T, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = output_cols / total_cols, y_expr = real, log_x = F, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = sql_size, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = sql_length, y_expr = real, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)

plot_cells(x_expr = real, y_expr = ram, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = ram, y_expr = cpu, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = total_cells, y_expr = ram, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)

plot_cells(x_expr = sql_hardness, y_expr = ram, log_x = F, beeswarm = T, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
plot_cells(x_expr = sql_hardness, y_expr = real, log_x = F, beeswarm = T, use_vbs = T, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)

plot_solved(x_expr = input_cells, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes, log_x = T)
plot_solved(x_expr = output_cells, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes, log_x = T)
plot_solved(x_expr = input_cols / tables, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes, log_x = T)
plot_solved(x_expr = sql_length, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes, log_x = T)
plot_solved(x_expr = tables, discrete_x = T, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes, log_x = F)
plot_solved(x_expr = output_cells, squares = squares, scythe = scythe, patsql = patsql, cubes = cubes, log_x = T)


patsql %>% ggplot(aes(x = input_cells, fill = solved)) +
  geom_histogram(position = "fill") +
  scale_x_log10()
cubes %>% ggplot(aes(x = input_cells, fill = solved)) +
  geom_histogram(position = "fill") +
  scale_x_log10()
cubes %>% ggplot(aes(x = input_cols, fill = solved)) +
  geom_histogram(position = "fill") +
  scale_x_log10()
cubes %>% ggplot(aes(x = input_cols / tables, fill = solved)) +
  geom_histogram(position = "fill") +
  scale_x_log10()
scythe %>% ggplot(aes(x = input_cols / tables, fill = solved)) + geom_histogram(position = "fill")
cubes %>% ggplot(aes(x = tables, fill = solved)) +
  geom_histogram(position = "fill") +
  scale_x_log10()

cubes_600_dis_data %>% ggplot(aes(x = total_queries, y = n_questions)) + geom_point()
cubes_600_dis_data %>% ggplot(aes(x = total_queries, y = final_queries_n)) +
  geom_density2d_filled() +
  geom_point(alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10()

cubes %>% ggplot(aes(x = real, y = ram)) +
  geom_point(alpha = 0.1) +
  scale_x_log10() +
  scale_y_log10()

questions_by_queries_plot(cubes_60_dis_data)
questions_by_queries_plot(cubes_600_dis_data)

plot_fuzzy_new(squares = squares,
               scythe = scythe,
               patsql = patsql,
               cubes = cubes,
               cubes_all600 = cubes_600) + facet_wrap(~benchmark, scales = "free")

plot_fuzzy(squares = squares, scythe = scythe, patsql = patsql, patsql_5 = patsql_5, cubes_1st = cubes,
           cubes_all60 = cubes_60, cubes_all60_dis = cubes_60_dis, cubes_all600 = cubes_600, cubes_all600_dis = cubes_600_dis)[[1]]
plot_fuzzy(refactor_2 = T, drop_nones = T, fill_bars = T, squares = squares, scythe = scythe, patsql = patsql, patsql8 = patsql_8, patsql8_5 = patsql_8_5, cubes_1st = cubes, cubes_all60 = cubes_60, cubes_all60_dis = cubes_60_dis, cubes_all600 = cubes_600, cubes_all600_dis = cubes_600_dis)

plot_pdf("solved", 1, .6,
         invsolved(facet = F, use_vbs = T, Scythe = scythe, "\\textsc{Squares}" = squares, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes, every_other = 150))

plot_pdf("solved_bars", 1.9, .75,
         bars(facet = T, use_vbs = F, Scythe = scythe, "\\textsc{Squares}" = squares, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes)[[1]] + theme(legend.position="right") )

plot_pdf("cells", 1.5, .6,
         instance_info_dist(input_cells, log_x = T, y_lab = "Density", x_lab = "Number of Cells in Input Tables") +
           theme(legend.position = 'right'))

plot_pdf("output_cells", 1.5, .6,
         instance_info_dist(output_cells, log_x = T, y_lab = "Density", x_lab = "Number of Cells in Output Table") +
           theme(legend.position = 'right'))

plot_pdf("cols_per_table", 1.5, .6,
         instance_info_dist(input_cols / tables, log_x = T, y_lab = "Density", x_lab = "Average Columns per Table") +
           theme(legend.position = 'right'))
#
# plot_pdf("scatter_ram_real", 1, 1,
#          plot_cells(x_expr = ram * 1024, y_expr = real, "\\textsc{Squares}" = squares, Scythe = scythe, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes, x_lab = "Memory", y_lab = "Time (s)", x_label = label_bytes()))
#
# plot_pdf("scatter_cells_real", 1, 1,
#          plot_cells(x_expr = total_cells, y_expr = real, "\\textsc{Squares}" = squares, Scythe = scythe, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes, x_lab = "Number of Cells", y_lab = "Time (s)"))

plot_pdf("solved_cells", 1, 1,
         plot_solved(x_expr = input_cells, Scythe = scythe, "\\textsc{Squares}" = squares, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes, x_lab = "Number of Cells in Input Tables", y_lab = "Percentage of Instances"))

plot_pdf("solved_output_cells", 1, 1,
         plot_solved(x_expr = output_cells, Scythe = scythe, "\\textsc{Squares}" = squares, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes, x_lab = "Number of Cells in Output Table", y_lab = "Percentage of Instances"))

plot_pdf("solved_cols_per_table", 1, 1,
         plot_solved(x_expr = input_cols / tables, Scythe = scythe, "\\textsc{Squares}" = squares, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes, x_lab = "Average Columns per Table", y_lab = "Percentage of Instances"))

# plot_pdf("hardness", 1, .9,
#          plot_solved(x_expr = sql_hardness, log_x = F, discrete_x = T, Scythe = scythe, "\\textsc{Squares}" = squares, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes, x_lab = "Instance Hardness", y_lab = "Percentage of Instances"))


# plot_fuzzy_new("\\textsc{Squares}" = squares, "Scythe" = scythe, "Scythe (60s)" = scythe_60, "Scythe (top 100)" = scythe_top100, "\\textsc{PatSQL}" = patsql, "\\textsc{PatSQL} (top 5)" = patsql_5, "\\textsc{Cubes}" = cubes, "\\textsc{Cubes} (all 60s)" = cubes_60, "\\textsc{Cubes} (all 600s)" = cubes_600)
plot_pdf("esem_fuzzy", 1.75, 0.6,
         plot_fuzzy_new("Scythe" = scythe,
                        # "Scythe (60s)" = scythe_60,
                        "\\textsc{Squares}" = squares, "\\textsc{PatSQL}" = patsql, "\\textsc{Cubes}" = cubes,
                        # "\\textsc{Cubes} (all 60s)" = cubes_60,
                        "\\textsc{Cubes} (all 600s)" = cubes_600) + theme(legend.position="right"))

plot_pdf("esem_fuzzy_dis", 1, 0.6,
         plot_fuzzy_new(
           # "\\textsc{Cubes} (all 60s)" = cubes_60 %>% mutate(status.dis = 0),
           #              "\\textsc{Cubes} (all 60s) + Dis." = left_join(cubes_60_dis, cubes_60_dis_data, by = 'name', suffix = c('', '.dis')),
           "\\textsc{Cubes} (all 600s)" = cubes_600 %>% mutate(status.dis = 0),
           "After Dis." = left_join(cubes_600_dis, cubes_600_dis_data, by = 'name', suffix = c('', '.dis')),
           has_dis_data = T, short_bars = T) + theme(legend.position="right"))

plot_pdf("dis_questions_asked", .9, .6,
         questions_by_queries_plot(cubes_600_dis_data))

data <- bind_rows(squares = squares, scythe = scythe, patsql = patsql, cubes = cubes, .id = 'try')
vbs_data <- vbs(squares = squares, scythe = scythe, patsql = patsql, cubes = cubes)
patsql_cubes_vbs_data <- vbs(patsql = patsql, cubes = cubes)

scythe %>% group_by(benchmark) %>% count()
scythe %>% count()

instance_info %>%
  filter(benchmark != 'spider' & benchmark != 'kaggle') %>%
  summarise(a = mean(input_cols))
instance_info %>%
  group_by(benchmark) %>%
  summarise(a = mean(input_cols))

data %>%
  group_by(try, solved) %>%
  summarise(c = n()) %>%
  group_by(try) %>%
  summarise(f = c / sum(c))

(vbs_data %>% filter(solved) %>% count()) / (vbs_data %>% count())
(patsql_cubes_vbs_data %>%
  filter(solved) %>%
  count()) / (patsql_cubes_vbs_data %>% count())

scythe %>%
  filter(solved) %>%
  summarise(a = max(input_cells))

scythe %>%
  filter(solved) %>%
  summarise(a = max(output_cells))

data %>%
  filter(solved) %>%
  mutate(a = input_cols / tables) %>%
  select(name, try, a, input_cols, tables) %>%
  arrange(-a)


compute_fuzzy_table(squares = squares,
                    scythe = scythe,
                    patsql = patsql,
                    cubes = seq)

scythe %>%
  filter(fuzzy == "Possibly Correct Top 5") %>%
  count() / scythe %>% count()

cubes_600 %>%
  filter(fuzzy == "Possibly Correct Top 5") %>%
  count() / scythe %>% count()
cubes_600 %>%
  filter(fuzzy == "Possibly Correct Top 5" | fuzzy == "Possibly Correct Any") %>%
  count() / scythe %>% count()

cubes_600 %>%
  filter(fuzzy == "Possibly Correct" |
           fuzzy == "Possibly Correct Top 5" |
           fuzzy == "Possibly Correct Any") %>%
  count() / scythe %>% count()
cubes_600_dis %>%
  filter(fuzzy == "Possibly Correct") %>%
  count() / scythe %>% count()
cubes_600_dis %>%
  filter(fuzzy == "Possibly Correct Top 5") %>%
  count()
cubes_600_dis %>%
  filter(fuzzy == "Possibly Correct Any") %>%
  count()

seq_600_dis_data %>%
  filter(total_queries >= 1 & total_queries <= 10) %>%
  summarise(a = median(n_questions))
seq_600_dis_data %>%
  filter(total_queries >= 11 & total_queries <= 100) %>%
  summarise(a = median(n_questions))
seq_600_dis_data %>%
  filter(total_queries >= 101) %>%
  summarise(a = median(n_questions))

seq_600_dis_data %>%
  filter(total_queries >= 1 & total_queries <= 10) %>%
  summarise(a = mean(n_questions))
seq_600_dis_data %>%
  filter(total_queries >= 11 & total_queries <= 100) %>%
  summarise(a = mean(n_questions))
seq_600_dis_data %>%
  filter(total_queries >= 101) %>%
  summarise(a = mean(n_questions))

seq_600_dis_data %>%
  filter(total_queries >= 11) %>%
  summarise(a = median(n_questions))

seq_600_dis_data %>%
  summarise(a = mean(n_questions))
cubes_16_600_dis_data %>%
  summarise(a = mean(n_questions))

cubes_16_600_dis_data %>%
  filter(n_questions >= 11) %>%
  summarise(a = min(total_queries))
cubes_16_600_dis_data %>%
  filter(total_queries <= 1000) %>%
  summarise(a = mean(n_questions))