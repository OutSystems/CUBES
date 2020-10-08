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
  my_theme <- theme_bw() + theme(legend.position = "bottom", legend.title = element_blank())
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
  textwidth <- 4.8041
  my_theme <- theme_bw() +
    theme(legend.position = "bottom", legend.title = element_blank()) +
    theme(text = element_text(size = 8))
}

source('loading.R')
source('plots.R')
source('tables.R')

scatter('squares' = squares, '16T' = c49_16_0f_c)
scatter('scythe' = scythe, '16T' = c49_16_0f_c)
scatter('4T' = c49_4_0f_c, '16T' = c49_16_0f_c)
scatter('bit' = bitenum, 'nobit' = bitenum_nobit)
scatter('bit' = bitenum, 'nosub' = bitenum_nosub)

cumsolved(use_vbs = F, 'bit' = bitenum, 'nobit' = bitenum_nobit, 'nosub' = bitenum_nosub, 'bit + nosub' = vbs(bitenum, bitenum_nosub))
cumsolved(use_vbs = F, 'bit' = bitenum, 'nosub' = bitenum_nosub, 'bit + nosub' = vbs(bitenum, bitenum_nosub))

times(squares)
times(squares, exclude_timeouts = T)
times(scythe, exclude_timeouts = T)
times(c49_16_0f_c)

scatter_ram('squares' = squares, '16T' = c49_16_0f_c_static)
scatter_ram('1T' = bitenum, '16T' = c49_16_0f_c)
scatter_ram('scythe' = scythe, '16T' = c49_16_0f_c)

cumsolved(use_vbs = F, 'squares' = squares, 'spider' = scythe, 'bitenum' = bitenum, '16T' = c49_16_0f_c)
cumsolved(use_vbs = F, exclude = c('spider'), 'squares' = squares, 'spider' = scythe, 'bitenum' = bitenum, '16T' = c49_16_0f_c)

b <- inner_join(bitenum_spider, c49_16_0f_c_spider, by = 'name') %>% filter(solved.x & !solved.y)
c <- inner_join(bitenum_spider, c49_16_0f_c_spider, by = 'name') %>% filter(!solved.x & solved.y)

squares %>% count()
squares %>% filter(solved) %>% count()
squares %>%
  filter(real <= 10) %>%
  filter(solved) %>%
  count() / squares %>% filter(solved) %>% count()
squares %>%
  filter(real <= 10) %>%
  filter(solved) %>%
  count()
squares %>%
  filter(solved) %>%
  summarise(avg = mean(ram))

scythe %>%
  inner_join(squares, by = 'name') %>%
  filter(solved.x & solved.y) %>%
  count()

scythe %>% count()
scythe %>% filter(solved) %>% count()
scythe %>%
  filter(real <= 10) %>%
  filter(solved) %>%
  count() / scythe %>% filter(solved) %>% count()
scythe %>%
  filter(real <= 10) %>%
  filter(solved) %>%
  count()

squares_scythe <- vbs(squares, scythe) %>% ungroup()

squares_scythe %>% filter(solved) %>% count()

bitenum %>% count()
bitenum %>% filter(solved) %>% count()
bitenum %>% filter(solved) %>% count() / bitenum %>% count()
bitenum %>%
  filter(real <= 10) %>%
  filter(solved) %>%
  count()

bitenum_nobit %>% filter(solved) %>% count() / bitenum_nobit %>% count()
bitenum %>%
  inner_join(bitenum_nosub, by = 'name') %>%
  filter(solved.x & !solved.y) %>%
  count()
bitenum %>%
  inner_join(bitenum_nosub, by = 'name') %>%
  filter(solved.y & !solved.x) %>%
  count()
bitenum_nosub %>% filter(solved) %>% count() / bitenum_nosub %>% count()

bitenum_nobit %>% count()
bitenum_nofd %>% count()

bitenum %>%
  inner_join(squares, by = 'name') %>%
  filter(solved.x) %>%
  mutate(ram.x = ram.x / 1024 / 1024) %>%
  arrange(-ram.x) %>%
  select(name, ram.x)

c49_16_0f_c %>% count()
c49_16_0f_c %>% filter(solved) %>% count() / c49_16_0f_c %>% count()
c49_16_0f_c %>%
  filter(real <= 10) %>%
  filter(solved) %>%
  count() / c49_16_0f_c %>% count()

squares %>%
  filter(solved) %>%
  summarise(a = mean(real))
scythe %>%
  filter(solved) %>%
  summarise(a = mean(real))
c49_16_0f_c %>%
  filter(solved) %>%
  summarise(a = mean(real))

solved_instances(c47_16_0f_c_spider) %>% arrange(percentage)
solved_instances(c48_16_0f_c_spider) %>% arrange(percentage)
solved_instances(c49_16_0f_c_spider) %>% arrange(percentage)

fails(bit = bitenum, nobit = bitenum_nobit)

bars(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)
bars(use_vbs = T, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)

scythe %>%
  filter(benchmark == 'outsystems') %>%
  mutate(ram = ram / 1024 / 1024)
scythe %>%
  mutate(ram = ram / 1024 / 1024) %>%
  arrange(-ram)

bars(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '4T' = c49_4_0f_c, '16T' = c49_16_0f_c)
bars(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '4T' = c49_4_0f_c, '8T' = c49_8_0f_c, '16T' = c49_16_0f_c_static)
boxplot(func = any, '16T' = c49_16_0f_c, '16T static' = c49_16_0f_c_static)

boxplot(func = any, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '4T' = c49_4_0f_c, '16T' = c49_16_0f_c)

boxplot_fails(func = all, 'bit' = bitenum, 'nobit' = bitenum_nobit)
bitenum %>% summarise(a = median(fails, na.rm = T))
bitenum %>% summarise(a = mean(fails, na.rm = T))
bitenum_nobit %>% summarise(a = median(fails, na.rm = T))
bitenum_nobit %>% summarise(a = mean(fails, na.rm = T))

c49_8_0f_c %>% count()

c43_16_0f_c_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c45_16_0f_c_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c47_16_0f_c_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c45_16_0f_c_dsl_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')
c46_16_0f_c_cube_5 %>% ggplot(aes(x = solution_n)) +
  geom_bar() +
  scale_x_continuous(n.breaks = 6) +
  labs(x = 'Distinct Solutions')

######   FINAL PLOTS   ######

tikz(file = "plot.tex", width = textwidth / 2, height = textwidth / 2, standAlone = T)
tikz(file = "plot.tex", width = textwidth, height = textwidth * 3 / 5, standAlone = T)
dev.off()

scatter('\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)
scatter(exclude = c('spider'), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)
scatter_ram('\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)

invsolved(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)
invsolved(use_vbs = F, log = T, legend.position = c(.25, .9), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)
invsolved(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Squares} + \\textsc{Scythe}' = vbs(squares, scythe))
invsolved(use_vbs = F, log = T, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Squares} + \\textsc{Scythe}' = vbs(squares, scythe))
invsolved(use_vbs = F, exclude = c('spider'), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)
invsolved(use_vbs = F, exclude = c('spider', '55-tests', 'leetcode', 'scythe/recent-posts', 'scythe/top-rated-posts', 'textbook'), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)
invsolved(use_vbs = F, exclude = c('spider', '55-tests', 'leetcode', 'scythe/recent-posts', 'scythe/top-rated-posts', 'outsystems'), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe)

scatter('\\textsc{Cubes-Seq}' = bitenum, '\\textsc{Squares}' = squares)
scatter('\\textsc{Cubes-Seq}' = bitenum, '\\textsc{Scythe}' = scythe)
scatter(exclude = c('spider'), '\\textsc{Cubes-Seq}' = bitenum, '\\textsc{Squares}' = squares)
scatter_ram('\\textsc{Cubes-Seq}' = bitenum, '\\textsc{Squares}' = squares)

invsolved(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum)
bars(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum)
invsolved(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Squares} + \\textsc{Scythe}' = vbs(squares, scythe), '\\textsc{Cubes-Seq}' = bitenum)
invsolved(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Squares} + \\textsc{Scythe}' = vbs(squares, scythe), '\\textsc{Cubes-Seq}' = bitenum)
invsolved(use_vbs = F, 'SQUARES' = squares, 'Scythe' = scythe, 'SQUARES + Scythe' = vbs(squares, scythe), '\\textsc{Cubes-Seq}' = bitenum, 'Sequential + SQUARES' = vbs(squares, bitenum), 'Sequential + Scythe' = vbs(scythe, bitenum))
invsolved(use_vbs = F, log = T, 'SQUARES' = squares, 'Scythe' = scythe, 'SQUARES + Scythe' = vbs(squares, scythe), '\\textsc{Cubes-Seq}' = bitenum, 'Sequential + SQUARES' = vbs(squares, bitenum), 'Sequential + Scythe' = vbs(scythe, bitenum))
invsolved(use_vbs = F, exclude = c('spider'), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum)

scatter('\\textsc{Cubes-Seq}' = bitenum, 'No column annotations' = bitenum_nobit)
scatter('\\textsc{Cubes-Seq}' = bitenum, 'No learning' = bitenum_nosub)
scatter('\\textsc{Cubes-Seq}' = bitenum, 'No FD' = bitenum_nofd)
scatter('\\textsc{Squares} + \\textsc{Scythe}' = vbs(squares, scythe), 'No FD' = bitenum_nofd)
cumsolved(use_vbs = F, '\\textsc{Cubes-Seq}' = bitenum, 'No column annotations' = bitenum_nobit, 'No learning' = bitenum_nosub, 'No FD' = bitenum_nofd)
invsolved(use_vbs = F, '\\textsc{Cubes-Seq}' = bitenum, 'No column annotations' = bitenum_nobit, 'No learning' = bitenum_nosub, 'No FD' = bitenum_nofd, '\\textsc{Squares}' = squares)
invsolved(use_vbs = F, log = T, '\\textsc{Cubes-Seq}' = bitenum, 'No column annotations' = bitenum_nobit, 'No learning' = bitenum_nosub, 'No FD' = bitenum_nofd, '\\textsc{Squares}' = squares)
bars(use_vbs = F, '\\textsc{Cubes-Seq}' = bitenum, 'No column annotations' = bitenum_nobit, 'No learning' = bitenum_nosub, 'No FD' = bitenum_nofd)

scatter('\\textsc{Squares}' = squares, '16T' = c49_16_0f_c)
scatter('\\textsc{Cubes-Seq}' = bitenum, '16T' = c49_16_0f_c)
scatter(exclude = c('spider'), '\\textsc{Cubes-Seq}' = bitenum, '16T' = c49_16_0f_c)

cumsolved(use_vbs = T, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum)
cumsolved(use_vbs = T, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '4T' = c49_4_0f_c, '16T' = c49_16_0f_c)
cumsolved(use_vbs = F, exclude = c('spider'), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '4T' = c49_4_0f_c, '16T' = c49_16_0f_c)
cumsolved(use_vbs = F, '\\textsc{Cubes-Seq}' = bitenum, 'Sequential + Scythe' = vbs(scythe, bitenum), '4T' = c49_4_0f_c, '4T + Scythe' = vbs(c49_4_0f_c, scythe), '16T' = c49_16_0f_c, '16T + Scythe' = vbs(c49_16_0f_c, scythe))

scatter('bitenum' = bitenum, 'Portfolio 16' = portfolio1)
scatter('\\textsc{Squares} + \\textsc{Scythe}' = vbs(squares, scythe), 'Portfolio 16' = portfolio1)
scatter('Portfolio 16' = portfolio1, 'D&6 16' = c49_16_0f_c)

cumsolved(use_vbs = F, '\\textsc{Cubes-Seq}' = bitenum, 'Sequential + Scythe' = vbs(scythe, bitenum), '4T' = c49_4_0f_c, '4T + Scythe' = vbs(c49_4_0f_c, scythe), '16T' = c49_16_0f_c, '16T + Scythe' = vbs(c49_16_0f_c, scythe), 'Portfolio 16' = portfolio1)
invsolved(use_vbs = F, '\\textsc{Cubes-Seq}' = bitenum, 'Sequential + Scythe' = vbs(scythe, bitenum), '4T' = c49_4_0f_c, '4T + Scythe' = vbs(c49_4_0f_c, scythe), '16T' = c49_16_0f_c, '16T + Scythe' = vbs(c49_16_0f_c, scythe), 'Portfolio 16' = portfolio1)
invsolved(use_vbs = F, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '16T' = c49_16_0f_c, '16T + Scythe' = vbs(c49_16_0f_c, scythe))
invsolved(use_vbs = F, log = T, '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '16T' = c49_16_0f_c, '16T + Scythe' = vbs(c49_16_0f_c, scythe))

invsolved(use_vbs = F, log = T, '\\textsc{Cubes-Seq}' = bitenum, '4T' = c49_4_0f_c, '16T' = c49_16_0f_c)

processes(portfolio1)

inner_join(bitenum, bitenum_nosub, by='name') %>% mutate(x = block.x / block.y) %>% select(name, x) %>% ggplot(aes(x = x)) + geom_histogram() + scale_x_log10(breaks = log_breaks(n=10))
inner_join(bitenum, bitenum_nosub, by='name') %>% mutate(x = enum.x / enum.y) %>% select(name, x) %>% ggplot(aes(x = x)) + geom_histogram() + scale_x_log10(breaks = log_breaks(n=10))
inner_join(bitenum, bitenum_nosub, by='name') %>% select(name, enum.x, enum.y)

invsolved(use_vbs=F, log=T, '\\textsc{Cubes-DC16}' = c49_16_0f_c, 'No force split' = c49_16_0f_c_no_split)
scatter('\\textsc{Cubes-DC16}' = c49_16_0f_c, 'No force split' = c49_16_0f_c_no_split)

######   PAPER PLOTS   ######

tikz(file = "plot.tex", width = textwidth * .49, height = textwidth * .49, standAlone = T)
tikz(file = "plot.tex", width = textwidth * .32, height = textwidth * .32, standAlone = T)
tikz(file = "plot.tex", width = textwidth, height = textwidth * 3 / 5, standAlone = T)
dev.off()
system('xelatex plot.tex')

plot_pdf('seq', .475, .475,
         invsolved(use_vbs = F, log = T, legend.position = c(.275, .875), '\\textsc{Squares}' = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum))
plot_pdf('scatter_squares_seq', .49, .49,
         scatter('\\textsc{Squares}' = squares, '\\textsc{Cubes-Seq}' = bitenum))

plot_pdf('seq_ablation', .475, .475,
         invsolved(use_vbs = F, log = T, legend.position = c(.65, .25), '\\textsc{Cubes-Seq}' = bitenum, 'No column annotations' = bitenum_nobit, 'No learning' = bitenum_nosub, 'No FD' = bitenum_nofd))
plot_pdf('seq_ablation_cols', .32, .32, scatter(text_size = 7, '\\textsc{Cubes-Seq}' = bitenum, 'No column annotations' = bitenum_nobit))
plot_pdf('seq_ablation_learn', .32, .32, scatter(text_size = 7, '\\textsc{Cubes-Seq}' = bitenum, 'No learning' = bitenum_nosub))
plot_pdf('seq_ablation_fd', .32, .32, scatter(text_size = 7, '\\textsc{Cubes-Seq}' = bitenum, 'No FD' = bitenum_nofd))

benchmark_summary('summary', "\\textsc{Squares}" = squares, '\\textsc{Scythe}' = scythe, '\\textsc{Cubes-Seq}' = bitenum, '\\hspace{1em}\\change{No column annot.}' = bitenum_nobit, '\\hspace{1em}No learning' = bitenum_nosub, '\\hspace{1em}No QFFD Theory' = bitenum_nofd, '\\textsc{Cubes-Port16}' = portfolio1, '\\textsc{Cubes-DC4}' = c49_4_0f_c, '\\textsc{Cubes-DC8}' = c49_8_0f_c, '\\textsc{Cubes-DC16}' = c49_16_0f_c, '\\hspace{1em}No force split' = c49_16_0f_c_no_split)