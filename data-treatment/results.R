library(ggplot2)
library(stringr)

original <- read.csv('~/repos/SQUARES/results_A.csv', header=F, col.names = c('name', 'real', 'system', 'user', 'ram'), stringsAsFactors=FALSE)
original$name <- str_replace(original$name, 'tests/', 'cp19/')
original$name <- str_replace(original$name, '.in', '')
original$real[original$real == -1] <- 600

t1 <- read.csv('try1.csv', header=F, col.names = c('name', 'real', 'system', 'user', 'ram'), stringsAsFactors=FALSE)
t1$name <- str_replace(t1$name, 'tests/', '')
t1$name <- str_replace(t1$name, '.yaml', '')
t1$real[t1$real == -1] <- 600

t2 <- read.csv('try2.csv', stringsAsFactors=FALSE)
t2$name <- str_replace(t2$name, 'tests/', '')
t2$name <- str_replace(t2$name, '.yaml', '')

t3 <- read.csv('try3.csv', stringsAsFactors=FALSE)
t3$name <- str_replace(t3$name, 'tests/', '')
t3$name <- str_replace(t3$name, '.yaml', '')

t4 <- read.csv('try4.csv', stringsAsFactors=FALSE)
t4$name <- str_replace(t4$name, 'tests/', '')
t4$name <- str_replace(t4$name, '.yaml', '')

tmp <- merge(t2, t4, by='name', suffixes = c("_original","_new"))
ggplot(tmp, aes(x=real_original, y=real_new)) + geom_point(color='red',alpha = 0.2,size=2) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_abline() + geom_hline(yintercept=600, linetype="dashed") + geom_vline(xintercept=600, linetype="dashed")
