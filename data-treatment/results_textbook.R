library(ggplot2)
library(stringr)

original <- read.csv('~/repos/SQUARES/data-treatment/original.csv', stringsAsFactors=FALSE)
original$name <- str_replace(original$name, 'tests/', 'textbook/')
original$name <- str_replace(original$name, '.in', '')

t5 <- read.csv('try5.csv', stringsAsFactors=FALSE)
t5$name <- str_replace(t5$name, 'tests/', '')
t5$name <- str_replace(t5$name, '.yaml', '')

t6 <- read.csv('try6.csv', stringsAsFactors=FALSE)
t6$name <- str_replace(t6$name, 'tests/', '')
t6$name <- str_replace(t6$name, '.yaml', '')

tmp <- merge(original, t1, by='name', suffixes = c("_original","_new"))
ggplot(tmp, aes(x=real_original, y=real_new)) + geom_point(color='red',alpha = 0.2,size=2) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_abline() + geom_hline(yintercept=600, linetype="dashed") + geom_vline(xintercept=600, linetype="dashed")
S