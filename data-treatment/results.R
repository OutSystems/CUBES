library(ggplot2)
library(stringr)

original <- read.csv('orig.csv', header=T, stringsAsFactors=FALSE)
original$name <- str_replace(original$name, '.in', '')
original_det <- read.csv('orig_det.csv', header=T, stringsAsFactors=FALSE)
original_det$name <- str_replace(original_det$name, '.in', '')

qffd <- read.csv('qffd.csv', header=T, stringsAsFactors=FALSE)
qffd_r <- read.csv('qffd-random.csv', header=T, stringsAsFactors=FALSE)
t1 <- read.csv('try1.csv', header=T, stringsAsFactors=FALSE)
t2 <- read.csv('try2.csv', header=T, stringsAsFactors=FALSE)
t3 <- read.csv('try3.csv', header=T, stringsAsFactors=FALSE)
t4 <- read.csv('try4.csv', header=T, stringsAsFactors=FALSE)
t5 <- read.csv('try5.csv', header=T, stringsAsFactors=FALSE)

A = 'original'
B = 't2'

tmp <- merge(eval(parse(text = A)), eval(parse(text = B)), by='name', suffixes = c("_original","_new"))
ggplot(tmp, aes(x=real_original, y=real_new)) + geom_point(color='red',alpha = 0.2,size=2) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_abline() + geom_hline(yintercept=900, linetype="dashed") + geom_vline(xintercept=900, linetype="dashed") + labs(y=B, x = A) + ggtitle('Real Time')

