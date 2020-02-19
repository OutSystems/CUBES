library(ggplot2)
library(stringr)
library(dplyr)

status_meanings = c('Success', 'Error', 'No SQL')

original <- read.csv('orig.csv', header=T, stringsAsFactors=FALSE)

qffd <- read.csv('qffd.csv', header=T, stringsAsFactors=FALSE)
qffd_r <- read.csv('qffd-random.csv', header=T, stringsAsFactors=FALSE)
t1 <- read.csv('try1.csv', header=T, stringsAsFactors=FALSE)
t2 <- read.csv('try2.csv', header=T, stringsAsFactors=FALSE)
t3 <- read.csv('try3.csv', header=T, stringsAsFactors=FALSE)
t4 <- read.csv('try4.csv', header=T, stringsAsFactors=FALSE)
t5 <- read.csv('try5.csv', header=T, stringsAsFactors=FALSE)
t6 <- read.csv('try6.csv', header=T, stringsAsFactors=FALSE)
t6_1 <- read.csv('try6_1.csv', header=T, stringsAsFactors=FALSE)

# times scatter plot
A = 't6';B = 't6_1';tmp <- merge(eval(parse(text = A)), eval(parse(text = B)), by='name', suffixes = c("_A","_B"));ggplot(tmp, aes(x=real_A, y=real_B)) + geom_point(color='red',alpha = 0.4,size=2) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_abline() + geom_hline(yintercept=600, linetype="dashed") + geom_vline(xintercept=600, linetype="dashed") + labs(y=B, x = A) + ggtitle('Real Time')

# process distribution
ggplot(t6, aes(x=factor(process))) + geom_bar(fill="turquoise")

# solved instances by try
tries = list('original' = original, 't1' = t1, 't2' = t2, 't3' = t3, 't4' = t4, 't5' = t5, 't6' = t6, 't6_1' = t6_1); solved = bind_rows(tries, .id='try'); results = factor(solved$status, labels=status_meanings); ggplot(solved, aes(x=try, fill=results)) + geom_bar(position="dodge")
