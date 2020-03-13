library(ggplot2)
library(stringr)
library(dplyr)

setwd('./data-treatment')

status_meanings <- c('R & SQL', 'Fail or Just R', 'Just R', 'Timeout')

original <- read.csv('squares3.csv', header=T, stringsAsFactors=FALSE)
original_cubes_n <- read.csv('original_cubes_n.csv', header=T, stringsAsFactors=FALSE)

qffd_r_n <- read.csv('qffd_r_n.csv', header=T, stringsAsFactors=FALSE)

t1 <- read.csv('try1.csv', header=T, stringsAsFactors=FALSE)
t1_n <- read.csv('try1_n.csv', header=T, stringsAsFactors=FALSE)
t2 <- read.csv('try2.csv', header=T, stringsAsFactors=FALSE)
t2_n <- read.csv('try2_n.csv', header=T, stringsAsFactors=FALSE)
t3 <- read.csv('try3.csv', header=T, stringsAsFactors=FALSE)
t3_n <- read.csv('try3_n.csv', header=T, stringsAsFactors=FALSE)
t4 <- read.csv('try4.csv', header=T, stringsAsFactors=FALSE)
t4_n <- read.csv('try4_n.csv', header=T, stringsAsFactors=FALSE)
t5 <- read.csv('try5.csv', header=T, stringsAsFactors=FALSE)
t5_n <- read.csv('try5_n.csv', header=T, stringsAsFactors=FALSE)
t6 <- read.csv('try6.csv', header=T, stringsAsFactors=FALSE)
t6_n <- read.csv('try6_n.csv', header=T, stringsAsFactors=FALSE)
t7 <- read.csv('try7.csv', header=T, stringsAsFactors=FALSE)
t7_n <- read.csv('try7_n.csv', header=T, stringsAsFactors=FALSE)

t9 <- read.csv('try9.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, '55-tests'))

# times scatter plot
A <- 'qffd_r_n';B <- 't6_n'; merge(eval(parse(text = A)), eval(parse(text = B)), by='name', suffixes = c("_A", "_B")) %>% filter(timeout_A == 'False' | timeout_B == 'False') %>% ggplot(aes(x=real_A, y=real_B)) + geom_point(color='red', alpha = 0.4, size=2) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_abline() + geom_hline(yintercept=600, linetype="dashed") + geom_vline(xintercept=600, linetype="dashed") + labs(y=B, x = A) + ggtitle('Real Time')

# process distribution
ggplot(t9, aes(x=factor(process))) + geom_bar(fill="turquoise")

#solve time distribution
ggplot(t9, aes(x=real)) + geom_histogram()

# solved instances by try
tries <- list('original' = original, 't1' = t1, 't2' = t2, 't3' = t3, 't4' = t4, 't5' = t5, 't6' = t6, 't7' = t7); solved <- bind_rows(tries, .id='try'); results <- factor(solved$status, labels=status_meanings, exclude = NULL); ggplot(solved, aes(x=try, fill=results)) + geom_bar(position="dodge") + scale_fill_brewer(palette = 'Dark2')
tries <- list('original' = original, 'qffd' = qffd_r_n, 't1' = t1_n, 't2' = t2_n, 't3' = t3_n, 't4' = t4_n, 't5' = t5_n, 't6' = t6_n, 't7' = t7_n); solved <- bind_rows(tries, .id='try'); results <- factor(solved$status, labels=status_meanings, exclude = NULL); ggplot(solved, aes(x=try, fill=results)) + geom_bar(position="dodge") + scale_fill_brewer(palette = 'Dark2')
tries <- list('original (1)' = original, 't6 (6)' = t6, 'qffd_r_n (1)' = qffd_r_n, 't6_n (6)' = t6_n, 't9 (8)' = t9); solved <- bind_rows(tries, .id='try'); results <- factor(solved$status, labels=status_meanings, exclude = NULL); ggplot(solved, aes(x=try, fill=results)) + geom_bar(position="dodge") + scale_fill_brewer(palette = 'Dark2')
