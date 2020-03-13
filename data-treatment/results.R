library(ggplot2)
library(stringr)
library(dplyr)

setwd('./data-treatment')

status_meanings <- c('R & SQL', 'Fail or Just R', 'Just R', 'Timeout')

test_filter <- '55-tests'

squares <- read.csv('squares.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))

single <- read.csv('single.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
qffd_r_n <- read.csv('qffd_r_n.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))

t1 <- read.csv('try1.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t2 <- read.csv('try2.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t3 <- read.csv('try3.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t4 <- read.csv('try4.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t5 <- read.csv('try5.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t6 <- read.csv('try6.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t7 <- read.csv('try7.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t8 <- read.csv('try8.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))
t9 <- read.csv('try9.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, test_filter))

# times scatter plot
A <- 'squares';B <- 'qffd_r_n'; merge(eval(parse(text = A)), eval(parse(text = B)), by='name', suffixes = c("_A", "_B")) %>% filter(timeout_A == 'False' | timeout_B == 'False') %>% ggplot(aes(x=real_A, y=real_B)) + geom_point(color='red', alpha = 0.4, size=2) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_abline() + geom_hline(yintercept=600, linetype="dashed") + geom_vline(xintercept=600, linetype="dashed") + labs(y=B, x = A) + ggtitle('Real Time')

# process distribution
ggplot(t9, aes(x=factor(process))) + geom_bar(fill="turquoise")

#solve time distribution
ggplot(t9, aes(x=real)) + geom_histogram()

# solved instances by try
tries <- list('original' = original, 't1' = t1, 't2' = t2, 't3' = t3, 't4' = t4, 't5' = t5, 't6' = t6, 't7' = t7); solved <- bind_rows(tries, .id='try'); results <- factor(solved$status, labels=status_meanings, exclude = NULL); ggplot(solved, aes(x=try, fill=results)) + geom_bar(position="dodge") + scale_fill_brewer(palette = 'Dark2')