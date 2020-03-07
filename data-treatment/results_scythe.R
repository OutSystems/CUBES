library(ggplot2)
library(stringr)
library(dplyr)

setwd('./data-treatment')

status_meanings <- c('Success', 'Error')

original <- read.csv('squares_scythe.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, 'top_rated')) %>% mutate(real = ifelse(status==1, 600, real)) %>% mutate(timeout = ifelse(status==1, 'True', timeout))

t8 <- read.csv('try8.csv', header=T, stringsAsFactors=FALSE) %>% filter(str_detect(name, 'top_rated'))


# times scatter plot
A <- 'original';B <- 't8'; merge(eval(parse(text = A)), eval(parse(text = B)), by='name', suffixes = c("_A", "_B")) %>% filter(timeout_A == 'False' | timeout_B == 'False') %>% ggplot(aes(x=real_A, y=real_B)) + geom_point(color='red', alpha = 0.4, size=2) + scale_x_continuous(trans='log10') + scale_y_continuous(trans='log10') + geom_abline() + geom_hline(yintercept=600, linetype="dashed") + geom_vline(xintercept=600, linetype="dashed") + labs(y=B, x = A) + ggtitle('Real Time')

# process distribution
ggplot(t8, aes(x=factor(process))) + geom_bar(fill="turquoise")

#solve time distribution
ggplot(original, aes(x=real)) + geom_histogram()

# solved instances by try
tries <- list('original' = original, 't8' = t8); solved <- bind_rows(tries, .id='try'); results <- factor(solved$status, labels=status_meanings); ggplot(solved, aes(x=try, fill=results)) + geom_bar(position="dodge")
