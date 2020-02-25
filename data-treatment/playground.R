library(dplyr)

i <- read.csv("tests-examples/scythe/top_rated_posts/tables/i019.csv", header=T)
o <- read.csv("tests-examples/scythe/top_rated_posts/tables/o019.csv", header=T)

t <- i %>% group_by(age) %>% summarise(n = n())
t1 <- t %>% filter(n > 1)
inner_join(i, t)