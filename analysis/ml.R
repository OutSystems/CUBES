library(ggplot2)
library(RColorBrewer)
library(readr)
library(ggcorrplot)
library(gbm)
library(r2pmml)
library(caret)
library(neuralnet)
library(tidyr)
library(dplyr)

setwd('./data-treatment')

c15 <- read_csv('cubes15_16_h.csv') %>% select(name, real)

data <- read_csv('data.csv') %>%
  inner_join(c15) %>% select(-name) %>% na.omit %>% mutate(hard = loc >= 4 | real > 100)

write_csv(data %>% select(-loc, -real), 'ml_data.csv')



corr <- cor(data)
p.mat <- cor_pmat(data)
ggcorrplot(corr, hc.order = TRUE, type = "lower", p.mat = p.mat)







index <- createDataPartition(data$hard, p = 0.7, list = FALSE)
train <- data[index, ]
test  <- data[-index, ]


model <- glm(hard ~ . - real - loc, data = data, family = binomial(link = 'logit'), weights = hard * 5 + 1)
model <- gbm(hard ~ . - real - loc, data = data, distribution = "adaboost", n.trees = 200, shrinkage = 0.01, cv.folds = 50, keep.data = TRUE, interaction.depth = 3)

best <- gbm.perf(model, method = "cv")

print(pretty.gbm.tree(model, i.tree = best))

predict(model, newdata = train, type='link')

ggplot(train, aes(x = predict(model, newdata = train, type='response'), y = hard)) + geom_point(size = 5, alpha = 0.8)
ggplot(test, aes(x = predict(model, newdata = test, type='response'), y = hard)) + geom_point(size = 5, alpha = 0.8)
ggplot(data, aes(x = predict(model, newdata = data), y = hard)) + geom_point(size = 5, alpha = 0.8)

model <- gbm(loc ~ . - real, data = data, distribution = "gaussian", n.trees = 50, shrinkage = 0.2,
             interaction.depth = 3, n.minobsinnode = 10, cv.folds = 10, keep.data = TRUE)
r2pmml(model, "model.pmml")







nn <- neuralnet(hard ~ . - real - loc, data=data, hidden=c(5, 3, 3, 3), act.fct = "tanh", linear.output = F)
plot(nn)



ggplot(data, aes(x = predict(nn, data), y = hard)) + geom_jitter(height=0.1, width=.01, size = 5, alpha = 0.8)





ggplot(data, aes(x = filters, y = columns, group = loc, color = real)) +
  scale_color_distiller(palette = "Spectral") +
  scale_x_log10() +
  geom_jitter(width = .1, height = .1, size = 5, alpha = 0.8)