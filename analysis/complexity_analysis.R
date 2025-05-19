library(readr)
library(statsExpressions)
library(stringr)

data <- read_csv("complexity_analysis.csv") %>%
  mutate(benchmark = gsub("_", "-", str_sub(str_extract(name, '.*/'), end = -2))) %>%
  mutate(benchmark = ifelse(str_detect(benchmark, 'spider'), 'spider', benchmark))

data %>% ggplot(aes(x = cube_length)) + geom_bar()
data %>% ggplot(aes(x = kw_length)) + geom_bar()

data %>% ggplot(aes(x = cube_length, y = kw_length)) + geom_point()

data %>%
  pivot_longer(starts_with("sql_kw_"), names_to = "keyword", values_to = "count") %>%
  ggplot(aes(y = reorder(keyword, count, FUN = mean), x = count)) + geom_boxplot()

data %>%
  pivot_longer(starts_with("dsl_op_"), names_to = "keyword", values_to = "count") %>%
  ggplot(aes(y = reorder(keyword, count, FUN = mean), x = count)) + geom_boxplot()

summary(data %>% select(cube_length, kw_length))

total_instances <- data %>% count() %>% as.numeric()

data %>%
  pivot_longer(starts_with("dsl_op_"), names_to = "dsl_op") %>%
  select(name, dsl_op, value) %>%
  mutate(dsl_op = str_remove(dsl_op, "dsl_op_")) %>%
  group_by(dsl_op) %>%
  summarise(used.in = sum(ifelse(value != 0, 1, 0)) / total_instances * 100,
            min = min(value),
            median = median(value),
            mean = mean(value),
            max = max(value)) %>%
  arrange(-mean)

data %>%
  pivot_longer(starts_with("sql_kw_"), names_to = "sql_kw") %>%
  select(name, sql_kw, value) %>%
  mutate(sql_kw = str_remove(sql_kw, "sql_kw_")) %>%
  group_by(sql_kw) %>%
  summarise(used.in = sum(ifelse(value != 0, 1, 0)) / total_instances * 100,
            min = min(value),
            median = median(value),
            mean = mean(value),
            max = max(value)) %>%
  arrange(-mean) %>%
  print(n = 50)
