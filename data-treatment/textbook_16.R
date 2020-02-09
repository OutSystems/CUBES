library(dplyr)

catalog <- read.csv("../tests-examples/textbook/tables/16-1.txt", header=T)
parts <- read.csv("../tests-examples/textbook/tables/16-2.txt", header=T)
suppliers <- read.csv("../tests-examples/textbook/tables/16-3.txt", header=T)

df1 <- inner_join(catalog, parts) %>% inner_join(suppliers)  %>% filter(S_name == "AWS")
df1
df2 <- catalog %>% distinct() %>% group_by(P_id) %>% summarise(n = n()) %>% filter(n == 1)
df2
semi_join(df1, df2)

df1 = inner_join(parts,catalog) %>% inner_join(suppliers) %>% 
  group_by(P_name,S_name) %>% summarise(n = n()) %>% 
  filter(S_name == "AWS")
df2 = inner_join(parts,catalog) %>% inner_join(suppliers) %>% 
  group_by(P_name) %>% summarise(n = n())
out <- inner_join(df1,df2) %>% select(P_name)
out
