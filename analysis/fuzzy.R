# SOLVED PERCENTAGE
squares %>% filter(solved) %>% count() / squares %>% count()
scythe %>% filter(solved) %>% count() / scythe %>% count()
patsql %>% filter(solved) %>% count() / patsql %>% count()
sequential %>% filter(solved) %>% count() / patsql %>% count()
portfolio5_4 %>% filter(solved) %>% count() / patsql %>% count()


# SIMPLE COUNTS
squares %>% filter(fuzzy == 'Incorrect by Fuzzying') %>% count()
squares %>% filter(fuzzy == 'Possibly Correct') %>% count()
squares %>% filter(fuzzy == 'Incorrect') %>% count()

# INCORRECT INSTANCE NAMES
squares %>% filter(fuzzy == 'Incorrect') %>% select(name)

# ACCURACY FUZZY
scythe %>% filter(status == 0 & fuzzy == 'Possibly Correct') %>% count()
squares %>% filter(status == 0 & fuzzy == 'Possibly Correct') %>% count()
patsql %>% filter(status == 0 & fuzzy == 'Possibly Correct') %>% count()
sequential %>% filter(status == 0 & fuzzy == 'Possibly Correct') %>% count()


sequential %>% filter(status == 0 & fuzzy == 'Possibly Correct' & benchmark == 'spider') %>% count() / sequential %>% filter(benchmark == 'spider') %>% count()

# ACCURACY BASE
scythe %>% filter(status == 0 & fuzzy == 'Possibly Correct' | fuzzy == 'Incorrect by Fuzzying') %>% count()
squares %>% filter(status == 0 & fuzzy == 'Possibly Correct' | fuzzy == 'Incorrect by Fuzzying') %>% count()
patsql %>% filter(status == 0 & fuzzy == 'Possibly Correct' | fuzzy == 'Incorrect by Fuzzying') %>% count()
sequential %>% filter(status == 0 & fuzzy == 'Possibly Correct' | fuzzy == 'Incorrect by Fuzzying') %>% count()

# ACCURACY FUZZY - ERROR AS INCORRECT
squares_2 %>% filter(status == 0 & fuzzy == 'Possibly Correct') %>% count() /
  squares_2 %>% filter(status == 0 & fuzzy != 'Fuzzer Error' & fuzzy != 'GT Mismatch') %>% count()

# ACCURACY BASE - ERROR AS INCORRECT
squares %>% filter(status == 0 & fuzzy == 'Possibly Correct' | fuzzy == 'Incorrect by Fuzzying') %>% count() /
  squares %>% filter(status == 0 & fuzzy != 'Fuzzer Error' & fuzzy != 'GT Mismatch') %>% count()

# ERRORS
a <- squares %>% filter(benchmark != '55-tests' & solved & fuzzy == 'Error')

# EQUIVALENT PROCESSES
c50_16 %>% filter(equivalent_p > 15 & real >= 20) %>% count() / c50_16 %>% filter(real >= 20) %>% count()