
library(devtools)
library(dplyr)
load_all()

towns <- read.csv("work/towns.csv") %>% filter(!is.na(group))

p <- pairs_blocking(towns, towns) %>% 
  pairs_filter_for_deduplication() %>%
  pairs_compare("name", default_comparator = jaro_winkler()) %>% 
  score_simsum() %>% 
  select_threshold(0.85)

res <- deduplicate_equivalence(p)

tmp <- res %>% group_by(duplicate_groups) %>% 
  summarise(n = n(), group = paste0(name, collapse = ", "))
View(tmp)

