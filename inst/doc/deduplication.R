## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----results='hide',message=FALSE,warning=FALSE--------------------------
library(reclin)
library(dplyr)

## ------------------------------------------------------------------------
data("town_names")
head(town_names)

## ------------------------------------------------------------------------
town_names$clean_name <- gsub("[^[:alnum:]]", "", town_names$name)
town_names$clean_name <- gsub("0", "o", town_names$clean_name)

## ------------------------------------------------------------------------
p <- pairs_blocking(town_names, town_names) %>% 
  pairs_filter_for_deduplication() %>%
  pairs_compare("clean_name", default_comparator = jaro_winkler()) %>% 
  score_simsum() %>% 
  select_threshold(0.88)
head(p)

## ------------------------------------------------------------------------
res <- deduplicate_equivalence(p)
head(res)

## ------------------------------------------------------------------------
length(unique(res$duplicate_groups))
length(unique(res$duplicate_groups))/nrow(res)

## ------------------------------------------------------------------------
res <- res %>% group_by(duplicate_groups, official_name) %>% mutate(n = n()) %>% 
  group_by(duplicate_groups) %>%
  mutate(group_name = first(official_name, order_by = desc(n)))

## ------------------------------------------------------------------------
precision <- res %>% group_by(group_name) %>% 
  summarise(precision = sum(group_name == official_name)/n())

precision_recall <- res %>% group_by(official_name) %>% 
  summarise(recall = sum(group_name == official_name)/n()) %>%
  left_join(precision, by = c("official_name" = "group_name")) %>% 
  mutate(precision = ifelse(is.na(precision), 0, precision))

precision_recall

## ------------------------------------------------------------------------
summarise(precision_recall, mean(recall), mean(precision))

