
library(devtools)
library(dplyr)
load_all()

towns <- read.csv("work/towns.csv") %>% filter(!is.na(group))

towns$clean_name <- gsub("[^[:alnum:]]", "", towns$name)
towns$clean_name <- gsub("0", "o", towns$clean_name)

p <- pairs_blocking(towns, towns) %>% 
  pairs_filter_for_deduplication() %>%
  pairs_compare("clean_name", default_comparator = jaro_winkler()) %>% 
  score_simsum() %>% 
  select_threshold(0.88)

res <- deduplicate_equivalence(p)

t <- table(res$duplicate_groups, res$group)
apply(t, 1, function(d) sum(d!=0))

sum(apply(t, 1, function(d) sum(d)-max(d)))/nrow(res)

length(unique(res$duplicate_groups))
length(unique(res$duplicate_groups))/nrow(res)


tmp <- res %>% group_by(duplicate_groups) %>% 
  summarise(n = n(), group = paste0(name, collapse = ", "))
# View(tmp)

res$name[res$group == 2 & res$duplicate_groups == 584]


library(stringdist)


lcs <- function(threshold = 0.95) {
  function(x, y) {
    if (!missing(y)) {
      d <- stringdist::stringdist(x, y, method = "lcs")
      maxd <- nchar(x) + nchar(y)
      1 - d/maxd
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}

jaccard <- function(threshold = 0.95) {
  function(x, y) {
    if (!missing(y)) {
      1-stringdist::stringdist(x, y, method = "jaccard", q = 2)
    } else {
      (x > threshold) & !is.na(x)
    }
  }
}


p <- pairs_blocking(towns, towns) %>% 
  pairs_filter_for_deduplication() %>%
  pairs_compare("clean_name", default_comparator = jaccard()) %>% 
  score_simsum() %>% 
  select_threshold(0.70)

res <- deduplicate_equivalence(p)

t <- table(res$duplicate_groups, res$group)
apply(t, 1, function(d) sum(d!=0))

sum(apply(t, 1, function(d) sum(d)-max(d)))/nrow(res)

length(unique(res$duplicate_groups))
length(unique(res$duplicate_groups))/nrow(res)
