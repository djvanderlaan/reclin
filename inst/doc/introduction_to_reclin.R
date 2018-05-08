## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----message=FALSE,results='hide',warning=FALSE,echo=TRUE----------------
library(reclin)
library(dplyr)

## ------------------------------------------------------------------------
data("linkexample1", "linkexample2")
print(linkexample1)
print(linkexample2)

## ------------------------------------------------------------------------
p <- pairs_blocking(linkexample1, linkexample2, "postcode", large = FALSE)
print(p)

## ------------------------------------------------------------------------
p <- pairs_compare(p, by = c("lastname", "firstname", "address", "sex"))
print(p)

## ------------------------------------------------------------------------
p <- pairs_compare(p, by = c("lastname", "firstname", "address", "sex"),
  default_comparator = jaro_winkler(0.9), overwrite = TRUE)
print(p)

## ------------------------------------------------------------------------
p <- score_simsum(p, var = "simsum")
print(p)

## ------------------------------------------------------------------------
m <- problink_em(p)
print(m)

## ------------------------------------------------------------------------
p <- score_problink(p, model = m, var = "weight")
print(p)

## ------------------------------------------------------------------------
p <- select_threshold(p, "weight", var = "threshold", threshold = 8)

## ------------------------------------------------------------------------
p <- add_from_x(p, id_x = "id")
p <- add_from_y(p, id_y = "id")
p$true <- p$id_x == p$id_y
table(as.data.frame(p[c("true", "threshold")]))

## ------------------------------------------------------------------------
p <- select_greedy(p, "weight", var = "greedy", threshold = 0)
table(as.data.frame(p[c("true", "greedy")]))

## ------------------------------------------------------------------------
p <- select_n_to_m(p, "weight", var = "ntom", threshold = 0)
table(as.data.frame(p[c("true", "ntom")]))

## ------------------------------------------------------------------------
linked_data_set <- link(p)
print(linked_data_set)

## ---- message=FALSE------------------------------------------------------
library(dplyr)

linked_data_set <- pairs_blocking(linkexample1, linkexample2, "postcode") %>%
  pairs_compare(by = c("lastname", "firstname", "address", "sex"),
      default_comparator = jaro_winkler(0.9)) %>%
  score_problink(var = "weight") %>%
  select_n_to_m("weight", var = "ntom", threshold = 0) %>%
  link()

