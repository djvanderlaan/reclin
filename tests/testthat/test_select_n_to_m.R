

context("select_n_to_m")


test_that("n_to_m large = TRUE", {
  
  data("linkexample1", "linkexample2")
  
  by <- c("lastname", "firstname", "address", "sex", "postcode")
  p <- pair_blocking(linkexample1, linkexample2) %>% 
    compare_pairs(by = by, default_comparator = jaro_winkler()) %>% 
    score_simsum() %>% 
    select_n_to_m(threshold = 2)
  
  expect_equal(names(p), c("x", "y", by, "simsum", "select"))
  
  expect_gt(min(p$simsum[p$select]), 2)
  expect_true(all(!duplicated(p$x[p$select])))
  expect_true(all(!duplicated(p$y[p$select])))
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_equal(attr(p, "score"), "simsum")
  expect_equal(attr(p, "select"), "select")
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "ldat")
})

gc()

test_that("n_to_m large = FALSE", {
  
  data("linkexample1", "linkexample2")
  
  by <- c("lastname", "firstname", "address", "sex", "postcode")
  p <- pair_blocking(linkexample1, linkexample2, large = FALSE) %>% 
    compare_pairs(by = by, default_comparator = jaro_winkler()) %>% 
    score_simsum() %>% 
    select_n_to_m(threshold = 2)
  
  expect_equal(names(p), c("x", "y", by, "simsum", "select"))
  
  expect_gt(min(p$simsum[p$select]), 2)
  expect_true(all(!duplicated(p$x[p$select])))
  expect_true(all(!duplicated(p$y[p$select])))
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_equal(attr(p, "score"), "simsum")
  expect_equal(attr(p, "select"), "select")
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "data.frame")
})

gc()