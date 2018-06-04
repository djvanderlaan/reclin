
context("score_problink")


test_that("problink large = TRUE", {
  
  data("linkexample1", "linkexample2")
  
  by <- c("lastname", "firstname", "address", "sex", "postcode")
  p <- pair_blocking(linkexample1, linkexample2) %>% 
    compare_pairs(by = by, default_comparator = jaro_winkler()) %>% 
    score_problink()
  
  expect_equal(names(p), c("x", "y", by, "weight"))
  
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_equal(attr(p, "score"), "weight")
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "ldat")
})


test_that("problink large = FALSE", {
  
  data("linkexample1", "linkexample2")
  
  by <- c("lastname", "firstname", "address", "sex", "postcode")
  p <- pair_blocking(linkexample1, linkexample2, large = FALSE) %>% 
    compare_pairs(by = by, default_comparator = jaro_winkler()) %>% 
    score_problink()
  
  expect_equal(names(p), c("x", "y", by, "weight"))
  
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_equal(attr(p, "score"), "weight")
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "data.frame")
})
