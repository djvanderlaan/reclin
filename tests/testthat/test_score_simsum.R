

context("score_simsum")


test_that("simsum large = TRUE", {
  
  data("linkexample1", "linkexample2")
  
  by <- c("lastname", "firstname", "address", "sex", "postcode")
  p <- pairs_blocking(linkexample1, linkexample2) %>% 
    pairs_compare(by = by, default_comparator = jaro_winkler()) %>% 
    score_simsum()
  
  expect_equal(names(p), c("x", "y", by, "simsum"))
  
  score <- rep(0, nrow(p))
  for (col in by) {
    d <- as_rvec(p[[col]])
    score <- score + ifelse(is.na(d), 0, d)
  }
  expect_equal(as_rvec(p$simsum), score)
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_equal(attr(p, "score"), "simsum")
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "ldat")
})


test_that("simsum large = TRUE", {
  
  data("linkexample1", "linkexample2")
  
  by <- c("lastname", "firstname", "address", "sex", "postcode")
  p <- pairs_blocking(linkexample1, linkexample2, large = FALSE) %>% 
    pairs_compare(by = by, default_comparator = jaro_winkler()) %>% 
    score_simsum()
  
  expect_equal(names(p), c("x", "y", by, "simsum"))
  
  score <- rep(0, nrow(p))
  for (col in by) {
    d <- (p[[col]])
    score <- score + ifelse(is.na(d), 0, d)
  }
  expect_equal((p$simsum), score)
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_equal(attr(p, "score"), "simsum")
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "data.frame")
})
