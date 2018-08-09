

context("compare_pairs")


test_that("compare large = TRUE", {
  
  data("linkexample1", "linkexample2")
  
  p <- pair_blocking(linkexample1, linkexample2) %>% 
    compare_pairs(by = c("lastname", "firstname", "address", "sex", "postcode"))
  
  
  x <- rep(1:nrow(linkexample1), times = nrow(linkexample2))
  y <- rep(1:nrow(linkexample2), each = nrow(linkexample1))
  
  expect_equal(names(p), c("x", "y", "lastname", "firstname", "address", "sex",
    "postcode"))
  expect_equal(as_rvec(p$lastname), 
    as.character(linkexample1$lastname[x]) == as.character(linkexample2$lastname[y]))
  expect_equal(as_rvec(p$sex), 
    as.character(linkexample1$sex[x]) == as.character(linkexample2$sex[y]))
  
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "ldat")
})

gc()

test_that("compare large = FALSE", {
  
  data("linkexample1", "linkexample2")
  
  p <- pair_blocking(linkexample1, linkexample2, large = FALSE) %>% 
    compare_pairs(by = c("lastname", "firstname", "address", "sex", "postcode"))
  
  
  x <- rep(1:nrow(linkexample1), times = nrow(linkexample2))
  y <- rep(1:nrow(linkexample2), each = nrow(linkexample1))
  
  expect_equal(names(p), c("x", "y", "lastname", "firstname", "address", "sex",
    "postcode"))
  expect_equal((p$lastname), 
    as.character(linkexample1$lastname[x]) == as.character(linkexample2$lastname[y]))
  expect_equal((p$sex), 
    as.character(linkexample1$sex[x]) == as.character(linkexample2$sex[y]))
  
  
  expect_equal(attr(p, "x"), linkexample1)
  expect_equal(attr(p, "y"), linkexample2)
  expect_null(attr(p, "blocking_var"))
  expect_equal(attr(p, "by"), c("lastname", "firstname", "address", "sex", 
    "postcode"))
  expect_s3_class(p, "compare")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "data.frame")
})

gc()


test_that("edge case of 0 row data.frames", {
  data("linkexample1", "linkexample2")
  p <- pair_blocking(linkexample1, linkexample2[FALSE, ], large = TRUE)  %>% 
    compare_pairs(by = c("lastname", "firstname", "address", "sex", "postcode"))
  expect_equal(nrow(p), 0)
  expect_equal(names(p), c("x", "y", "lastname", "firstname", "address", "sex",
    "postcode"))
  expect_s3_class(p, "ldat")
  
  p <- pair_blocking(linkexample1, linkexample2[FALSE, ], large = FALSE)  %>% 
    compare_pairs(by = c("lastname", "firstname", "address", "sex", "postcode"))
  expect_equal(nrow(p), 0)
  expect_equal(names(p), c("x", "y", "lastname", "firstname", "address", "sex",
    "postcode"))
  expect_s3_class(p, "data.frame")
})

gc()