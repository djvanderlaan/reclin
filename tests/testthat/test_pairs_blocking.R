
context("pair_blocking")


test_that("blocking large = TRUE", {
  
  d1 <- data.frame(a = c(1,1,2,2))
  d2 <- data.frame(a = c(1,2,2,3))
  
  p <- pair_blocking(d1, d2) 
  
  expect_equal(as_rvec(p$x), rep(1:4, 4))
  expect_equal(as_rvec(p$y), rep(1:4, each = 4))
  
  
  p <- pair_blocking(d1, d2, blocking_var = "a")
  
  expect_equal(as_rvec(p$x), c(1,2,3,3,4,4))
  expect_equal(as_rvec(p$y), c(1,1,2,3,2,3))
  
  expect_equal(attr(p, "x"), d1)
  expect_equal(attr(p, "y"), d2)
  expect_equal(attr(p, "blocking_var"), "a")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "ldat")
})

gc()


test_that("blocking large = FALSE", {
  
  d1 <- data.frame(a = c(1,1,2,2))
  d2 <- data.frame(a = c(1,2,2,3))
  
  p <- pair_blocking(d1, d2, large = FALSE) 
  
  expect_equal(p$x, rep(1:4, 4))
  expect_equal(p$y, rep(1:4, each = 4))
  
  
  p <- pair_blocking(d1, d2, blocking_var = "a", large = FALSE)
  
  expect_equal(p$x, c(1,2,3,3,4,4))
  expect_equal(p$y, c(1,1,2,3,2,3))
  
  expect_equal(attr(p, "x"), d1)
  expect_equal(attr(p, "y"), d2)
  expect_equal(attr(p, "blocking_var"), "a")
  expect_s3_class(p, "pairs")
  expect_s3_class(p, "pairs_blocking")
  expect_s3_class(p, "data.frame")
})

gc()

test_that("edge case of 0 row data.frames", {
  data("linkexample1", "linkexample2")
  p <- pair_blocking(linkexample1, linkexample2[FALSE, ])
  expect_equal(nrow(p), 0)
  expect_equal(ncol(p), 2)
  expect_equal(names(p), c("x", "y"))
  expect_s3_class(p, "ldat")
  
  p <- pair_blocking(linkexample1, linkexample2[FALSE, ], large = FALSE)
  expect_equal(nrow(p), 0)
  expect_equal(ncol(p), 2)
  expect_equal(names(p), c("x", "y"))
  expect_s3_class(p, "data.frame")
  
  p <- pair_blocking(linkexample1[FALSE, ], linkexample2)
  expect_equal(nrow(p), 0)
  expect_equal(ncol(p), 2)
  expect_equal(names(p), c("x", "y"))
  expect_s3_class(p, "ldat")
  
  p <- pair_blocking(linkexample1[FALSE, ], linkexample2, large = FALSE)
  expect_equal(nrow(p), 0)
  expect_equal(ncol(p), 2)
  expect_equal(names(p), c("x", "y"))
  expect_s3_class(p, "data.frame")

  p <- pair_blocking(linkexample1[FALSE, ], linkexample2[FALSE, ])
  expect_equal(nrow(p), 0)
  expect_equal(ncol(p), 2)
  expect_equal(names(p), c("x", "y"))
  expect_s3_class(p, "ldat")

  p <- pair_blocking(linkexample1[FALSE, ], linkexample2[FALSE, ], large = FALSE)
  expect_equal(nrow(p), 0)
  expect_equal(ncol(p), 2)
  expect_equal(names(p), c("x", "y"))
  expect_s3_class(p, "data.frame")
  
})

gc()

