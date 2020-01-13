
random_strings <- function(n, lmin, lmax, characters = letters) {
  res <- character(n)
  for (i in seq_len(lmax)) {
    res <- paste0(res, sample(characters, n, replace = TRUE))
  }
  if (lmin < lmax) {
    l <- sample(lmin:lmax, n, replace = TRUE)
    substr(res, start = 1 , stop = l)
  } else res
}

random_date <- function(n, date_start, date_end) {
  date_start <- as.Date(date_start)
  date_end   <- as.Date(date_end)
  dates <- seq(date_start, date_end, by = "days")
  sample(dates, n, replace = TRUE)
}

random_postcode <- function(n, nunique) {
  nnumbers <- ceiling(nunique/26/26)
  nnumbers <- max(nnumbers, 1L)
  numbers <- seq(1000, 1000 + nnumbers, by = 1L)
  num <- sample(numbers, n, replace = TRUE)
  ltr <- random_strings(n, 2, 2, LETTERS)
  paste0(num, ltr)
}

yr <- function(dates) {
  as.integer(format(as.Date(dates), "%Y"))
}

mo <- function(dates) {
  as.integer(format(as.Date(dates), "%m"))
}

dy <- function(dates) {
  as.integer(format(as.Date(dates), "%d"))
}

error_letter_swap <- function(str, perr = 1) {
  
  str <- random
  err <- rbinom(length(str), size = 1, prob = perr)
}


random_postcode(1000, nunique = 10000)

dy(random_date(10, "2019-01-01", "2019-12-31"))



system.time(x <- random_strings(1E6, 1, 5))

x <- c("abc", "abc")
substr(x, start = 1, stop = c(1,2)
)  
