
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
  err <- rbinom(length(str), size = 1, prob = perr) > 0.5
  nc  <- nchar(str)
  err <- err & (nc > 1)
  nc <- nc[err]
  s <- (sample(max(nc), length(nc), replace = TRUE) %% (nc-1)) + 1
  old <- substr(str[err], s, s)
  new <- substr(str[err], s+1, s+1)
  substr(str[err], s, s) <- new
  substr(str[err], s+1, s+1) <- old
  str
}

error_record_replace <- function(x, perr = 1) {
  err <- rbinom(NROW(x), size = 1, prob = perr) > 0.5
  nerr <- sum(err)
  if (is.data.frame(x)) {
    x[err, ] <- x[sample(NROW(x), nerr, replace = TRUE), ]
    x
  } else {
    x[err] <- sample(x)[err]
    x  
  }
}





random_data <- function(n1, n2, overlap, perr = 0.05) {
  n <- n1 + overlap*n2
  nhh <- n / 4
  
  hhsize <- sample(1:6, nhh, replace = TRUE)
  while (sum(hhsize) != sum(hhsize)) {
    np <- sum(hhsize)
    sel <- sample(which(hhsize > 1), max(np - n, 0))
    hhsize[sel] <- hhsize[sel] - 1
    
    np <- sum(hhsize)
    sel <- sample(length(hhsize), min(length(hhsize), max(n - np, 0)))
    hhsize[sel] <- hhsize[sel] + 1
  }
  
  hh <- data.frame(
    last_name = random_strings(nhh, 20, 30),
    postcode = random_postcode(n, nunique = nhh),
    street = random_strings(n, 20, 30),
    number = sample(1:200, n, replace = TRUE),
    stringsAsFactors = FALSE
  )
  exp <- rep(1:nhh, hhsize)
  dta <- hh[exp, ]
  dta$first_name <- random_strings(nrow(dta), 2, 20)
  dta$dob <- random_date(nrow(dta), "1950-01-01", "2019-12-31")
  dta$dob_yr <- yr(dta$dob)
  dta$dob_mo <- mo(dta$dob)
  dta$dob_dy <- dy(dta$dob)
  dta$dob <- NULL
  dta$id <- seq(1, nrow(dta))
  
  dta1 <- head(dta, n1)
  dta2 <- tail(dta, n2)
  
  dta2$last_name <- error_letter_swap(dta2$last_name, perr = perr)
  dta2$first_name <- error_letter_swap(dta2$first_name, perr = perr)
  dta2$postcode <- error_record_replace(dta2$postcode, perr = perr)
  dta2$street <- error_record_replace(dta2$street, perr = perr)
  dta2$number <- error_record_replace(dta2$number, perr = perr)
  dta2$dob_mo <- error_record_replace(dta2$dob_mo, perr = perr)
  dta2$dob_dy <- error_record_replace(dta2$dob_dy, perr = perr)
  list(a = dta1, b = dta2)
}




