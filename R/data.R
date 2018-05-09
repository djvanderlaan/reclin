
#' Tiny example dataset for probabilistic linkage
#'
#' Contains fictional records of 7 persons.
#'
#' \itemize{
#'   \item id the id of the person; this contains no errors and can be used to 
#'     validate the linkage. 
#'   \item lastname the lastname of the person; contains errors.
#'   \item firstname the firstname of the persons; contains errors.
#'   \item address the address; contains errors.
#'   \item sex the sex; contains errors and missing values.
#'   \item postcode the postcode; contains no errors. 
#' }
#'
#' @docType data
#' @keywords datasets
#' @name linkexample1
#' @rdname linkexample
#' @format Two data frames with resp. 6 and 5 records and 6 columns. 
NULL

#' @name linkexample2
#' @rdname linkexample
NULL

#' Spelling variations of a set of town names
#'
#' Contains spelling variations found in various files of a set of town/village
#' names. Names were selected that contain 'rdam' or 'rdm'. The correct/official
#' names are also given. This data set can be used as an example data set for 
#' deduplication
#'
#' \itemize{
#'   \item name the name of the town/village as found in the files
#'   \item official_name the official/correct name
#' }
#'
#' @docType data
#' @keywords datasets
#' @name town_names
#' @format Data frames with 584 records and two columns.
NULL

