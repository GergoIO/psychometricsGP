#' Round value or vector
#'
#' @description Return a rounded value as a string, ready for adding to reports
#' There is additional functionality to handle different data types if this function is used as part of a loop.
#' If NA data is input, the default value of "NA" is returned.
#' This can be overriden by setting the variable 'valueIfNA'.
#' Otherwise, the input value is converted to numeric, rounded to the number of given decimals and converted to a string (this preserves trailing zeros). Input strings which cannot be converted to a numeric value (eg inputting a string) will return "NA".
#'
#' @param value The value to be rounded (could be NA - for use in loops)
#' @param decimals (numeric, integer) The number of decimals to round to
#' @param valueIfNA (OPTIONAL) The value to return if the input value is NA
#'
#' @return The rounded value is returned as a string
#' @export
#'
#' @examples fnRnd(value = 23.3333, decimals = 2, valueIfNA = "N/A"),
#' fnRnd(value = c(2.55,3.44, "4", "NA", NA), decimals = 2, valueIfNA = "N/A")

################################################################################

fnRnd <- function(value = NULL,
                  decimals = NULL,
                  valueIfNA = NULL) {
  if (is.null(value) |
      is.null(decimals)) {
    stop("fnRnd: One of the required variables for this function has not been specified.")
  } else if (!is.numeric(decimals)) {
    stop("fnRnd: The decimals variable must be numeric.")
  } else {
    # First convert all items to numeric and round + format it (to string)
    # All numbers or numbers saved as strings will be rounded appropriately
    # All other values will go to "  NA"
    value <- suppressWarnings(format(round(as.numeric(value),
                                           decimals), nsmall = decimals))
    # Convert all instances of NA (string) to just NA (not string)
    value <-
      gsub("NA", ifelse(is.null(valueIfNA), NA, valueIfNA), value)
    # Sometimes spaces were introduced before a number. This removes them
    value <- gsub(" ", "", value)
    return(value)
  }
}
