#' Round value
#'
#' @description Return a rounded value as a string, ready for adding to reports
#' There is additional functionality to handle different data types if this function is used as part of a loop.
#' If NA data is input, the default value of "NA" is returned.
#' This can be overriden by setting the variable 'valueIfNA'.
#' Otherwise, the input value is converted to numeric, rounded to the number of given decimals and converted to a string (this preserves trailing zeros). Input strings which cannot be converted to a numeric value (eg inputting a string) will just return whatever was input, also as a string.
#'
#' @param value The value to be rounded (could be NA - for use in loops)
#' @param decimals (numeric, integer) The number of decimals to round to
#' @param valueIfNA (OPTIONAL) The value to return if the input value is NA
#'
#' @return The rounded value is returned as a string
#' @export
#'
#' @examples fnRnd(value = 23.3333, decimals = 2, valueIfNA = "N/A")

################################################################################

fnRnd <- function(value = NULL,
                           decimals = NULL,
                           valueIfNA = NULL) {
  if (is.null(value) == TRUE |
      is.null(decimals) == TRUE) {
    stop("fnRnd: One of the required variables for this function has not been specified.")
  } else if(is.numeric(decimals) == FALSE){
    stop("fnRnd: The decimals variable must be numeric.")
  }
  else{
    if (is.na(value) == TRUE) {
      if (is.null(valueIfNA) == FALSE) {
        return(valueIfNA)
      } else{
        return("NA")
      }
    } else if (is.numeric(value) == FALSE){
      return(value)
      }else{
      # Extra conversion to numeric below helps to catch non numeric inputs
      return(suppressWarnings(format(round(as.numeric(value), decimals), nsmall = decimals)))
    }
  }
}
