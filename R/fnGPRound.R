#' A function to round values in accordance with PSMD policy
#'
#' @description fnGPRound will round numeric values in accordance with PSMD policy. The policy (as at August 2016) proposes the use of the asymmetric round-half-up method (arithmetic rounding).
#' This method rounds values at or above the midpoint away from zero and values below the midpoint towards zero to the closest required decimal place. For example, when rounded to two decimal places values of 1.000 to 1.004 become 1.00 and values of 1.005 to 1.009 become 1.01. This applies irrespective of a number's sign so that values of -1.000 to -1.004 become -1.00 and values of -1.005 to -1.009 become -1.01.
#'
#' Copied from PsychometricsTNG package.
#' No changes by GP
#'
#' Adapted from an anonymous post at Statistically Significant; http://alandgraf.blogspot.co.uk/2012/06/rounding-in-r.html
#'
#' @usage fnGPRound(x, n)
#'
#' @param x numeric value or vector of values to be rounded. NAs are preserved but non-numeric values will cause an error.
#' @param n number of decimal places required (default = 0). Trailing zeroes will be excluded.
#'
#' @examples  fnGPRound(x = 13.4345, n = 2)
#'
#' fnGPRound(13.4345, 3)
#'
#' fnGPRound(c(2.245, -2.245, 0.005, 1.133, NA, -0.515), 2)
#'
#' fnGPRound(seq(0.05,1.15,0.1), 1)
#'
#' @export


fnGPRound <-function (x,n=0) {
  # Written by: Martin Roberts
  # Last updated: 14/02/2019
  ################################################
  tol <- 1*10^(n-14)  # Tolerance for error: deals with error in the way that computers store decimal numbers
  z <- abs(x) * 10^n  # Ignore sign of x and shift decimal point right by n places
  z <- z + 0.5
  z <- trunc(z) + ifelse(trunc(z)+1-z < tol, 1, 0)
  z <- z/10^n * sign(x)   # Shift decimal point left by n places and restore sign of x
  z
}
