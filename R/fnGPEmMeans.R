#' Produces a table of estimated marginal means covering each level of each factor in a linear model.
#' No new GP changes
#'
#' @description fnGPEmMeans identifies the factor levels in a linear model and calculates the corresponding estimated marginal means.
#' The output is a data.frame tabulating the sample size and the estimated marginal mean for each factor level.
#' See Examples for more details.
#'
#' @usage fnGPEmMeans<-function(lm,digits=2)
#'
#' @param lm A linear model of class lm, lme, aov, etc.
#' See \code{\link[emmeans:emmeans]{emmeans help page}} for the range of supported model objects.
#' The model must contain main effects only: inclusion of interactions terms will result in an error.
#' @param digits Number of decimal places required for the estimated marginal means. Defaults to 2.
#'
#' @note
#' This function uses \code{\link[emmeans]{emmeans}} for the underlying calculations.
#'
#' @import emmeans
#'
#' @return The function outputs a data.frame tabulating the sample size and the estimated marginal mean for each factor level..
#'
#' @examples
#' fit1<-aov(breaks ~ wool + tension, warpbreaks)
#' fnGPEmMeans(lm=fit1,digits=3)
#'
#' fnGPEmMeans(lm=aov(yield ~ block + N + P + K, npk),digits=1)
#'
#' # Note that factors may need to be defined
#' fnGPEmMeans(lm=aov(uptake ~ Type + Treatment + conc , CO2))
#'    CO2$Concentration<-factor(CO2$conc)
#' fnGPEmMeans(lm=aov(uptake ~ Type + Treatment + Concentration , CO2))
#'
#' @source Written by Martin Roberts (psychometrics@plymouth.ac.uk)
#'
#' @export


fnGPEmMeans <- function(lm, digits = 2) {
  # Written by: Martin Roberts
  # Last updated: 19/10/2018
  ################################################
  # lm = model of class lm, lme, aov, etc.  See http://127.0.0.1:29359/library/emmeans/doc/models.html for range of supported model objects.
  # digits = no. of decimal places required for the estimated marginal means
  data <- model.frame(lm)
  emms <- data.frame()
  for (i in attr(terms(lm), "term.labels")) {
    df <- data.frame(emmeans(lm, i))
    df <- cbind(
      Factor = c(i, rep("", dim(df)[1] - 1)),
      Level = paste(ifelse(i == "Stage", i, ""), as.vector(lm[["xlevels"]][[i]])),
      N = table(model.frame(lm)[, i]),
      adjmean = formatC(
        fnGPRound(df[, "emmean"], digits),
        digits = digits,
        format = "f"
      )
    )
    emms <- rbind(emms, df, stringsAsFactors = FALSE)
  }
  for (i in 3:4) {
    emms[, i] <- as.numeric(emms[, i])
  }
  colnames(emms)[4] <- "Adjusted mean"
  rownames(emms) <- c()
  emms
}
