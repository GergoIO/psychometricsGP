#' Pivoting CAPT responses dataframes
#'
#' @param data (dataframe) Incoming data eg a dataframe with cols StudentID, TopicCode, IsCorrect
#' @param colIsCorrect (default = IsCorrect), String/variable. The name of the column containing the item scores in each row
#' @param colTopicCode (default = TopicCode), String/variable. The name of the column containing the topic codes in each row
#' @param listTopicCodes (default = sort(unique(tab$blueprintCurr$TopicCode))). List of all topic codes (either answered/unanswered)
#'
#' @return returns a new dataframe that has been pivotted to be wider
#' @export
#'
#' @examples
#'df <- data.frame(
#' StudentID = c(
#'   "001",
#'   "001",
#'   "001",
#'   "001",
#'   "001",
#'   "002",
#'   "002",
#'   "002",
#'   "002",
#'   "002",
#'   "003",
#'   "003",
#'   "003",
#'   "003",
#'   "003"
#' ),
#' TopicCode = c(
#'   "B",
#'   "C",
#'   "D",
#'   "E",
#'   "F",
#'   "A",
#'   "C",
#'   "D",
#'   "E",
#'   "F",
#'   "A",
#'   "B",
#'   "D",
#'   "E",
#'   "F"
#' ),
#' IsCorrect = c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0)
#' )
#'
#' pivotDf <- df %>%
#'   responsesPivot(
#'     colIsCorrect = IsCorrect,
#'     colTopicCode = TopicCode,
#'     listTopicCodes = c("A", "B", "C", "D", "E", "F")
#'   )
#'
################################################################################
#'
fnCAPTResponsesPivot <- function(data,
                                 colIsCorrect = IsCorrect,
                                 colTopicCode = TopicCode,
                                 listTopicCodes = sort(unique(tab$blueprintCurr$TopicCode))) {
  data |>
    mutate({{colTopicCode}} := factor({{colTopicCode}}, levels = listTopicCodes)) |>
    pivot_wider(
      names_from = {{colTopicCode}},
      values_from = {{colIsCorrect}},
      names_expand = TRUE,
      values_fn = list,
      values_fill = NA
    )
}
