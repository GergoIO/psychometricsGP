#' Process Scores from Responses
#'
#' This function processes student responses to generate scores based on an answer key,
#' handles negative marking, removes excluded items, and separates scores by stage.
#'
#' @param responses A data frame containing student responses, including the columns
#'                  for `StudentID`, `Stage`, and the responses for each question.
#' @param demog A data frame containing demographic information, including
#'              `StudentID` and `Stage`.
#' @param answer_key A data frame containing the correct answers for each question,
#'                   typically with a column named `CorrectAnswer`.
#' @param dont_know_value A value indicating the response for "don't know" (e.g., 0 or 6).
#' @param items_excluded A character vector of item names to be excluded from the scores.
#' @param stages A numeric vector indicating the stages used for separating the scores.
#'
#' @return A list containing several data frames:
#'   \item{scores_all_neg}{Data frame of scores with negative marking applied.}
#'   \item{scores_ids_all_neg}{Data frame of scores with IDs and negative marking applied.}
#'   \item{scores_neg}{Data frame of scores after removing excluded items and negative marking.}
#'   \item{scores_ids_neg}{Data frame of scores with IDs after removing excluded items and negative marking.}
#'   \item{scores}{Data frame of scores after removing negative marking.}
#'   \item{scores_ids}{Data frame of scores with IDs after removing negative marking.}
#'   \item{scores_all}{Data frame of all scores after removing negative marking.}
#'   \item{scores_ids_all}{Data frame of all scores with IDs after removing negative marking.}
#'   \item{stages_scores_all_neg}{List of data frames, each containing scores for a specific stage with negative marking applied.}
#'   \item{stages_scores_all}{List of data frames, each containing scores for a specific stage after removing negative marking.}
#'   \item{stages_scores_neg}{List of data frames, each containing scores for a specific stage after removing excluded items and negative marking.}
#'   \item{stages_scores}{List of data frames, each containing scores for a specific stage after removing excluded items.}
#'
#' @examples
#' # Example usage of fn_process_scores
#' results <- fn_process_scores(responses = df_responses,
#'                            demog = df_demographics,
#'                            answer_key = df_answer_key,
#'                            dont_know_value = 6,
#'                            items_excluded = c(2, 4),
#'                            stages = 1:3)
#'
#' @export
fn_process_scores <- function(responses,
                           demog,
                           answer_key,
                           dont_know_value,
                           items_excluded,
                           stages) {
  # Initialize an empty list to store results
  results <- list()

  ### scores_all_neg and scores_ids_all_neg
  results$scores_all_neg <- responses |>
    select(-any_of(c("StudentID", "Stage", "Name"))) |>
    apply(MARGIN = 1, function(x) {
      ifelse(x == answer_key$CorrectAnswer,
             1,
             ifelse(x == as.character(dont_know_value), 0, -0.25))
    }) |>
    t() |>
    data.frame(check.names = FALSE)

  results$scores_ids_all_neg <- cbind("StudentID" = responses[, "StudentID"], results$scores_all_neg)

  ### scores_neg and scores_ids_neg
  results$scores_neg <- fnRmColsByName(results$scores_all_neg, items_excluded)
  results$scores_ids_neg <- fnRmColsByName(results$scores_ids_all_neg, items_excluded)

  ### scores, scores_ids, scores_all, scores_ids_all
  results$scores <- fn_remove_neg_marking(scores_data = results$scores_neg)
  results$scores_ids <- fn_remove_neg_marking(scores_data = results$scores_ids_neg)
  results$scores_all <- fn_remove_neg_marking(scores_data = results$scores_all_neg)
  results$scores_ids_all <- fn_remove_neg_marking(scores_data = results$scores_ids_all_neg)

  ### Stage Separation
  stage_ids_scores_all_neg <- demog |>
    select(c(StudentID, Stage)) |>
    right_join(results$scores_ids_all_neg, by = "StudentID")

  ### Separation
  results$stages_scores_all_neg <- split(stage_ids_scores_all_neg |>
                                           select(-c(StudentID, Stage)),
                                         stage_ids_scores_all_neg$Stage)
  names(results$stages_scores_all_neg) <- glue("stage{stages}")

  # Remove negative marking
  results$stages_scores_all <- lapply(results$stages_scores_all_neg, function(x) {
    fn_remove_neg_marking(x)
  })

  # Remove excluded items
  results$stages_scores_neg <- lapply(results$stages_scores_all_neg, function(x) {
    fn_remove_cols_by_name(input = x, items_to_remove = items_excluded)
  })

  # Remove excluded items
  results$stages_scores <- lapply(results$stages_scores_all, function(x) {
    fn_remove_cols_by_name(input = x, items_to_remove = items_excluded)
  })

  return(results)
}
