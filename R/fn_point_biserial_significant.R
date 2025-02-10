#' Calculate PtBis and Statistical Significance for Test Items
#'
#' @param data (dataframe) Incoming long data eg a dataframe with cols student_id, item_id, and is_correct
#' @param assessment (String) The name of the test.
#' @param col_student_id (default = "StudentID"), String The name of the column containing the Student ID number
#' @param col_is_correct (default = "IsCorrect"), String. The name of the column containing the item score in each row
#' @param col_item_id (default = "ItemID"), String. The name of the column containing the ItemID in each row
#' @param significance_level (default = 0.05) Numeric. Set the significance level for each point biserial correlation
#'
#' @return returns a new dataframe storing the point biserial info for all input items
#' @export
#'
#' @examples
#' df <- data.frame(
#'   StudentID = rep(1:3, each = 6),
#'   ItemID = rep(c("A", "B", "C", "D", "E", "F"), 3),
#'   IsCorrect = c(1, 0, 1, 1, 0, 1, 1, 1, 0, 1, 0, 0, 0, 1, 0, 0, 1, 1, 1, 0)
#' )
#' df |> fn_point_biserial_significant()

fn_point_biserial_significant <- function(data,
                                          assessment = "Test",
                                          col_student_id = "StudentID",
                                          col_is_correct = "IsCorrect",
                                          col_item_id = "ItemID",
                                          significance_level = 0.05) {
  data <- data |>
    select(all_of(c(
      col_student_id, col_item_id, col_is_correct
    ))) |>
    group_by_at(col_student_id) |>
    mutate(total_score = sum(across(all_of(col_is_correct))))

  data_pt_bis <- data.frame(
    assessment = character(),
    item_id = character(),
    correlation_coefficient_cor = numeric(),
    p_value_cor = numeric(),
    correlation_coefficient_cor_test = numeric(),
    p_value_cor_test = numeric(),
    stringsAsFactors = FALSE
  )

  n_items_processed <- 0
  pct_items_progressed_next <- 20
  list_item_numbers <- unique(data[[col_item_id]])

  for (item_number in list_item_numbers) {
    data_item <- data |> filter(!!sym(col_item_id) == item_number)
    correlation_cor <- cor(data_item[[col_is_correct]], data_item$total_score - data_item[[col_is_correct]])

    if (sum(data_item[[col_is_correct]]) == 0 |
        sum(data_item[[col_is_correct]]) == nrow(data_item) |
        sum(is.finite(data_item$total_score)) < 2 |
        nrow(data_item) == 2) {
      correlation_result <- list(r = NA, p = NA)
    } else {
      total_score_excl_curr_item <- data_item$total_score - data_item[[col_is_correct]]

      result <- tryCatch(
        cor.test(data_item[[col_is_correct]], total_score_excl_curr_item),
        error = function(e) {
          cat("Error occurred during cor.test for the following data:\n")
          print(data_item)
          correlation_result <- list(r = NA, p = NA)
        }
      )

      if (is.null(result)) {
        correlation_result <- list(r = NA, p = NA)
      } else {
        correlation_result <- list(r = result$estimate, p = result$p.value)
      }
    }

    data_pt_bis <- rbind(
      data_pt_bis,
      data.frame(
        Assessment = assessment,
        item_id = item_number,
        correlation_coefficient_cor = correlation_cor,
        p_value_cor = NA,
        correlation_coefficient_cor_test = correlation_result$r,
        p_value_cor_test = correlation_result$p,
        correlation_coefficient_significant = ifelse(
          correlation_result$p < significance_level,
          correlation_result$r,
          NA
        )
      )
    )

    n_items_processed <- n_items_processed + 1
    pct_items_processed_curr <- n_items_processed / length(unique(list_item_numbers)) * 100
    if (pct_items_processed_curr >= pct_items_progressed_next) {
      print(paste(
        "PtBis Calculation Progress:",
        floor(pct_items_processed_curr),
        "% of items considered"
      ))
      pct_items_progressed_next <- pct_items_progressed_next + 20
    }
  }
  return(data_pt_bis)
}
