#' Apply Grade Boundaries and Assign Grades by Stage (No Global Lookups)
#'
#' Assign grades to assessment results using explicit, user-supplied grade boundaries,
#' grade bounds type, stage mapping, and other parameters.
#' No lookups are performed in external/global objects — all configuration must be
#' provided during the call.
#'
#' The input \code{data} must contain columns representing the current stage and the
#' percentage score, named as specified by \code{stage_col} (default: "StageCurrent") and
#' \code{score_pct_col} (default: "ScorePct").
#'
#' Grade boundaries can be expressed either as raw scores or as percentage scores; these
#' are converted as needed based on \code{grade_bounds_type} which must be either
#' \dQuote{Percentage Score} or \dQuote{Raw Score}.
#'
#' @param data A data frame with at least two columns: one for stage IDs, one for percentage scores.
#' @param n_grade_bounds_per_stage Integer scalar: number of grade boundaries for each stage.
#' @param grade_bounds A concatenated vector of grade boundaries for all stages.
#'   Must have length \code{n_grade_bounds_per_stage * length(stages_with_grading)}.
#' @param grade_bounds_type Character scalar; must be one of \dQuote{Percentage Score} or \dQuote{Raw Score}.
#' @param stages_with_grading Numeric vector of stage IDs to be graded.
#' @param max_raw_score Numeric scalar representing the maximum raw score achievable.
#' @param grade_labels Character vector with length \code{n_grade_bounds_per_stage + 1}, ordered grade labels.
#' @param grade_bound_suffixes Character vector with length \code{n_grade_bounds_per_stage} used for naming boundary columns.
#' @param grading_method_name Character scalar used as part of new column names.
#' @param grading_method_name_abbreviated Character scalar (optional) abbreviated string used for column names.
#' @param stage_col Character scalar naming the column in \code{data} that contains stage IDs. Default: \dQuote{StageCurrent}.
#' @param score_pct_col Character scalar naming the column in \code{data} that contains percentage scores. Default: \dQuote{ScorePct}.
#' @param formative_label Character scalar designating formative-only grade bounds. Default: \dQuote{Formative}.
#' @param require_integer_stages Logical scalar indicating if \code{stages_with_grading} should be strictly integer. Default: \code{TRUE}.
#'
#' @return Modified \code{data} with new columns for grade boundaries and assigned grades.
#'   Boundary columns are named \code{GradeBoundPct_<abbrev>_<suffix>} and \code{GradeBoundRaw_<abbrev>_<suffix>}.
#'   The grade column is named \code{Grade_<abbrev>}, where \code{<abbrev>} is \code{grading_method_name_abbreviated} if provided or \code{grading_method_name}.
#'
#' @export
#'
#' @examples
#' library(tibble)
#' # Simple pass/fail grading on two stages
#' scores <- tibble(StageCurrent = c(1, 1, 2, 2),
#'                  ScorePct = c(45, 82, 40, 99))
#' fn_apply_grade_bounds(
#'   data = scores,
#'   n_grade_bounds_per_stage = 1,
#'   grade_bounds = c(50, 60),
#'   grade_bounds_type = "Percentage Score",
#'   stages_with_grading = c(1, 2),
#'   max_raw_score = 100,
#'   grade_labels = c("Fail", "Pass"),
#'   grade_bound_suffixes = c("FP"),
#'   grading_method_name = "FP"
#' )
#'
#' # Formative stage example (bounds all Formative)
#' scores2 <- tibble(StageCurrent = c(1, 2, 2), ScorePct = c(10, 40, 90))
#' fn_apply_grade_bounds(
#'   data = scores2,
#'   n_grade_bounds_per_stage = 2,
#'   grade_bounds = c("Formative", "Formative", 35, 85),
#'   grade_bounds_type = "Percentage Score",
#'   stages_with_grading = c(1, 2),
#'   max_raw_score = 100,
#'   grade_labels = c("Unsat", "Sat", "Exc"),
#'   grade_bound_suffixes = c("UB", "SE"),
#'   grading_method_name = "UBSE"
#' )
fn_apply_grade_bounds <- function(
    data,
    n_grade_bounds_per_stage,
    grade_bounds,
    grade_bounds_type,
    stages_with_grading,
    max_raw_score,
    grade_labels,
    grade_bound_suffixes,
    grading_method_name,
    grading_method_name_abbreviated = NULL,
    stage_col = "StageCurrent",
    score_pct_col = "ScorePct",
    formative_label = "Formative",
    require_integer_stages = TRUE
) {
  # Helper for error messaging prefix
  fn_name <- "fn_apply_grade_bounds"

  grade_method_colname <- if (!is.null(grading_method_name_abbreviated)) grading_method_name_abbreviated else grading_method_name

  # Load dplyr for pipes and mutate
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop(sprintf("%s: The dplyr package is required but not installed.", fn_name))
  }
  library(dplyr)

  # ----- Input Column Checks -----
  if (!stage_col %in% names(data)) {
    stop(sprintf("%s: Input data is missing the stage column '%s'.", fn_name, stage_col))
  }
  if (!score_pct_col %in% names(data)) {
    stop(sprintf("%s: Input data is missing the score percentage column '%s'.", fn_name, score_pct_col))
  }

  # Warn if columns contain all NA (likely a user issue)
  if (all(is.na(data[[stage_col]]))) {
    warning(sprintf("%s: All values are NA in stage column '%s'.", fn_name, stage_col))
  }
  if (all(is.na(data[[score_pct_col]]))) {
    warning(sprintf("%s: All values are NA in score percentage column '%s'.", fn_name, score_pct_col))
  }

  # ----- Stage Type and Duplication Checks -----
  if (require_integer_stages && any(as.integer(data[[stage_col]]) != data[[stage_col]], na.rm = TRUE)) {
    stop(sprintf("%s: Stage column '%s' contains non-integer values but require_integer_stages=TRUE.", fn_name, stage_col))
  }
  if (require_integer_stages && any(as.integer(stages_with_grading) != stages_with_grading)) {
    stop(sprintf("%s: 'stages_with_grading' vector contains non-integer values but require_integer_stages=TRUE.", fn_name))
  }
  if (any(duplicated(stages_with_grading))) {
    stop(sprintf("%s: 'stages_with_grading' contains duplicates, please remove.", fn_name))
  }

  # ----- Argument Length and Type Validation -----
  if (length(grade_bounds) != n_grade_bounds_per_stage * length(stages_with_grading)) {
    stop(sprintf("%s: grade_bounds length (%d) does not equal n_grade_bounds_per_stage (%d) * length(stages_with_grading) (%d).",
                 fn_name, length(grade_bounds), n_grade_bounds_per_stage, length(stages_with_grading)))
  }
  if (length(grade_bound_suffixes) != n_grade_bounds_per_stage) {
    stop(sprintf("%s: grade_bound_suffixes length (%d) must equal n_grade_bounds_per_stage (%d).",
                 fn_name, length(grade_bound_suffixes), n_grade_bounds_per_stage))
  }
  if (length(grade_labels) != (n_grade_bounds_per_stage + 1)) {
    stop(sprintf("%s: grade_labels length (%d) must equal n_grade_bounds_per_stage + 1 (%d).",
                 fn_name, length(grade_labels), n_grade_bounds_per_stage + 1))
  }
  if (!is.character(grade_bounds_type) || length(grade_bounds_type) != 1) {
    stop(sprintf("%s: grade_bounds_type must be a single character string.", fn_name))
  }
  if (!grade_bounds_type %in% c("Percentage Score", "Raw Score")) {
    stop(sprintf("%s: grade_bounds_type must be one of 'Percentage Score' or 'Raw Score'. Given: %s", fn_name, grade_bounds_type))
  }
  if (!is.numeric(max_raw_score) || length(max_raw_score) != 1 || max_raw_score <= 0) {
    stop(sprintf("%s: max_raw_score must be a single positive numeric value.", fn_name))
  }

  # ----- Grade Bounds Validation -----
  # Check each stage's bounds for validity:
  for (i_stage in seq_along(stages_with_grading)) {
    idx_start <- ((i_stage - 1) * n_grade_bounds_per_stage + 1)
    idx_end <- i_stage * n_grade_bounds_per_stage
    bounds_stage <- grade_bounds[idx_start:idx_end]
    formative_mask <- bounds_stage == formative_label

    # Ensure partial formative bounds cause error
    if (any(formative_mask) && !all(formative_mask)) {
      stop(sprintf("%s: Stage '%s' grade bounds are partially formative; all or none must be '%s'.",
                   fn_name, stages_with_grading[i_stage], formative_label))
    }

    # If not formative, check numeric bounds
    if (!all(formative_mask)) {
      numeric_bounds <- suppressWarnings(as.numeric(bounds_stage))
      if (any(is.na(numeric_bounds))) {
        stop(sprintf("%s: Non-numeric grade bounds found in stage '%s'.", fn_name, stages_with_grading[i_stage]))
      }
      # Check bounds increasing order (strictly non-decreasing)
      if (any(diff(numeric_bounds) < 0)) {
        stop(sprintf("%s: Grade bounds for stage '%s' are not in ascending order.", fn_name, stages_with_grading[i_stage]))
      }
      # Additional check for percentage bounds range
      if (grade_bounds_type == "Percentage Score") {
        if (any(numeric_bounds < 0 | numeric_bounds > 100)) {
          stop(sprintf("%s: Percentage grade bounds for stage '%s' must be between 0 and 100.", fn_name, stages_with_grading[i_stage]))
        }
      }
    }
  }

  # ----- Initialize grade column if missing or wrong type -----
  grade_col <- paste0("Grade_", grade_method_colname)
  if (!grade_col %in% names(data) || !is.character(data[[grade_col]])) {
    data[[grade_col]] <- rep(NA_character_, nrow(data))
  }

  pct_cols <- paste0("GradeBoundPct_", grade_method_colname, "_", grade_bound_suffixes)
  raw_cols <- paste0("GradeBoundRaw_", grade_method_colname, "_", grade_bound_suffixes)

  # Initialize or reset boundary columns if missing or wrong type
  for (col in c(pct_cols, raw_cols)) {
    if (!col %in% names(data) || !is.numeric(data[[col]])) {
      data[[col]] <- rep(NA_real_, nrow(data))
    }
  }

  # ----- Index matching and initial grade assignment -----
  data <- data %>%
    mutate(index_stage = match(.data[[stage_col]], stages_with_grading)) %>%
    mutate(
      !!grade_col := ifelse(is.na(index_stage), "", !!sym(grade_col))
    )

  valid_rows <- which(!is.na(data$index_stage))

  # Determine formative stages once for all:
  formative_stages <- sapply(seq_along(stages_with_grading), function(i) {
    idxs <- ((i - 1) * n_grade_bounds_per_stage + 1):(i * n_grade_bounds_per_stage)
    all(grade_bounds[idxs] == formative_label)
  })

  # ----- Assign bounds and grades for non-formative stages -----
  for (i_stage in which(!formative_stages)) {
    idx_bounds <- ((i_stage - 1) * n_grade_bounds_per_stage + 1):(i_stage * n_grade_bounds_per_stage)
    bounds_stage_raw <- grade_bounds[idx_bounds]
    bounds_numeric <- as.numeric(bounds_stage_raw)

    if (grade_bounds_type == "Percentage Score") {
      bounds_pct <- bounds_numeric
      bounds_raw <- bounds_numeric * max_raw_score / 100
    } else if (grade_bounds_type == "Raw Score") {
      bounds_raw <- bounds_numeric
      bounds_pct <- bounds_numeric / max_raw_score * 100
    }

    rows_stage <- valid_rows[data$index_stage[valid_rows] == i_stage]

    for (i in seq_along(pct_cols)) {
      data[[pct_cols[i]]][rows_stage] <- bounds_pct[i]
    }
    for (i in seq_along(raw_cols)) {
      data[[raw_cols[i]]][rows_stage] <- bounds_raw[i]
    }

    bounds_mat <- matrix(rep(bounds_pct, length(rows_stage)), nrow = length(rows_stage), byrow = TRUE)

    assign_grades <- function(score, bounds, labels) {
      if (is.na(score)) return(labels[1])
      for (j in seq_along(bounds)) {
        if (score < bounds[j]) return(labels[j])
      }
      labels[length(labels)]
    }

    to_assign <- is.na(data[[grade_col]][rows_stage])
    if (any(to_assign)) {
      assign_rows <- rows_stage[to_assign]
      Grade_vec <- mapply(
        assign_grades,
        data[[score_pct_col]][assign_rows],
        split(bounds_mat[to_assign, , drop = FALSE], seq_along(assign_rows)),
        MoreArgs = list(labels = grade_labels)
      )
      data[[grade_col]][assign_rows] <- Grade_vec
    }
  }

  # ----- Handle formative-only stages -----
  formative_stage_indices <- which(formative_stages)
  if (length(formative_stage_indices) > 0) {
    for (i_stage in formative_stage_indices) {
      rows_stage <- valid_rows[data$index_stage[valid_rows] == i_stage]
      for (col in pct_cols) data[[col]][rows_stage] <- NA_real_
      for (col in raw_cols) data[[col]][rows_stage] <- NA_real_
      data[[grade_col]][rows_stage] <- ""
    }
  }

  # Remove temporary index column
  data <- data |> select(-index_stage)

  return(data)
}
