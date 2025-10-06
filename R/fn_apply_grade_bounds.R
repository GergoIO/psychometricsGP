#' Apply Grade Boundaries and Assign Grades by Stage
#'
#' This function assigns grades to assessment results based on fixed grade boundaries configured for numeric stage IDs.
#' It supports multiple grade boundaries per stage and dynamically creates boundary columns and a grade column.
#' Grade boundaries can be provided per stage as raw scores or percent scores; the function converts as needed.
#' Grades are assigned to intervals defined by the boundaries, promoting grades when the score equals a boundary.
#' Rows with unmatched stages or stages marked as formative receive special handling.
#'
#' For stages where all grade bounds equal a formative label (default "Formative"), the function:
#' - sets all boundary columns (percentage and raw) to NA,
#' - sets the grade column to blank "",
#' - skips boundary conversion and grade assignment.
#'
#' Partial formative bounds for a stage (some formative, some numeric) cause an error.
#'
#' @param .grades_short A data frame containing at least the current stage and percentage score columns.
#'   The stage column should be numeric or factor matching the `stages_with_grading`.
#' @param grading_method_name Character. Internal name/key of the grading method configuration.
#'   Used to locate bounds and types in global `grading` object if `grade_bounds` not supplied directly.
#' @param n_grade_bounds_per_stage Integer. Number of boundaries configured per stage.
#' @param grade_bounds Vector of grade boundaries (numeric or character) for all stages concatenated.
#'   Length must be `n_grade_bounds_per_stage * length(stages_with_grading)`.
#'   Optional; function retrieves from `grading$values[[grading_method_name]]` if missing.
#' @param grade_bounds_type Character. Type of grade bounds: "PCT Score" or "Raw Score".
#'   Optional; retrieves from `grading$value_type[[grading_method_name]]` if missing.
#' @param stages_with_grading Numeric vector. Vector of numeric stage IDs to be graded.
#'   Length multiplied by `n_grade_bounds_per_stage` must equal length of `grade_bounds`.
#'   Optional; retrieves from `grading$values$stages_with_grading` if missing.
#' @param assessment_max_score Numeric. Maximum raw score of the assessment.
#'   Required if converting bounds between raw and percentage scores.
#'   Optional; retrieves from `assessment$score_raw_max` if missing.
#' @param grade_labels Character vector. Ordered vector of grade labels, length = `n_grade_bounds_per_stage + 1`.
#'   Example: c("Fail", "Pass") for 1 boundary; c("Fail", "Pass", "Excellent") for 2 boundaries.
#' @param grade_bound_suffixes Character vector. Suffixes for naming boundary columns.
#'   Length must equal `n_grade_bounds_per_stage`.
#'   Example: c("FP") for 1 boundary, c("FP", "PE") for 2, c("UB", "BS", "SE") for 3.
#' @param grading_method_name_abbreviated Character. Optional abbreviated name for naming columns.
#'   If NULL, uses `grading_method_name` as column name base.
#' @param stage_col Character. Name of the column in `.grades_short` holding the current stage ID.
#'   Default is "StageCurrent".
#' @param score_pct_col Character. Name of the column in `.grades_short` holding the percentage score.
#'   Default is "ScorePct".
#' @param formative_label Character. Special label string used to mark formative grade bounds.
#'   Default is "Formative".
#'
#' @return A modified `.grades_short` data frame with added columns for boundaries and assigned grades.
#'   Boundary columns are named `GradeBoundPct_<abbrev>_<suffix>` and `GradeBoundRaw_<abbrev>_<suffix>`.
#'   Grade column is named `Grade_<abbrev>`.
#' @export
#'
#' @examples
#' \dontrun{
#' # FP example with numeric stages
#' .grades_short <- tibble::tibble(
#'   StageCurrent = c(1, 1, 1),
#'   ScorePct = c(45, 75, NA)
#' )
#' .grades_short <- fn_apply_grade_bounds(
#'   .grades_short,
#'   grading_method_name = "fixed_fail_pass",
#'   grading_method_name_abbreviated = "FixedFP",
#'   n_grade_bounds_per_stage = 1,
#'   grade_bounds = c(50),
#'   grade_bounds_type = "PCT Score",
#'   stages_with_grading = c(1),
#'   assessment_max_score = 100,
#'   grade_labels = c("Fail", "Pass"),
#'   grade_bound_suffixes = c("FP")
#' )
#'
#' # UBSE example with two numeric stages (1 formative, 2 graded)
#' # Stage 1 is formative (all bounds "Formative"), stage 2 has numeric bounds
#' .grades_short <- tibble::tibble(
#'   StageCurrent = c(1, 1, 2, 2),
#'   ScorePct = c(25, 40, 55, 85)
#' )
#' .grades_short <- fn_apply_grade_bounds(
#'   .grades_short,
#'   grading_method_name = "fixed_ubse",
#'   grading_method_name_abbreviated = "FixedUBSE",
#'   n_grade_bounds_per_stage = 3,
#'   grade_bounds = c(
#'     "Formative", "Formative", "Formative",  # Stage 1 (formative)
#'     40, 60, 90                             # Stage 2
#'   ),
#'   grade_bounds_type = "PCT Score",
#'   stages_with_grading = c(1, 2),
#'   assessment_max_score = 100,
#'   grade_labels = c("Unsat", "Bord", "Sat", "Exc"),
#'   grade_bound_suffixes = c("UB", "BS", "SE"),
#'   formative_label = "Formative"
#' )
#' }
fn_apply_grade_bounds <- function(
    .grades_short,
    grading_method_name,
    n_grade_bounds_per_stage,
    grade_bounds = NULL,
    grade_bounds_type = NULL,
    stages_with_grading = NULL,
    assessment_max_score = NULL,
    grade_labels = c("Fail", "Pass"),
    grade_bound_suffixes = c("FP"),
    grading_method_name_abbreviated = NULL,
    stage_col = "StageCurrent",
    score_pct_col = "ScorePct",
    formative_label = "Formative"
) {
  grade_method_colname <- if (!is.null(grading_method_name_abbreviated)) grading_method_name_abbreviated else grading_method_name

  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("The dplyr package is required but not installed.")
  }
  library(dplyr)

  if (is.null(grade_bounds)) {
    grade_bounds <- grading$values[[grading_method_name]]
    if (is.null(grade_bounds)) stop(sprintf("No grade_bounds found at grading$values[['%s']].", grading_method_name))
  }
  if (is.null(grade_bounds_type)) {
    grade_bounds_type <- grading$value_type[[grading_method_name]]
    if (is.null(grade_bounds_type)) stop(sprintf("No grade_bounds_type found at grading$value_type[['%s']].", grading_method_name))
  }
  if (is.null(stages_with_grading)) {
    stages_with_grading <- grading$values$stages_with_grading
    if (is.null(stages_with_grading)) stop("No stages_with_grading found at grading$values$stages_with_grading.")
  }
  if (is.null(assessment_max_score)) {
    assessment_max_score <- assessment$score_raw_max
    if (is.null(assessment_max_score)) stop("No assessment_max_score found at assessment$score_raw_max.")
  }

  if (length(grade_bounds) != n_grade_bounds_per_stage * length(stages_with_grading)) {
    stop(
      sprintf(
        "'%s': Mismatch between grade_bounds length (%d) and stages_with_grading length (%d) * n_grade_bounds_per_stage (%d)",
        grading_method_name, length(grade_bounds), length(stages_with_grading), n_grade_bounds_per_stage
      )
    )
  }
  if (length(grade_bound_suffixes) != n_grade_bounds_per_stage) {
    stop(
      sprintf(
        "'%s': Number of grade_bound_suffixes (%d) must equal n_grade_bounds_per_stage (%d)",
        grading_method_name, length(grade_bound_suffixes), n_grade_bounds_per_stage
      )
    )
  }
  if (length(grade_labels) != (n_grade_bounds_per_stage + 1)) {
    stop(
      sprintf(
        "'%s': Length of grade_labels (%d) must be equal to n_grade_bounds_per_stage + 1 (%d)",
        grading_method_name, length(grade_labels), n_grade_bounds_per_stage + 1
      )
    )
  }

  for (i_stage in seq_along(stages_with_grading)) {
    bounds_stage <- grade_bounds[((i_stage - 1) * n_grade_bounds_per_stage + 1):(i_stage * n_grade_bounds_per_stage)]
    formative_mask <- bounds_stage == formative_label
    if (any(formative_mask) && !all(formative_mask)) {
      stop(sprintf(
        "Stage '%s' grade bounds contain partial formative entries; all bounds must be '%s' or none.",
        stages_with_grading[i_stage], formative_label
      ))
    }
  }

  grade_col <- paste0("Grade_", grade_method_colname)
  if (!grade_col %in% names(.grades_short) || !is.character(.grades_short[[grade_col]])) {
    .grades_short[[grade_col]] <- rep(NA_character_, nrow(.grades_short))
  }

  # Create column names grouped: all PCT, then all RAW
  pct_cols <- paste0("GradeBoundPct_", grade_method_colname, "_", grade_bound_suffixes)
  raw_cols <- paste0("GradeBoundRaw_", grade_method_colname, "_", grade_bound_suffixes)

  # Initialize boundary columns if missing or wrong type
  for (col in c(pct_cols, raw_cols)) {
    if (!col %in% names(.grades_short) || !is.numeric(.grades_short[[col]])) {
      .grades_short[[col]] <- rep(NA_real_, nrow(.grades_short))
    }
  }

  # Add index for existing stages
  .grades_short <- .grades_short %>%
    mutate(index_stage = match(.data[[stage_col]], stages_with_grading)) %>%
    mutate(
      !!grade_col := ifelse(is.na(index_stage), "", !!sym(grade_col))
    )

  valid_rows <- which(!is.na(.grades_short$index_stage))
  base_indices <- (.grades_short$index_stage[valid_rows] - 1) * n_grade_bounds_per_stage

  formative_stages <- sapply(seq_along(stages_with_grading), function(i) {
    idx <- ((i - 1) * n_grade_bounds_per_stage + 1):(i * n_grade_bounds_per_stage)
    all(grade_bounds[idx] == formative_label)
  })

  for (i_stage in which(!formative_stages)) {
    idx_bounds <- ((i_stage - 1) * n_grade_bounds_per_stage + 1):(i_stage * n_grade_bounds_per_stage)
    bounds_stage_raw <- grade_bounds[idx_bounds]
    bounds_stage_num <- as.numeric(bounds_stage_raw)

    if (grade_bounds_type == "PCT Score") {
      bounds_pct <- bounds_stage_num
      bounds_raw <- bounds_stage_num * assessment_max_score / 100
    } else if (grade_bounds_type == "Raw Score") {
      bounds_raw <- bounds_stage_num
      bounds_pct <- bounds_stage_num / assessment_max_score * 100
    } else {
      stop(sprintf("Unknown grade_bounds_type '%s'", grade_bounds_type))
    }

    rows_stage <- valid_rows[which(.grades_short$index_stage[valid_rows] == i_stage)]

    # Fill all PCT bounds first
    for (i in seq_along(pct_cols)) {
      .grades_short[[pct_cols[i]]][rows_stage] <- bounds_pct[i]
    }

    # Fill all RAW bounds next
    for (i in seq_along(raw_cols)) {
      .grades_short[[raw_cols[i]]][rows_stage] <- bounds_raw[i]
    }

    bounds_mat <- matrix(rep(bounds_pct, length(rows_stage)), nrow = length(rows_stage), byrow = TRUE)

    assign_grades <- function(score, bounds, labels) {
      if (is.na(score)) return(labels[1])
      for (j in seq_along(bounds)) {
        if (score < bounds[j]) return(labels[j])
      }
      labels[length(labels)]
    }

    to_assign <- is.na(.grades_short[[grade_col]][rows_stage])
    if (any(to_assign)) {
      assign_rows <- rows_stage[to_assign]
      Grade_vec <- mapply(
        assign_grades,
        .grades_short[[score_pct_col]][assign_rows],
        split(bounds_mat[to_assign, , drop = FALSE], seq_along(assign_rows)),
        MoreArgs = list(labels = grade_labels)
      )
      .grades_short[[grade_col]][assign_rows] <- Grade_vec
    }
  }

  # Assign NA boundaries and blank grade for formative stages
  formative_stage_indices <- which(formative_stages)
  if (length(formative_stage_indices) > 0) {
    for (i_stage in formative_stage_indices) {
      rows_stage <- valid_rows[which(.grades_short$index_stage[valid_rows] == i_stage)]
      for (col in pct_cols) .grades_short[[col]][rows_stage] <- NA_real_
      for (col in raw_cols) .grades_short[[col]][rows_stage] <- NA_real_
      .grades_short[[grade_col]][rows_stage] <- ""
    }
  }

  .grades_short <- .grades_short |> select(-index_stage)

  return(.grades_short)
}
