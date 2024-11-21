#' Initialise the default cover page for a report
#'
#' @param report_var The variable name of the report
#' @param report_title This string will be the report title, shown prominently on the coverpage
#' @param vars_assessment This variable should be a list containing as least the following defined variables: academic_year, programme (OPTIONAL: programme is overridden by programme_long so long as programme_long is defined) stage/stages (the function automatically handles either scenario with the default being to use stage), module, assessment, assessment_date. These variables will be input on the cover page.
#'
#' @return Nothing is explicitly returned, rather the cover page of the variable containing the report is populated with the required information.
#' @export
#'
#' @examples fn_rpt_cover_sheet(report_var, report_title, vars_assessment)
#'

fn_rpt_cover_sheet <-
  function (report_var = NULL,
            report_title = NULL,
            vars_assessment = NULL) {
    if (is.null(report_var) |
        is.null(report_title) |
        is.null(vars_assessment)) {
      stop(
        "fn_rpt_cover_sheet: One of the required variables for this function has not been specified."
      )
    }
    if (is.null(vars_assessment$academic_year)) {
      stop(
        "fn_rpt_cover_sheet: An item named 'academic_year' must defined in the provided 'vars_assessment' list (use vars_assessment$academic_year <- *** and replace 'vars_assessment' with actual variable name."
      )
    }
    if (is.null(vars_assessment$programme)) {
      stop(
        "fn_rpt_cover_sheet: An item named 'programme' must defined in the provided 'vars_assessment' list (use vars_assessment$programme <- *** and replace 'vars_assessment' with actual variable name."
      )
    }
    if (is.null(vars_assessment$module)) {
      stop(
        "fn_rpt_cover_sheet: An item named 'module' must defined in the provided 'vars_assessment' list (use vars_assessment$module <- *** and replace 'vars_assessment' with actual variable name."
      )
    }
    if (is.null(vars_assessment$assessment)) {
      stop(
        "fn_rpt_cover_sheet: An item named 'assessment' must defined in the provided 'vars_assessment' list (use vars_assessment$assessment <- *** and replace 'vars_assessment' with actual variable name."
      )
    }
    if (is.null(vars_assessment$assessment_date)) {
      stop(
        "fn_rpt_cover_sheet: An item named 'assessment_date' must defined in the provided 'vars_assessment' list (use vars_assessment$assessment_date <- *** and replace 'vars_assessment' with actual variable name."
      )
    } else{
      report_var <-
        body_replace_all_text(
          report_var,
          old_value = "YearPlaceholder",
          new_value = fn_academic_year_format(
            academic_year_short = vars_assessment$academic_year,
            type = "Short Slash"
          ),
          only_at_cursor = FALSE
        )
      # If "programme_long" is defined, use that instead of "programme"
      if (!is.null(vars_assessment$programme_long)) {
        report_var <-
          body_replace_all_text(
            report_var,
            old_value = "ProgrammePlaceholder",
            new_value = vars_assessment$programme_long,
            only_at_cursor = FALSE
          )
      } else{
        report_var <-
          body_replace_all_text(
            report_var,
            old_value = "ProgrammePlaceholder",
            new_value = vars_assessment$programme,
            only_at_cursor = FALSE
          )
      }
      # Reason for additional is_scalar_character check
      # cnst$stage can be defined even though it is not for a single stage (not identified why this happens yet)
      # if cnst$stage = "2, 3, 4, 5" etc then the stages version should be used even though cnst$stage is defined
      # this first version can only be used when cnst$stage is a single number
      if (!is.null(vars_assessment$stage)  &&
          is_scalar_character(as.character(vars_assessment$stage))) {
        report_var <-
          body_replace_all_text(
            report_var,
            old_value = "StagePlaceholder",
            new_value = as.character(vars_assessment$stage),
            only_at_cursor = FALSE
          )
      } else if (!is.null(vars_assessment$stages)) {
        report_var <-
          body_replace_all_text(
            report_var,
            old_value = "StagePlaceholder",
            new_value = as.character(sub(
              ",([^,]*)$",
              " and\\1",
              paste0(vars_assessment$stages, collapse = ", ")
            )),
            only_at_cursor = FALSE
          )
      }
      report_var <-
        body_replace_all_text(
          report_var,
          old_value = "ModulePlaceholder",
          new_value = vars_assessment$module,
          only_at_cursor = FALSE
        )
      report_var <-
        body_replace_all_text(
          report_var,
          old_value = "AssessmentPlaceholder",
          new_value = vars_assessment$assessment,
          only_at_cursor = FALSE
        )
      report_var <-
        body_replace_all_text(
          report_var,
          old_value = "DatePlaceholder",
          new_value = vars_assessment$assessment_date,
          only_at_cursor = FALSE
        )
      report_var <-
        cursor_reach(report_var, keyword = "ReportTitlePlaceholder")
      report_var <-
        body_add_par(report_var,
                     report_title,
                     style = "heading 1",
                     pos = "on")
      report_var <-
        body_replace_all_text(
          report_var,
          old_value = "StartPlaceholder",
          new_value = "",
          only_at_cursor = FALSE
        )
      report_var <- cursor_end(report_var)
    }
  }# END
