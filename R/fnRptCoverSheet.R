#' Initialise the default cover page for a report
#'
#' @param rptVar The variable name of the report
#' @param rptTitle This string will be the report title, shown prominently on the coverpage
#' @param lstOfDetails This variable should be a list containing as least the following defined variables: academicYear, programme, stage, module, assessmentNumber, assessmentDate. These variables will be input on the cover page.
#'
#' @return Nothing is explicitly returned, rather the cover page of the variable containing the report is populated with the required information.
#' @export
#'
#' @examples fnRptCoverSheet(rptVar, rptTitle, lstOfDetails)

################################################################################

fnRptCoverSheet <-
  function (rptVar = NULL,
            rptTitle = NULL,
            lstOfDetails = NULL) {
    continue <- TRUE
    if (is.null(rptVar) == TRUE |
        is.null(rptTitle) == TRUE |
        is.null(lstOfDetails) == TRUE) {
      continue <- FALSE
      warning("One of the required variables for this function has not been specified.")
    }
    if (continue == TRUE) {
      rptVar <-
        body_replace_all_text(
          rptVar,
          old_value = "YearPlaceholder",
          new_value = lstOfDetails$academicYear,
          only_at_cursor = FALSE
        )
      rptVar <-
        body_replace_all_text(
          rptVar,
          old_value = "ProgrammePlaceholder",
          new_value = lstOfDetails$programme,
          only_at_cursor = FALSE
        )
      rptVar <-
        body_replace_all_text(
          rptVar,
          old_value = "StagePlaceholder",
          new_value = as.character(lstOfDetails$stage),
          only_at_cursor = FALSE
        )
      rptVar <-
        body_replace_all_text(
          rptVar,
          old_value = "ModulePlaceholder",
          new_value = lstOfDetails$module,
          only_at_cursor = FALSE
        )
      rptVar <-
        body_replace_all_text(
          rptVar,
          old_value = "AssessmentPlaceholder",
          new_value = lstOfDetails$assessmentNumber,
          only_at_cursor = FALSE
        )
      rptVar <-
        body_replace_all_text(
          rptVar,
          old_value = "DatePlaceholder",
          new_value = lstOfDetails$assessmentDate,
          only_at_cursor = FALSE
        )
      rptVar <-
        cursor_reach(rptVar, keyword = "ReportTitlePlaceholder")
      rptVar <-
        body_add_par(rptVar, rptTitle, style = "heading 1", pos = "on")
      rptVar <-
        body_replace_all_text(
          rptVar,
          old_value = "StartPlaceholder",
          new_value = "",
          only_at_cursor = FALSE
        )
      rptVar <- cursor_end(rptVar)
    }
  }# END
