#' Initialise the default cover page for a report
#'
#' @param reportVar The variable name of the report
#' @param reportTitle This string will be the report title, shown prominently on the coverpage
#' @param listOfDetails This variable should be a list containing as least the following defined variables: academicYear, programme, stage/stages (the function automatically handles either scenario with the default being to use stage), module, assessmentNumber, assessmentDate. These variables will be input on the cover page.
#'
#' @return Nothing is explicitly returned, rather the cover page of the variable containing the report is populated with the required information.
#' @export
#'
#' @examples fnRptCoverSheet(reportVar, reportTitle, listOfDetails)

################################################################################

fnRptCoverSheet <-
  function (reportVar = NULL,
            reportTitle = NULL,
            listOfDetails = NULL) {
    if (is.null(reportVar) == TRUE |
        is.null(reportTitle) == TRUE |
        is.null(listOfDetails) == TRUE) {
      stop(
        "fnRptCoverSheet: One of the required variables for this function has not been specified."
      )
    }
    if (is.null(listOfDetails$academicYear) == TRUE) {
      stop(
        "fnRptCoverSheet: An item named 'academicYear' must defined in the provided 'listOfDetails' list (use listOfDetails$academicYear <- *** and replace 'listOfDetails' with actual variable name."
      )
    }
    if (is.null(listOfDetails$programme) == TRUE) {
      stop(
        "fnRptCoverSheet: An item named 'programme' must defined in the provided 'listOfDetails' list (use listOfDetails$programme <- *** and replace 'listOfDetails' with actual variable name."
      )
    }
    if (is.null(listOfDetails$module) == TRUE) {
      stop(
        "fnRptCoverSheet: An item named 'module' must defined in the provided 'listOfDetails' list (use listOfDetails$module <- *** and replace 'listOfDetails' with actual variable name."
      )
    }
    if (is.null(listOfDetails$assessment) == TRUE) {
      stop(
        "fnRptCoverSheet: An item named 'assessment' must defined in the provided 'listOfDetails' list (use listOfDetails$assessment <- *** and replace 'listOfDetails' with actual variable name."
      )
    }
    if (is.null(listOfDetails$assessmentDate) == TRUE) {
      stop(
        "fnRptCoverSheet: An item named 'assessmentDate' must defined in the provided 'listOfDetails' list (use listOfDetails$assessmentDate <- *** and replace 'listOfDetails' with actual variable name."
      )
    } else{
      reportVar <-
        body_replace_all_text(
          reportVar,
          old_value = "YearPlaceholder",
          new_value = fnAcademicYearFormat(listOfDetails$academicYear, "Short Slash"),
          only_at_cursor = FALSE
        )
      reportVar <-
        body_replace_all_text(
          reportVar,
          old_value = "ProgrammePlaceholder",
          new_value = listOfDetails$programme,
          only_at_cursor = FALSE
        )
      # Reason for additional is_scalar_character check
      # cnst$stage can be defined even though it is not for a single stage (not identified why this happens yet)
      # if cnst$stage = "2, 3, 4, 5" etc then the stages version should be used even though cnst$stage is defined
      # this first version can only be used when cnst$stage is a single number
      if (is.null(listOfDetails$stage) == FALSE  &&
          is_scalar_character(as.character(listOfDetails$stage)) == TRUE) {
        reportVar <-
          body_replace_all_text(
            reportVar,
            old_value = "StagePlaceholder",
            new_value = as.character(listOfDetails$stage),
            only_at_cursor = FALSE
          )
      } else if (is.null(listOfDetails$stages) == FALSE) {
        reportVar <-
          body_replace_all_text(
            reportVar,
            old_value = "StagePlaceholder",
            new_value = as.character(sub(
              ",([^,]*)$",
              " and\\1",
              paste0(listOfDetails$stages, collapse = ", ")
            )),
            only_at_cursor = FALSE
          )
      }
      reportVar <-
        body_replace_all_text(
          reportVar,
          old_value = "ModulePlaceholder",
          new_value = listOfDetails$module,
          only_at_cursor = FALSE
        )
      reportVar <-
        body_replace_all_text(
          reportVar,
          old_value = "AssessmentPlaceholder",
          new_value = listOfDetails$assessment,
          only_at_cursor = FALSE
        )
      reportVar <-
        body_replace_all_text(
          reportVar,
          old_value = "DatePlaceholder",
          new_value = listOfDetails$assessmentDate,
          only_at_cursor = FALSE
        )
      reportVar <-
        cursor_reach(reportVar, keyword = "ReportTitlePlaceholder")
      reportVar <-
        body_add_par(reportVar, reportTitle, style = "heading 1", pos = "on")
      reportVar <-
        body_replace_all_text(
          reportVar,
          old_value = "StartPlaceholder",
          new_value = "",
          only_at_cursor = FALSE
        )
      reportVar <- cursor_end(reportVar)
    }
  }# END
