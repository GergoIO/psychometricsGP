#' Format the academic year. This function prevents the need to save the academic year in many different formats under different variables. Instead this function formats the short form of the academic year (removing any dashes) and outputs the requested type of long form academic year
#'
#' @param academicYearShort Insert the shortened version of the current academic year which needs formatting (eg for the academic year 2021 through 2022 this would be of the form 2122 or 21-22)
#' @param type Select the type of formatted output. Use one of: 'Long Dash', 'Long Slash', 'Short Dash' or 'Short Slash' only. (eg for the academic year 2021 through 2022 the following outputs can be achieved: 'Long Dash': 2021-2022, 'Long Slash': 2021/2022, 'Short Dash': 2021-22 or 'Short Slash': 2021/22.)
#'
#' @return Returns the formated academic year
#' @export
#'
#' @examples fnAcademicYearFormat(academicYearShort = "2122", type= "Long Slash"), fnAcademicYearFormat("21-22", "Short Dash")

################################################################################

fnAcademicYearFormat <-
  function(academicYearShort = NULL,
           type = NULL) {
    `%!in%` <- Negate(`%in%`)

    if (is.null(academicYearShort) |
        is.null(type)) {
      stop("One of the required variables for this function has not been specified.")
    } else if (type %!in% c("Long Dash", "Long Slash", "Short Dash", "Short Slash")) {
      stop(
        "Incorrect type selected. Please use one of: 'Long Dash', 'Long Slash', 'Short Dash' or 'Short Slash' only."
      )
    }
    else{
      # Make sure there are not dashes in the short form of the academic year to start with. Ie academicYearShort should be of the for 2122 and not 21-22 to represent the academic year from 2021 to 2022
      academicYearShort <- gsub("-", "", academicYearShort)
      if (type == "Long Dash") {
        return(
          glue(
            '20{substr(academicYearShort,1,2)}-20{substr(academicYearShort,3,4)}'
          )
        )
      }
      if (type == "Long Slash") {
        return(
          glue(
            '20{substr(academicYearShort,1,2)}/20{substr(academicYearShort,3,4)}'
          )
        )
      }
      if (type == "Short Dash") {
        return(
          glue(
            '20{substr(academicYearShort,1,2)}-{substr(academicYearShort,3,4)}'
          )
        )
      }
      if (type == "Short Slash") {
        return(
          glue(
            '20{substr(academicYearShort,1,2)}/{substr(academicYearShort,3,4)}'
          )
        )
      }
    }
  } # END
