#' Changes a whole number or integer value (up to 999) to a lower case character string,
#' with the option to convert to title case.
#'
#' @description This function takes an object, integer or whole number (up to a value of 999/"999") and will convert
#' it to a character string. Useful when including objects (e.g number of passing students) within report text.
#' Where the object or value is greater than 999/"999" and/or includes a decimal point the original string/
#' numeric value will be returned.
#'
#' @usage fnGPNumToWord(x, Capital=FALSE)
#'
#' @param x The value or object to convert.
#'
#' @param Capital Default is FALSE. Option to create a title case character string with Capital=TRUE.
#'
#' @examples
#' x<-3
#' fnGPNumToWord(x)
#'
#' fnGPNumToWord(12,TRUE)
#'
#' fnGPNumToWord(999)
#'
#' fnGPNumToWord(10.1)
#'
#' studentsSick<-5
#' paste0("There were ",fnGPNumToWord(studentsSick)," students absent due to sickness.")
#'
#' studentsECs<-8
#' paste0(fnGPNumToWord(studentsECs,TRUE)," students were absent with ECs.")
#'
#' studentsECs<-0
#' paste0("There were ",fnGPNumToWord(studentsECs)," students with ECs.")
#'
#' fnGPNumToWord(999,TRUE)
#'
#' @export

########################################

fnGPNumToWord <- function(x, Capital = FALSE)
{
  y <- as.character(x)
  dig <- nchar(y)
  z <- ""

  if (dig > 3) {
    z <- x
  }

  if (grepl(".", y, fixed = TRUE)) {
    z <- x
  }

  if (dig < 4 & !grepl(".", y, fixed = TRUE) & Capital == TRUE) {
    # three digit numbers
    if (dig == 3)                     {
      dig1 <-
        switch(
          substr(y, 1, 1),
          `1` = "One hundred",
          `2` = "Two hundred",
          `3` = "Three hundred",
          `4` = "Four hundred",
          `5` = "Five hundred",
          `6` = "Six hundred",
          `7` = "Seven hundred",
          `8` = "Eight hundred",
          `9` = "Nine hundred"
        )
    }

    if (dig == 3 &
        substr(y, 2, 2) != "1") {
      dig2 <-
        switch(
          substr(y, 2, 2),
          `0` = "",
          `2` = "twenty",
          `3` = "thirty",
          `4` = "forty",
          `5` = "fifty",
          `6` = "sixty",
          `7` = "seventy",
          `8` = "eighty",
          `9` = "ninety"
        )
    }

    if (dig == 3 &
        substr(y, 2, 2) != "1") {
      dig3 <-
        switch(
          substr(y, 3, 3),
          `0` = "",
          `1` = "one",
          `2` = "two",
          `3` = "three",
          `4` = "four",
          `5` = "five",
          `6` = "six",
          `7` = "seven",
          `8` = "eight",
          `9` = "nine"
        )
    }

    if (dig == 3 &
        substr(y, 2, 2) == "1") {
      dig2 <-
        switch(
          substr(y, 2, 3),
          `10` = "ten",
          `11` = "eleven",
          `12` = "twelve",
          `13` = "thirteen",
          `14` = "fourteen",
          `15` = "fifteen",
          `16` = "sixteen",
          `17` = "seventeen",
          `18` = "eighteen",
          `19` = "nineteen"
        )
    }
    if (dig == 3 & substr(y, 2, 2) == "1") {
      dig3 <- ""
    }


    # two digit numbers
    if (dig == 2 &
        substr(y, 1, 1) == "1") {
      dig2 <-
        switch(
          y,
          `10` = "Ten",
          `11` = "Eleven",
          `12` = "Twelve",
          `13` = "Thirteen",
          `14` = "Fourteen",
          `15` = "Fifteen",
          `16` = "Sixteen",
          `17` = "Seventeen",
          `18` = "Eighteen",
          `19` = "Nineteen"
        )
    }

    if (dig == 2 &
        substr(y, 1, 1) != "1") {
      dig2 <-
        switch(
          substr(y, 1, 1),
          `2` = "Twenty",
          `3` = "Thirty",
          `4` = "Forty",
          `5` = "Fifty",
          `6` = "Sixty",
          `7` = "Seventy",
          `8` = "Eighty",
          `9` = "Ninety"
        )
    }

    if (dig == 2 &
        substr(y, 1, 1) != "1") {
      dig3 <-
        switch(
          substr(y, 2, 2),
          `0` = "",
          `1` = "one",
          `2` = "two",
          `3` = "three",
          `4` = "four",
          `5` = "five",
          `6` = "six",
          `7` = "seven",
          `8` = "eight",
          `9` = "nine"
        )
    }

    # one digit numbers (Capital=TRUE)
    if (dig == 1 &
        Capital == TRUE)   {
      dig3 <-
        switch(
          y,
          `0` = "No",
          `1` = "One",
          `2` = "Two",
          `3` = "Three",
          `4` = "Four",
          `5` = "Five",
          `6` = "Six",
          `7` = "Seven",
          `8` = "Eight",
          `9` = "Nine"
        )
    }

    if (dig == 3) {
      z <- paste(dig1, "and", dig2, dig3, sep = " ")
    }
    if (dig == 3 & substr(y, 2, 2) == "0" &
        substr(y, 3, 3) == "0") {
      z <- dig1
    }
    if (dig == 3 &
        substr(y, 2, 2) == "1") {
      z <- paste(dig1, "and", dig2, sep = " ")
    }
    if (dig == 2 & substr(y, 1, 1) != "1") {
      z <- paste(dig2, dig3, sep = " ")
    }
    if (dig == 2 & substr(y, 1, 1) == "1") {
      z <- dig2
    }
    if (dig == 1) {
      z <- dig3
    }
    if (grepl("  ", z)) {
      z <- gsub("  ", " ", z)
    }
    if (grepl(" ", substr(z, nchar(z), nchar(z)))) {
      z <- substr(z, 1, nchar(z) - 1)
    }

  }


  if (dig < 4 & !grepl(".", y, fixed = TRUE) & Capital == FALSE) {
    # three digit numbers
    if (dig == 3)                     {
      dig1 <-
        switch(
          substr(y, 1, 1),
          `1` = "one hundred",
          `2` = "two hundred",
          `3` = "three hundred",
          `4` = "four hundred",
          `5` = "five hundred",
          `6` = "fix hundred",
          `7` = "seven hundred",
          `8` = "eight hundred",
          `9` = "nine hundred"
        )
    }

    if (dig == 3 &
        substr(y, 2, 2) != "1") {
      dig2 <-
        switch(
          substr(y, 2, 2),
          `0` = "",
          `2` = "twenty",
          `3` = "thirty",
          `4` = "forty",
          `5` = "fifty",
          `6` = "sixty",
          `7` = "seventy",
          `8` = "eighty",
          `9` = "ninety"
        )
    }

    if (dig == 3 &
        substr(y, 2, 2) != "1") {
      dig3 <-
        switch(
          substr(y, 3, 3),
          `0` = "",
          `1` = "one",
          `2` = "two",
          `3` = "three",
          `4` = "four",
          `5` = "five",
          `6` = "six",
          `7` = "seven",
          `8` = "eight",
          `9` = "nine"
        )
    }

    if (dig == 3 &
        substr(y, 2, 2) == "1") {
      dig2 <-
        switch(
          substr(y, 2, 3),
          `10` = "ten",
          `11` = "eleven",
          `12` = "twelve",
          `13` = "thirteen",
          `14` = "fourteen",
          `15` = "fifteen",
          `16` = "sixteen",
          `17` = "seventeen",
          `18` = "eighteen",
          `19` = "nineteen"
        )
    }
    if (dig == 3 & substr(y, 2, 2) == "1") {
      dig3 <- ""
    }


    # two digit numbers
    if (dig == 2 &
        substr(y, 1, 1) == "1") {
      dig2 <-
        switch(
          y,
          `10` = "ten",
          `11` = "eleven",
          `12` = "twelve",
          `13` = "thirteen",
          `14` = "fourteen",
          `15` = "fifteen",
          `16` = "sixteen",
          `17` = "seventeen",
          `18` = "eighteen",
          `19` = "nineteen"
        )
    }

    if (dig == 2 &
        substr(y, 1, 1) != "1") {
      dig2 <-
        switch(
          substr(y, 1, 1),
          `2` = "twenty",
          `3` = "thirty",
          `4` = "forty",
          `5` = "fifty",
          `6` = "sixty",
          `7` = "seventy",
          `8` = "eighty",
          `9` = "ninety"
        )
    }

    if (dig == 2 &
        substr(y, 1, 1) != "1") {
      dig3 <-
        switch(
          substr(y, 2, 2),
          `0` = "",
          `1` = "one",
          `2` = "two",
          `3` = "three",
          `4` = "four",
          `5` = "five",
          `6` = "six",
          `7` = "seven",
          `8` = "eight",
          `9` = "nine"
        )
    }

    # one digit numbers
    if (dig == 1)   {
      dig3 <-
        switch(
          y,
          `0` = "no",
          `1` = "one",
          `2` = "two",
          `3` = "three",
          `4` = "four",
          `5` = "five",
          `6` = "six",
          `7` = "seven",
          `8` = "eight",
          `9` = "nine"
        )
    }

    if (dig == 3) {
      z <- paste(dig1, "and", dig2, dig3, sep = " ")
    }
    if (dig == 3 & substr(y, 2, 2) == "0" &
        substr(y, 3, 3) == "0") {
      z <- dig1
    }
    if (dig == 3 &
        substr(y, 2, 2) == "1") {
      z <- paste(dig1, "and", dig2, sep = " ")
    }
    if (dig == 2 & substr(y, 1, 1) != "1") {
      z <- paste(dig2, dig3, sep = " ")
    }
    if (dig == 2 & substr(y, 1, 1) == "1") {
      z <- dig2
    }
    if (dig == 1) {
      z <- dig3
    }
    if (grepl("  ", z)) {
      z <- gsub("  ", " ", z)
    }
    if (grepl(" ", substr(z, nchar(z), nchar(z)))) {
      z <- substr(z, 1, nchar(z) - 1)
    }

  }

  z
}
