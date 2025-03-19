#' Save an Excel workbook with automatic fallback to temporary file
#'
#' @description
#' This function attempts to save an Excel workbook to the specified path. If the target file
#' is locked or inaccessible, it will save a temporary version with a custom suffix.
#' It attempts to clean up previous temporary files before saving, and if temporary files
#' cannot be deleted, it will create a new temporary file with an incremented number.
#' By default, it will create the target directory if it doesn't exist.
#'
#' @param wb A workbook object created with createWorkbook() from the openxlsx package
#' @param path The directory where the file should be saved
#' @param filename The name of the file to save (without the path)
#' @param overwrite Logical. If TRUE, overwrites the file if it exists. Default is TRUE.
#' @param temp_suffix Character. The suffix to append to filename for temporary versions. Default is "(TEMP Version)".
#' @param quiet Logical. If TRUE, suppresses informational messages. Default is FALSE.
#' @param suppress_messages Logical. If TRUE, suppresses permission denied warnings when checking files. Default is TRUE.
#' @param create_dir Logical. If TRUE, creates the directory if it doesn't exist. Default is TRUE.
#'
#' @return The full path to the saved file (either the original target or the temporary version)
#'
#' @examples
#' \dontrun{
#' # Create a workbook and add data
#' wb <- createWorkbook()
#' addWorksheet(wb, "Sheet1")
#' writeData(wb, "Sheet1", data.frame(x = 1:5, y = letters[1:5]))
#'
#' # Save the workbook with default settings
#' fn_save_excel(wb, "output/reports", "Data.xlsx")
#'
#' # Save with custom temporary suffix
#' fn_save_excel(wb, "output/reports", "Data.xlsx", temp_suffix = "(BACKUP)")
#' }
#'
#' @importFrom utils file_test
#' @export
fn_save_excel <- function(wb, path, filename,
                          overwrite = TRUE,
                          temp_suffix = "(TEMP Version)",
                          quiet = FALSE,
                          suppress_messages = TRUE,
                          create_dir = TRUE) {

  # Check if openxlsx package is available
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("The openxlsx package is required for this function. Please install it.")
  }

  # Internal function to output messages if not in quiet mode
  msg <- function(...) {
    if (!quiet) {
      message(...)
    }
  }

  # Check if directory exists and create it if needed
  if (!dir.exists(path)) {
    if (create_dir) {
      msg("Directory does not exist. Creating: ", path)
      dir_created <- dir.create(path, recursive = TRUE, showWarnings = !suppress_messages)
      if (!dir_created) {
        stop("Failed to create directory: ", path)
      }
      msg("Directory created successfully.")
    } else {
      stop("Directory does not exist: ", path)
    }
  }

  # Function to check if a file is accessible for writing (not locked)
  is_file_locked <- function(file_path) {
    if (!file.exists(file_path)) {
      return(FALSE)  # File doesn't exist, so not locked
    }

    # Try to open the file for writing to check if it's locked
    check_lock <- function() {
      tryCatch({
        con <- file(file_path, "a")
        close(con)
        return(FALSE)  # File is not locked
      }, error = function(e) {
        return(TRUE)   # File is locked
      })
    }

    # Either suppress warnings or not based on parameter
    if (suppress_messages) {
      return(suppressWarnings(check_lock()))
    } else {
      return(check_lock())
    }
  }

  # Function to get file parts (base name, extension)
  get_file_parts <- function(filename) {
    filename_parts <- strsplit(filename, "\\.")[[1]]
    base_name <- paste(filename_parts[1:(length(filename_parts)-1)], collapse = ".")
    ext <- filename_parts[length(filename_parts)]
    return(list(base_name = base_name, ext = ext))
  }

  # Function to try deleting temp files (all versions including numbered ones)
  cleanup_temp_files <- function() {
    file_parts <- get_file_parts(filename)
    base_name <- file_parts$base_name
    ext <- file_parts$ext

    # Create patterns that match our temp files (with and without digit suffix)
    # Basic pattern for temp files without digit
    base_pattern <- paste0(gsub("([\\(\\)\\.])", "\\\\\\1", base_name),
                           " ",
                           gsub("([\\(\\)\\.])", "\\\\\\1", temp_suffix),
                           "\\.",
                           ext,
                           "$")

    # Pattern for temp files with digits
    digit_pattern <- paste0(gsub("([\\(\\)\\.])", "\\\\\\1", base_name),
                            " ",
                            gsub("([\\(\\)\\.])", "\\\\\\1", temp_suffix),
                            " [0-9]+\\.",
                            ext,
                            "$")

    # Find all temp files (both with and without digit suffix)
    temp_files_basic <- list.files(path, pattern = base_pattern, full.names = TRUE)
    temp_files_digit <- list.files(path, pattern = digit_pattern, full.names = TRUE)
    temp_files <- c(temp_files_basic, temp_files_digit)

    if (length(temp_files) > 0) {
      msg("Found ", length(temp_files), " temporary file(s). Attempting to delete.")
      for (file in temp_files) {
        # Use suppressWarnings to prevent "Permission denied" warnings
        deleted <- suppressWarnings(try(file.remove(file), silent = TRUE))
        # Only report successful deletions
        if (!inherits(deleted, "try-error") && deleted) {
          msg("Successfully deleted: ", basename(file))
        }
      }
    }
  }

  # Function to determine the next available filename
  find_available_filename <- function() {
    # First check if target file is available
    target_file <- file.path(path, filename)
    if (!is_file_locked(target_file)) {
      return(target_file)
    }

    # Target file is locked, try basic temp file
    file_parts <- get_file_parts(filename)
    base_name <- file_parts$base_name
    ext <- file_parts$ext
    temp_filename <- paste0(base_name, " ", temp_suffix, ".", ext)
    temp_file <- file.path(path, temp_filename)

    if (!is_file_locked(temp_file)) {
      return(temp_file)
    }

    # Basic temp file is also locked, find next available numbered version
    digit <- 2
    while (TRUE) {
      numbered_temp_filename <- paste0(base_name, " ", temp_suffix, " ", digit, ".", ext)
      numbered_temp_file <- file.path(path, numbered_temp_filename)

      if (!file.exists(numbered_temp_file) || !is_file_locked(numbered_temp_file)) {
        return(numbered_temp_file)
      }

      digit <- digit + 1

      # Safety check to avoid infinite loop
      if (digit > 100) {
        stop("Unable to find an available filename after 100 attempts")
      }
    }
  }

  # Always attempt to clean up temp files before saving
  cleanup_temp_files()

  # Find an available filename
  save_file <- find_available_filename()

  # Try to save the workbook
  tryCatch({
    openxlsx::saveWorkbook(wb, save_file, overwrite = overwrite)

    # Determine what type of file was saved and show appropriate message
    original_target <- file.path(path, filename)
    if (save_file != original_target) {
      # This is a temp file
      if (grepl(paste0(temp_suffix, " [0-9]+\\."), basename(save_file))) {
        msg("Original file and basic temp file are in use. Saved as numbered temp version: ", basename(save_file))
      } else {
        msg("Original file is in use. Saved temporary version: ", basename(save_file))
      }
    } else {
      msg("Successfully saved Excel workbook to ", save_file)
    }
  }, error = function(e) {
    stop("Error saving Excel workbook: ", e$message)
  })

  # Return the path to the saved file
  return(save_file)
}
