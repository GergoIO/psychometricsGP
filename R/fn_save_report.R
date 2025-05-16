#' Save a report with retry mechanism and file locking protection
#'
#' @param report The report object to save
#' @param path Directory path where the file should be saved
#' @param filename Filename to save the report as
#' @param temp_suffix Suffix to append for temporary versions (default: "(TEMP Version)")
#' @param quiet Logical, suppress messages if TRUE (default: FALSE)
#' @param suppress_messages Logical, suppress system warning messages (default: TRUE)
#' @param create_dir Logical, create directory if it doesn't exist (default: TRUE)
#' @param max_retries Number of save attempts before giving up (default: 3)
#' @param retry_delay Seconds to wait between retry attempts (default: 2)
#'
#' @return Path to the saved file
#'
#' @details
#' This function saves reports to a specified location with built-in handling for
#' file locks and network issues. It includes automatic retry logic for shared drives,
#' creates temporary versions if files are locked, and cleans up temporary files.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' fn_save_report(my_report, "P:/Reports", "quarterly_report.docx")
#'
#' # With custom retry settings for slow network
#' fn_save_report(my_report, "P:/Reports", "quarterly_report.docx",
#'                max_retries = 5, retry_delay = 5)
#' }
#'
#' @export
fn_save_report <- function(report, path, filename,
                            temp_suffix = "(TEMP Version)",
                            quiet = FALSE,
                            suppress_messages = TRUE,
                            create_dir = TRUE,
                            max_retries = 3,
                            retry_delay = 2) {

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

  # Implementation of retry logic for saving the report
  save_with_retry <- function(report, file_path, attempt = 1) {
    tryCatch({
      print(report, target = file_path)
      msg("Successfully saved report to ", file_path)
      return(file_path)
    }, error = function(e) {
      if (attempt < max_retries) {
        msg("Attempt ", attempt, " failed: ", e$message)
        msg("Waiting ", retry_delay, " seconds before retry...")
        Sys.sleep(retry_delay)
        return(save_with_retry(report, file_path, attempt + 1))
      } else {
        stop("Error saving report after ", max_retries, " attempts: ", e$message)
      }
    })
  }

  # Find an available filename
  save_file <- find_available_filename()

  # Determine what type of file is being saved and show appropriate message
  original_target <- file.path(path, filename)
  if (save_file != original_target) {
    # This is a temp file
    if (grepl(paste0(temp_suffix, " [0-9]+\\."), basename(save_file))) {
      msg("Original file and basic temp file are in use. Using numbered temp version: ", basename(save_file))
    } else {
      msg("Original file is in use. Using temporary version: ", basename(save_file))
    }
  }

  # Try to save the report with retry logic
  return(save_with_retry(report, save_file))
}
