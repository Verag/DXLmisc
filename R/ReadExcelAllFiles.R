#' Read all sheets from an Excel file and clean column and sheet names
#'
#' This function reads all sheets from an Excel file and returns them as a list of data frames or tibbles.
#' The user can specify the engine to use (`readxl` or `openxlsx`), clean column and sheet names,
#' and optionally read specific sheets.
#'
#' @param filename A string indicating the path to the Excel file.
#' @param tibble Logical. If `TRUE`, the function returns tibbles; otherwise, data frames. Default is `FALSE`.
#' @param engine A string indicating which package to use for reading the Excel file.
#'   Options are `"readxl"` (default) or `"openxlsx"`.
#' @param sheet_names A character vector of specific sheet names to read. If `NULL` (default), all sheets are read.
#' @param ... Additional arguments passed to the underlying reading functions (`readxl::read_excel` or `openxlsx::read.xlsx`).
#'
#' @return A named list where each element is a data frame or tibble containing the data from one sheet.
#' The names of the list are the cleaned names of the sheets.
#'
#' @details
#' - **Engine options**:
#'   - `"readxl"`: Uses the `readxl` package to read the Excel file.
#'   - `"openxlsx"`: Uses the `openxlsx` package to read the Excel file.
#'
#' - **Cleaning**:
#'   The function uses `janitor::make_clean_names` to clean the column and sheet names, ensuring consistency.
#'
#' @examples
#' # Read all sheets using readxl (default)
#' results <- read_excel_allsheets("data.xlsx", tibble = TRUE, engine = "readxl")
#'
#' # Read specific sheets using openxlsx
#' results <- read_excel_allsheets("data.xlsx", engine = "openxlsx", sheet_names = c("Sheet1", "Sheet2"))
#'
#' # Pass additional arguments to read_excel (e.g., skip rows)
#' results <- read_excel_allsheets("data.xlsx", engine = "readxl", skip = 1)
#'
#' @export
read_excel_allsheets <- function(filename, tibble = FALSE, engine = "readxl", sheet_names = NULL, ...) {

  # Function to check and load required packages
  check_and_load <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("The package '", pkg, "' is required but not installed. ",
                  "Please install it using: install.packages('", pkg, "')"))
    }
  }

  # Ensure necessary packages are loaded based on the engine
  check_and_load("janitor")
  if (engine == "readxl") check_and_load("readxl")
  if (engine == "openxlsx") check_and_load("openxlsx")

  # Check if the file exists
  if (!file.exists(filename)) {
    stop("The specified file was not found: ", filename)
  }

  # Function to clean column names in a dataset
  clean_columns <- function(data) {
    colnames(data) <- janitor::make_clean_names(colnames(data))
    if (!tibble) {
      data <- as.data.frame(data)
    }
    return(data)
  }

  # Function to read all sheets using the 'readxl' package
  read_with_readxl <- function() {
    sheets <- readxl::excel_sheets(filename)
    if (!is.null(sheet_names)) {
      sheets <- sheets[sheets %in% sheet_names]
    }
    purrr::map(sheets, ~ clean_columns(readxl::read_excel(filename, sheet = .x, ...)))
  }

  # Function to read all sheets using the 'openxlsx' package
  read_with_openxlsx <- function() {
    sheets <- openxlsx::getSheetNames(filename)
    if (!is.null(sheet_names)) {
      sheets <- sheets[sheets %in% sheet_names]
    }
    purrr::map(sheets, ~ clean_columns(openxlsx::read.xlsx(filename, sheet = .x, ...)))
  }

  # Select the appropriate engine and read the data
  data_list <- switch(engine,
                      readxl = read_with_readxl(),
                      openxlsx = read_with_openxlsx(),
                      stop("Invalid engine. Choose either 'readxl' or 'openxlsx'.")
  )

  # Assign clean names to the list elements based on the sheet names
  sheet_names <- if (engine == "readxl") readxl::excel_sheets(filename) else openxlsx::getSheetNames(filename)
  names(data_list) <- janitor::make_clean_names(sheet_names)

  return(data_list)
}

