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
#' results <- read_excel_allsheets("data.xlsx", engine = "openxlsx",
#' sheet_names = c("Sheet1", "Sheet2"))
#'
#' # Pass additional arguments to read_excel (e.g., skip rows)
#' results <- read_excel_allsheets("data.xlsx", engine = "readxl", skip = 1)
#'
#' @export
read_excel_allsheets <- function(filename, tibble = FALSE, engine = "readxl", ...) {

  # Function to check and load required packages
  check_and_load <- function(pkg) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste0("The package '", pkg, "' is required but not installed. ",
                  "Please install it using: install.packages('", pkg, "')"))
    }
  }

  if (engine == "readxl") {
    sheets <- readxl::excel_sheets(filename)

    process_sheet <- function(sheet) {
      data <- readxl::read_excel(filename, sheet = sheet, guess_max = guess_max, ...)
      colnames(data) <- janitor::make_clean_names(colnames(data))
      if (!tibble) {
        data <- as.data.frame(data)
      }
      data
    }
  } else if (engine == "openxlsx") {
    sheets <- openxlsx::getSheetNames(filename)

    process_sheet <- function(sheet) {
      data <- openxlsx::read.xlsx(filename, sheet = sheet, ...)
      colnames(data) <- janitor::make_clean_names(colnames(data))
      if (!tibble) {
        data <- as.data.frame(data)
      }
      data
    }

  } else {
    stop("O engine definido é inválido.")
  }

  data_list <- purrr::map(sheets, process_sheet)
  names(data_list) <- janitor::make_clean_names(sheets)

  return(data_list)
}
