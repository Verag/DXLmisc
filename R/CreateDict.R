create_dict <- function(raw_data, output_file = "dict_data.xlsx") {
  # Check if the input is a data frame and not empty
  if (!is.data.frame(raw_data) || nrow(raw_data) == 0) {
    stop("Please provide a non-empty object of class 'data.frame'.")
  }

  message("Conditions are met to generate a dictionary.")

  warning("The dictionary distinguishes between 'POSIXct, POSIXt', character, and numeric columns.
          Please validate the formatting of the columns to achieve the desired results.")
  warning(paste("An xlsx file will be saved with the name", output_file,
                "in the working environment where the function is executed."))

  # Create the dictionary data frame
  dict_data <- data.frame(
    names = colnames(raw_data),
    class_format = vapply(raw_data, function(x) toString(class(x)), character(1)),
    values = vapply(raw_data, function(x) {
      if (is.character(x)) {
        toString(unique(x))
      } else if (inherits(x, c("POSIXct", "POSIXt"))) {
        toString(paste0(min(x, na.rm = TRUE), " --- ", max(x, na.rm = TRUE)))
      } else {
        round(mean(x, na.rm = TRUE), 2)
      }
    }, character(1)),
    na_exist = vapply(raw_data, function(x) any(is.na(x)), logical(1)),
    stringsAsFactors = FALSE
  )

  # Write the dictionary to an Excel file
  openxlsx::write.xlsx(dict_data, output_file, overwrite = TRUE, row.names = FALSE)
}
