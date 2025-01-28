#' Convert Portuguese Date Strings to Date Objects
#'
#' This function processes a vector of date strings in Portuguese and converts them into `Date` objects.
#' It supports various date formats, Portuguese month names, and multiple dates within a single string.
#' When no year is specified in the date string, a default year can be applied.
#'
#' @param dates_vector A character vector containing the date strings to be converted.
#' @param default_year An integer specifying the default year to use when the date string does not include a year. Defaults to the current year.
#'
#' @return A `Date` vector where each element corresponds to the last valid date found in the respective string from `dates_vector`. If no valid date is found, the element will be `NA`.
#'
#' @details
#' - The function translates Portuguese month names into English for compatibility with R's date parsing functions.
#' - If a string contains multiple dates separated by delimiters (e.g., "e", "a", ";"), only the last date is returned.
#' - Non-parsable strings will result in `NA`.
#' - Common delimiters and structures like "até", "e", and commas are handled automatically.
#'
#' @examples
#' # Example date vector
#' dates_vector <- c("08 e 09 de Outubro", "09/04",
#'                   "26/07 a 31/07 e 06/08 a 10/08 de 2024",
#'                   "Text without dates", "15-10-2024 até 17-10-2024")
#'
#' # Convert Portuguese date strings to Date objects
#' convert_to_date_pt(dates_vector)
#'
#' @import lubridate
#' @export
convert_to_date_pt <- function(dates_vector, default_year = year(Sys.Date())) {
  require(lubridate)

  # Define month names in Portuguese (using Unicode escapes)
  month_names_pt <- c("\u006a\u0061\u006e\u0065\u0069\u0072\u006f", "\u0066\u0065\u0076\u0065\u0072\u0065\u0069\u0072\u006f",
                      "\u006d\u0061\u0072\u00e7\u006f", "\u0061\u0062\u0072\u0069\u006c", "\u006d\u0061\u0069\u006f",
                      "\u006a\u0075\u006e\u0068\u006f", "\u006a\u0075\u006c\u0068\u006f", "\u0061\u0067\u006f\u0073\u0074\u006f",
                      "\u0073\u0065\u0074\u0065\u006d\u0062\u0072\u006f", "\u006f\u0075\u0074\u0075\u0062\u0072\u006f",
                      "\u006e\u006f\u0076\u0065\u006d\u0062\u0072\u006f", "\u0064\u0065\u007a\u0065\u006d\u0062\u0072\u006f")

  # Define possible date formats
  formats <- c("%d/%m/%Y", "%d/%m", "%d-%m-%Y", "%d-%m", "%d %B %Y", "%d %B", "%B %d %Y", "%B %d")

  # Helper function to clean and format date strings
  clean_date_string <- function(date_string) {
    clean_string <- tolower(trimws(date_string))

    # Replace Portuguese month names with English month names
    for (i in seq_along(month_names_pt)) {
      clean_string <- gsub(month_names_pt[i], month.name[i], clean_string)
    }

    # Remove the word "de" and adjust separators
    clean_string <- gsub("\\bde\\b", "", clean_string)
    clean_string <- gsub("\\.", "/", clean_string)

    return(clean_string)
  }

  # Function to attempt converting a single string into a date
  parse_single_date <- function(clean_string) {
    for (fmt in formats) {
      parsed_date <- tryCatch(as.Date(clean_string, format = fmt), error = function(e) NA)
      if (!is.na(parsed_date)) return(parsed_date)
    }

    # Use lubridate's parse_date_time as a fallback
    return(tryCatch(parse_date_time(clean_string, orders = "dmy"), error = function(e) NA))
  }

  # Process each entry in the dates vector
  parsed_dates <- sapply(dates_vector, function(date_string) {
    if (is.na(date_string)) return(NA)

    # Clean and split the string into separate parts
    clean_string <- clean_date_string(date_string)
    date_parts <- unlist(strsplit(clean_string, " até | a | e |,|;"))
    date_parts <- trimws(date_parts)

    # Convert each part to a date
    parsed_dates <- sapply(date_parts, parse_single_date)

    # Add default year if the date does not include a year
    parsed_dates <- sapply(parsed_dates, function(date) {
      if (!is.na(date) && year(date) == 1970) {
        return(update(date, year = default_year))
      }
      return(date)
    })

    # Return the last valid date (or NA if none exist)
    return(tail(na.omit(parsed_dates), 1))
  })

  # Return the result as a Date vector
  return(as_date(parsed_dates))
}

