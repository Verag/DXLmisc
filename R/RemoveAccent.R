#' Remove All Accents from Strings
#'
#' This function removes accents from strings, converting accented characters to their unaccented equivalents.
#'
#' @param str A character vector to process.
#' @param pattern A character vector indicating which accent types to remove. Defaults to `"all"`, which removes all accents.
#' @return A character vector with accents removed.
#' @export
remove_accent <- function(str, pattern = "all") {
  if (!is.character(str)) {
    str <- as.character(str)
  }

  pattern <- unique(pattern)

  if (any(pattern == "Ç")) {
    pattern[pattern == "Ç"] <- "ç"
  }

  # Define accented symbols and their replacements using Unicode escapes
  symbols <- c(
    acute = "\u00e1\u00e9\u00ed\u00f3\u00fa\u00c1\u00c9\u00cd\u00d3\u00da\u00fd\u00dd",
    grave = "\u00e0\u00e8\u00ec\u00f2\u00f9\u00c0\u00c8\u00cc\u00d2\u00d9",
    circunflex = "\u00e2\u00ea\u00ee\u00f4\u00fb\u00c2\u00ca\u00ce\u00d4\u00db",
    tilde = "\u00e3\u00f5\u00c3\u00d5\u00f1\u00d1",
    umlaut = "\u00e4\u00eb\u00ef\u00f6\u00fc\u00c4\u00cb\u00cf\u00d6\u00dc\u00ff",
    cedil = "\u00e7\u00c7"
  )

  nudeSymbols <- c(
    acute = "aeiouAEIOUyY",
    grave = "aeiouAEIOU",
    circunflex = "aeiouAEIOU",
    tilde = "aoAOnN",
    umlaut = "aeiouAEIOUy",
    cedil = "cC"
  )

  accentTypes <- c("´", "`", "^", "~", "¨", "ç")

  # Handle "all" pattern
  if (any(c("all", "al", "a", "todos", "t", "to", "tod", "todo") %in% pattern)) {
    return(chartr(paste(symbols, collapse = ""), paste(nudeSymbols, collapse = ""), str))
  }

  # Handle specific accent types
  for (i in which(accentTypes %in% pattern)) {
    str <- chartr(symbols[i], nudeSymbols[i], str)
  }

  return(str)
}
