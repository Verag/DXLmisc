#' remove all accent from string
#'
#' remove all accent from string
#'
#' @param str character variable
#'
#' @param pattern numeric variable
#'
#' @return string
#'
#' @export
remove_accent <- function(str, pattern = "all") {
  if (!is.character(str)) {
    str <- as.character(str)
  }

  pattern <- unique(pattern)

  if (any(pattern == "Ç")) {
    pattern[pattern == "Ç"] <- "ç"
  }

  symbols <- c(
    acute = "áéíóúÁÉÍÓÚýÝ",
    grave = "àèìòùÀÈÌÒÙ",
    circunflex = "âêîôûÂÊÎÔÛ",
    tilde = "ãõÃÕñÑ",
    umlaut = "äëïöüÄËÏÖÜÿ",
    cedil = "çÇ"
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

  if (any(c("all", "al", "a", "todos", "t", "to", "tod", "todo") %in% pattern)) {
    return(chartr(paste(symbols, collapse = ""), paste(nudeSymbols, collapse = ""), str))
  }

  for (i in which(accentTypes %in% pattern)) {
    str <- chartr(symbols[i], nudeSymbols[i], str)
  }

  return(str)
}
