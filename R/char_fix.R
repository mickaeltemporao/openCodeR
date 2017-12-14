#' Character String Fix
#'
#' @param x character vector
#' @return character vector converted to lower case without spelling mistakes
#' @export
#' @importFrom tm stripWhitespace
#' @importFrom hunspell hunspell_check
#' @importFrom hunspell hunspell_suggest
#' @importFrom qdap qprep
#' @importFrom qdap bag_o_words
#' @examples
#' char_fix("I like breer")
char_fix <- function (x) {

  x <- str_replace_all(x, "[^[:graph:]]", " ")
  x <- tolower(x)
  x <- stripWhitespace(x)

  x <- qprep(x)
  x <- bag_o_words(x)
  misspelled <- !hunspell_check(x)

  for (item in x[misspelled]) {
    x[which(x == item)] <- hunspell_suggest(item)[[1]][[1]]
  }

  x <- paste(x, collapse = " ")
  tolower(x)

}
