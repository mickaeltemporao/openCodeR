#' Read character
#' #export
read_char <- function() {
  n <- readline(prompt="Input value: ")
  return(as.character(n))
}
