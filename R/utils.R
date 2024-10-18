## paste pipe
`%+%`  <- function(a,b) paste(a, b, sep = "")

## make ascii hline
hline <- function(nchar, symbol = "-") {
  paste(rep("", nchar), collapse = symbol)
}
