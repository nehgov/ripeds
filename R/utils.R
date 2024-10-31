## paste pipe
`%+%`  <- function(a,b) paste(a, b, sep = "")

## make ascii hline
hline <- function(nchar, symbol = "-") {
  paste(rep("", nchar), collapse = symbol)
}

## vectorized function to return hash value from selected environment
get_hash <- function(x, hash_env) {
  sapply(x, FUN = get, envir = hash_env)
}

## convert calendar year (YYYY) to academic year (YY/YY+1)
cyear_to_ayear <- function(x) {
  paste0(substr(x, 3, 4), substr(x + 1, 3, 4))
}
