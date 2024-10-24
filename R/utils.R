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
