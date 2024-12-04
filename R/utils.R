
## -----------------------------------------------------------------------------
## general formatting / string helpers
## -----------------------------------------------------------------------------

## paste pipe
`%+%`  <- function(a,b) paste(a, b, sep = "")

## make ascii hline
hline <- function(nchar, symbol = "-") {
  paste(rep("", nchar), collapse = symbol)
}

## -----------------------------------------------------------------------------
## helpers for conversion among hash environments
## -----------------------------------------------------------------------------

## convert hash string short value to human readable value
convert_hash_name <- function(x) {
  switch(x,
         "idxf" = "filename",
         "idxv" = "varname",
         "idxd" = "description"
         )
}

## convert data frame column names from hash to human readable values
convert_hash_df_names <- function(df) {
  names(df) <- lapply(names(df), function(x) convert_hash_name(x))
  df
}

## vectorized function to return hash value from selected environment
get_hash <- function(x, hash_env) {
  sapply(x, FUN = get, envir = hash_env)
}

## support function to convert data frame vectors based on hash name
convert_hash_vec <- function(df, x) {
  switch(x,
         "idxf" = get_hash(df[[x]], file_hash_lu),
         "idxv" = get_hash(df[[x]], vars_hash_lu),
         "idxd" = get_hash(df[[x]], desc_hash_lu))
}

## convert data frame columns from hash to values
convert_hash_df <- function(df) {
  cbind(sapply(names(df), function(x) convert_hash_vec(df, x))) |>
    as.data.frame()
}

## -----------------------------------------------------------------------------
## helper functions to support base R pipe chains / reduce code
## -----------------------------------------------------------------------------

make_distinct <- function(df, cols) {
  df[!duplicated(df[cols]),]
}

## convert calendar year (YYYY) to academic year (YY/YY+1)
cyear_to_ayear <- function(x) {
  paste0(substr(x, 3, 4), substr(x + 1, 3, 4))
}

## order variables so they are returned as user input them
order_vars <- function(varlist, split_character = ",") {
  trimws(unlist(strsplit(toString(varlist), split = split_character)))
}

## -----------------------------------------------------------------------------
## helper functions unique to package structure / design
## -----------------------------------------------------------------------------

## confirm that first argument is ipedscall list
confirm_chain <- function(x) {
  ## error message
  m <- "Chain not properly initialized. Be sure to start with ipeds_init()."
  ## must force() the chain so it works in order, but need to try() first
  ## and capture result
  res <- try(force(x), silent = TRUE)
  ## if try-error and any of following:
  ## 1. "ipedscall" is missing
  ## 2. error in filter (meaning no arguments at all in ipeds_filter())
  ## 3. object isn't found (meaning ipedscall isn't first)
  if (identical(class(res), "try-error")
      & (grepl("argument \"ipedscall\" is missing, with no default\n", res[1])
        | grepl("Error in filter .+ : subscript out of bounds\n", res[1])
        | grepl("object '.+' not found", res[1]))) {
    stop(m, call. = FALSE)
    ## if no try-error and:
    ## 1. is list
    ## 2. is longer than 1 element
    ## 3. contains "ipeds_init_list" == TRUE
  } else if (is.list(x) && length(x) > 1 && x[["ipeds_init_list"]]) {
    res
  ##   ## if no try-error, but ipeds_year() is called, this will catch that
  ## } else if (is.numeric(x) | x == "latest") {
  ##   stop(m, call. = FALSE)
  ## }
  }
}

## confirm variables in dictionary
confirm_vars <- function(varlist) {
  lapply(varlist, function(v) {
    if (!ipeds_dict(tolower(as.character(v)), confirm = TRUE)) {
      stop("Variable \"" %+% v %+% "\" not found in dictionary. "
           %+% "Please check your spelling or search dictionary: "
           %+% "?ipeds_dict()", call. = FALSE)
    }
  })
}
