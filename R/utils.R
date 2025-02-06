## utils.R

## -----------------------------------------------------------------------------
## general formatting / string helpers
## -----------------------------------------------------------------------------

## paste pipe
`%+%`  <- function(a,b) paste(a, b, sep = "")

## make ascii hline
hline <- function(nchar, symbol = "-") {
  paste(rep(symbol, nchar), collapse = "")
}

## paste varlist into single string separated by OR operator for inclusive
## search in ipeds_dict()
create_search_str <- function(varlist) {
  paste(varlist, collapse = "|")
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
## I/O helper functions
## -----------------------------------------------------------------------------

## check zip file contents and get file name
get_internal_file_name <- function(zfile, use_revised = TRUE) {
  if (!use_revised) {
    return(paste0(get_file_stub_name(zfile, lower = TRUE), ".csv"))
  }
  revised_exists <- utils::unzip(zfile, list = TRUE)[["Name"]] |>
    grepl(pattern = "_rv")
  if (any(revised_exists)) {
    paste0(get_file_stub_name(zfile, lower = TRUE), "_rv.csv")
  } else {
    paste0(get_file_stub_name(zfile, lower = TRUE), ".csv")
  }
}

## find a file's location and, if missing, download
get_file_location_or_download <- function(f, local_dir = NA) {
  if (file.exists(file.path(tempdir(), f))) {
    return(tempdir())
  } else if (!is.na(local_dir)) {
    if (!file.exists(file.path(local_dir, f))) {
      ipeds_download_to_disk(get_file_stub_name(f), to_dir = local_dir)
    }
    return(local_dir)
  } else if (is.na(local_dir)) {
    ipeds_download_to_disk(get_file_stub_name(f), to_dir = tempdir())
    return(tempdir())
  }
}

## remove path and file ending
get_file_stub_name <- function(file, lower = FALSE) {
  stub <- basename(tools::file_path_sans_ext(file))
  if (lower) { tolower(stub) } else { stub }
}

## -----------------------------------------------------------------------------
## helper functions to support base R pipe chains / reduce code
## -----------------------------------------------------------------------------

lower_names_df <- function(df) {
  names(df) <- tolower(names(df))
  df
}

bind_rows_df <- function(df_list) {
  as.data.frame(do.call("rbind", df_list))
}

make_distinct <- function(df, cols) {
  if (missing(cols)) { cols <- names(df) }
  df[!duplicated(df[cols]),]
}

filter_equals <- function(df, colname, x) {
  df[df[[colname]] == x, ]
}

filter_in <- function(df, colname, x) {
  df[df[[colname]] %in% x, ]
}

filter_rows <- function(df, filter_df, by_vars = c("unitid", "year")) {
  merge(df, filter_df, by = by_vars)
}

## convert calendar year (YYYY) to academic year (YY/YY+1)
cyear_to_ayear <- function(x) {
  paste0(substr(x, 3, 4), substr(x + 1, 3, 4))
}

## order variables so they are returned as user input them
order_vars <- function(varlist, split_character = ",") {
  trimws(unlist(strsplit(toString(varlist), split = split_character)))
}

## rolling full join on a list of data frames
roll_join_full <- function(df_list, join_vars) {
  Reduce(function(x, y) merge(x, y, all = TRUE, by = join_vars), df_list)
}

## pull all the variable names from a dictionary (can be subset) for a
## particular file; add unitid as a variable for linking purposes
get_vars_from_file <- function(fname, dict) {
  c("unitid", filter_equals(dict, "filename", fname)[["varname"]])
}

## get the year associated with a file name (useful because some files have
## academic year splits in their name, e.g., 1819
get_file_year <- function(fname) {
  df <- filter_equals(ipeds_file_table(), "file", fname)
  df <- make_distinct(df, "file")
  df[["year"]]
}

## return vector files for set of years
subset_file_table_by_year <- function(years) {
  ft <- ipeds_file_table()
  ft[ft[["year"]] %in% years, "file"]
}

## return 1 for long data files
is_long_file <- function(filename) {
  sapply(filename, function(x) (file_hash[[x]] %in% long_hash[["long"]]))
}

## starting with full ipeds_dict(), subset to only those files and years that
## contain variables and years of interest
subset_dict_by_var_year <- function(search_str, vars_to_keep, fyears_to_keep) {
  dict <- ipeds_dict(search_str, search_col = "varname", return_dict = TRUE,
                     print_off = TRUE)
  dict <- filter_in(dict, "varname", vars_to_keep)
  dict <- filter_in(dict, "filename", fyears_to_keep)
  dict[["long"]] <- is_long_file(dict[["filename"]])
  dict
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
  ## 1. ipedscall is missing
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
    ## if no try-error, but ipeds_year() is called, this will catch that
  } else if (all(!is.na(as.numeric(x))) | is.numeric(x)) {
    stop(m, call. = FALSE)
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
