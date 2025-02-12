#' Search data dictionary.
#'
#' This function is used to search the IPEDS data dictionary.
#'
#' @param search_string Character string for search. Can use regular expression
#'   for search. Must escape special characters, \code{. \ | ( ) [ \{ ^ $ * +
#'   ?}, with a doublebackslash \code{\\\\}.
#' @param search_col Column to search. The default is to search all columns.
#'   Other options include: "varname", "description", "filename".
#' @param exact_match Set to TRUE if you only want exact search string matches.
#'   Note that setting exact_match to TRUE may not work as expected if search string
#'   includes regular expressions.
#' @param limit Only the first 10 dictionary items are returned by default.
#'   Increase to return more values. Set to \code{Inf} to return all items
#'   matched in search'
#' @param confirm Use to confirm status of variable name in dictionary. Returns
#'   \code{TRUE} or \code{FALSE}.
#' @param return_dict Return a tibble of the subset data dictionary.
#' @param print_off Do not print to console; useful if you only want to return a
#'   tibble of dictionary values.
#'
#' @section Interactive dictionary:
#'
#' When using `ipeds_dict()` interactively (the default), formatted output will
#' be returned to the console.
#'
#' ```r
#' > ipeds_dict("efydesom")
#'
#' =====================================================================
#' VARIABLE: efydesom
#' =====================================================================
#'
#' :::::::::::::::::::          DESCRIPTION          :::::::::::::::::::
#'
#' Students enrolled in some but not all distance education courses
#'
#' :::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
#'
#' ../FILES
#'
#'  |__ EFFY2020_DIST*
#'  |__ EFFY2021_DIST*
#'  |__ EFFY2022_DIST*
#'  |__ EFFY2023_DIST*
#'
#'  * Denotes a long file in which institutions may have more than one
#'  record (UNITID values repeated across multiple rows).
#'
#' =====================================================================
#' Printed information for 1 of out 1 variables.
#' ```
#'
#' Output includes:
#'
#' * Variable name
#' * Each unique description found across dictionary files provided by NCES
#' * All complete data files in which the variable with that description is found
#' * A flag (*) if necessary to denote the data file is a long file
#' * A note to indicate if more results are available than the limit are available
#'
#' @examples
#' \dontrun{
#' ## simple search for "state" in any part of the dictionary
#' ipeds_dict("state")
#'
#' ## variable names starting with "st"
#' ipeds_dict("^st", search_col = "varname")
#'
#' ## return full dictionary (only recommended if not printing and
#' ## storing in object)
#' df <- ipeds_dict(".", limit = Inf, print_off = TRUE, return_dict = TRUE)
#' }

#' @export
ipeds_dict <- function(search_string,
                       search_col = c("all",
                                      "description",
                                      "varname",
                                      "filename"),
                       exact_match = FALSE, limit = 10, confirm = FALSE,
                       return_dict = FALSE, print_off = FALSE) {

  ## only for confirm
  if (confirm) {
    return(!is.null(vars_hash[[tolower(search_string)]]))
  }

  ## ----------------------
  ## set search column
  ## ----------------------
  search_col <- switch(match.arg(search_col),
                       "all" = "all",
                       "description" = "desc",
                       "varname" = "vars",
                       "filename" = "file")

  ## ----------------------
  ## regex search
  ## ----------------------

  ## set search string with boundaries for exact match
  if (exact_match) {
    search_string <- paste0("\\b", search_string, "\\b")
  }

  ## search
  if (search_col == "all") {
    vals <- c()
    for (col in c("desc", "vars", "file")) {
      keys <- grep(search_string,
                   names(get(paste0(col, "_hash"))),
                   ignore.case = TRUE,
                   value = TRUE)
      if (length(keys) > 0) {
        tmp_vals <- get_hash(keys, get(paste0(col, "_hash")))
        vals <- c(vals, tmp_vals)
      }
    }
  } else {
    vals <- c()
    keys <- grep(search_string,
                 names(get(paste0(search_col, "_hash"))),
                 ignore.case = TRUE,
                 value = TRUE)
    if (length(keys) > 0) {
      tmp_vals <- get_hash(keys, get(paste0(search_col, "_hash")))
      vals <- c(vals, tmp_vals)
    }
  }

  ## ----------------------
  ## return message if 0
  ## ----------------------
  if (length(vals) == 0) {
    stop("\nNo matches! Try again with new string or column.\n\n", call. = FALSE)
  }

  ## ----------------------
  ## build dictionary
  ## ----------------------
  ## init main list
  dict_list <- list()
  ## main loop through file, varnames, description
  for (i in c("f", "v", "d")) {
    ## pull idx* that match
    subvals <- grep(i, vals, value = TRUE)
    ## skip if none
    if (length(subvals) == 0) next
    ## init sublist
    dict_sublist <- vector("list", length(subvals))
    ## pull corresponding lists from main hash; have to use if/else so that
    ## column names align with idx* type (e.g., f --> idxf)
    for (j in 1:length(subvals)) {
      if (i == "f") {
        idxf = subvals[j]
        idxv = main_hash[[subvals[j]]]["idxv"] |> unlist()
        idxd = main_hash[[subvals[j]]]["idxd"] |> unlist()
      } else if (i == "v") {
        idxf = main_hash[[subvals[j]]]["idxf"] |> unlist()
        idxv = subvals[j]
        idxd = main_hash[[subvals[j]]]["idxd"] |> unlist()
      } else if (i == "d") {
        idxf = main_hash[[subvals[j]]]["idxf"] |> unlist()
        idxv = main_hash[[subvals[j]]]["idxv"] |> unlist()
        idxd = subvals[j]
      }
      ## bind lists to tibble and place in sublist
      dict_sublist[[j]] <- data.frame(
        "idxf" = idxf,
        "idxv" = idxv,
        "idxd" = idxd,
        row.names = NULL
      )
    }
    ## bind sublists into main list
    dict_list[[i]] <- do.call("rbind", dict_sublist)
  }

  ## clean up dictionary:
  ## bind main list into one data frame
  ## remove duplicate rows (make distinct)
  ## convert idx* to actual values
  ## arrange
  dict <- clean_hash_dict(dict_list)

  ## ----------------------
  ## pretty print ascii
  ## ----------------------
  if (!print_off) {

    ## ----------------------
    ## unique variables
    ## ----------------------
    uvars <- make_distinct(dict, "varname")[["varname"]]

    ## ----------------------
    ## loop through vars
    ## ----------------------
    for (i in 1:min(length(uvars), limit)) {
      ## variable name
      varn <- uvars[i]
      ## ascii header: varname
      cat("\n" %+% hline(70, "=") %+% "\n")
      cat("VARIABLE: " %+% varn)
      cat("\n" %+% hline(70, "=") %+% "\n")

      ## ----------------------
      ## unique descriptions
      ## ----------------------
      udesc <- dict[dict["varname"] == varn,] |>
        make_distinct(cols = "description") |>
        _[["description"]]

      if (length(udesc) > 1) {
        cat("\n" %+% "NOTE: This variable has (" %+% length(udesc) %+% ") unique descriptions.\n")
      }
      ## ----------------------
      ## loop through desc
      ## ----------------------
      long_flag <- FALSE
      for (j in 1:length(udesc)) {
        ## variable name
        desc <- udesc[j]

        ## ascii: description
        cat("\n")
        if (length(udesc) > 1) {
          if (j < 10) {
            desch <- hline(19, ":") %+%
              hline(9, " ") %+%
              "DESCRIPTION (" %+%
              j %+% ")" %+%
              hline(8, " ") %+%
              hline(19, ":") %+%
              "\n\n"
          } else {
            desch <- hline(19, ":")  %+%
              hline(8, " ") %+%
              "DESCRIPTION (" %+%
              j %+%
              ")" %+%
              hline(8, " ") %+%
              hline(19, ":") %+%
              "\n\n"
          }
        } else {
          desch <- hline(20, ":") %+%
            hline(10, " ") %+%
            "DESCRIPTION" %+%
            hline(9, " ") %+%
            hline(20, ":") %+%
            "\n\n"
        }
        cat(desch)
        cat(strwrap(desc, 70) %+% "\n")
        cat("\n" %+% hline(70, ":") %+% "\n")
        cat("\n")

        ## subset files for unique varname / description pair
        ufiles <- dict[dict["varname"] == varn & dict["description"] == desc,] |>
          _[["filename"]] |>
          sort()

        ## ascii: filenames
        cat("../FILES ")
        cat("\n\n")
        for (k in ufiles) {
          if (file_hash[[k]] %in% long_hash[["long"]]) {
            cat(" |__ " %+% k %+% "*" %+% "\n")
            long_flag <- TRUE
          } else {
            cat(" |__ " %+% k %+%  "\n")
          }
        }
        if (long_flag) {
          cat("\n" %+% " " %+% strwrap(paste("* Denotes a long file in which institutions",
                                             "may have more than one record (UNITID values",
                                             "repeated across multiple rows)."), 70))
          cat("\n")
        }
      }
    }

    cat("\n" %+% hline(70, "=") %+% "\n")
    cat("Printed information for " %+% min(length(uvars), limit) %+% " of out ")
    cat(length(uvars) %+% " variables.\n")
    if (limit < length(uvars)) cat("Increase limit to see more variables.\n")
    cat("\n")
  }

  ## return_dict ? return(dict) : <>
  if (return_dict) return(dict)
}

## function to convert dictionary list to usable data frame; see utils.R for
## internal helper functions
clean_hash_dict <- function(df_list) {
  df <- do.call("rbind", df_list)
  df <- make_distinct(df, names(df))
  df <- convert_hash_df(df)
  df <- convert_hash_df_names(df)
  rownames(df) <- NULL
  df
}
