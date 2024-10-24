#' Search data dictionary.
#'
#' This function is used to search the IPEDS data dictionary.
#'
#' @param search_string Character string for search. Can use regular expression
#'   for search. Must escape special characters, \code{. \ | ( ) [ \{ ^ $ * +
#'   ?}, with a doublebackslash \code{\\\\}.
#' @param search_col Column to search. The default is to search all columns.
#'   Other options include: "varname", "description", "file_name".
#' @param ignore_case Search is case insensitive by default. Change to
#'   \code{FALSE} to restrict search to exact case matches.
#' @param limit Only the first 10 dictionary items are returned by default.
#'   Increase to return more values. Set to \code{Inf} to return all items
#'   matched in search'
#' @param confirm Use to confirm status of variable name in dictionary. Returns
#'   \code{TRUE} or \code{FALSE}.
#' @param return_dict Return a tibble of the subset data dictionary.
#' @param print_off Do not print to console; useful if you only want to return a
#'   tibble of dictionary values.
#'
#' @examples
#' ## simple search for "state" in any part of the dictionary
#' ipeds_dict("state")
#'
#' ## variable names starting with "st"
#' ipeds_dict("^st", search_col = "varname")
#'
#' ## return full dictionary (only recommended if not printing and
#' ## storing in object)
#' df <- ipeds_dict(".", limit = Inf, print_off = TRUE, return_df = TRUE)

#' @export
ipeds_dict <- function(search_string,
                       search_col = c("all",
                                      "description",
                                      "varname",
                                      "file_name"),
                       ignore_case = TRUE, limit = 10, confirm = FALSE,
                       return_dict = FALSE, print_off = FALSE) {

  ## ----------------------
  ## set search column
  ## ----------------------
  search_col <- switch(match.arg(search_col),
                       "all" = "all",
                       "description" = "desc",
                       "varname" = "vars",
                       "file_name" = "file")

  ## ----------------------
  ## regex search
  ## ----------------------
  if (search_col == "all") {
    vals <- c()
    for (col in c("desc", "vars", "file")) {
      keys <- grep(search_string,
                   names(get(paste0(col, "_hash"))),
                   ignore.case = ignore_case,
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
                 ignore.case = ignore_case,
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
    return(cat("\nNo matches! Try again with new string or column.\n\n"))
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
      dict_sublist[[j]] <- dplyr::tibble(
        idxf = idxf,
        idxv = idxv,
        idxd = idxd
      )
    }
    ## bind sublists into main list
    dict_list[[i]] <- dplyr::bind_rows(dict_sublist)
  }
  ## bind main list into one tibble; remove duplicate rows (which can occur when
  ## searching across more than one column); convert idx* to actual values;
  ## select new columns; arrange
  dict <- dplyr::bind_rows(dict_list) |>
    dplyr::distinct() |>
    dplyr::mutate(file = get_hash(idxf, file_hash_lu),
                  varname = get_hash(idxv, vars_hash_lu),
                  description = get_hash(idxd, desc_hash_lu)) |>
    dplyr::select(file, varname, description) |>
    dplyr::arrange(file, varname)

  ## ## pull data
  ## out <- dict[rows,]

  ## ## get unique varnames
  ## uniqv <- unique(out[["varname"]])

  ## ## pretty print
  ## if (!print_off) {
  ##   for (i in 1:min(length(uniqv), limit)) {

  ##     ## subset
  ##     d <- out[out[["varname"]] == uniqv[i],]

  ##     ## console table
  ##     cat("\n" %+% hline(70) %+% "\n")
  ##     cat("varname: " %+% d[["varname"]][1])

  ##     cat(rep("", 53 - nchar(d[["varname"]][1]) -
  ##                   nchar(d[["source"]][1])))
  ##     cat("source: " %+% d[["source"]][1])
  ##     cat("\n" %+% hline(70) %+% "\n")

  ##     cat("DESCRIPTION:\n\n")
  ##     cat(strwrap(d[["description"]][1], 70) %+% "\n")
  ##     cat("\n")
  ##     ## cat("VALUES: ")
  ##     ## if (is.na(d[["value"]][1])) {
  ##     ##   cat("NA\n\n")
  ##     ## } else {
  ##     ##   cat("\n\n")
  ##     ##   for (j in seq(nrow(d))) {
  ##     ##     cat(d[["value"]][j] %+% " = " %+% d[["label"]][j] %+% "\n")
  ##     ##   }
  ##     ##   cat("\n")
  ##     ## }
  ##   }

  ##   cat(hline(70) %+% "\n")
  ##   cat("Printed information for " %+% min(length(uniqv), limit) %+% " of out ")
  ##   cat(length(uniqv) %+% " variables.\n")
  ##   if (limit < length(uniqv)) cat("Increase limit to see more variables.\n")
  ##   cat("\n")
  ## }

  ## return_dict ? return(dict) : <>
  if (return_dict) return(dict)
}
