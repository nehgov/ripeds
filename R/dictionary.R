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
#' @param return_df Return a tibble of the subset data dictionary.
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
                       return_df = FALSE, print_off = FALSE) {

  search_col <- switch(match.arg(search_col),
                       "all" = "all",
                       "description" = "desc",
                       "varname" = "vars",
                       "file_name" = "file")

  ## ----------------------
  ## get values
  ## ----------------------
  ## if (match.arg(search_col) == "all") {
  ##   rows <- rep(FALSE, nrow(dict))
  ##   for (col in c("description", "varname", "file_name")) {
  ##     tmp_rows <- grepl(search_string, dict[[col]], ignore.case = ignore_case)
  ##     rows <- rows | tmp_rows     # promote to TRUE either TRUE
  ##   }
  ## } else {
  ##   rows <- grepl(search_string, dict[[match.arg(search_col)]],
  ##                 ignore.case = ignore_case)
  ## }

  if (search_col == "all") {
    vals <- c()
    for (col in c("desc", "vars", "file")) {
      keys <- grep(search_string,
                   names(get(paste0(col, "_hash"))),
                   ignore.case = ignore_case,
                   value = TRUE)
      if (length(keys) > 0) {
        tmp_vals <- c()
        tmp_vals <- lapply(keys,
                           FUN = get,
                           envir = get(paste0(col, "_hash"))) |>
          unlist()
        vals <- c(vals, tmp_vals)
      }
    }
  } else {
    keys <- grep(search_string,
                 names(get(paste0(search_col, "_hash"))),
                 ignore.case = ignore_case,
                 value = TRUE)
    if (length(keys) > 0) {
      tmp_vals <- c()
      tmp_vals <- lapply(keys,
                         FUN = get,
                         envir = get(paste0(search_col, "_hash"))) |>
        unlist()
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

  ## TODO
  ## assign column names as appropriate based on type (vars, file, desc)
  ## loop through
  ## add to sublist
  ## bind sublist
  ## bind list
  ## check for duplicates
  ## convert from idx to actual values

  dict_list <- vector("list", length(vals))
  for (i in c("f", "v", "d") {
    subvals <- grep(i, vals, value = TRUE)
    if (length(subvals) == 0) next
    dict_sublist <- vector("list", length(subvals))
    for (j in 1:length(vals)) {
      dict_sublist[[j]] <- dplyr::tibble(
        x = vals[j],
        y = main_hash[[vals[j]]][1] |> unlist(),
        z = main_hash[[vals[j]]][2] |> unlist()
      )
    }
    dist_list[i] <- dplyr::bind_rows(dict_sublist)
  }
    dict <- dplyr::bind_rows(dict_list)
    d

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

  ## return_df ? return(out) : <>
  ## if (return_df) {
  ##   var_order <- c("varname", "description", "file_name")
  ##   out <- tidyr::as_tibble(out) |>
  ##     dplyr::select(dplyr::one_of(var_order))
  ##   return(out)
  ## }
}
