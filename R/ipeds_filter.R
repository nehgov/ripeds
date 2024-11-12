#' Filter IPEDS data by variable values.
#'
#' This function is used to filter downloaded IPEDS data.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'     functions in the chain (ignore)
#' @param ... Expressions to evaluate
#'
#' @examples
#' \dontrun{
#' }

#' @export
ipeds_filter <- function(ipedscall, ...) {

  ## confirm ...
  ## if (missing(...)) {
  ##   confirm_chain(ipedscall)
  ##   stop("Incomplete ipeds_filter()! You must include a filter expression if using ipeds_filter().",
  ##        call. = FALSE)
  ## }

  ## get expressions as str


  ## get unadjusted filter
  ## filter_enq <- rlang::enquos(...)

  ## pass to _ function
  ## ipeds_filter_(ipedscall, filter_string = filter_str, ...)

## }

## ipeds_filter_ <- function(ipedscall, filter_string, ...) {
  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    ## confirm filter_string
    if (missing(...)) {
      stop("Incomplete ipeds_filter()! You must include a filter expression if using ipeds_filter().",
           call. = FALSE)
    }

    ## get expressions so we can parse and check variables
    filter_quo <- rlang::enquos(...)
    expr <- lapply(filter_quo, rlang::quo_get_expr)[[1L]] |> deparse()
    filter_vars <- regmatches(expr, gregexpr("((?![0-9]+)[A-Za-z0-9]+)", expr, perl = TRUE))[[1L]]
    print(filter_quo)
    print(filter_vars)
    ## confirm variable(s) in dictionary
    ## lapply(filter_str_expr, function(x) confirm_vars(as.character(x[[2]])))

    ## store filter variables
    ## filter_vars <- lapply(filter_str_expr, function(x) x[[2]]) |> paste()

    ## return
    ipedscall[["filter"]] <- filter_quo
    ipedscall[["filter_vars"]] <- filter_vars
    ipedscall
  })

}
