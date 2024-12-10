#' Filter IPEDS data by variable values.
#'
#' Filter downloaded IPEDS data. Simple filters separated by commas will be
#' treated as if separated by an `&` (`AND`) boolean. More complex multi-part
#' filters, such as those using an `|` (OR) boolean, must be input as a string
#' or via an object which stores the string filter.
#'
#' Non-standard evaluation may be used for simple filters and when
#' `ipeds_init(use_nse = TRUE)`. In all other cases, a string filter must be
#' used.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'     functions in the chain (ignore)
#' @param ... Expressions to evaluate
#'
#' @examples
#' \dontrun{
#' # using non-standard evaluation, you can separate filters by a comma, which
#' # will serve as an (AND) boolean
#' ipeds_filter(stabbr == "KY", control < 3)
#' ipeds_filter(stabbr %in% c("KY","TN"))
#' ipeds_filter(grepl("community", instnm))
#'
#' # more complex filters, such as those using an | (OR) operator, should be
#' # input as a string, either directly or as stored in an object
#' ipeds_filter("stabbr == 'KY' | stabbr == 'TN'")
#'
#' filter_str <- "stabbr == 'KY' | stabbr == 'TN'"
#' ipeds_filter(filter_str)
#' }
#'
#' @export
ipeds_filter <- function(ipedscall, ...) {

  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    ## confirm filter string
    if (missing(...)) {
      stop("Incomplete ipeds_filter()! You must include a filter expression if using ipeds_filter().",
           call. = FALSE)
    }

    ## check to see if nse is used
    expr <- eval(substitute(alist(...)))
    is_call <- sapply(expr, \(x) is.call(x))

    ## get expressions and variables
    if (ipedscall[["nse"]] & all(is_call)) {
      vars <- lapply(expr, \(x) x[[2]])
    } else if (any(is_call)) {
      stop("Cannot mix string and non-standard evaluation filters. Please choose one or the other.",
           call. = FALSE)
    } else {
      expr <- lapply(list(...), \(x) parse(text = x)[[1]])
      vars <- unlist(lapply(expr, \(x) all.vars(x)))
    }

    ## confirm variable(s) in dictionary
    confirm_vars(vars)

    ## return
    ipedscall[["filter"]] <- expr
    ipedscall[["fvars"]] <- vars
    ipedscall
  })
}

