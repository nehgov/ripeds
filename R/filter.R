#' Filter IPEDS data by variable values.
#'
#' This function is used to filter downloaded IPEDS data.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'     functions in the chain (ignore)
#' @param ... Expressions to evaluate
#'
#' @export
ipeds_filter <- function(ipedscall, ...) {

  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    ## confirm filter_string
    if (missing(...)) {
      stop("Incomplete ipeds_filter()! You must include a filter expression if using ipeds_filter().",
           call. = FALSE)
    }

    ## get expressions so we can parse and check variables
    if (ipedscall[["nse"]]) {
      expr <- eval(substitute(alist(...)))
      vars <- lapply(expr, \(x) x[[2]])
    } else {
      expr <- list(...)
      expr <- lapply(expr, \(x) parse(text = x)[[1]])[[1]]
      vars <- list()
      while (length(expr) == 3) {
        subexpr <- expr[[2]]
        if (length(subexpr) == 1) {
          v <- as.character(subexpr)
        } else {
          v <- as.character(subexpr[[2]])
        }
        vars[[v]] <- v
        expr <- expr[[3]]
      }
      vars <- unname(vars)
    }

    ## confirm variable(s) in dictionary
    confirm_vars(vars)

    ## return
    ipedscall[["filter"]] <- expr
    ipedscall[["filter_vars"]] <- vars
    ipedscall
  })
}

## TODO: need to account for nested ()
## this
## xx <- "x == 1 | (y == 2 & stabbr %in% c('KY'))"
## yy <- parse(text = xx)[[1]]
## yy

test_filter <- function(...) {
  expr <- list(...)
  expr <- lapply(expr, \(x) parse(text = x)[[1]])[[1]]
  vars <- list()
  while (length(expr) == 3) {
    subexpr <- expr[[2]]
    print(subexpr)
    if (length(subexpr) == 1) {
      v <- as.character(subexpr)
    } else {
      v <- as.character(subexpr[[2]])
    }
    vars[[v]] <- v
    expr <- expr[[3]]
  }
  unname(vars)
}
