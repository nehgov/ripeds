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
      vars <- lapply(expr, \(x) all.vars(x)) |> unlist()
    }

    ## confirm variable(s) in dictionary
    confirm_vars(vars)

    ## return
    ipedscall[["filter"]] <- expr
    ipedscall[["filter_vars"]] <- vars
    ipedscall
  })
}

