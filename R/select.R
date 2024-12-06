#' Select IPEDS data variables.
#'
#' This function is used to select the variables returned in the final dataset.
#' By default, non-standard evaluation is assumed, meaning that you can input
#' variable names without quotation, separated by commas. If you set
#' \code{use_nse = FALSE} in [ipeds_init()], then you must use quotations around
#' column names or use an external object.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'     functions in the chain (ignore)
#' @param ... Desired variable names separated by commas (not case sensitive)
#' @examples
#' \dontrun{
#' # with non-standard evalution
#' ipeds_select(UNITID)
#' ipeds_select(UNITID, INSTNM)
#' ipeds_select(unitid, instnm)
#'
#' # without non-standard evaluation
#' ipeds_select("UNITID")
#' ipeds_select("unitid", "instnm")
#' ipeds_select(c("unitid", "instnm"))
#'
#' vars <- c("unitid", "instnm")
#' ipeds_select(vars)
#' }

#' @export
ipeds_select <- function(ipedscall, ...) {

  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    if (ipedscall[["nse"]]) {
      vars <- eval(substitute(alist(...)))
    } else {
      vars <- lapply(list(...), \(x) eval(x))
    }

    ## confirm has a least one variable
    if (missing(vars) || length(vars) < 1) {
      stop("Incomplete ipeds_select()! You must select at least one variable.",
           call. = FALSE)
    }

    ## unlist vars
    vars <- unlist(vars, use.names = FALSE)

    ## confirm variables exist in dictionary
    confirm_vars(vars)

    ## store vars in order for later ordering; return
    ipedscall[["sorder"]] <- order_vars(vars)
    ipedscall[["svars"]] <- vars
    ipedscall
  })

}
