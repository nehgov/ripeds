#' Select IPEDS data variables.
#'
#' This function is used to select the variables returned in the final dataset.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'     functions in the chain (ignore)
#' @param ... Desired variable names separated by commas (not case sensitive)
#' @examples
#' \dontrun{
#' ipeds_select(UNITID)
#' ipeds_select(UNITID, INSTNM)
#' ipeds_select(unitid, instnm)
#' }

#' @export
ipeds_select <- function(ipedscall, ...) {

  ## vars in ... to list
  vars <- lapply(lazyeval::lazy_dots(...), function(x) bquote(.(x[["expr"]])))

  ## give to _ version
  ipeds_select_(ipedscall, vars)

}

#' @describeIn ipeds_select Standard evaluation version of
#'     \code{\link{ipeds_select}} (\code{vars} must be string or vector
#'     of strings when using this version)
#'
#' @param vars Character string of variable name or vector of
#'     character string variable names
#'
#' @examples
#' \dontrun{
#' ipeds_select_("UNITID")
#' ipeds_select_(c("UNITID", "INSTNM"))
#' ipeds_select_(c("unitid", "instnm"))
#'
#' ## stored in object
#' vars_to_pull <- c("unitid","instnm")
#' ipeds_select(vars_to_pull)
#' }
#'
#' @export
ipeds_select_ <- function(ipedscall, vars) {
  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    ## confirm has a least one variable
    if (missing(vars) || length(vars) < 1) {
      stop("Incomplete ipeds_select()! You must select at least one variable.",
           call. = FALSE)
    }

    ## ## look for tidyselect helpers
    ## shl <- c("starts_with","ends_with","contains","matches","everything")
    ## for (i in 1:length(vars)) {
    ##   v <- vars[[i]]
    ##   if (is.call(v)) {
    ##     v <- as.list(parse(text = gsub("^_", "zzz_", v))) # no leading hyphens
    ##     v[[1]] <- as.character(v[[1]])
    ##     if (v[[1]] != "everything") {
    ##       v[[2]] <- gsub("^zzz_", "_", as.character(v[[2]]))
    ##     }
    ##     if (v[[1]] %in% shl) {
    ##       if (v[[1]] != "everything") {
    ##         fun <- get(v[[1]], asNamespace("tidyselect"))
    ##         index <- do.call(fun, list(v[[2]], vars = get_hash())
    ##         v <- dict[index, col]
    ##       } else {
    ##         v <- unique(dict[[col]])
    ##       }
    ##     } else {
    ##       stop("Can only use the following tidyselect helpers:\n",
    ##            paste(" - ", paste0(shl, "()"), collapse = "\n"),
    ##            call. = FALSE)
    ##     }
    ##   }
    ##   vars[[i]] <- v
    ## }

    ## unlist vars
    vars <- unlist(vars, use.names = FALSE)

    ## confirm variables exist in dictionary
    for (v in vars) {
      if (!ipeds_dict(toupper(as.character(v)), confirm = TRUE)) {
        stop("Variable \"" %+% v %+% "\" not found in dictionary. "
             %+% "Please check your spelling or search dictionary: "
             %+% "?ipeds_dict()", call. = FALSE)
      }
    }

    ## store vars in order for later ordering
    ipedscall[["select_order"]] <- trimws(unlist(strsplit(toString(vars), split = ",")))

    ## return
    ipedscall[["select"]] <- vars
    ipedscall
  })

}
