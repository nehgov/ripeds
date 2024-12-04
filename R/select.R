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

  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    ## vars in ... to list
    vars <- lapply(lazyeval::lazy_dots(...), function(x) bquote(.(x[["expr"]])))

    ## confirm has a least one variable
    if (missing(vars) || length(vars) < 1) {
      stop("Incomplete ipeds_select()! You must select at least one variable.",
           call. = FALSE)
    }

    ## look for tidyselect helpers
    ## shl <- c("starts_with","ends_with","contains","matches","everything")
    shl <- c("starts_with","ends_with","contains","matches")
    for (i in 1:length(vars)) {
      v <- vars[[i]]
      if (is.call(v)) {
        v <- as.list(parse(text = gsub("^_", "zzz_", v))) # no leading hyphens
        v[[1]] <- as.character(v[[1]])
        if (v[[1]] != "everything") {
          v[[2]] <- gsub("^zzz_", "_", as.character(v[[2]]))
        }
        if (v[[1]] %in% shl) {
          if (v[[1]] != "everything") {
            fun <- get(v[[1]], asNamespace("tidyselect"))
            v <- do.call(fun, list(v[[2]] |> tolower(), vars = vars_hash |> names()))
            v <- names(vars_hash)[v]
          } else {
            ## TODO: if going to allow everything(), then need to require user to
            ## select file name or year to reduce size
            v <- "everything___"
          }
        } else {
          stop("Can only use the following tidyselect helpers:\n",
               paste(" - ", paste0(shl, "()"), collapse = "\n"),
               call. = FALSE)
        }
      }
      vars[[i]] <- v
    }

    ## unlist vars
    vars <- unlist(vars, use.names = FALSE)

    ## confirm variables exist in dictionary
    confirm_vars(vars)

    ## store vars in order for later ordering
    ipedscall[["select_order"]] <- order_vars(vars)

    ## return
    ipedscall[["select"]] <- vars
    ipedscall
  })

}
