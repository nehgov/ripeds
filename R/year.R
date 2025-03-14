#' Select scorecard data year.
#'
#' This function is used to select the year of the data.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'     functions in the chain (ignore)
#' @param year Four-digit year or numeric vector of years.
#'
#' @section Important notes:
#' \enumerate{
#' \item Not all variables have a year option.
#' \item The year selected is not necessarily the year the data were produced.
#' It may be the year the data were collected. For data collected over split
#' years (fall to spring), it is likely the year represents the fall data (\emph{e.g.,}
#' 2011 for 2011/2012 data).
#' }
#'
#' @examples
#' \dontrun{
#' ipeds_year(2012)
#' ipeds_year(c(2010, 2012, 2014))
#' ipeds_year(2010:2013)
#' }

#' @export
ipeds_year <- function(ipedscall, year) {
  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)
    ## check second argument isn't missing and is numeric
    if (missing(year) || !is.numeric(year)
        || any(year < min(ipeds_file_table()[["year"]]))
        || any(year > max(ipeds_file_table()[["year"]]))) {
      stop("Must provide a 4-digit year or vector of 4-digit years ",
           "within bounds of IPEDS years:\n\n",
           "Earliest available year:    ",
           min(ipeds_file_table()[["year"]]), "\n",
           "Most recent available year: ",
           max(ipeds_file_table()[["year"]]),
           call. = FALSE)
    }
    ipedscall[["year"]] <- year
    ipedscall
  })
}
