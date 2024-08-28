#' All currently available complete data files
#'
#' Return a table with currently available data files. The first request in a
#' session will scrape table from NCES website. Subsequent function calls in the
#' same session will return saved table unless user requests to redownload the
#' table.
#'
#' @param redownload_table Reconnect to URL to download table of files (FALSE)
#'
#' @export
ipeds_file_table <- function(redownload_table = FALSE) {
  ## file
  f <- file.path(tempdir(), "ipeds_file_list.RDS")
  ## check for file
  if (file.exists(f) & !redownload_table) {
    readRDS(f)
  } else {
    ## base url; options (select all)
    base_url <- "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx"
    opts <- "year=-1&surveyNumber=-1"
    ## get file table
    ftab <- rvest::read_html(paste(base_url, opts, sep = "?")) |>
      rvest::html_element("#contentPlaceHolder_tblResult") |>
      rvest::html_table() |>
      dplyr::rename_all(tolower) |>
      dplyr::select(year, survey, title, file = `data file`) |>
      dplyr::distinct(year, survey, title, file)
    ## store
    saveRDS(ftab, file.path(tempdir(), "ipeds_file_list.RDS"))
    ## return
    ftab
  }
}
