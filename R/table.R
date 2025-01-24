#' Return a table of all currently available complete data files
#'
#' Return a table with currently available IPEDS complete data files (see
#' \url{https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx}). Unless called by
#' the user on its own, the first [ipeds_init()] call in a session will call
#' [ipeds_file_table()] to scrape the table from NCES website and store the
#' results in a temporary directory. Subsequent data requests during the session
#' will return the table in memory unless the user calls requests to redownload the
#' table.
#'
#' @param redownload Re-scrape NCES website to generate table of files. Defaults
#'   to `FALSE`.
#'
#' @return A data frame with the following columns:
#'
#' * `year`: Survey year assigned to the file by NCES
#' * `survey`: Survey file category
#' * `title`: Survey file title
#' * `file`: Survey file name
#'
#' @examples
#' \dontrun{
#' # First call in a session will scrape NCES website and return
#' ipeds_file_table()
#'
#' # Second call in the session will return table in memory
#' ipeds_file_table()
#'
#' # Will re-scrape NCES website
#' ipeds_file_table(redownload = TRUE)
#' }
#'
#' @export
ipeds_file_table <- function(redownload = FALSE) {
  ## file
  f <- file.path(tempdir(), "ipeds_file_list.RDS")
  ## check for file
  if (file.exists(f) & !redownload) {
    readRDS(f)
  } else {
    ## base url; options (select all)
    base_url <- "https://nces.ed.gov/ipeds/datacenter/DataFiles.aspx"
    opts <- "year=-1&surveyNumber=-1"
    ## get file table
    download.file(url = paste(base_url, opts, sep = "?"),
                  destfile = file.path(tempdir(), "scrape.html"),
                  quiet = TRUE)
    ## ftab <- rvest::read_html(paste(base_url, opts, sep = "?"), "utf-8")
    ftab <- rvest::read_html(file.path(tempdir(), "scrape.html"))
    ftab <- rvest::html_element(ftab, "#contentPlaceHolder_tblResult")
    ftab <- rvest::html_table(ftab)
    ftab <- as.data.frame(ftab)
    ## lower names; select columns; remove potential duplicates
    names(ftab) <- tolower(names(ftab))
    names(ftab)[names(ftab) == "data file"] <- "file"
    ftab <- ftab[,c("year", "survey", "title", "file")]
    ftab <- make_distinct(ftab, names(ftab))
    ## store
    saveRDS(ftab, file.path(tempdir(), "ipeds_file_list.RDS"))
    ## return
    ftab
  }
}
