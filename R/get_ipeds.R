#' Download IPEDS files
#'
#' Download selected IPEDS files and place into a tibble
#'
#' @export
get_ipeds <- function(year = 2022:2023, survey, title = "directory", file_name, local_dir = NA,
                      revised_files = TRUE) {
  ## get file table to compare against
  itab <- ipeds_file_table()
  ## filter file table
  if (!is.null(year)) {
    fitab <- itab |> dplyr::filter(year %in% !!year)
  } else if (!is.null(survey)) {
  } else if (!is.null(title)) {
    fitab <- fitab |> dplyr::filter(grepl(!!title, tolower(title)))
  }
  ## get file names
  fnames <- fitab |> pull(file)
  ## base url
  base_url <- "https://nces.ed.gov/ipeds/datacenter/data"
  ## TODO: set up local directory option
  tdir <- tempdir()
  out_list <- list()
  for (i in fnames) {
    f <- paste0(i, ".zip")
    if (!file.exists(file.path(tdir, f))) {
      download.file(file.path(base_url, f), file.path(tdir, f), quiet = TRUE, mode = "wb")
    }
    out_list[[i]] <- readr::read_csv(unz(file.path(tdir, f), paste0(tolower(i), ".csv")),
                                     show_col_types = FALSE)
  }
  ## bind
  out <- dplyr::bind_rows(out_list)
  ## return
  out
}
