#' Download IPEDS files
#'
#' Download selected IPEDS files and place into a tibble
#'
#' @export
get_ipeds <- function(year, survey, title, file, local_dir = NA,
                      revised_files = TRUE, lower_names = TRUE,
                      filter_bool = c("and", "or"),
                      bind = TRUE, join = FALSE) {
  ## get file table to compare against
  itab <- ipeds_file_table()
  ## if all arguments are missing, stop
  if (missing(year) & missing(survey) & missing(title) & missing(file)) {
    stop("No options for downloads given. Must include a value for a ",
         "least one option: year, survey, title, or file_name")
  }
  ## filter_bool "and" := restrictive filters
  ## filter_bool "or" := inclusive filters
  fbool <- match.arg(filter_bool)
  ## AND
  if (fbool == "and") {
    ## filter by year
    if (!missing(year)) {
      fitab <- itab |> dplyr::filter(year %in% !!year)
    }
    ## filter by survey name
    if (!missing(survey)) {
      fitab <- fitab |> dplyr::filter(grepl(tolower(!!survey),
                                            tolower(survey)))
    }
    ## filter by survey title
    if (!missing(title)) {
      fitab <- fitab |> dplyr::filter(grepl(tolower(!!title),
                                            tolower(title)))
    }
    ## filter by file name
    if (!missing(file_name)) {
      fitab <- fitab |> dplyr::filter(grepl(tolower(!!file),
                                            tolower(file)))
    }
    ## get file names
    fnames <- fitab |> dplyr::pull(file)
  } else if (fbool == "or") {
    fitab_list <- list()
    ## filter by year
    if (!missing(year)) {
      fitab_list[[1]] <- itab |> dplyr::filter(year %in% !!year)
    }
    ## filter by survey name
    if (!missing(survey)) {
      fitab_list[[2]] <- itab |> dplyr::filter(grepl(tolower(!!survey),
                                                     tolower(survey)))
    }
    ## filter by survey title
    if (!missing(title)) {
      fitab_list[[3]] <- itab |> dplyr::filter(grepl(tolower(!!title),
                                                     tolower(title)))
    }
    ## filter by file name
    if (!missing(file)) {
      fitab_list[[4]] <- itab |> dplyr::filter(grepl(tolower(!!file),
                                                     tolower(file)))
    }
    ## combine and pull distinct file names
    fnames <- dplyr::bind_rows(fitab_list) |> distinct(file) |> pull(file)
  }
  ## check that file name vector is longer than zero
  if (length(fnames) == 0) {
    stop("Options too restrictive. No IPEDS files fit all filters. ",
         "Either choose fewer filters or change filter_bool to 'or'")
  }
  ## use a local directory if selected
  tdir <- ifelse(!is.na(local_dir), local_dir, tempdir())
  ## base url
  base_url <- "https://nces.ed.gov/ipeds/datacenter/data"
  ## set up output list
  out_list <- list()
  ## loop through file names
  for (i in fnames) {
    f <- paste0(i, ".zip")
    if (!file.exists(file.path(tdir, f))) {
      download.file(file.path(base_url, f), file.path(tdir, f), quiet = TRUE,
                    mode = "wb")
    }
    out <- readr::read_csv(unz(file.path(tdir, f),
                               paste0(tolower(i), ".csv")),
                           show_col_types = FALSE)
    if (lower_names) {
      out <- out |> dplyr::rename_all(tolower)
    }
    ## save in list
    out_list[[i]] <- out
  }
  ## bind
  if (bind) {
    ## TODO: bind only alike
    out <- dplyr::bind_rows(out_list)
    return(out)
  }
  ## join
  ## TODO: join
}
