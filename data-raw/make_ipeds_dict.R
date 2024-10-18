## make_ipeds_dict.R

## macros
base_url <- "https://nces.ed.gov/ipeds/datacenter/data"
f_ending <- "_Dict.zip"

## functions
parse_xlsx <- function(zipfile, dfile) {
  ## unzip file
  f <- unzip(zipfile, dfile, exdir = tempdir())
  ## get sheet names
  sheets <- readxl::excel_sheets(f)
  ## get (V|v)arlist sheet
  sname <- sheets[grepl("[V|v]arlist", sheets)]
  ## read
  out <- readxl::read_excel(f, sheet = sname)
  ## clean up
  invisible(file.remove(f))
  ## return
  out
}
parse_html <- function(zipfile, dfile) {
  ## unzip file
  f <- unzip(zipfile, dfile, exdir = tempdir())
  ## read in html
  f <- readLines(f, warn = FALSE)
  ## strip unicode
  f <- iconv(f, "latin1", "ascii", sub = "")
  ## keep only those that:
  ## 1. start with text
  ## 2. start with <br><br>TEXT and end with <hr>
  ## reg1 <- "^[[:upper:][:digit:]]{3,}.+-?[[:digit:]]*- ?[[:upper:]].+$"
  f <- grep("^[[:upper:][:digit:]]{3,}.+-[[:digit:]]+-[[:alpha:]].+$|<br><br>\\w.+</table><hr>", f, value = TRUE)
  ## pull out relevant variable name for the <br><br> values
  f <- gsub("^<br><br>.+</table><hr>(.+)$", "\\1", f)
  ## split
  varname <- gsub("^([[:upper:][:digit:]]{3,}_?[[:alnum:]]*)-.+$", "\\1", f)
  vartitle <- gsub("^([[:upper:][:digit:]]{3,}_?[[:alnum:]]*)-([[:digit:]]*)?-?([Imputation]*.+)$", "\\3", f)
  ## put into tibble
  dplyr::tibble(varname = varname,
                vartitle = vartitle)
}
patch_varname <- function(varname) {
  ## patch for bad column names (10 October 2024)
  dplyr::case_when(
    varname == "XEFGNDRU...66" ~ "XEFGNDRUN",
    varname == "XEFGNDRU...70" ~ "XEFGNDRUA",
    TRUE ~ varname
  )
}
get_varnames <- function(zipfile) {
  ## get files in zipfile
  flist <- unzip(zipfile, list = TRUE)
  ## return revised file (_rv) if exists
  fnames <- flist[["Name"]]
  fname <- ifelse(length(fnames) == 2, grep("_[R|r][V|v]", fnames, value = TRUE, invert = TRUE), fnames)
  ## get variables (read header line only)
  varname <- readr::read_csv(unz(zipfile, fname), n_max = 0, show_col_types = FALSE,
                             name_repair = "unique") |> names()
  ## fix
  varname <- patch_varname(varname)
}


## ## read variables from data files themselves
## dfiles <- list.files(file.path("_zip", "data"))

## ## get column names attached to file
## df <- purrr::map(dfiles,
##                  ~ dplyr::tibble(file = tools::file_path_sans_ext(.x),
##                                  varname = get_varnames(file.path("_zip", "data", .x)))
##                  ) |>
##   dplyr::bind_rows()


## get list of available IPEDS dictionary files
itab <- ripeds::ipeds_file_table() |> dplyr::distinct(year, file)

## -------------------------------------
## TEMP FILTER
## -------------------------------------

itab <- itab |> dplyr::filter(year >= 2012)

## -------------------------------------

## init list of proper size
dict <- vector("list", length = nrow(itab))

## loop
for (i in 1:length(dict)) {
  ## create file for download
  zipf <- paste0(itab[["file"]][i], f_ending)
  ## use local directory first
  tdir <- file.path("_zip", "dict")
  ## look in local first
  if (!file.exists(file.path(tdir, zipf))) {
    ## download to local directory
    download.file(file.path(base_url, zipf),
                  file.path(tdir, zipf),
                  quiet = TRUE, mode = "wb")
    ## add counter so not throttled
    if (i %% 50 == 0) Sys.sleep(10)
  }
  ## get file name inside zip
  fname <- unzip(file.path(tdir, zipf), list = TRUE)[["Name"]]
  ## get dictionary file type
  ftype <- tools::file_ext(fname)
  ## extract based on file type
  if (ftype == "xlsx") {
    out <- parse_xlsx(file.path(tdir, zipf), fname)
  } else {
    next
    ## out <- parse_html(file.path(tdir, zipf), fname)
  }
  ## munge and add to list
  dict[[i]] <- out |>
    dplyr::mutate(file = itab[["file"]][i]) |>
    dplyr::rename_all(tolower) |>
    dplyr::select(file, varname, vartitle) |>
    dplyr::filter(!is.na(varname))
}

## bind
dict <- dplyr::bind_rows(dict)

## -----------------------------------------------------------------------------
## create hash maps
## -----------------------------------------------------------------------------

dict_hash <- new.env(parent = emptyenv())

tmp <- dict |> dplyr::distinct(varname, vartitle) |> dplyr::arrange(varname)

for (i in 1:nrow(tmp)) {
  ## key stub (varname)
  key <- tmp[["varname"]][i]
  if (key == "UNITID") next
  ## years
  key_y <- paste0(key, "_y")
  dict_hash[[key_y]] <- dict |>
    dplyr::filter(varname == key) |>
    dplyr::pull(file) |>
    list()
  ## titles
  for (j in 1:length(unlist(dict_hash[[key_y]]))) {
    f <- unlist(dict_hash[[key_y]])[j]
    dict_hash[[paste(key, f, sep = "_")]] <- dict |>
      dplyr::filter(varname == key, file == f) |>
      dplyr::pull(vartitle)
  }
}

usethis::proj_set("..")
usethis::use_data(dict, dict_hash, overwrite = TRUE, internal = TRUE)
