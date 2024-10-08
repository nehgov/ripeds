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
  f <- readLines(hfile)
  ## strip unicode
  f <- iconv(f, "latin1", "ascii", sub = "")
  ## keep only those that:
  ## 1. start with text
  ## 2. start with <br><br>TEXT and end with <hr>
  f <- grep("^[A-Z0-9].+-[0-9]+-[A-Za-z].+$|<br><br>[A-Z].+<hr>", f, value = TRUE)
  ## pull out relevant variable name for the <br><br> values
  f <- gsub("^<br><br>.+<hr>(.+)$", "\\1", f)
  ## split
  varname <- gsub("^([A-Z]+_?[A-Z]*[0-9]*[A-Z]*)-.+$", "\\1", f)
  vartitle <- gsub("^([A-Z]+_?[A-Z]*[0-9]*[A-Z]*)-([0-9]*)?-?([Imputation]*.+)$", "\\3", f)
  ## put into tibble
  dplyr::tibble(varname = varnames,
                vartitle = vartitle)
}

## get list of available IPEDS dictionary files
itab <- ripeds::ipeds_file_table() |> dplyr::filter(year %in% 2000:2000)

## init list of proper size
dict <- vector("list", length = nrow(itab))

## loop
for (i in 1:length(dict)) {
  ## create file for download
  zipf <- paste0(itab[["file"]][i], f_ending)
  ## download to temporary directory
  download.file(file.path(base_url, zipf), file.path(tempdir(), zipf),
                quiet = TRUE, mode = "wb")
  ## get file name inside zip
  fname <- unzip(file.path(tempdir(), zipf), list = TRUE)[["Name"]]
  ## get dictionary file type
  ftype <- tools::file_ext(fname)
  ## extract based on file type
  if (ftype == "xlsx") {
    out <- parse_xlsx(file.path(tempdir(), zipf), fname)
  } else {
    out <- parse_html(file.path(tempdir(), zipf), fname)
  }
  ## munge and add to list
  dict[[i]] <- out |>
    dplyr::mutate(file = itab[["file"]][i]) |>
    dplyr::rename_all(tolower) |>
    dplyr::select(file, varname, vartitle)
}

## bind
dict <- dplyr::bind_rows(dict)
