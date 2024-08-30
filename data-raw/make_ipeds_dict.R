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
parse_html <- function(zip, dfile) {
}

## get list of available IPEDS dictionary files
itab <- ripeds::ipeds_file_table() |> filter(year == 2022)

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
dict <- bind_rows(dict)
