## -----------------------------------------------------------------------------
##
## [ PROJ ] ripeds
## [ FILE ] make_ipeds_dict.R
##
## -----------------------------------------------------------------------------

## NOTES
##
## Use this script to create underlying data dictionary / hash tables. Should be
## run with each new update of IPEDS (approximately once a quarter).

## -----------------------------------------------------------------------------
## macros and paths
## -----------------------------------------------------------------------------

base_url <- "https://nces.ed.gov/ipeds/datacenter/data"
f_ending <- "_Dict.zip"
zipf_dir <- file.path("..", "_zip")
data_dir <- file.path(zipf_dir, "data")
dict_dir <- file.path(zipf_dir, "dict")

## -----------------------------------------------------------------------------
## functions
## -----------------------------------------------------------------------------

## pull dictionary information from xls[x] spreadsheets
parse_xlsx <- function(zipfile, dfile) {
  f <- unzip(zipfile, dfile, exdir = tempdir())
  sheets <- readxl::excel_sheets(f)
  sname <- sheets[grepl("[V|v]arlist", sheets)]
  out <- readxl::read_excel(f, sheet = sname)
  invisible(file.remove(f))
  out
}

## pull information from html files
parse_html <- function(zipfile, dfile, dfvarname) {
  ## ## unzip file
  ## f <- unzip(zipfile, dfile, exdir = tempdir())
  ## ## read in html
  ## f <- readLines(f, warn = FALSE)
  ## ## strip unicode
  ## f <- iconv(f, "latin1", "ascii", sub = "")
  ## ## keep only those that:
  ## ## 1. start with text
  ## ## 2. start with <br><br>TEXT and end with <hr>
  ## ## reg1 <- "^[[:upper:][:digit:]]{3,}.+-?[[:digit:]]*- ?[[:upper:]].+$"
  ## f <- grep("^[[:upper:][:digit:]]{3,}.+-[[:digit:]]+-[[:alpha:]].+$|<br><br>\\w.+</table><hr>", f, value = TRUE)
  ## ## pull out relevant variable name for the <br><br> values
  ## f <- gsub("^<br><br>.+</table><hr>(.+)$", "\\1", f)
  ## ## split
  ## varname <- gsub("^([[:upper:][:digit:]]{3,}_?[[:alnum:]]*)-.+$", "\\1", f)
  ## vartitle <- gsub("^([[:upper:][:digit:]]{3,}_?[[:alnum:]]*)-([[:digit:]]*)?-?([Imputation]*.+)$", "\\3", f)
  ## ## put into tibble
  ## dplyr::tibble(varname = varname,
  ##               vartitle = vartitle)
  fname <- tools::file_path_sans_ext(basename(zipfile)) |> gsub(pattern = "_Dict", replacement = "")
  dplyr::tibble(varname = dfvarname |> dplyr::filter(file_name == fname) |> dplyr::pull(varname),
                vartitle = dfvarname |> dplyr::filter(file_name == fname) |> dplyr::pull(varname))
}

## patch bad column names (as of 10 October 2024)
patch_varname <- function(varname) {
  dplyr::case_when(
    varname == "XEFGNDRU...66" ~ "XEFGNDRUN",
    varname == "XEFGNDRU...70" ~ "XEFGNDRUA",
    TRUE ~ varname
  )
}

## pull revised ("*_rv") name if there are more than two files listed, else just
## pull the name of the file
get_revised_if <- function(flist) {
  fnames <- flist[["Name"]]
  ifelse(length(fnames) == 2,
         grep("_[R|r][V|v]",
              fnames,
              value = TRUE,
              invert = TRUE),
         fnames)
}

## pull variable names from zipped data files and patch as needed
get_varnames <- function(zipfile) {
  flist <- unzip(zipfile, list = TRUE)
  fname <- get_revised_if(flist[["Name"]])
  varname <- readr::read_csv(unz(zipfile, fname),
                             n_max = 0,
                             show_col_types = FALSE,
                             name_repair = "unique") |>
    names()
  varname <- patch_varname(varname)
}

## set up indicator for long files
get_longfiles <- function(zipfile) {
  flist <- unzip(zipfile, list = TRUE)
  fname <- get_revised_if(flist[["Name"]])
  max_count <- readr::read_csv(unz(zipfile, fname),
                               show_col_types = FALSE,
                               name_repair = "unique") |>
    dplyr::rename_all(tolower) |>
    dplyr::count(unitid) |>
    dplyr::summarise(n = max(n)) |>
    dplyr::pull()
  ifelse(max_count > 1, 1, 0)
}

## -----------------------------------------------------------------------------
## read
## -----------------------------------------------------------------------------

## read variables from data files themselves
dfiles <- list.files(data_dir)

## get column names attached to file
df <- purrr::map(dfiles,
                 ~ dplyr::tibble(file_name = tools::file_path_sans_ext(.x),
                                 varname = get_varnames(file.path(data_dir, .x)))
                 ) |>
  dplyr::bind_rows()

## get file names that are long
df_long <- purrr::map(dfiles,
                      ~ dplyr::tibble(file_name = tools::file_path_sans_ext(.x),
                                      long = get_longfiles(file.path(data_dir, .x)))
                      ) |>
  dplyr::bind_rows()

## get list of available IPEDS dictionary files
itab <- ripeds::ipeds_file_table() |> dplyr::distinct(year, file)

## -----------------------------------------------------------------------------
## set up dictionary
## -----------------------------------------------------------------------------

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
  if (ftype %in% c("xlsx", "xls")) {
    out <- parse_xlsx(file.path(tdir, zipf), fname)
  } else {
    out <- parse_html(file.path(tdir, zipf), fname, df)
  }
  ## munge and add to list
  dict[[i]] <- out |>
    dplyr::mutate(file_name = itab[["file"]][i]) |>
    dplyr::rename_all(tolower) |>
    dplyr::mutate(varname = tolower(varname)) |>
    dplyr::select(file_name, varname, vartitle) |>
    dplyr::filter(!is.na(varname))
}

## bind
dict <- dplyr::bind_rows(dict) |>
  dplyr::rename(description = vartitle) |>
  dplyr::left_join(df_long, by = "file_name")

## -----------------------------------------------------------------------------
## create hash maps
## -----------------------------------------------------------------------------

## -------------------------------------
## index frames
## -------------------------------------

## f := file names
## v := variable names
## d := description strings

idx_file <- dict |>
  dplyr::distinct(file_name, long) |>
  dplyr::arrange(file_name) |>
  dplyr::mutate(idxf = paste0("fi", dplyr::row_number()))

idx_vars <- dict |>
  dplyr::distinct(varname) |>
  dplyr::arrange(varname) |>
  dplyr::mutate(idxv = paste0("vi", dplyr::row_number()))

idx_desc <- dict |>
  dplyr::distinct(description) |>
  dplyr::arrange(description) |>
  dplyr::mutate(idxd = paste0("di", dplyr::row_number()))

idx_dict <- dict |>
  dplyr::left_join(idx_file, by = "file_name") |>
  dplyr::left_join(idx_vars, by = "varname") |>
  dplyr::left_join(idx_desc, by = "description") |>
  dplyr::select(starts_with("idx"))

## -------------------------------------
## hash maps
## -------------------------------------

## main := used to store idx values in lists
## *_hash := key --> val (actual name to idx)
## *_hash_lu := val --> key (lookup of actual name using idx)

## ----------------------
## main has
## ----------------------

main_hash <- new.env(parent = emptyenv())

## ----------------------
## file hashes
## ----------------------

file_hash <- new.env(parent = emptyenv())
file_hash_lu <- new.env(parent = emptyenv())

for (i in 1:nrow(idx_file)) {
  key <- idx_file[["file_name"]][i]
  val <- idx_file[["idxf"]][i]
  file_hash[[key]] <- val
  file_hash_lu[[val]] <- key
  main_hash[[val]] <- list(
    "idxv" =  idx_dict |> dplyr::filter(idxf == val) |> dplyr::pull(idxv),
    "idxd" =  idx_dict |> dplyr::filter(idxf == val) |> dplyr::pull(idxd)
  )
}

## ----------------------
## variable hashes
## ----------------------

vars_hash <- new.env(parent = emptyenv())
vars_hash_lu <- new.env(parent = emptyenv())

for (i in 1:nrow(idx_vars)) {
  key <- idx_vars[["varname"]][i]
  val <- idx_vars[["idxv"]][i]
  vars_hash[[key]] <- val
  vars_hash_lu[[val]] <- key
  main_hash[[val]] <- list(
    "idxf" =  idx_dict |> dplyr::filter(idxv == val) |> dplyr::pull(idxf),
    "idxd" =  idx_dict |> dplyr::filter(idxv == val) |> dplyr::pull(idxd)
  )
}

## ----------------------
## description hashes
## ----------------------

desc_hash <- new.env(parent = emptyenv())
desc_hash_lu <- new.env(parent = emptyenv())

for (i in 1:nrow(idx_desc)) {
  key <- idx_desc[["description"]][i]
  val <- idx_desc[["idxd"]][i]
  desc_hash[[key]] <- val
  desc_hash_lu[[val]] <- key
  main_hash[[val]] <- list(
    "idxf" =  idx_dict |> dplyr::filter(idxd == val) |> dplyr::pull(idxf),
    "idxv" =  idx_dict |> dplyr::filter(idxd == val) |> dplyr::pull(idxv)
  )
}

## -------------------------------------
## add long list
## -------------------------------------

long_hash <- new.env(parent = emptyenv())
long_hash[["long"]] <- idx_file |> dplyr::filter(long == 1) |> dplyr::pull(idxf)
long_hash[["wide"]] <- idx_file |> dplyr::filter(long == 0) |> dplyr::pull(idxf)

## -----------------------------------------------------------------------------
## store hash environments in sysdata.rda
## -----------------------------------------------------------------------------

usethis::proj_set(".")
usethis::use_data(main_hash, file_hash, vars_hash, desc_hash,
                  file_hash_lu, vars_hash_lu, desc_hash_lu,
                  long_hash, overwrite = TRUE, internal = TRUE)

## -----------------------------------------------------------------------------
################################################################################
