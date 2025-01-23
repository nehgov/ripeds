#' Save downloaded IPEDS files in temporary directory to disk
#'
#' Save IPEDS data files (zip) stored in temporary directory to disk for future
#' use. By default, the function saves a copy of the files in the local
#' directory while leaving a copy in the temporary directory. In addition the
#' function will create subdirectories as necessary to save the files.
#'
#' @param to_dir Directory path for saving zip files (will be created if it does
#'   not exist unless `create_dir` is set to `FALSE`).
#' @param overwrite Overwrite files in local directory with those in temporary
#'   directory.
#' @param remove_from_tempdir Remove files from temporary directory after saving
#'   in local directory (they otherwise will be removed at the end of the R
#'   session by default). Default is `FALSE`.
#' @param create_dir Recursively create directory path specified by user if it
#'   does not exist. Default is `TRUE`.
#'
#' @examples
#' \dontrun{
#' # save any files in tempdir() that match names from ipeds_file_table() to
#' # local directory, preferring local copies, or, if missing, creating
#' # directory if necessary; files will be left in tempdir() until session is closed
#' ipeds_temp_to_disk(local_dir = ".")
#'
#' # will error if directory does not exist
#' ipeds_temp_to_disk(local_dir = file.path(".", "data"), create_dir = FALSE)
#'
#' # overwrite local versions of files if they exist
#' ipeds_temp_to_disk(local_dir = ".", overwrite = TRUE)
#'
#' # remove saved files from tempdir() after copy
#' ipeds_temp_to_disk(local_dir = ".", remove_from_tempdir = TRUE)
#' }
#'
#' @export
ipeds_temp_to_disk <- function(to_dir, overwrite = FALSE, remove_from_tempdir = FALSE,
                               create_dir = TRUE) {
  ## check path and create if doesn't exist (or give warning if user sets
  ## create_directory = FALSE
  if (!dir.exists(to_dir)) {
    if (create_dir) {
      dir.create(to_dir, showWarnings = FALSE, recursive = TRUE)
    } else {
      stop("Directory does not exist. ",
           "Either create directory path or set create_dir = TRUE")
    }
  }
  ## get vector of available IPEDS files
  ifvec <- paste0(ipeds_file_table()[["file"]], ".zip")
  ## get vector of files in tempdir()
  tfvec <- list.files(tempdir())
  ## compare
  files_to_save <- tfvec[which(tfvec %in% ifvec)]
  ## if no files in temporary directory, give message
  if (length(files_to_save) == 0) {
    message("No files in temporary directory to save.")
    return()
  }
  ## copy
  invisible(file.copy(file.path(tempdir(), files_to_save),
                      file.path(to_dir, files_to_save),
                      overwrite = overwrite))
  ## remove from memory if option is selected
  if (remove_from_tempdir) {
    invisible(file.remove(file.path(tempdir(), files_to_save)))
  }
}

#' Download IPEDS files directly to disk
#'
#' Directly download and save IPEDS zip files to disk for future usage. By
#' default, the function will create subdirectories as necessary in which to
#' save the files.
#'
#' @param to_dir Directory path for saving zip files (will be created if it does
#'   not exist unless `create_dir` is set to `FALSE`).
#' @param files Vector of survey file names without ending to download (e.g.,
#'   HD2023).
#' @param use_ipeds_dict Output from [ipeds_dict()] in which `return_dict = TRUE`;
#'   if argument isn't missing, it will be chosen over input to files argument
#'   if also included.
#' @param type Download the data file (default) or associated dictionary file.
#' @param overwrite Overwrite files on local directory with those in temporary
#'   directory. Default is `FALSE` (keep already existing files).
#' @param create_dir Recursively create directory path specified by user if it
#'   does not exist. Default is `TRUE`.
#'
#' @examples
#' \dontrun{
#' ipeds_download_to_disk(".", c("HD2020", "HD2021"))
#' ipeds_download_to_disk(".", c("HD2020", "HD2021"), overwrite = TRUE)
#'
#' files <- c("HD2020", "HD2021")
#' ipeds_download_to_disk(".", files)
#'
#' # using return from ipeds_file_table() to down all 2020 files
#' dict <- ipeds_file_table()
#' ipeds_download_to_disk(".", ipeds_dict = dict[dict$year == 2020,])
#'
#' # download associated dictionary files
#' ipeds_download_to_disk(".", c("HD2020", "HD2021"), type = "dictionary")
#' }
#'
#' @export
ipeds_download_to_disk <- function(to_dir, files = NULL, use_ipeds_dict = NULL,
                                   type = c("data", "dictionary"),
                                   overwrite = FALSE, create_dir = TRUE) {
  ## choose dictionary first
  using_dict <- FALSE
  if (!is.null(use_ipeds_dict)) {
    files <- make_distinct(use_ipeds_dict, "filename")[["filename"]]
    using_dict <- TRUE
  }
  ## confirm files in IPEDS
  check <- files %in% ipeds_file_table()[["file"]]
  if (!all(check)) {
    message("The following files are not found in IPEDS ",
            "and have been removed from download list:\n\n",
            paste0("- ", files[!check], "\n"))
  }
  ## stop if none
  if (length(files[check]) == 0 && !using_dict) {
    stop("No files to download. Recheck list.", call. = FALSE)
  }
  ## update file list
  files <- files[check]
  ## check path and create if doesn't exist (or give warning if user sets
  ## create_directory = FALSE
  if (!dir.exists(to_dir)) {
    if (create_dir) {
      dir.create(to_dir, showWarnings = FALSE, recursive = TRUE)
    } else {
      stop("\nDirectory does not exist. ",
           "Either create directory path or set create_dir = TRUE",
           .call = FALSE)
    }
  }
  ## set ending if dictionary files are chosen
  if (match.arg(type) == "dictionary") {
    files <- paste0(files, "_Dict")
  }
  ## check for already downloaded
  if (!overwrite) {
    existing <- tools::file_path_sans_ext(list.files(to_dir))
    overlap <- files[which(files %in% existing)]
    if (length(overlap) > 0) {
      message("The following files have already been downloaded:\n\n",
              paste0("- ", overlap, "\n"),
              "\nIf you would like to redownload, set overwrite = TRUE")
    }
    ## update file list
    files <- files[which(!files %in% overlap)]
    ## stop if none
    if (length(files) == 0) {
      stop("No files to download. All already downloaded.", call. = FALSE)
    }
  }
  ## add zip ending
  fzipvec <- paste0(files, ".zip")
  ## set base url
  base_url <- "https://nces.ed.gov/ipeds/datacenter/data"
  ## loop through
  for (i in 1:length(fzipvec)) {
    f <- fzipvec[i]
    if (!file.exists(file.path(to_dir, f))) {
      ## download to local directory
      utils::download.file(file.path(base_url, f),
                           file.path(to_dir, f),
                           quiet = TRUE, mode = "wb")
      ## add counter so not throttled
      if (i %% 50 == 0) Sys.sleep(10)
    }
  }
}
