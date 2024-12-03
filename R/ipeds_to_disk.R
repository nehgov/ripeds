#' Save downloaded IPEDS files in temporary directory to disk.
#'
#' Save IPEDS zip files currently stored in temporary directory to disk for
#' future usage. By default, the function makes a copy of the files in the new
#' directory, leaving a copy in the temporary directory. By default, the
#' function will create directories as necessary to save the files.
#'
#' @param to_dir Directory path to copy
#' @param overwrite_existing Overwrite files on local directory with those in
#'   temporary directory
#' @param remove_from_tempdir Remove files from temporary directory after saving
#'   in new location
#' @param create_directory Recursively create directory path specified by user
#'   if it does not exist
#'
#' @export
ipeds_tmp_to_disk <- function(to_dir, overwrite_existing = FALSE,
                              remove_from_tempdir = FALSE,
                              create_directory = TRUE) {
  ## check path and create if doesn't exist (or give warning if user sets
  ## create_directory = FALSE
  if (!dir.exists(to_dir)) {
    if (create_directory) {
      dir.create(to_dir, showWarnings = FALSE, recursive = TRUE)
    } else {
      stop("Directory does not exist. ",
           "Either create directory path or set create_directory = TRUE")
    }
  }
  ## get vector of available IPEDS files
  ifvec <- ipeds_file_table() |> dplyr::pull(file) |> paste0(".zip")
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
                      overwrite = overwrite_existing))
  ## remove from memory if option is selected
  if (remove_from_tempdir) {
    invisible(file.remove(file.path(tempdir(), files_to_save)))
  }
}

#' Download IPEDS files directly to disk.
#'
#' Directly download and save IPEDS zip files to disk for future usage. By
#' default, the function will create directories as necessary to save the files.
#'
#' @param files Vector of files (without ending) to download (e.g., HD2023)
#' @param to_dir Directory path to copy
#' @param ipeds_dict_df Output from ipeds_dict(return_dict = TRUE); if argument
#'   isn't missing, it will be choosen over input to files argument if also
#'   included
#' @param overwrite_existing Overwrite files on local directory with those in
#'   temporary directory
#' @param create_directory Recursively create directory path specified by user
#'   if it does not exist
#'
#' @export
ipeds_download_to_disk <- function(files,
                                   to_dir,
                                   ipeds_dict_df = NULL,
                                   overwrite_existing = FALSE,
                                   create_directory = TRUE) {
  ## choose dictionary first
  if (!is.null(ipeds_dict_df)) {
    files <- ipeds_dict_df |> dplyr::distinct(filename) |> dplyr::pull()
  }
  ## confirm files in IPEDS
  check <- files %in% ipeds_file_table()[["file"]]
  if (!all(check)) {
    message("The following files are not found in IPEDS ",
            "and have been removed from download list:\n\n",
            paste0("- ", files[!check], "\n"))
  }
  ## stop if none
  if (length(files[check]) == 0) {
    stop("No files to download. Recheck list.", call. = FALSE)
  }
  ## update file list
  files <- files[check]
  ## check path and create if doesn't exist (or give warning if user sets
  ## create_directory = FALSE
  if (!dir.exists(to_dir)) {
    if (create_directory) {
      dir.create(to_dir, showWarnings = FALSE, recursive = TRUE)
    } else {
      stop("\nDirectory does not exist. ",
           "Either create directory path or set create_directory = TRUE",
           .call = FALSE)
    }
  }
  ## check for already downloaded
  if (!overwrite_existing) {
    existing <- list.files(to_dir) |> tools::file_path_sans_ext()
    overlap <- files[which(files %in% existing)]
    if (length(overlap) > 0) {
      message("The following files have already been downloaded:\n\n",
              paste0("- ", overlap, "\n"),
              "\nIf you would like to redownload, set overwrite_exiting = TRUE")
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
  for (i in length(fzipvec)) {
    f <- fzipvec[i]
    if (!file.exists(file.path(to_dir, f))) {
      ## download to local directory
      download.file(file.path(base_url, f),
                    file.path(to_dir, f),
                    quiet = TRUE, mode = "wb")
      ## add counter so not throttled
      if (i %% 50 == 0) Sys.sleep(10)
    }
  }
}
