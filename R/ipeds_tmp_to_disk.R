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
