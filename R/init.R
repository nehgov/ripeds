#' Initialize chained request.
#'
#' This function initializes the data request. It should always be
#' the first in the series of piped functions.
#'
#' @param local_dir Local directory to check for complete IPEDS zip files that
#'   have already been downloaded. Newly downloaded files will be placed here as
#'   well. If unset, all files will be stored in tempdir().
#' @param use_revised_files When IPEDS zip files contain revised data, use the
#'   revised files. Default is TRUE.
#'
#' @examples
#' \dontrun{
#' ipeds_init()
#' }

#' @export
ipeds_init <- function(local_dir = NA, use_revised_files = TRUE) {
  ## run ipeds_file_table() to bring into memory
  ipeds_file_table()
  ## set up list for chained call
  list("ipeds_init_list" = TRUE,
       "local_dir" = local_dir,
       "revised_files" = use_revised_files,
       "dict" = NULL,
       "survey" = NULL,
       "select" = NULL,
       "select_order" = NULL,
       "filter" = NULL,
       "filter_vars" = NULL,
       "year" = NULL)
}
