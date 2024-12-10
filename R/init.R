#' Initialize chained request.
#'
#' Initialize the data request. It should always be the first in the series of
#' piped functions.
#'
#' @param local_dir Local directory to check for complete IPEDS zip files that
#'   have already been downloaded. Newly downloaded files will be placed here as
#'   well. If unset, all files will be stored in tempdir().
#' @param use_revised_files When IPEDS zip files contain revised data, use the
#'   revised files. Default is TRUE.
#' @param use_nse Use non-standard evaluation in chain. Default is TRUE.
#'
#' @examples
#' \dontrun{
#' ipeds_init()
#'
#' # set local directory to search first for existing IPEDS data files and in
#' # which to save any that must be downloaded
#' ipeds_init(local_dir = ".")
#'
#' # use original files even if revise files exist
#' ipeds_init(use_revised_files = FALSE)
#'
#' # use standard evaluation throughout the ipeds_*() chain
#' ipeds_init(use_nse = FALSE)
#' }

#' @export
ipeds_init <- function(local_dir = NA, use_revised_files = TRUE,
                       use_nse = TRUE) {
  ## run ipeds_file_table() to bring into memory
  ipeds_file_table()
  ## set up list for chained call
  list("ipeds_init_list" = TRUE,
       "ldir" = local_dir,
       "revfiles" = use_revised_files,
       "dict" = NULL,
       "survey" = NULL,
       "svars" = NULL,
       "sorder" = NULL,
       "filter" = NULL,
       "fvars" = NULL,
       "year" = NULL,
       "nse" = use_nse)
}
