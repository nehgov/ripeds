#' Initialize chained request.
#'
#' This function initializes the data request. It should always be
#' the first in the series of piped functions.
#'
#' @examples
#' \dontrun{
#' ipeds_init()
#' }

#' @export
ipeds_init <- function(local_dir = NA, revised_files = TRUE) {
  ## run ipeds_file_table() to bring into memory
  ipeds_file_table()
  ## set up list for chained call
  list("ipeds_init_list" = TRUE,
       "local_dir" = local_dir,
       "revised_files" = revised_files,
       "dict" = NULL,
       "survey" = NULL,
       "select" = NULL,
       "select_order" = NULL,
       "filter" = NULL,
       "year" = NULL)
}
