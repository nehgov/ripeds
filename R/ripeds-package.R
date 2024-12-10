#' @title Download and Manipulate Integrated Postsecondary Education Data System (IPEDS) Data
#'
#' @details The `ripeds` R package provides a series of piped functions to facilitate
#' downloading Department of Education Integrated Postsecondary Education Data
#' System (IPEDS) complete data files.
#'
#' Use `ipeds_dict()` to help find variable names and available years. Next
#' perform a data pull using a chain of piped commands. All command pipes must
#' start with `ipeds_init()`, end with `ipeds_get()`, and be linked with the
#' base pipe, `|>`, or magrittr pipe function, `%>%`. Internal commands,
#' `ipeds_select()`, `ipeds_filter()`, and `ipeds_year()` come in any order in
#' the pipe chain. Only `ipeds_select()` is required in addition to
#' `ipeds_init()` and `ipeds_get()`.
#'
#' @aliases ripeds-package
#' @keywords package
"_PACKAGE"
