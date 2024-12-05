#' Get IPEDS data.
#'
#' This function gets IPEDS data by compiling and
#' converting all the previous piped output into a single URL string
#' that is used to get the data.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'   functions in the chain (ignore)
#' @param bind Row bind all same name survey files (e.g., HD2022 and HD2023)
#' @param join Join different name survey files by UNITID and year
#'
#' @export
ipeds_get <- function(ipedscall, bind = TRUE, join = TRUE) {
  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    ## check if missing ipeds_select()
    if (is.null(ipedscall[["select"]])) {
      stop("No variables selected. Use ipeds_select() to choose variables.",
           call. = FALSE)
    }

    ## check if year is missing; use latest year from HD if missing
    if (is.null(ipedscall[["year"]])) {
      message("No year selected. Most recent year of data will be assummed.")
      ipedscall[["year"]] <- max(ipeds_file_table()[["year"]])
    }

    ## subset all files in years of interest
    file_year <- subset_file_table_by_year(ipedscall[["year"]])

    ## -------------------------------------
    ## set up search strings / variables
    ## -------------------------------------

    ## filter
    if (!is.null(ipedscall[["filter_vars"]])) {
      f_search_str <- paste(ipedscall[["filter_vars"]], collapse = "|")
      f_keep_vars <- ipedscall[["filter_vars"]]
    }

    ## select
    if (ipedscall[["include_filter_vars"]] && !is.null(ipedscall[["filter_vars"]])) {
      s_search_str <- paste(c(ipedscall[["select"]], ipedscall[["filter_vars"]]), collapse = "|")
      s_keep_vars <- c(ipedscall[["select"]], ipedscall[["filter_vars"]])
    } else {
      s_search_str <- paste(ipedscall[["select"]], collapse = "|")
      s_keep_vars <- ipedscall[["select"]]
    }

    ## -------------------------------------
    ## make dictionaries
    ## -------------------------------------

    # filter_dict <- subset_dictionary_by_var_year(f_search_str, f_keep_vars, file_year)
    select_dict <- subset_dictionary_by_var_year(s_search_str, s_keep_vars, file_year)

    ## -------------------------------------
    ## get files
    ## -------------------------------------

    ## unique files; names
    ufiles <- make_distinct(select_dict, "filename")[["filename"]]
    out_list <- lapply(ufiles, \(x) {
      read_select_vars_from_zip(x, select_dict, ipedscall[["local_dir"]], ipedscall[["revised_files"]])
    })
    names(out_list) <- unname(ufiles)

    ## -----------------------------------------------------------------------------
    ## return
    ## -----------------------------------------------------------------------------
    if (!bind) {
      return(out_list)
    } else if (bind) {
      bound_outlist <- bind_like_files(out_list, select_dict)
      if (join) {
        join_all_files(bound_outlist)
      } else {
        return(bound_outlist)
      }
    }
  })
}

read_select_vars_from_zip <- function(fname, dict, local_dir = NA, use_revised = TRUE) {
  ## get zipfile name
  zf <- paste0(fname, ".zip")
  ## get variables associated with this file to select
  select_vars <- get_vars_from_file(fname, dict)
  ## file location / download if necessary
  zdir <- get_file_location_or_download(zf, local_dir)
  ## internal file name to be read
  ifile <- get_internal_file_name(file.path(zdir, zf), use_revised)
  ## read in CSV, lower column names, select variables
  df <- utils::read.csv(unz(file.path(zdir, zf), ifile), na.strings = c("", NA, NULL, ".")) |>
    lower_names_df() |>
    _[,select_vars]
  ## add year and file name; return
  df[["year"]] <- get_file_year(fname)
  df[["file"]] <- fname
  df
}

subset_file_table_by_year <- function(years) {
  ft <- ipeds_file_table()
  ft[ft[["year"]] %in% years, "file"]
}

subset_dictionary_by_var_year <- function(search_str, vars_to_keep, years_to_keep) {
  ipeds_dict(search_str, search_col = "varname", return_dict = TRUE, print_off = TRUE) |>
    filter_in("varname", vars_to_keep) |>
    filter_in("filename", years_to_keep)
}

get_file_year <- function(fname) {
  filter_equals(ipeds_file_table(), "file", fname) |>
    make_distinct("file") |>
    _[["year"]]
}

get_vars_from_file <- function(fname, dict) {
  c("unitid", filter_equals(dict, "filename", fname)[["varname"]])
}

bind_like_files <- function(df_list, dict) {
  ## get list of like data files based on having same variable (e.g., HD* or IC*)
  vars <- make_distinct(dict, "varname")[["varname"]]
  lname_groups <- lapply(vars, \(x) filter_equals(dict, "varname", x)[["filename"]]) |>
    bind_rows_df() |>
    make_distinct()
  ## row bind only those from like files (they are just different years)
  outlist <- apply(lname_groups, 1, function(x) {
    fn <- unlist(x) |> stats::na.omit() |> c()
    out <- bind_rows_df(df_list[match(fn, names(df_list))])
    row.names(out) <- NULL
    out
  })
  ## remove row names and return
  names(outlist) <- NULL
  return(outlist)
}

join_all_files <- function(bound_outlist) {
  lapply(bound_outlist, \(x) { subset(x, select = -c(file)) }) |>
    roll_join_full(join_vars = c("unitid", "year"))
}

  ## filter if there is a filter string
  ## TODO
  ## Need to account for filter upper/lower
  ## Need to track which files filter var lives in and only filter there (otherwise, the object isn't found)
## Need to account for unitids / years that end up filtered so that those can be taken care of
## in the end with the bind/join
## if (!is.null(ipedscall[["filter"]])) {
##   expr <- lapply(ipedscall[["filter"]], function(x) rlang::quo_get_expr(x))[[1]] |> deparse()
##   out <- dplyr::filter(out, eval(!!rlang::parse_expr(expr)))
##   ## include_filter_vars ? add them to order list at end : <>
##   if (include_filter_vars) {
##     to_add <- ipedscall[["filter_vars"]]
##     to_add <- to_add[to_add %in% dplyr::pull(dict, varname)]
##     ipedscall[["select_order"]] <- c(ipedscall[["select_order"]], to_add)
##   }
## }
## put in order of variable request
## out <- dplyr::select(out, dplyr::any_of(c("unitid",
##                                           "year",
##                                           ipedscall[["select_order"]],
##                                           "file")))
## ## save in list
## out_list[[i]] <- out
