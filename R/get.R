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
#' @param use_revised Use revised files ("*_rv") if they exist (default: TRUE)
#' @param include_filter_vars Include filtering variables in output table
#'
#' @export
ipeds_get <- function(ipedscall, bind = TRUE, join = TRUE,
                      use_revised = TRUE, include_filter_vars = TRUE) {
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
      ipedscall[["year"]] <- ipeds_file_table() |> dplyr::pull(year) |> max()
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
    if (!is.null(ipedscall[["filter_vars"]])) {
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
    ufiles <- select_dict |> dplyr::distinct(filename) |> dplyr::pull()
    out_list <- lapply(ufiles, function(x) read_select_vars_from_zip(ipedscall, x, select_dict, use_revised))
    names(out_list) <- ufiles |> unname()

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

read_select_vars_from_zip <- function(ipedscall, fname, dict, use_revised) {
  ## get zipfile name
  zf <- paste0(fname, ".zip")
  ## get variables associated with this file to select
  select_vars <- get_vars_from_file(fname, dict)
  ## file location / download if necessary
  zdir <- get_file_location_or_download(zf, ipedscall[["local_dir"]])
  ## internal file name to be read
  ifile <- get_internal_file_name(file.path(zdir, zf), use_revised)
  ## read in CSV; add file name and year to file; lower column names
  readr::read_csv(unz(file.path(zdir, zf), ifile),
                  show_col_types = FALSE,
                  progress = FALSE,
                  na = c("", NA, NULL, ".")) |>
    dplyr::rename_all(tolower) |>
    dplyr::select(all_of(select_vars)) |>
    dplyr::mutate(year = get_file_year(fname),
                  file = fname)
}

subset_file_table_by_year <- function(years) {
  ipeds_file_table() |>
    dplyr::filter(year %in% !!years) |>
    dplyr::pull(file)
}

subset_dictionary_by_var_year <- function(search_str, vars_to_keep, years_to_keep) {
  ipeds_dict(search_str, search_col = "varname", return_dict = TRUE, print_off = TRUE) |>
    dplyr::filter(varname %in% !!vars_to_keep) |>
    dplyr::filter(filename %in% years_to_keep)
}

get_file_year <- function(fname) {
  ipeds_file_table() |>
    dplyr::filter(file == fname) |>
    dplyr::distinct(file, .keep_all = TRUE) |>
    dplyr::pull(year)
}

get_vars_from_file <- function(fname, ipeds_dict) {
  ipeds_dict |>
    dplyr::filter(filename == fname) |>
    dplyr::pull(varname) |>
    {\(x) c(x, "unitid")}()
}

get_file_stub_name <- function(file, lower = FALSE) {
  stub <- basename(tools::file_path_sans_ext(file))
  if (lower) { tolower(stub) } else { stub }
}

## check zip file contents and get file name
get_internal_file_name <- function(zfile, use_revised = TRUE) {
  if (!use_revised) return(paste0(get_file_stub_name(zfile, lower = TRUE), ".csv"))
  revised_exists <- unzip(zfile, list = TRUE)[["Name"]] |> grepl(pattern = "_rv")
  if (any(revised_exists)) {
    paste0(get_file_stub_name(zfile, lower = TRUE), "_rv.csv")
  } else {
    paste0(get_file_stub_name(zfile, lower = TRUE), ".csv")
  }
}

get_file_location_or_download <- function(f, local_dir = NULL) {
  if (file.exists(file.path(tempdir(), f))) {
    return(tempdir())
  } else if (!is.na(local_dir)) {
    if (!file.exists(file.path(local_dir, f))) {
      ipeds_download_to_disk(get_file_stub_name(f), to_dir = local_dir)
    }
    return(local_dir)
  } else if (is.na(local_dir)) {
    ipeds_download_to_disk(get_file_stub_name(f), to_dir = tempdir())
    return(tempdir())
  }
}

remove_df_col <- function(df, col) {
  df |> dplyr::select(-dplyr::any_of(col))
}


bind_like_files <- function(df_list, dictionary) {
  ## get list of like data files based on having same variable (e.g., HD* or IC*)
  vars <- dictionary |> dplyr::distinct(varname) |> dplyr::pull()
  lname_groups <- list()
  for (v in vars) {
    lname_groups[[v]] <- dictionary |> dplyr::filter(varname == v) |> dplyr::pull(filename)
  }
  ## get distinct groups
  lname_groups <- dplyr::bind_rows(lname_groups) |> dplyr::distinct()
  outlist <- list()
  for (i in 1:nrow(lname_groups)) {
    ## get data frame names that match
    fn <- lname_groups[i,] |> unlist() |> na.omit() |> c()
    outlist[[i]] <- dplyr::bind_rows(df_list[match(fn, names(df_list))])
  }
  return(outlist)
}

join_all_files <- function(bound_outlist) {
  lapply(bound_outlist, function(x) remove_df_col(x, "file")) |>
    purrr::reduce(dplyr::full_join, by = c("unitid", "year"))
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
