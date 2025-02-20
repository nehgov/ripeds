#' Get IPEDS data.
#'
#' Retrieve IPEDS data by establishing which data files are required for
#' filtering (if included) and those required for selected variables.
#'
#' @param ipedscall Current list of parameters carried forward from prior
#'   functions in the chain (ignore)
#' @param bind Row bind all same name survey files (e.g., HD2022 and HD2023)
#' @param join Join different name survey files by UNITID and year. If `bind =
#'   FALSE`, then `join` will be set to `FALSE` and the function argument
#'   ignored.
#'
#' @return Depending on argument combination, the chain will return one of the
#'   following objects:
#'
#' 1. `bind = FALSE, join = FALSE`: A list of files with no further
#' processing (each unique complete data file required returned as a list item).
#' 2. `bind = TRUE, join = FALSE`: A list of files in which like files
#' (e.g., HD*, IC*) are row bound together but unjoined to unlike files
#' 3. `bind = TRUE, join = TRUE`: A data frame in which like files
#' are bound and all are joined
#'
#' @details
#' Notes on filters:
#'
#' Filters will be attempted depending on how the user selects to return the
#' data. By default (`join = TRUE`), the complete filter will be applied to the
#' final joined data set.
#'
#' When the user chooses only to bind like files (`join = FALSE`) or return all
#' files separately (`bind = FALSE`), attempts will be made to apply the filter
#' to the files to which they apply. This may be impossible if the filter is
#' complex, requiring consideration of variables across multiple files that the
#' user chose not to join. Users will receive a warning message in this
#' situation and the return of the unfiltered data files. In the situation in
#' which a filter applies only to one type of data file and works, but also
#' removes missing (`NA`) values, other data files nominally unaffected by the
#' filter may also have rows removed if that institution had completely missing
#' data in the filtered file.
#'
#' The more complicated the data call (many selected variables, many
#' selected years, more complex filter), the longer the data request may take,
#' particularly if downloading files, and the greater the likelihood of
#' unexpected behavior with the join. Users may wish to break up large complex
#' requests into multiple smaller requests or elect to return a list of unbound
#' / unjoined data frames they can manipulate directly.
#'
#' @examples
#' \dontrun{
#' # default: bind = TRUE, join = TRUE
#' ipeds_get()
#'
#' # bind only
#' ipeds_get(join = FALSE)
#'
#' # non-bind, non-join
#' ipeds_get(bind = FALSE, join = FALSE)
#' ipeds_get(bind = FALSE) # join will be set to FALSE by default
#' }
#'
#' @export
ipeds_get <- function(ipedscall, bind = TRUE, join = TRUE) {
  suppressWarnings({
    ## check first argument
    confirm_chain(ipedscall)

    ## check if missing ipeds_select()
    if (is.null(ipedscall[["svars"]])) {
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

    ## store opts
    ldir <- ipedscall[["ldir"]]
    revf <- ipedscall[["revfiles"]]

    ## init long switch as FALSE
    long <- FALSE

    ## -------------------------------------
    ## filter (if used)
    ## -------------------------------------

    if (!is.null(ipedscall[["fvars"]])) {
      ## set search/filter strings and variable lists
      f_search_str <- create_search_str(ipedscall[["fvars"]])
      s_search_str <- create_search_str(c(ipedscall[["svars"]], ipedscall[["fvars"]]))
      f_keep_vars <- ipedscall[["fvars"]]
      s_keep_vars <- c(ipedscall[["svars"]], ipedscall[["fvars"]])
      ## create dictionary for filter variables
      f_dict <- subset_dict_by_var_year(f_search_str, f_keep_vars, file_year)
      ## check if any files necessary for filter are long
      long <- any(f_dict[["long"]])
      ## get list of files
      flist <- get_file_list(f_dict, ldir, revf)
      ## bind into one large data frame
      fdf <- bind_like_files(flist, f_dict)
      fdf <- join_all_files(fdf)
      ## filter and return data frame of relevant unitids and years
      fdf <- get_filtered_id_years(fdf, ipedscall[["filter"]])
    } else {
      ## set search strings and variable lists
      s_search_str <- create_search_str(ipedscall[["svars"]])
      s_keep_vars <- ipedscall[["svars"]]
    }

    ## -------------------------------------
    ## select
    ## -------------------------------------

    ## create dictionary for selected variables
    s_dict <- subset_dict_by_var_year(s_search_str, s_keep_vars, file_year)
    ## get list of files, filtering if option chosen
    if (exists("fdf", inherits = FALSE)) {
      flist <- get_file_list(s_dict, ldir, revf, fdf)
    } else {
      flist <- get_file_list(s_dict, ldir, revf)
    }

    ## -------------------------------------
    ## return
    ## -------------------------------------
    return_by_option(ipedscall, flist, s_dict, bind, join, long)
  })
}

## wrapper function to use a subset dictionary to iteratively read in all files
## associated with the variable and year selection
get_file_list <- function(dict, local_dir = NA, use_revised = TRUE, filter_df = NULL) {
  ufiles <- make_distinct(dict, "filename")[["filename"]]
  out_list <- lapply(ufiles, function(x) {
    read_select_vars_from_zip(x, dict, local_dir, use_revised, filter_df)
  })
  names(out_list) <- unname(ufiles)
  out_list
}

## get variables from zipped csv data file, downloading if necessary; using
## subset dictionary, select variables and interest; add year and file name to
## data frame and return
read_select_vars_from_zip <- function(fname, dict, local_dir = NA,
                                      use_revised = TRUE, filter_df = NULL) {
  zf <- paste0(fname, ".zip")
  select_vars <- get_vars_from_file(fname, dict)
  zdir <- get_file_location_or_download(zf, local_dir)
  ifile <- get_internal_file_name(file.path(zdir, zf), use_revised)
  df <- utils::read.csv(unz(file.path(zdir, zf), ifile),
                        na.strings = c("", NA, NULL, "."))
  df <- lower_names_df(df)
  df <- df[,select_vars]
  df[["year"]] <- get_file_year(fname)
  df[["file"]] <- fname
  if (!is.null(filter_df)) {
    filter_rows(df, filter_df)
  } else {
    df
  }
}

## use dictionary to group tables that are the same, but just different years;
## row bind those into a smaller list
bind_like_files <- function(df_list, dict) {
  ## get groups of like data files based on having same variable (e.g., HD* or IC*)
  vars <- make_distinct(dict, "varname")[["varname"]]
  lname_groups <- lapply(vars, function(x) {
    filter_equals(dict, "varname", x)[["filename"]]
  })
  lname_groups <- bind_rows_df(lname_groups)
  lname_groups <- make_distinct(lname_groups)
  if (is.vector(lname_groups)) {
    names(df_list) <- NULL
    return(df_list)
  }
  ## row bind those from like files
  outlist <- apply(lname_groups, 1, function(x) {
    fn <- c(stats::na.omit(unlist(x)))
    out <- bind_rows_df(df_list[match(fn, names(df_list))])
    row.names(out) <- NULL
    out
  })
  ## remove list names and return
  names(outlist) <- NULL
  return(outlist)
}

## takes list of files and attempts full join (may be messy with complex calls)
join_all_files <- function(bound_outlist, by_vars = c("unitid", "year")) {
  olist <- lapply(bound_outlist, function(x) { subset(x, select = -c(file)) })
  roll_join_full(olist, join_vars = by_vars)
}

## iteratively work through a data frame, filtering based on list of filters;
## this assumes and "&" for filters separated by commas; return data frame of
## of only unitid and year
get_filtered_id_years <- function(df, filter_list) {
  df <- subset(df, eval(filter_list[[1]]))
  if (length(filter_list) > 1) {
    get_filtered_id_years(df, filter_list[-1])
  } else {
    return(subset(df, select = c("unitid", "year")))
  }
}

## iteratively work through a data frame, filtering based on list of filters;
## this assumes and "&" for filters separated by commas; return full data frame
get_filtered_df <- function(df, filter_list) {
  df <- subset(df, eval(filter_list[[1]]))
  if (length(filter_list) > 1) {
    get_filtered_df(df, filter_list[-1])
  } else {
    rownames(df) <- NULL
    df
  }
}

## try filter on unjoined data; return original data frame and message if
## unsuccessful; returned filtered data frame if successful
try_filter <- function(flist, dict, ipedscall) {
  file_list <- list()
  for (i in 1:length(flist)) {
    ## only pull list elements for long data files
    if (any(dict[dict[["filename"]] == names(flist)[i], "long"])) {
      ## possible filters
      pfilters <- lapply(colnames(flist[[i]]),
                         function(x) {
                           grep(x, ipedscall[["filter"]], value = TRUE)
                         })
      ## filter to potential filters
      pfilters <- Filter(function(x) length(x) > 0, pfilters)
      ## move on if no filter for long
      if (length(pfilters) == 0) next
      ## try filters
      errvec <- c()
      for (j in pfilters) {
        flist[[i]] <- tryCatch(subset(flist[[i]],
                                      subset = eval(parse(text = j[[1]]))),
                               error = function(e) {
                                 errvec <<- c(errvec, j[[1]])
                                 return(flist[[i]])
                               })
      }
      file_list[names(flist)[i]] <- errvec
      rownames(flist[[i]]) <- NULL
    } else {
      next
    }
  }
  file_df <- as.data.frame(do.call(rbind, file_list))
  if (nrow(file_df) > 0) {
    colnames(file_df) <- ""
    m <- paste0("An error occurred trying to apply the following filter(s) ",
                "to complete data file(s):\n", " ",
                paste(utils::capture.output(file_df), collapse = "\n"), "\n\n",
                "This error is likely because the filter(s) ",
                "contain(s) variables not in this complete data file(s), ",
                "which means it is unlikely the filter(s) worked as ",
                "expected. Your options are:\n\n",
                " (1) Filter the returned data set with a new filter\n",
                " (2) Set ipeds_get(join == TRUE) and rerun to apply the ",
                "original filter to the fully joined final data set\n",
                " (3) Rerun using a less complex filter\n")
    message(m)
  }
  return(flist)
}

## choosing return format depending on argument choice and type of filter
return_by_option <- function(ipedscall, flist, dict, bind, join, long) {
  ## NOTES:
  ## - only concerned about long file filters here since normal wide files are
  ##   already filtered by unitid and year by this point
  ## - when bind == FALSE, should return a list for consistency, even if the
  ##   list only has one item

  ## check if file list contains only one file
  flist_one <- (length(flist) == 1)

  ## -------------------------------------
  ## !bind / !join
  ## -------------------------------------

  ## bind: FALSE
  ## join: (ignored since flist_one = TRUE)
  ## long: TRUE
  ## flist_one: TRUE
  if (!bind && long && flist_one) {
    return(list(get_filtered_df(flist[[1]], ipedscall[["filter"]])))
  }
  ## bind: FALSE
  ## join: (ignored since bind = FALSE)
  ## long: TRUE
  ## flist_one: FALSE
  if (!bind && long && !flist_one) {
    flist <- try_filter(flist, dict, ipedscall)
    return(unname(flist))
  }
  ## bind: FALSE
  ## join: (ignored since bind = FALSE)
  ## long: FALSE
  ## flist_one: (ignored since bind = FALSE)
  if (!bind && !long) {
    return(unname(flist))
  }

  ## -------------------------------------
  ## bind / !join
  ## -------------------------------------

  ## bind: TRUE
  ## join: FALSE
  ## long: FALSE
  ## flist_one: TRUE
  if (bind && !long && flist_one && !join) {
    return(unname(flist))
  }
  ## bind: TRUE
  ## join: FALSE
  ## long: TRUE
  ## flist_one: TRUE
  if (bind && long && flist_one && !join) {
    return(list(get_filtered_df(flist[[1]], ipedscall[["filter"]])))
  }
  ## bind: TRUE
  ## join: FALSE
  ## long: TRUE
  ## flist_one: FALSE
  if (bind && long && !flist_one && !join) {
    flist <- try_filter(flist, dict, ipedscall)
    blist <- bind_like_files(flist, dict)
    return(unname(blist))
  }
  ## bind: TRUE
  ## join: FALSE
  ## long: FALSE
  ## flist_one: FALSE
  if (bind && !long && !flist_one && !join) {
    blist <- bind_like_files(flist, dict)
    return(unname(blist))
  }

  ## -------------------------------------
  ## bind / join
  ## -------------------------------------

  ## bind: (ignored since join = TRUE)
  ## join: TRUE
  ## long: TRUE
  ## flist_one: TRUE
  if (long && flist_one && join) {
    return(get_filtered_df(flist[[1]], ipedscall[["filter"]]))
  }
  ## bind: (ignored since join = TRUE)
  ## join: TRUE
  ## long: TRUE
  ## flist_one: FALSE
  if (long && !flist_one && join) {
    blist <- bind_like_files(flist, dict)
    if (length(blist) == 1) {
      return(get_filtered_df(blist[[1]], ipedscall[["filter"]]))
    } else {
      return(get_filtered_df(join_all_files(blist), ipedscall[["filter"]]))
    }
  }
  ## bind: (ignored since join = TRUE)
  ## join: TRUE
  ## long: FALSE
  ## flist_one: TRUE
  if (!long && flist_one && join) {
    return(subset(unname(flist)[[1]], select = -c(file)))
  }
  ## bind: (ignored since join = TRUE)
  ## join: TRUE
  ## long: FALSE
  ## flist_one: FALSE
  if (!long && !flist_one && join) {
    blist <- bind_like_files(flist, dict)
    if (length(blist) == 1) {
      return(subset(blist[[1]], select = -c(file)))
    } else {
      return(join_all_files(blist))
    }
  }
}
