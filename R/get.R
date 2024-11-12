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
#' @param lower_names Lower column names in returned data
#' @param include_filter_vars Include filtering variables in output table
#'
#' @export
ipeds_get <- function(ipedscall, bind = TRUE, join = TRUE,
                      lower_names = TRUE, include_filter_vars = TRUE) {
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

    ## -------------------------------------
    ## make list of files that apply
    ## -------------------------------------
    if(!is.null(ipedscall[["filter_vars"]])) {
      search_str <- paste(c(ipedscall[["select"]], ipedscall[["filter_vars"]]), collapse = "|")
      keep_vars <- toupper(c(ipedscall[["select"]], ipedscall[["filter_vars"]]))
    } else {
      search_str <- paste(ipedscall[["select"]], collapse = "|")
      keep_vars <- toupper(ipedscall[["select"]])
    }
    file_year <- ipeds_file_table() |>
      dplyr::filter(year %in% ipedscall[["year"]]) |>
      dplyr::pull(file)
    dict <- ipeds_dict(search_str, search_col = "varname", return_dict = TRUE,
                       print_off = TRUE) |>
      ## filter to exact match
      dplyr::filter(varname %in% !!keep_vars) |>
      ## filter to year(s)
      dplyr::filter(filename %in% file_year)

    ## -------------------------------------
    ## get files
    ## -------------------------------------
    ## unique files
    ufiles <- dict |> dplyr::distinct(filename) |> dplyr::pull()
    ## use a local directory if selected
    tdir <- ifelse(!is.na(ipedscall[["local_dir"]]),
                   ipedscall[["local_dir"]],
                   tempdir())
    ## base url
    base_url <- "https://nces.ed.gov/ipeds/datacenter/data"
    ## set up output list
    out_list <- list()
    ## loop through file names
    for (i in ufiles) {
      f <- paste0(i, ".zip")
      ## precedence
      ## 1. tempdir() (assuming a download in this session
      ## 2. local_dir (already existing and chosen)
      ## 3. download
      if (file.exists(file.path(tempdir(), f))) {
        zdir <- tempdir()
      } else if (!is.null(ipedscall[["local_dir"]])
                 && file.exists(file.path(ipedscall[["local_dir"]], f))) {
        zdir <- ipedscall[["local_dir"]]
      }
      ## if file isn't in either place, download
      if (!file.exists(file.path(tempdir(), f))
          && !file.exists(file.path(ipedscall[["local_dir"]], f))) {
        download.file(file.path(base_url, f),
                      file.path(tdir, f), quiet = TRUE,
                      mode = "wb")
        zdir <- tdir
      }
      ## get variables associated with this file to select
      select_vars <- dict |>
        dplyr::filter(filename == i) |>
        dplyr::pull(varname) |>
        {\(x) c(x, "UNITID")}()
      ## regardless of location, read in CSV
      out <- readr::read_csv(unz(file.path(zdir, f),
                                   paste0(tolower(i), ".csv")),
                             show_col_types = FALSE,
                             col_select = dplyr::all_of(select_vars)) |>
        mutate(YEAR = ipeds_file_table() |>
                 dplyr::filter(file == i) |>
                 dplyr::pull(year),
               FILE = i)
      ## filter if there is a filter string
      if (!is.null(ipedscall[["filter"]])) {
        expr <- lapply(ipedscall[["filter"]], function(x) rlang::quo_get_expr(x))[[1]] |> deparse()
        out <- dplyr::filter(out, eval(!!!rlang::parse_expr(expr)))
        ## include_filter_vars ? add them to order list at end : <>
        if (include_filter_vars) {
          to_add <- toupper(ipedscall[["filter_vars"]])
          to_add <- to_add[to_add %in% dplyr::pull(dict, varname)]
          ipedscall[["select_order"]] <- c(ipedscall[["select_order"]], to_add)
        }
      }
      ## put in order of variable request
      out <- dplyr::select(out, dplyr::one_of("UNITID",
                                              "YEAR",
                                              toupper(ipedscall[["select_order"]]),
                                              "FILE"))
      ## save in list
      out_list[[i]] <- out |> dplyr::rename_all(tolower)
    }
    ## -------------------------------------
    ## return
    ## -------------------------------------
    ## ----------------------
    ## as list each file
    ## ----------------------
    if (!bind) {
      if (!lower_names) {
        out_list <- lapply(out_list, function(x) x |> dplyr::rename_all(toupper))
      }
      return(out_list)
    }
    ## ----------------------
    ## bind like together
    ## ----------------------
    if (bind) {
      ## get list of like data files based on having same variable
      ## e.g., HD* or IC*
      vars <- dict |> dplyr::distinct(varname) |> pull()
      lname_groups <- list()
      for (v in vars) {
        lname_groups[[v]] <- dict |> dplyr::filter(varname == v) |> pull(filename)
      }
      ## get distinct groups
      lname_groups <- dplyr::bind_rows(lname_groups) |> dplyr::distinct()
      b_outlist <- list()
      for (i in 1:nrow(lname_groups)) {
        ## get data frame names that match
        fn <- c(lname_groups[i,], use.names = FALSE) |> unlist() |> na.omit()
        b_outlist[[i]] <- dplyr::bind_rows(out_list[grepl(fn, names(out_list))])
      }
      ## ----------------------
      ## join into one tibble
      ## ----------------------
      if (join) {
        b_outlist <- lapply(b_outlist, function(x) { x |> dplyr::select(-dplyr::any_of("file")) })
        out <- b_outlist |>
          purrr::reduce(dplyr::full_join, by = c("unitid", "year"))
        if (!lower_names) {
          out <- out |> dplyr::rename_all(toupper)
        }
        return(out)
      } else {
        if (!lower_names) {
          b_outlist <- lapply(b_outlist, function(x) x |> dplyr::rename_all(toupper))
        }
        return(b_outlist)
      }
    }
  })
}
