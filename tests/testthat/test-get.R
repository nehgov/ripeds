## -- get --

test_that("Errors for non-init()", {
  expect_error(ipeds_get(),
               "Chain not properly initialized. Be sure to start with ipeds_init().")
})

test_that("Errors for missing ipeds_select()", {
  expect_error(ipeds_init() |> ipeds_filter(unitid == 999999) |> ipeds_get(),
               "No variables selected. Use ipeds_select() to choose variables.",
               fixed = TRUE)
})

## read in temp files
lapply(c(paste0(c("HD2021", "HD2022", "EF2021A_DIST", "EF2022A_DIST"), ".zip"),
         "ipeds_file_list.RDS"),
       function(x) {
         file.copy(file.path("inst", "extdata", x),
                   file.path(tempdir(), x))
       })

## -----------------------------------------------------------------------------
## single wide table return
## -----------------------------------------------------------------------------
test_that("Single table request: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021) |>
    ipeds_get()
  df_comp <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","instnm")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "HD2021"
  expect_equal(df, df_comp)
})

test_that("Single table request: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021) |>
    ipeds_get(join = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","instnm")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "HD2021"
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

test_that("Single table request: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021) |>
    ipeds_get(bind = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","instnm")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "HD2021"
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

## -----------------------------------------------------------------------------
## multiple wide table return
## -----------------------------------------------------------------------------
test_that("Multiple table request: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021:2022) |>
    ipeds_get()
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("HD", x, ".zip")
                      cf <- paste0("hd", x, ".csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","instnm")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("HD", x)
                      tmp
                    })
  df_comp <- do.call(rbind, df_list)
  expect_equal(df, df_comp)
})

test_that("Multiple table request: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021:2022) |>
    ipeds_get(join = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("HD", x, ".zip")
                      cf <- paste0("hd", x, ".csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","instnm")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("HD", x)
                      tmp
                    })
  df_comp <- list(do.call(rbind, df_list))
  expect_equal(df, df_comp)
})

test_that("Multiple table request: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021:2022) |>
    ipeds_get(bind = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("HD", x, ".zip")
                      cf <- paste0("hd", x, ".csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","instnm")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("HD", x)
                      tmp
                    })
  expect_equal(df, df_list)
})

## -----------------------------------------------------------------------------
## single wide table return with a filter
## -----------------------------------------------------------------------------
test_that("Single table request w/filter: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021) |>
    ipeds_filter(stabbr == "KY") |>
    ipeds_get()
  df_comp <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[df_comp[["stabbr"]] == "KY", ]
  df_comp <- df_comp[,c("unitid","instnm","stabbr")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "HD2021"
  df_comp <- df_comp[,c(1,4,2,3,5)]
  rownames(df_comp) <- NULL
  expect_equal(df, df_comp)
})

test_that("Single table request w/filter: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021) |>
    ipeds_filter(stabbr == "KY") |>
    ipeds_get(join = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[df_comp[["stabbr"]] == "KY", ]
  df_comp <- df_comp[,c("unitid","instnm","stabbr")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "HD2021"
  df_comp <- df_comp[,c(1,4,2,3,5)]
  rownames(df_comp) <- NULL
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

test_that("Single table request w/filter: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021) |>
    ipeds_filter(stabbr == "KY") |>
    ipeds_get(bind = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[df_comp[["stabbr"]] == "KY", ]
  df_comp <- df_comp[,c("unitid","instnm","stabbr")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "HD2021"
  df_comp <- df_comp[,c(1,4,2,3,5)]
  rownames(df_comp) <- NULL
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

## -----------------------------------------------------------------------------
## multiple wide table return with a filter
## -----------------------------------------------------------------------------
test_that("Multiple table request w/filter: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021:2022) |>
    ipeds_filter(stabbr == "KY") |>
    ipeds_get()
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("HD", x, ".zip")
                      cf <- paste0("hd", x, ".csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[tmp[["stabbr"]] == "KY", ]
                      tmp <- tmp[,c("unitid","instnm","stabbr")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("HD", x)
                      tmp <- tmp[,c(1,4,2,3,5)]
                      rownames(tmp) <- NULL
                      tmp
                    })
  df_comp <- do.call(rbind, df_list)
  expect_equal(df, df_comp)
})

test_that("Mutiple table request w/filter: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021:2022) |>
    ipeds_filter(stabbr == "KY") |>
    ipeds_get(join = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("HD", x, ".zip")
                      cf <- paste0("hd", x, ".csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[tmp[["stabbr"]] == "KY", ]
                      tmp <- tmp[,c("unitid","instnm","stabbr")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("HD", x)
                      tmp <- tmp[,c(1,4,2,3,5)]
                      rownames(tmp) <- NULL
                      tmp
                    })
  df_comp <- list(do.call(rbind, df_list))
  expect_equal(df, df_comp)
})

test_that("Mutiple table request w/filter: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm) |>
    ipeds_year(2021:2022) |>
    ipeds_filter(stabbr == "KY") |>
    ipeds_get(bind = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("HD", x, ".zip")
                      cf <- paste0("hd", x, ".csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[tmp[["stabbr"]] == "KY", ]
                      tmp <- tmp[,c("unitid","instnm","stabbr")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("HD", x)
                      tmp <- tmp[,c(1,4,2,3,5)]
                      rownames(tmp) <- NULL
                      tmp
                    })
  expect_equal(df, df_list)
})

## -----------------------------------------------------------------------------
## single long table return
## -----------------------------------------------------------------------------
test_that("Single long table request: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021) |>
    ipeds_get()
  df_comp <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","efdesom")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "EF2021A_DIST"
  expect_equal(df, df_comp)
})

test_that("Single long table request: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021) |>
    ipeds_get(join = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","efdesom")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "EF2021A_DIST"
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

test_that("Single long table request: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021) |>
    ipeds_get(bind = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","efdesom")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "EF2021A_DIST"
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

## -----------------------------------------------------------------------------
## multiple long table return
## -----------------------------------------------------------------------------
test_that("Multiple long table request: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021:2022) |>
    ipeds_get()
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("EF", x, "A_DIST.zip")
                      cf <- paste0("ef", x, "a_dist_rv.csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","efdesom")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("EF", x, "A_DIST")
                      tmp
                    })
  df_comp <- do.call(rbind, df_list)
  expect_equal(df, df_comp)
})

test_that("Multiple long table request: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021:2022) |>
    ipeds_get(join = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("EF", x, "A_DIST.zip")
                      cf <- paste0("ef", x, "a_dist_rv.csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","efdesom")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("EF", x, "A_DIST")
                      tmp
                    })
  df_comp <- list(do.call(rbind, df_list))
  expect_equal(df, df_comp)
})

test_that("Multiple long table request: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021:2022) |>
    ipeds_get(bind = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("EF", x, "A_DIST.zip")
                      cf <- paste0("ef", x, "a_dist_rv.csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","efdesom")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("EF", x, "A_DIST")
                      tmp
                    })
  expect_equal(df, df_list)
})

## -----------------------------------------------------------------------------
## single long table return with filter
## -----------------------------------------------------------------------------
test_that("Single long table request w/filter: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get()
  df_comp <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","efdesom","efdelev")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "EF2021A_DIST"
  df_comp <- df_comp[df_comp[["efdelev"]] == 3,]
  df_comp <- df_comp[,c(1,4,2,3,5)]
  row.names(df_comp) <- NULL
  expect_equal(df, df_comp)
})

test_that("Single long table request w/filter: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get(join = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","efdesom","efdelev")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "EF2021A_DIST"
  df_comp <- df_comp[df_comp[["efdelev"]] == 3,]
  df_comp <- df_comp[,c(1,4,2,3,5)]
  row.names(df_comp) <- NULL
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

test_that("Single long table request w/filter: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get(bind = FALSE)
  df_comp <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_comp) <- tolower(colnames(df_comp))
  df_comp <- df_comp[,c("unitid","efdesom","efdelev")]
  df_comp["year"] <- 2021
  df_comp["file"] <- "EF2021A_DIST"
  df_comp <- df_comp[df_comp[["efdelev"]] == 3,]
  df_comp <- df_comp[,c(1,4,2,3,5)]
  row.names(df_comp) <- NULL
  df_comp <- list(df_comp)
  expect_equal(df, df_comp)
})

## -----------------------------------------------------------------------------
## multiple long table return with a filter
## -----------------------------------------------------------------------------
test_that("Multiple long table request w/filter: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021:2022) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get()
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("EF", x, "A_DIST.zip")
                      cf <- paste0("ef", x, "a_dist_rv.csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","efdesom","efdelev")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("EF", x, "A_DIST")
                      tmp <- tmp[tmp[["efdelev"]] == 3,]
                      tmp <- tmp[,c(1,4,2,3,5)]
                      row.names(tmp) <- NULL
                      tmp
                    })
  df_comp <- do.call(rbind, df_list)
  expect_equal(df, df_comp)
})

test_that("Multiple long table request w/filter: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021:2022) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get(join = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("EF", x, "A_DIST.zip")
                      cf <- paste0("ef", x, "a_dist_rv.csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","efdesom","efdelev")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("EF", x, "A_DIST")
                      tmp <- tmp[tmp[["efdelev"]] == 3,]
                      tmp <- tmp[,c(1,4,2,3,5)]
                      row.names(tmp) <- NULL
                      tmp
                    })
  df_comp <- list(do.call(rbind, df_list))
  expect_equal(df, df_comp)
})

test_that("Multiple long table request w/filter: (bind = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(efdesom) |>
    ipeds_year(2021:2022) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get(bind = FALSE)
  df_list <- list()
  df_list <- lapply(2022:2021,
                    function(x) {
                      zf <- paste0("EF", x, "A_DIST.zip")
                      cf <- paste0("ef", x, "a_dist_rv.csv")
                      tmp <- read.csv(base::unz(file.path(tempdir(), zf), cf))
                      colnames(tmp) <- tolower(colnames(tmp))
                      tmp <- tmp[,c("unitid","efdesom","efdelev")]
                      tmp["year"] <- x
                      tmp["file"] <- paste0("EF", x, "A_DIST")
                      tmp <- tmp[tmp[["efdelev"]] == 3,]
                      tmp <- tmp[,c(1,4,2,3,5)]
                      row.names(tmp) <- NULL
                      tmp
                    })
  expect_equal(df, df_list)
})
## -----------------------------------------------------------------------------
## single wide and long table return
## -----------------------------------------------------------------------------
test_that("Single wide and long table request: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm, efdesom) |>
    ipeds_year(2021) |>
    ipeds_get()

  df_one <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_one) <- tolower(colnames(df_one))
  df_one <- df_one[,c("unitid","instnm")]
  df_one["year"] <- 2021
  df_two <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_two) <- tolower(colnames(df_two))
  df_two <- df_two[,c("unitid","efdesom")]
  df_two["year"] <- 2021

  df_comp <- merge(df_one, df_two, by = c("unitid", "year"), all = TRUE)
  df_comp <- df_comp[, c(1,2,4,3)]
  rownames(df) <- NULL

  expect_equal(df, df_comp)
})

## -----------------------------------------------------------------------------
## single wide and long table return with filter
## -----------------------------------------------------------------------------
test_that("Single wide and long table request w/filter: (default), return data.frame", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(instnm, efdesom) |>
    ipeds_year(2021) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get()

  df_one <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_one) <- tolower(colnames(df_one))
  df_one <- df_one[,c("unitid","instnm")]
  df_one["year"] <- 2021
  df_two <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_two) <- tolower(colnames(df_two))
  df_two <- df_two[,c("unitid","efdesom","efdelev")]
  df_two["year"] <- 2021

  df_comp <- merge(df_one, df_two, by = c("unitid", "year"), all = TRUE)
  df_comp <- df_comp[df_comp$efdelev == 3 & !is.na(df_comp$efdelev),]
  df_comp <- df_comp[, c(1,2,4,5,3)]
  rownames(df_comp) <- NULL

  expect_equal(df, df_comp)
})

test_that("Single wide and long table request w/filter: (join = FALSE), return list", {
  df <- ipeds_init(local_dir = tempdir()) |>
    ipeds_select(stabbr, efdesom) |>
    ipeds_year(2021) |>
    ipeds_filter(efdelev == 3) |>
    ipeds_get(join = FALSE)

  ## HD
  df_one <- read.csv(base::unz(file.path(tempdir(), "HD2021.zip"), "hd2021.csv"))
  colnames(df_one) <- tolower(colnames(df_one))
  df_one <- df_one[,c("unitid","stabbr")]
  df_one["year"] <- 2021
  df_one["file"] <- "HD2021"
  df_one <- df_one[, c(1,3,2,4)]
  ## EF
  df_two <- read.csv(base::unz(file.path(tempdir(), "EF2021A_DIST.zip"), "ef2021a_dist_rv.csv"))
  colnames(df_two) <- tolower(colnames(df_two))
  df_two <- df_two[,c("unitid","efdesom","efdelev")]
  df_two <- df_two[df_two$efdelev == 3 & !is.na(df_two$efdelev),]
  df_two["year"] <- 2021
  df_two["file"] <- "EF2021A_DIST"
  df_two <- df_two[, c(1,4,2,3,5)]
  rownames(df_two) <- NULL
  ## filter HD to remove missing from EF
  df_one <- df_one[df_one[["unitid"]] %in% df_two[["unitid"]],]
  rownames(df_one) <- NULL
  ## combine
  df_comp <- list(df_two, df_one)

  expect_equal(df, df_comp)
})
