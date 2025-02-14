## -----------------------------------------------------------------------------
## -- select --
## -----------------------------------------------------------------------------

dil <- list("ipeds_init_list" = TRUE,
            "ldir" = NA,
            "revfiles" = TRUE,
            "dict" = NULL,
            "survey" = NULL,
            "svars" = NULL,
            "sorder" = NULL,
            "filter" = NULL,
            "fvars" = NULL,
            "year" = NULL,
            "nse" = TRUE)

test_that("Errors for non-init()", {
  expect_error(ipeds_select(unitid),
               "Chain not properly initialized. Be sure to start with ipeds_init().")
})

test_that("Errors for blank", {
  expect_error(ipeds_select(dil),
               "Incomplete ipeds_select()! You must select at least one variable.",
               fixed = TRUE)
})

test_that("Error for bad variable names", {
  expect_error(ipeds_select(dil, x),
               paste("Variable \"x\" not found in dictionary.",
                     "Please check your spelling or search dictionary:",
                     "?ipeds_dict()"),
               fixed = TRUE)
})

## -----------------------------------------------------------------------------
## nse select
## -----------------------------------------------------------------------------

test_that("Simple select is parsed", {
  call <- ipeds_select(dil, instnm)
  expect_equal(is.name(call$svars[[1]]), TRUE)
  expect_equal(call$svars[[1]], as.name("instnm"))
  expect_equal(call$sorder[[1]], "instnm")
})

test_that("Multiple select is parsed", {
  call <- ipeds_select(dil, instnm, stabbr, control)
  expect_equal(length(call$svars), 3)
  expect_equal(is.name(call$svars[[1]]), TRUE)
  expect_equal(is.name(call$svars[[2]]), TRUE)
  expect_equal(is.name(call$svars[[3]]), TRUE)
  expect_equal(call$svars[[1]], as.name("instnm"))
  expect_equal(call$svars[[2]], as.name("stabbr"))
  expect_equal(call$svars[[3]], as.name("control"))
  expect_equal(call$sorder, c("instnm", "stabbr", "control"))
})

## -----------------------------------------------------------------------------
## string select
## -----------------------------------------------------------------------------

test_that("Simple string select is parsed", {
  call <- ipeds_select(dil, "instnm")
  expect_equal(is.character(call$svars[[1]]), TRUE)
  expect_equal(call$svars[[1]], "instnm")
  expect_equal(call$sorder[[1]], "instnm")
})

test_that("Multiple string select is parsed", {
  call <- ipeds_select(dil, "instnm", "stabbr", "control")
  expect_equal(length(call$svars), 3)
  expect_equal(is.character(call$svars[[1]]), TRUE)
  expect_equal(is.character(call$svars[[2]]), TRUE)
  expect_equal(is.character(call$svars[[3]]), TRUE)
  expect_equal(call$svars[[1]], "instnm")
  expect_equal(call$svars[[2]], "stabbr")
  expect_equal(call$svars[[3]], "control")
  expect_equal(call$sorder, c("instnm", "stabbr", "control"))
})

test_that("Multiple string using c() select is parsed", {
  dil$nse <- FALSE
  call <- ipeds_select(dil, c("instnm", "stabbr", "control"))
  expect_equal(length(call$svars), 3)
  expect_equal(is.character(call$svars[[1]]), TRUE)
  expect_equal(is.character(call$svars[[2]]), TRUE)
  expect_equal(is.character(call$svars[[3]]), TRUE)
  expect_equal(call$svars[[1]], "instnm")
  expect_equal(call$svars[[2]], "stabbr")
  expect_equal(call$svars[[3]], "control")
  expect_equal(call$sorder, c("instnm", "stabbr", "control"))
})

test_that("Multiple string from object select is parsed", {
  dil$nse <- FALSE
  select_vars <- c("instnm", "stabbr", "control")
  call <- ipeds_select(dil, select_vars)
  expect_equal(length(call$svars), 3)
  expect_equal(is.character(call$svars[[1]]), TRUE)
  expect_equal(is.character(call$svars[[2]]), TRUE)
  expect_equal(is.character(call$svars[[3]]), TRUE)
  expect_equal(call$svars[[1]], "instnm")
  expect_equal(call$svars[[2]], "stabbr")
  expect_equal(call$svars[[3]], "control")
  expect_equal(call$sorder, c("instnm", "stabbr", "control"))
})
