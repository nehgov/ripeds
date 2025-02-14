## -----------------------------------------------------------------------------
## -- filter --
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

## -----------------------------------------------------------------------------
## errors
## -----------------------------------------------------------------------------

test_that("Errors for non-init()", {
  expect_error(ipeds_filter(unitid == 99999),
               "Chain not properly initialized. Be sure to start with ipeds_init().")
})

test_that("Error for bad variable names", {
  expect_error(ipeds_filter(dil, x == 99999),
               paste("Variable \"x\" not found in dictionary.",
                     "Please check your spelling or search dictionary:",
                     "?ipeds_dict()"),
               fixed = TRUE)
})

## -----------------------------------------------------------------------------
## nse filters
## -----------------------------------------------------------------------------

test_that("Simple filter is parsed", {
  call <- ipeds_filter(dil, stabbr == "KY")
  expect_equal(is.name(call$fvars[[1]]), TRUE)
  expect_equal(call$fvars[[1]], as.name("stabbr"))
  expect_equal(deparse(call$filter[[1]]), "stabbr == \"KY\"")
})

test_that("Double filter is parsed", {
  call <- ipeds_filter(dil, stabbr == "KY", control %in% c(1:2))
  expect_equal(length(call$fvars), 2)
  expect_equal(is.name(call$fvars[[1]]), TRUE)
  expect_equal(is.name(call$fvars[[2]]), TRUE)
  expect_equal(call$fvars[[1]], as.name("stabbr"))
  expect_equal(call$fvars[[2]], as.name("control"))
  expect_equal(deparse(call$filter[[1]]), "stabbr == \"KY\"")
  expect_equal(deparse(call$filter[[2]]), "control %in% c(1:2)")
})

## -----------------------------------------------------------------------------
## string filters
## -----------------------------------------------------------------------------

test_that("Simple string filter is parsed", {
  call <- ipeds_filter(dil, "stabbr == 'KY'")
  expect_equal(is.character(call$fvars[[1]]), TRUE)
  expect_equal(call$fvars[[1]], "stabbr")
  expect_equal(deparse(call$filter[[1]]), "stabbr == \"KY\"")
})

test_that("Double string filter is parsed", {
  call <- ipeds_filter(dil, "stabbr == 'KY' | control %in% c(1:2)")
  expect_equal(length(call$fvars), 2)
  expect_equal(is.character(call$fvars[[1]]), TRUE)
  expect_equal(is.character(call$fvars[[2]]), TRUE)
  expect_equal(call$fvars[[1]], "stabbr")
  expect_equal(call$fvars[[2]], "control")
  expect_equal(deparse(call$filter[[1]]), "stabbr == \"KY\" | control %in% c(1:2)")
})
