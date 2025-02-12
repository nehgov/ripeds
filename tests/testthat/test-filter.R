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
