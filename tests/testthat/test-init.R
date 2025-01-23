## -- init --

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

test_that("Default ipedscall from ipeds_init()", {
  expect_equal(ipeds_init(), dil)
})

test_that("Did not start with ipeds_init()", {
  message <- "Chain not properly initialized. Be sure to start with ipeds_init()."

  ## one off
  expect_error(ipeds_select(), message)
  expect_error(ipeds_filter(), message)
  expect_error(ipeds_year(), message)
  expect_error(ipeds_get(), message)

  ## otherwise correct chains
  expect_error(ipeds_filter(unitid == 1000) |>
                 ipeds_select(unitid), message)
  expect_error(ipeds_filter(unitid == 1000) |>
                 ipeds_select(unitid) |>
                 ipeds_year(2020), message)
  expect_error(ipeds_filter(unitid == 1000) |>
                 ipeds_select(unitid) |>
                 ipeds_year(2020) |>
                 ipeds_get(), message)

  expect_error(ipeds_select(unitid) |>
                 ipeds_filter(unitid == 1000), message)
  expect_error(ipeds_select(unitid) |>
                 ipeds_filter(unitid == 1000) |>
                 ipeds_year(2020), message)
  expect_error(ipeds_select(unitid) |>
                 ipeds_filter(unitid == 1000) |>
                 ipeds_year(2020) |>
                 ipeds_get(), message)

  ## uninitialized chains with other errors (un-init should take precedence)
  expect_error(ipeds_filter(x == 1000) |>
                 ipeds_select(unitid), message)
  expect_error(ipeds_filter(unitid == 1000) |>
                 ipeds_select(x) |>
                 ipeds_year(2020), message)
  expect_error(ipeds_filter(unitid == 1000) |>
                 ipeds_select(unitid) |>
                 ipeds_year(1800) |>
                 ipeds_get(), message)
  expect_error(ipeds_filter(unitid = 1000) |>
                 ipeds_select() |>
                 ipeds_year(1800) |>
                 ipeds_get(), message)

})
