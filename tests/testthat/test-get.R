## -- get --

test_that("Errors for non-init()", {
  expect_error(ipeds_get(),
               "Chain not properly initialized. Be sure to start with ipeds_init().")
})

test_that("Errors for missing sc_select()", {
  expect_error(ipeds_init() |> ipeds_filter(unitid == 999999) |> ipeds_get(),
               "No variables selected. Use ipeds_select() to choose variables.",
               fixed = TRUE)
})
