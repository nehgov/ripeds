## -----------------------------------------------------------------------------
## -- dictionary --
## -----------------------------------------------------------------------------

test_that("Dictionary does not return correct object", {
  dict <- ipeds_dict("stabbr", exact_match = TRUE, return_dict = TRUE,
                     print_off = TRUE)
  expect_identical(dict[["varname"]], rep("stabbr", nrow(dict)))
  expect_equal(any(grepl("HD2002", dict[["filename"]])), TRUE)
  expect_equal(dict[dict[["filename"]] == "HD2023", "description"],
               "State abbreviation")
})

test_that("Error for non-match", {
  expect_error(ipeds_dict("stabbrr", exact_match = TRUE, return_dict = TRUE,
                          print_off = TRUE),
               "No matches! Try again with new string or column.")
  expect_error(ipeds_dict("stabbrr", return_dict = TRUE, print_off = TRUE),
               "No matches! Try again with new string or column.")
})
