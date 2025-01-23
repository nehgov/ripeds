## -- dictionary --

test_that("Dictionary does not return correct object", {
  varn <- ipeds_dict("stabbr", exact_match = TRUE, return_dict = TRUE, print_off = TRUE)[1, "varname"]
  expect_identical("stabbr", varn)
})

test_that("Error for non-match", {
  expect_error(ipeds_dict("stabbrr", exact_match = TRUE, return_dict = TRUE, print_off = TRUE),
               "No matches! Try again with new string or column.")
  expect_error(ipeds_dict("stabbrr", return_dict = TRUE, print_off = TRUE),
               "No matches! Try again with new string or column.")
})
