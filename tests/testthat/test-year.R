## -- year --

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
  expect_error(ipeds_year(2023),
               "Chain not properly initialized. Be sure to start with ipeds_init().")
})

test_that("Bad year range, type, or missing", {
  expect_error(ipeds_year(dil, 1847),
               paste0("Must provide a 4-digit year or vector of 4-digit years ",
                      "within bounds of IPEDS years:\n\n",
                      "Earliest available year:    ",
                      min(ipeds_file_table()[["year"]]), "\n",
                      "Most recent available year: ",
                      max(ipeds_file_table()[["year"]])), fixed = TRUE)
  expect_error(ipeds_year(dil, 2100),
               paste0("Must provide a 4-digit year or vector of 4-digit years ",
                      "within bounds of IPEDS years:\n\n",
                      "Earliest available year:    ",
                      min(ipeds_file_table()[["year"]]), "\n",
                      "Most recent available year: ",
                      max(ipeds_file_table()[["year"]])), fixed = TRUE)
  expect_error(ipeds_year(dil, "x"),
               paste0("Must provide a 4-digit year or vector of 4-digit years ",
                      "within bounds of IPEDS years:\n\n",
                      "Earliest available year:    ",
                      min(ipeds_file_table()[["year"]]), "\n",
                      "Most recent available year: ",
                      max(ipeds_file_table()[["year"]])), fixed = TRUE)
  expect_error(ipeds_year(dil, "latest"),
               paste0("Must provide a 4-digit year or vector of 4-digit years ",
                      "within bounds of IPEDS years:\n\n",
                      "Earliest available year:    ",
                      min(ipeds_file_table()[["year"]]), "\n",
                      "Most recent available year: ",
                      max(ipeds_file_table()[["year"]])), fixed = TRUE)
  expect_error(ipeds_year(dil),
               paste0("Must provide a 4-digit year or vector of 4-digit years ",
                      "within bounds of IPEDS years:\n\n",
                      "Earliest available year:    ",
                      min(ipeds_file_table()[["year"]]), "\n",
                      "Most recent available year: ",
                      max(ipeds_file_table()[["year"]])), fixed = TRUE)
})
