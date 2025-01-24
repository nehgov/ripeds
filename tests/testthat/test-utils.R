## -- utils --

## formatting ___

test_that("%+%", {
  expect_equal("a" %+% "b", paste0("a", "b"))
  expect_equal("a" %+% "b", paste("a", "b", sep = ""))
})

test_that("hline", {
  expect_equal(hline(5), "-----")
  expect_equal(hline(3, "="), "===")
})

test_that("create_search_str", {
  expect_equal(create_search_str(letters[1:3]), "a|b|c")
})

## hash helpers ___

test_that("convert_hash_name", {
  expect_equal(convert_hash_name("idxf"), "filename")
  expect_equal(convert_hash_name("idxv"), "varname")
  expect_equal(convert_hash_name("idxd"), "description")
})

test_that("convert_hash_df_names", {
  df <- data.frame("idxf" = letters[1:3],
                   "idxv" = letters[1:3],
                   "idxd" = letters[1:3])
  expect_equal(names(convert_hash_df_names(df)),
               c("filename", "varname", "description"))
})

## get_hash
## convert_hash_vec
## convert_hash_df

## i/o ___

test_that("get_file_stub_name", {
  expect_equal(get_file_stub_name("../dir/subdir/file.txt"), "file")
})

## get_internal_file_name
## get_file_location_or_download

## helper functions ___

test_that("lower_names_df", {
  df <- data.frame("A" = 1:2,
                   "B" = 3:4,
                   "CDE" = 5:6)
  expect_equal(names(lower_names_df(df)), c("a", "b", "cde"))
})

test_that("bind_rows_df", {
  df_list <- list(data.frame("a" = 1, "b" = 3, "c" = 5),
                  data.frame("a" = 2, "b" = 4, "c" = 6))
  df <- data.frame("a" = 1:2, "b" = 3:4, "c" = 5:6)
  expect_equal(bind_rows_df(df_list), df)
})

test_that("make_distinct", {
  df <- data.frame("a" = 1, "b" = c(1,2,2), "c" = 3)
  expect_equal(make_distinct(df), data.frame("a" = 1, "b" = c(1,2), "c" = 3))
  expect_equal(make_distinct(df, c("a", "c")), data.frame("a" = 1, "b" = 1, "c" = 3))
})

test_that("filter_equals", {
  df <- data.frame("a" = 1, "b" = c(1,2,2), "c" = 3)
  expect_equal(filter_equals(df, "b", 1), data.frame("a" = 1, "b" = 1, "c" = 3))
  expect_equal(filter_equals(df, "b", 2), data.frame("a" = 1, "b" = c(2,2), "c" = 3),
               ignore_attr = TRUE)
  expect_equal(filter_equals(df, "c", 3), df)
})

test_that("filter_in", {
  df <- data.frame("a" = 1, "b" = c(1,2,2), "c" = 3)
  expect_equal(filter_in(df, "b", 1), data.frame("a" = 1, "b" = 1, "c" = 3))
  expect_equal(filter_in(df, "b", 2), data.frame("a" = 1, "b" = c(2,2), "c" = 3),
               ignore_attr = TRUE)
  expect_equal(filter_in(df, "c", 3), df)
})

test_that("filter_rows", {
  df <- data.frame("unitid" = rep(1:5, each = 10), "year" = 2010:2019, "c" = letters[1:10])
  fdf <- data.frame("unitid" = c(1,1,5,5), "year" = c(2010,2012,2010,2012))
  expect_equal(filter_rows(df, fdf), data.frame("unitid" = c(1,1,5,5),
                                                "year" = c(2010,2012,2010,2012),
                                                "c" = c("a","c","a","c")))
})

test_that("cyear_to_ayear", {
  expect_equal(cyear_to_ayear(2020), "2021")
  expect_equal(cyear_to_ayear(1999), "9900")
})

## order_vars
## roll_join_full
## get_vars_from_file
## get_file_year
## subset_file_table_by_year
## subset_dict_by_var_year

## package structure helpers ___

## confirm_chain
## confirm_vars
