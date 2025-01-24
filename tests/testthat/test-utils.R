## -- utils --

## formatting

test_that("paste sugar", {
  expect_equal("a" %+% "b", paste0("a", "b"))
  expect_equal("a" %+% "b", paste("a", "b", sep = ""))
})

## i/o

test_that("get_file_stub_name", {
  expect_equal(get_file_stub_name("../dir/subdir/file.txt"), "file")
})
