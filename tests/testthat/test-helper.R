

test_that("get_file_stub_name", {
  expect_equal(get_file_stub_name("../dir/subdir/file.txt"), "file")
})
