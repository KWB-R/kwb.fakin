#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("get_path_stat_matrix() works", {

  expect_error(
    kwb.fakin:::get_path_stat_matrix()
    # argument "project_folder" is missing, with no default
  )

})
