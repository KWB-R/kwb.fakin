#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hauke on 2019-07-14 11:27:57.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("get_rmd_script_function_enumeration() works", {

  expect_error(
    kwb.fakin:::get_rmd_script_function_enumeration()
    # argument "all_function_info" is missing, with no default
  )

})

