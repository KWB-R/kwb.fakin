#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hauke on 2019-07-14 11:27:57.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("report_about_r_scripts() works", {

  expect_error(
    kwb.fakin:::report_about_r_scripts()
    # argument "root" is missing, with no default
  )

})

