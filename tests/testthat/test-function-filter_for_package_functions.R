#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hauke on 2019-07-14 11:27:40.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("filter_for_package_functions() works", {

  expect_error(
    kwb.fakin:::filter_for_package_functions()
    # no item called "getNamespace(package)" on the search list
  )

})
