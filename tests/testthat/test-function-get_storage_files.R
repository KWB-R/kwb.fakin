#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hauke on 2019-07-14 11:27:59.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("get_storage_files() works", {

  expect_error(
    kwb.fakin:::get_storage_files()
    # argument "name" is missing, with no default
  )

})

