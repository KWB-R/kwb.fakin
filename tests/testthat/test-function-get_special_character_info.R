#
# This test file has been generated by kwb.test::create_test_files()
# launched by user hauke on 2019-07-14 11:27:58.
# Your are strongly encouraged to modify the dummy functions
# so that real cases are tested. You should then delete this comment.
#

test_that("get_special_character_info() works", {

  expect_error(
    kwb.fakin:::get_special_character_info()
    # argument "text" is missing, with no default
  )

})

