test_that("get_special_character_info() works", {

  f <- kwb.fakin:::get_special_character_info

  expect_error(f())
  expect_error(f(c("a", "b")))
  expect_error(f(c("no special")))
  f(c("very spéciàl"))
})
