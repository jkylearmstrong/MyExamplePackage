devtools::load_all()

test_that("Add Two Numbers", {
  expect_identical(add_two_nums(6, 4), 10)
  expect_identical(add_two_nums(-2L, pi), -2L + pi)
  expect_identical(add_two_nums(c(2,3),c(1,2)), c(2,3)+c(1,2)) 
})