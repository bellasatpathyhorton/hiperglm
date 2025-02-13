test_that("function correctly returns TRUE", {
  expect_true(are_all_close(5, 5))
})

test_that("function correctly returns FALSE because relative error is above tolerance", {
  expect_true(are_all_close(0.1, 0.1000003))
})

test_that("function correctly returns FALSE because absolute error is above tolerance", {
  testthat::expect_true(are_all_close(0.1, 0.100003))
})
