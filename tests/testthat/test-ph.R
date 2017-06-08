context("pH values")

test_that("is_valied_pH function exists", {
  expect_true(is.function(is_valid_pH))
})

test_that("is_valied_pH retunrs FALSE", {
  expect_false(is_valid_pH(22))
})

test_that("negative pH is FALSE", {
  expect_false(is_valid_pH(-2))
})

test_that("edge cases are TRUE", {
  expect_true(is_valid_pH(0))
  expect_true(is_valid_pH(14))
})
