### is_truthy ###

test_that("NULL and NA are falsey", {
  expect_false(is_truthy(NULL))
  expect_false(is_truthy(NA))
})

test_that("Logical values behave correctly", {
  expect_false(is_truthy(FALSE))
  expect_true(is_truthy(TRUE))
})

test_that("Numeric values behave correctly", {
  expect_false(is_truthy(0))   # 0 is falsey
  expect_true(is_truthy(1))   # 1 is truthy
  expect_true(is_truthy(42))  # Any non-zero number is truthy
  expect_false(is_truthy(NA_real_))  # NA_real_ is falsey
})

test_that("Strings behave correctly", {
  expect_false(is_truthy(""))           # Empty string is falsey
  expect_true(is_truthy("hello"))       # Non-empty string is truthy
  expect_true(is_truthy(" "))           # String with a space is truthy
  expect_true(is_truthy("0"))           # We do NOT expect Perl behaviour
  expect_true(is_truthy("some string")) # Non-empty string is truthy
  expect_false(is_truthy(NA_character_))  # NA_character_ is falsey
})

test_that("Vector inputs are an error", {
  expect_error(is_truthy(c(TRUE, FALSE)), "is_truthy.+only handles scalar")
})

test_that("Factors and other types are handled gracefully", {
  expect_true(is_truthy(as.factor("yes"))) # Factors behave like non-empty strings
  expect_true(is_truthy(as.factor("no")))  # and there is no clever semantics
  expect_true(is_truthy(as.factor(0)))     # especially for this unlikely use-case?
  expect_false(is_truthy(as.factor(NA)))   # Factors with NA are falsey
})
