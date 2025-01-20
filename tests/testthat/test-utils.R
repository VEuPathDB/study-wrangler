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

### generate_alphanumeric_id ###

test_that("generate_alphanumeric_id() is deterministic with same seed_string", {
  id1 <- generate_alphanumeric_id(seed_string = "hello")
  id2 <- generate_alphanumeric_id(seed_string = "hello")
  expect_true(id1 == id2)
})

test_that("Different seed strings produce different IDs", {
  id1 <- generate_alphanumeric_id(seed_string = "hello")
  id2 <- generate_alphanumeric_id(seed_string = "world")
  expect_false(id1 == id2, info = "Different seed strings should produce different IDs")
})

test_that("Simple reversals do not fool generate_alphanumeric_id()", {
  id1 <- generate_alphanumeric_id(seed_string = "abc")
  id2 <- generate_alphanumeric_id(seed_string = "cba")
  expect_false(id1 == id2)
})

test_that("Empty seed string produces a valid ID", {
  id <- generate_alphanumeric_id(seed_string = "")
  expect_true(is_truthy(id))  # Ensure the ID is valid
})

test_that("missing seed string throws an error", {
  expect_error(id <- generate_alphanumeric_id())
})

test_that("Different-length seeds produce different IDs", {
  id1 <- generate_alphanumeric_id(seed_string = "hello")
  id2 <- generate_alphanumeric_id(seed_string = "hello_world")
  expect_false(id1 == id2, info = "Seeds with different lengths should produce different IDs")
})

test_that("Default ID length is 11", {
  id <- generate_alphanumeric_id("hello world")
  expect_equal(nchar(id), 11, info = "Default ID length should be 11 characters")
})

test_that("Custom length produces IDs of the correct size", {
  id <- generate_alphanumeric_id("hello world", length = 20)
  expect_equal(nchar(id), 20, info = "Custom length should produce IDs of the specified size")
})

test_that("Large seed strings are handled gracefully", {
  long_seed <- paste(rep("a", 1000), collapse = "")
  id <- generate_alphanumeric_id(seed_string = long_seed)
  expect_true(is_truthy(id))  # Ensure the ID is valid
})

test_that("Seed strings are case-sensitive", {
  id1 <- generate_alphanumeric_id(seed_string = "hello")
  id2 <- generate_alphanumeric_id(seed_string = "HELLO")
  expect_false(id1 == id2, info = "Case-sensitive seed strings should produce different IDs")
})

test_that("max_decimals() works as intended", {
  
  expect_no_error(
    expect_equal(
      max_decimals(c("hello", "world")),
      0
    )
  )

  expect_equal(
    max_decimals(c(-1,0,1)),
    0
  )
  
  expect_equal(
    max_decimals(c(-100.123, 0.12345, 50.12)),
    5
  )

  # it can't count more than 15 places intentionally
  # ideally it would warn about this but I don't think that's worth implementing
  expect_no_warning(
    expect_no_error(
      expect_equal(
        max_decimals(0.1234567890123456789),
        15
      )
    )
  )
    
  # Zeros with trailing decimals
  expect_equal(
    max_decimals(c(0.0, 0.00, 0.000)),
    0
  )
  
  # Very small numbers (avoiding scientific notation issues)
  expect_equal(
    max_decimals(c(1e-5, 1e-10, 1e-15)),
    15
  )
  
  # Mixed zeros and decimals
  expect_equal(
    max_decimals(c(0, 0.123, 1.0)),
    3
  )
  
  # Non-numeric coerced to numeric
  expect_no_error(
    expect_equal(
      max_decimals(as.numeric(c("1.23", "4.567", "0.12345"))),
      5
    )
  )  
})
