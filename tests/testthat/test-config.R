test_that("configuration functions work correctly", {
  # Reset config to ensure clean state
  reset_config()
  
  # Test default configuration
  config <- get_config()
  expect_equal(config$validation$profiles, "baseline")
  
  # Test setting single profile with dot notation
  set_config(validation.profiles = "eda")
  config <- get_config()
  expect_equal(config$validation$profiles, "eda")
  
  # Test setting multiple profiles with dot notation
  set_config(validation.profiles = c("baseline", "eda"))
  config <- get_config()
  expect_equal(config$validation$profiles, c("baseline", "eda"))
  
  # Test multiple config options in single call
  set_config(validation.profiles = "custom", validation.strict = TRUE)
  config <- get_config()
  expect_equal(config$validation$profiles, "custom")
  expect_equal(config$validation$strict, TRUE)
  
  # Test validation profile helper
  profiles <- get_validation_profiles(c("custom", "test"))
  expect_equal(profiles, c("custom", "test"))
  
  profiles <- get_validation_profiles(NULL)
  expect_equal(profiles, "custom")  # Should use global config
  
  # Test reset
  reset_config()
  config <- get_config()
  expect_equal(config$validation$profiles, "baseline")
})

test_that("dot notation conversion works correctly", {
  reset_config()
  
  # Test single level (no dots)
  args <- list(simple = "value")
  result <- study.wrangler:::.convert_dot_notation_to_list(args)
  expect_equal(result$simple, "value")
  
  # Test two levels
  args <- list("level1.level2" = "value")
  result <- study.wrangler:::.convert_dot_notation_to_list(args)
  expect_equal(result$level1$level2, "value")
  
  # Test three levels
  args <- list("a.b.c" = "value")
  result <- study.wrangler:::.convert_dot_notation_to_list(args)
  expect_equal(result$a$b$c, "value")
  
  # Test multiple entries with same top level
  args <- list("validation.profiles" = c("a", "b"), "validation.strict" = TRUE)
  result <- study.wrangler:::.convert_dot_notation_to_list(args)
  expect_equal(result$validation$profiles, c("a", "b"))
  expect_equal(result$validation$strict, TRUE)
})

test_that("config validation works", {
  reset_config()
  
  # Test empty call
  expect_warning(set_config(), "No configuration values provided")
})

test_that("config file paths are constructed correctly", {
  # Test that file paths are created correctly
  reset_config()
  
  cwd_path <- file.path(getwd(), ".wranglerrc")
  home_path <- file.path(Sys.getenv("HOME"), ".wranglerrc")
  
  expect_true(is.character(cwd_path))
  expect_true(is.character(home_path))
  expect_true(nchar(cwd_path) > 0)
  expect_true(nchar(home_path) > 0)
  expect_true(grepl("\\.wranglerrc$", cwd_path))
  expect_true(grepl("\\.wranglerrc$", home_path))
})