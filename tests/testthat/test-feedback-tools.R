# Tests for feedback_tools.R — format-aware message selection

test_that("format='full' emits message even when upload_message is present", {
  tools <- create_feedback_tools(format = "full")
  tools$add_feedback("R fix: use foo()", upload_message = "Please fix column 'foo'.")
  expect_warning(
    tools$give_feedback(),
    "R fix: use foo()",
    fixed = TRUE
  )
})

test_that("format='upload' emits upload_message when present", {
  tools <- create_feedback_tools(format = "upload")
  tools$add_feedback("R fix: use foo()", upload_message = "Please fix column 'foo'.")
  expect_warning(
    tools$give_feedback(),
    "Please fix column 'foo'.",
    fixed = TRUE
  )
})

test_that("format='upload' falls back to message when upload_message is NULL", {
  tools <- create_feedback_tools(format = "upload")
  tools$add_feedback("R fix: use foo()")
  expect_warning(
    tools$give_feedback(),
    "R fix: use foo()",
    fixed = TRUE
  )
})

test_that("format='upload' falls back to message when upload_message is empty string", {
  tools <- create_feedback_tools(format = "upload")
  tools$add_feedback("R fix: use foo()", upload_message = "")
  expect_warning(
    tools$give_feedback(),
    "R fix: use foo()",
    fixed = TRUE
  )
})

test_that("fatal path respects format='upload' with fatal_upload_message", {
  tools <- create_feedback_tools(format = "upload")
  expect_warning(
    tools$give_feedback(
      fatal_message = "R code: entity %>% fix()",
      fatal_upload_message = "Your data could not be loaded."
    ),
    "Your data could not be loaded.",
    fixed = TRUE
  )
})

test_that("fatal path falls back to fatal_message when fatal_upload_message is NULL", {
  tools <- create_feedback_tools(format = "upload")
  expect_warning(
    tools$give_feedback(fatal_message = "R code: entity %>% fix()"),
    "R code: entity %>% fix()",
    fixed = TRUE
  )
})

test_that("advisory path respects format='upload'", {
  tools <- create_feedback_tools(format = "upload")
  tools$add_feedback("R advisory detail", advisory = TRUE, upload_message = "Please check column 'bar'.")
  expect_message(
    expect_message(
      tools$give_feedback(),
      "Please check column 'bar'.",
      fixed = TRUE
    ),
    "Validation successful!",
    fixed = TRUE
  )
})
  
test_that("default format is 'full'", {
  tools <- create_feedback_tools()
  tools$add_feedback("R fix message", upload_message = "Upload message")
  expect_warning(
    tools$give_feedback(),
    "R fix message",
    fixed = TRUE
  )
})

test_that("get_is_valid returns FALSE after non-advisory feedback", {
  tools <- create_feedback_tools()
  expect_true(tools$get_is_valid())
  tools$add_feedback("issue")
  expect_false(tools$get_is_valid())
})

test_that("advisory feedback does not change is_valid", {
  tools <- create_feedback_tools()
  tools$add_feedback("note", advisory = TRUE)
  expect_true(tools$get_is_valid())
})
