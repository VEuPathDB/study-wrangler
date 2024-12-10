
pet_types = c("dog", "cat", "puppy")

dog = fct("dog", levels = pet_types)

pets <- tibble(
  name = c("Max", "Bella", "Chuck", "Luna", "Cooper"),
  # note the so-far unused "puppy" level:
  pet_type = factor(c("dog", "dog", "cat", "dog", "cat"), levels = pet_types),
  age = c(1, 3, 5, 2, 4),
  owner = c("Alice", "Bob", "Charlie", "Dana", "Eve")
)

test_that("fac_mutate() handles simple two-argument cases", {

  expect_silent(
    expect_equal(
      as.character(fct_mutate(dog, 'cat')),
      "cat"
    )
  )

  # even if one or both is named
  expect_silent(
    expect_equal(
      as.character(fct_mutate(dog, true_value='cat')),
      "cat"
    )
  )

  expect_silent(
    expect_equal(
      as.character(fct_mutate(factor=dog, true_value='cat')),
      "cat"
    )
  )

})

# test_that("fac_mutate() handles preserves levels and errors on non-level values", {
  
