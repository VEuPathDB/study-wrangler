
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


test_that("fac_mutate() handles preserves levels", {
  expect_equal(
    levels(fct_mutate(dog, "puppy")),
    pet_types
  )
})
  
test_that("fac_mutate() errors on non-level values", {
  expect_error(
    oops <- fct_mutate(dog, "llama"),
    "Error: Value 'llama' is not an existing level in the factor"
  )
})

test_that("fac_mutate() conditions work on column mutations", {
  expect_equal(
    pets %>% mutate(
      pet_type = fct_mutate(
        pet_type,
        pet_type == 'dog' & age <= 2,
        'puppy'
      )) %>% pull(pet_type) %>% as.character(),
    c("puppy", "dog", "cat", "puppy", "cat")
  )
  
  # also mutate for the negative condition
  # and add a new level automatically
  expect_equal(
    pets %>% mutate(
      pet_type = fct_mutate(
        pet_type,
        pet_type == 'dog' & age <= 2,
        'puppy',
        'not a puppy',
        .auto_expand = TRUE
      )) %>% pull(pet_type) %>% as.character(),
    c("puppy", "not a puppy", "not a puppy", "puppy", "not a puppy")
  )
  
  # for fun, let's try that another way
  expect_equal(
    pets %>% mutate(
      pet_type = fct_mutate(
        fct_recode(pet_type, 'not a puppy' = 'dog', 'not a puppy' = 'cat'),
        pet_type == 'dog' & age <= 2,
        'puppy',
        'not a puppy'
      )) %>% pull(pet_type) %>% as.character(),
    c("puppy", "not a puppy", "not a puppy", "puppy", "not a puppy")
  )
})
