test_that("EntityPath creation only allows lists of entities", {
  expect_no_error(
    study <- make_study(name = 'my study')
  )
  
  entities <- study %>% get_entities()
  
  # Valid case: list of entities
  expect_no_error(
    entity_path <- EntityPath(entities)
  )
  
  # Invalid case: list of non-entity objects
  pets <- list("dog", "cat", "rabbit")
  expect_error(
    entity_path <- EntityPath(pets),
    "All elements of the input list must be of class 'Entity'."
  )
  
  # Invalid case: not even a list
  expect_error(
    entity_path <- EntityPath("not a list"),
    "The input to EntityPath must be a list."
  )
})
