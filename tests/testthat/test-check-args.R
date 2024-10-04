library(testthat)

testthat::test_that("validate_arg_types passes with valid arguments", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  
  testthat::testthat::expect_true(validate_arg_types(x = 42, y = "test", type_map = type_map))  # numeric and character
  testthat::testthat::expect_true(validate_arg_types(x = NULL, y = "test", type_map = type_map))  # NULL and character
})

testthat::test_that("validate_arg_types fails with invalid argument types", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  
  testthat::expect_error(
    validate_arg_types(x = "string_instead_of_numeric", y = "test", type_map = type_map), 
    "Argument 'x' must be of type 'numeric' but got 'character'"
  )
  
  testthat::expect_error(
    validate_arg_types(x = 42, y = 100, type_map = type_map), 
    "Argument 'y' must be of type 'character' but got 'numeric'"
  )
})

testthat::test_that("validate_arg_types fails with unexpected argument", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  
  testthat::expect_error(
    validate_arg_types(x = 42, z = "unexpected", type_map = type_map), 
    "Unexpected argument: z"
  )
})

testthat::test_that("validate_arg_types allows multiple valid types", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  # x can be numeric or NULL
  testthat::expect_true(validate_arg_types(x = NULL, y = "test", type_map = type_map))
  testthat::expect_true(validate_arg_types(x = 100, y = "valid_string", type_map = type_map))
})

testthat::test_that("validate_arg_types fails if no arguments are passed", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  testthat::expect_error(
    validate_arg_types(type_map = type_map), 
    "Unexpected argument"
  )
})

testthat::test_that("validate_arg_types handles missing type_map", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  testthat::expect_error(
    validate_arg_types(x = 42, y = "test"), 
    "argument \"type_map\" is missing, with no default"
  )
})
