library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)

# source("testing_utils.R")
source("tests/testthat/testing_utils.R")

devtools::load_all()

testthat::test_that("validate_arg_types passes with valid arguments", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  
  testthat::expect_true(validate_arg_types(x = 42, y = "test", type_map = type_map))  # numeric and character
  testthat::expect_true(validate_arg_types(x = NULL, y = "test", type_map = type_map))  # NULL and character
})

testthat::test_that("validate_arg_types fails with invalid argument types", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  
  testthat::expect_error(
    validate_arg_types(x = "string_instead_of_numeric", y = "test", type_map = type_map)
  )
  
  testthat::expect_error(
    validate_arg_types(x = 42, y = 100, type_map = type_map)
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
  error_msg <- paste0("No arguments provided but 'type_map' has ", length(type_map), " provided argument types")
  
  testthat::expect_error(
    validate_arg_types(type_map = type_map), 
    error_msg
  )
})

testthat::test_that("validate_arg_types handles missing type_map", {
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character"
  )
  
  error_msg <- 'argument "type_map" is missing, with no default'
  
  testthat::expect_error(
    validate_arg_types(x = 42, y = "test"), 
    error_msg
  )
  
})


testthat::test_that("validate_arg_types passes with valid arguments 2", {
  
  type_map <- list(
    x = c("numeric", "NULL"),
    y = "character",
    z = "NULL"
  )
  
  testthat::expect_true(  validate_arg_types(
    x  = 42, 
    y  = "test", 
    z  = NULL,
    type_map = list(
                  x = c("numeric", "NULL"),
                  y = "character",
                  z = "NULL"
                )
    ))  
})

testthat::test_that("validate_arg_types passes with all valid logical arguments values", {
  

  
  valid_logical_vals <- c(TRUE, FALSE, NA)
  
  for (v in valid_logical_vals) {
    # message(v)
    testthat::expect_true(  
      validate_arg_types(
                    x        = v,
                    type_map = list(x = "logical")
                    )
      )
  }
  
})


testthat::test_that("validate_arg_types() fails when given NULL in type_map (non stringified 'NULL')", {
  
  type_map <- list(
    x = NULL 
  )
  
  
  testthat::expect_error(  
    validate_arg_types(
      x        = NULL,
      type_map = list(
                    x = NULL 
                  )
      )
    )
  
})

testthat::test_that("validate_arg_types() passes when given stringified 'NULL' in type_map", {
  
  testthat::expect_true(  
    validate_arg_types(
      x        = NULL,
      type_map = list(
        x = "NULL" 
      )
    )
  )
  
})

testthat::test_that("validate_arg_types() passes when given 'Inf' and '-Inf' as arguments w/ 'numeric' as type", {
  
  testthat::expect_true(  
    validate_arg_types(
      x        = Inf,
      type_map = list(
        x = "numeric" 
      )
    )
  )
  
  testthat::expect_true(  
    validate_arg_types(
      x        = -Inf,
      type_map = list(
        x = "numeric" 
      )
    )
  )
  
})

testthat::test_that("validate_arg_types() passes given a 'dataframe' w/ dataframe type", {
  
  df <- data.frame()
  
  testthat::expect_true(
    validate_arg_types(
      x        = df,
      type_map = list(
        x = "data.frame" 
      )
    )
  )
  
})

testthat::test_that("validate_arg_types() fails given a 'dataframe' w/ tibble types", {
  df <- data.frame()
  # tbl <- dplyr::tibble()
  # df_sf <- sf::st_as_sf(data.frame(x = 40, y = 50), coords = c("x", "y"))
  # class(df)
  
  testthat::expect_error(
    validate_arg_types(
      x        = df,
      type_map = list(
        x = "tbl_df" 
      )
    )
  )
  
  testthat::expect_error(
    validate_arg_types(
      x        = df,
      type_map = list(
        x = "tbl" 
      )
    )
  )
  
})

testthat::test_that("validate_arg_types() passes given a 'tibble' w/ dataframe and tibble types", {
  
  tbl <- dplyr::tibble()
  
  testthat::expect_true(  
    validate_arg_types(
      x        = tbl,
      type_map = list(
        x = "data.frame" 
      )
    )
  )
  
  testthat::expect_true(  
    validate_arg_types(
      x        = tbl,
      type_map = list(
        x = "tbl_df" 
      )
    )
  )
  
  testthat::expect_true(  
    validate_arg_types(
      x        = tbl,
      type_map = list(
        x = "tbl" 
      )
    )
  )
  
})

testthat::test_that("validate_arg_types() passes given a 'sf dataframe' and ", {
  df_sf <- sf::st_as_sf(
    data.frame(x = 40, y = 50), 
    coords = c("x", "y")
    )
  
  # class(df_sf)
  testthat::expect_true(  
    validate_arg_types(
      x        = df_sf,
      type_map = list(
        x = "sf" 
      )
    )
  )
  
  testthat::expect_true(  
    validate_arg_types(
      x        = df_sf,
      type_map = list(
        x = "data.frame" 
      )
    )
  )
  
})


testthat::test_that("validate_arg_types() passes given a 'sf tibble' w/ tibble, data.frame, and sf types ", {
  
  tbl_sf <- sf::st_as_sf(
    dplyr::tibble(x = 40, y = 50), 
    coords = c("x", "y")
  )
  testthat::expect_true(  
    validate_arg_types(
      x        = tbl_sf,
      type_map = list(
        x = "sf" 
      )
    )
  )
  
  testthat::expect_true(  
    validate_arg_types(
      x        = tbl_sf,
      type_map = list(
        x = "tbl" 
      )
    )
  )
  testthat::expect_true(  
    validate_arg_types(
      x        = tbl_sf,
      type_map = list(
        x = "tbl_df" 
      )
    )
  )
  
  testthat::expect_true(  
    validate_arg_types(
      x        = tbl_sf,
      type_map = list(
        x = "data.frame" 
      )
    )
  )
  
})

















