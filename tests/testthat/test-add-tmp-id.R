library(testthat)
library(dplyr)
# library(hydrofabric3D)
source("testing_utils.R")

# source("tests/testthat/testing_utils.R")
# devtools::load_all()
# -------------------------------------------------------------------
# ---- hydrofabric::add_tmp_id() ----
# -------------------------------------------------------------------

# Create a sample dataframe for testing
df <- data.frame(
  hy_id = c("a", "b", "c"),
  cs_id = c("x", "y", "z")
)

testthat::test_that("Adding tmp_id column with default columns", {
  result <- add_tmp_id(df)
  testthat::expect_equal(names(result), c("hy_id", "cs_id", "tmp_id"))
  testthat::expect_equal(result$tmp_id, c("a_x", "b_y", "c_z"))
  
})

testthat::test_that("Adding tmp_id column with specified columns", {
  result <- add_tmp_id(df, "hy_id", "cs_id")
  testthat::expect_equal(names(result), c("hy_id", "cs_id", "tmp_id"))
  testthat::expect_equal(result$tmp_id, c("a_x", "b_y", "c_z"))
  
})

testthat::test_that("Adding tmp_id column with specified columns in reverse order", {
  result <- add_tmp_id(df, "cs_id", "hy_id")
  testthat::expect_equal(names(result), c("hy_id", "cs_id", "tmp_id"))
  testthat::expect_equal(result$tmp_id, c("x_a", "y_b", "z_c"))
})

testthat::test_that("Adding tmp_id column with specified columns with no quotes (tidy select)", {
  result <- add_tmp_id(df, "hy_id", "cs_id")
  testthat::expect_equal(names(result), c("hy_id", "cs_id", "tmp_id"))
  testthat::expect_equal(result$tmp_id, c("a_x", "b_y", "c_z"))
  
})

testthat::test_that("Adding tmp_id column with non-character columns", {
  df_numeric <- data.frame(
    hy_id = c(1, 2, 3),
    cs_id = c(10, 20, 30)
  )
  result <- add_tmp_id(df_numeric)
  testthat::expect_equal(names(result), c("hy_id", "cs_id", "tmp_id"))
  testthat::expect_equal(result$tmp_id, c("1_10", "2_20", "3_30"))
})
