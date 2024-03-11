library(testthat)
library(dplyr)
library(hydrofabric3D)

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
  result <- add_tmp_id(df, hy_id, cs_id)
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

# -------------------------------------------------------------------
# ---- hydrofabric::get_point_type_counts() ----
# -------------------------------------------------------------------

hy_id <- c("A", "A", "A", "A", 
           "B", "B", "B", "B", "B", 
           "C", "C", "C",
           "D", "D", "D", "D", "D",
           "E", "E", "E", "E", "E", "E"
           )
cs_id <- c(1, 1, 1, 1,
           1, 1, 1, 1, 1,
           1, 1, 1,
           1, 1, 1, 1, 1,
           1, 1, 1, 1, 1, 1
           )
point_type <- c("left_bank", "channel", "bottom", "right_bank",  # A
                "left_bank", "channel", "channel", "bottom", "channel", # B
                "left_bank", "bottom", "right_bank", # C
                "left_bank", "channel", "bottom", "bottom", "right_bank", # D
                "left_bank", "channel", "bottom", "bottom", "channel", "right_bank" # E
                )

classified_pts <- data.frame(hy_id, cs_id, point_type)

testthat::test_that("Adding point type counts with default arguments", {
  
  # result <- get_point_type_counts(classified_pts)
  result <- get_point_type_counts(classified_pts, add = FALSE)
  
  testthat::expect_equal(names(result), c("hy_id", "cs_id", "left_bank_count", "right_bank_count", "channel_count", "bottom_count"))
  testthat::expect_equal(unique(result$hy_id), c("A", "B", "C", "D", "E"))

  testthat::expect_equal(result[result$hy_id == "A", ]$left_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "A", ]$right_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "A", ]$channel_count, 1)
  testthat::expect_equal(result[result$hy_id == "A", ]$bottom_count, 1)
  
  testthat::expect_equal(result[result$hy_id == "B", ]$left_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "B", ]$right_bank_count, 0)
  testthat::expect_equal(result[result$hy_id == "B", ]$channel_count, 3)
  testthat::expect_equal(result[result$hy_id == "B", ]$bottom_count, 1)
  
  testthat::expect_equal(result[result$hy_id == "C", ]$left_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "C", ]$right_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "C", ]$channel_count, 0)
  testthat::expect_equal(result[result$hy_id == "C", ]$bottom_count, 1)
  
  testthat::expect_equal(result[result$hy_id == "D", ]$left_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "D", ]$right_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "D", ]$channel_count, 1)
  testthat::expect_equal(result[result$hy_id == "D", ]$bottom_count, 2)
  
  testthat::expect_equal(result[result$hy_id == "E", ]$left_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "E", ]$right_bank_count, 1)
  testthat::expect_equal(result[result$hy_id == "E", ]$channel_count, 2)
  testthat::expect_equal(result[result$hy_id == "E", ]$bottom_count, 2)
  
  # result[result$hy_id == "E", ]$left_bank_count 
  # # testthat::expect_equal(result[result$hy_id == "E", ]$right_bank_count, c(0, 0, 0, 0 )
  # # result[result$hy_id == "E", ]$channel_count
  # # result[result$hy_id == "E", ]$bottom_count
  # testthat::expect_equal(result$left_bank_count, c(1, 1, 1, 2, 1))
  # testthat::expect_equal(result$right_bank_count, c(1, 0, 1, 1, 1))
  # testthat::expect_equal(result$channel_count, c(1, 3, 0, 2, 2))
  # testthat::expect_equal(result$bottom_count, c(1, 1, 1, 2, 2))
})