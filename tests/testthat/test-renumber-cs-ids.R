library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::renumber_cs_ids() ----
# -------------------------------------------------------------------

test_that("renumber_cs_ids(), renumbering when works correctly even when no cs_ids need to be resequenced", {

  df <- data.frame(
    crosswalk_id = c(rep("A", 3), rep("B", 3)),
    cs_id = c(1, 2, 3, 1, 2, 3),
    cs_measure = c(0, 50, 100, 0, 50, 100)
  )

  result <- renumber_cs_ids(df, crosswalk_id = "crosswalk_id")

 testthat::expect_equal(result$cs_id, 
               df$cs_id
                # c(1, 2, 3, 1, 2, 3)
            )
})

test_that("renumber_cs_ids(), renumber a single misnumbered cs_id for a single crosswalk_id", {

  df <- data.frame(
    crosswalk_id = rep("A", 5),
    cs_id = c(1, 2, 4, 5, 6),
    cs_measure = c(0, 25, 50, 75, 100)
  )

  result <- renumber_cs_ids(df, crosswalk_id = "crosswalk_id")

  testthat::expect_equal(result$cs_id, 1:5)

})

test_that("renumber_cs_ids(), error if crosswalk_id column is missing", {
    
  df <- data.frame(
    other_id = rep("A", 3),
    cs_id = c(1, 2, 3),
    cs_measure = c(0, 50, 100)
  )

  testthat::expect_error(renumber_cs_ids(df, crosswalk_id = "crosswalk_id"), 
               "df' is missing one or more of the required columns:
 > crosswalk_id")

})

test_that("renumber_cs_ids(), error if crosswalk_id contains NA values", {
  df <- data.frame(
    crosswalk_id = c("A", "A", NA),
    cs_id = c(1, 2, 3),
    cs_measure = c(0, 50, 100)
  )
  testthat::expect_error(renumber_cs_ids(df, crosswalk_id = "crosswalk_id"), 
               "contains NA values")
})

test_that("renumber_cs_ids(), error if cs_id contains NA values", {
  df <- data.frame(
    crosswalk_id = c("A", "A", "A"),
    cs_id = c(1, NA, 3),
    cs_measure = c(0, 50, 100)
  )
  
  testthat::expect_error(renumber_cs_ids(df, crosswalk_id = "crosswalk_id"), 
               "contains NA values")

})

test_that("renumber_cs_ids(), non-sequential cs_id values are renumbered correctly for 2 crosswalk_ids", {
  df <- data.frame(
    crosswalk_id = c(rep("A", 3), rep("B", 3)),
    cs_id = c(1, 3, 5, 2, 4, 6),
    cs_measure = c(0, 50, 100, 0, 50, 100)
  )

  result <- renumber_cs_ids(df, crosswalk_id = "crosswalk_id")
  
  testthat::expect_equal(result$cs_id, 
                        c(1, 2, 3, 1, 2, 3)
                        )
})

test_that("renumber_cs_ids(), non-sequential cs_id values are renumbered correctly for 2 crosswalk_ids and crosswalk_ids are out of order", {
  df <- data.frame(
    crosswalk_id = c("A", "B", "A", "B", "A", "B"),
    cs_id = c(1, 6, 5, 2, 4, 3),
    cs_measure = c(0, 100, 100, 0, 50, 50)
  )

  result <- renumber_cs_ids(df, crosswalk_id = "crosswalk_id")
  
  testthat::expect_equal(result$cs_id, 
                        c(1, 3, 3, 1, 2, 2)
                        )
})

test_that("renumber_cs_ids(), renumber a set of cross section points that have a single misnumbered cs_id across the entire crosswalk_id", {

  df <- data.frame(
    crosswalk_id = rep("A", 12),
    cs_id = c(
            1, 1, 1, 
            2, 2, 2,
            4, 4, 4, 
            5, 5, 5
            ),
    pt_id = c(
            1, 2, 3,
            1, 2, 3,
            1, 2, 3,
            1, 2, 3
            ),
    cs_measure = c(
            0, 0, 0,
            25, 25, 25,
            50, 50, 50,
            75, 75, 75
            )
  )

  result <- renumber_cs_ids(df, crosswalk_id = "crosswalk_id")

  # CS ID 4 is missing, so it should be renumbered to 3
  testthat::expect_equal(result$cs_id, 
                                c(
                                1, 1, 1, 
                                2, 2, 2, 
                                3, 3, 3, 
                                4, 4, 4
                                )
                            )

})

test_that("renumber_cs_ids(), renumber 2 sets of cross section points that have a single misnumbered cs_id across the entire crosswalk_id", {

    df1 <- data.frame(
        crosswalk_id = rep("A", 12),
        cs_id = c(
                1, 1, 1, 
                2, 2, 2,
                4, 4, 4, 
                5, 5, 5
                ),
        pt_id = c(
                1, 2, 3,
                1, 2, 3,
                1, 2, 3,
                1, 2, 3
                ),
        cs_measure = c(
                0, 0, 0,
                25, 25, 25,
                50, 50, 50,
                75, 75, 75
                )
    )

    df2 <- data.frame(
        crosswalk_id = rep("B", 9),
        cs_id = c(
                1, 1, 1, 
                10, 10, 10,
                4, 4, 4
                ),
        pt_id = c(
                1, 2, 3,
                1, 2, 3,
                1, 2, 3
                ),
        cs_measure = c(
                0, 0, 0,
                25, 25, 25,
                75, 75, 75
                )
    )

    df <- rbind(df1, df2)

    result <- renumber_cs_ids(df, crosswalk_id = "crosswalk_id")

    # check cs_id "A" was renumbered correctly
    testthat::expect_equal(result$cs_id[result$crosswalk_id == "A"], 
                                    c(
                                    1, 1, 1, 
                                    2, 2, 2, 
                                    3, 3, 3, 
                                    4, 4, 4
                                    )
                                )

    # check cs_id "B" was renumbered correctly
    testthat::expect_equal(result$cs_id[result$crosswalk_id == "B"], 
                                    c(
                                    1, 1, 1, 
                                    2, 2, 2, 
                                    3, 3, 3
                                    )
                                )

})