library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)
# 
source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::add_cs_area() ----
# -------------------------------------------------------------------

testthat::test_that("cs_area is calculated correctly for a single U shaped set of cross section points", {
  
  cs <-
    data.frame(
      hy_id      = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
      cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      Z          = c(9, 7, 5, 5, 2, 2, 5, 5, 7, 9)
    )
  
  csa <- hydrofabric3D::add_cs_area(cs)
  # hydrofabric3D::plot_cs_pts(cs, "hy_id", size = 4)
  # hydrofabric3D::add_cs_area(cs)
  
  testthat::expect_true(
    dplyr::between(csa$cs_area[1], 3.1, 3.3)
  )
  testthat::expect_true(
    dplyr::between(csa$cs_area[10], 3.1, 3.3)
  )
  
  testthat::expect_equal(
    csa$cs_area[5], 0
  )
  testthat::expect_equal(
    csa$cs_area[6], 0
  )
  
})

testthat::test_that("cs_area is calculated correctly for a single rectangular shaped set of cross section points with 5 total points", {
  
  cs <-
    data.frame(
      hy_id      = c("A", "A", "A", "A", "A"),
      cs_id      = c(1, 1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4, 5),
      cs_lengthm        = c(100, 100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3, 0.4),
      Z          = c(4, 1, 1, 1, 4)
    )
  
  csa <- hydrofabric3D::add_cs_area(cs)
  
  testthat::expect_true(
    dplyr::between(csa$cs_area[1], 0.5, 0.7)
  )
  testthat::expect_true(
    dplyr::between(csa$cs_area[5], 0.5, 0.7)
  )
  
  testthat::expect_equal(
    csa$cs_area[2], 0
  )
  testthat::expect_equal(
    csa$cs_area[3], 0
  )
  testthat::expect_equal(
    csa$cs_area[4], 0
  )
})

testthat::test_that("cs_area is calculated correctly for a single rectangle shaped set of cross section points with 4 total points", {
  
  cs <-
    data.frame(
      hy_id      = c("A", "A", "A", "A"),
      cs_id      = c(1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4),
      cs_lengthm        = c(100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3),
      Z          = c(4, 1, 1, 4)
    )
  
  csa <- hydrofabric3D::add_cs_area(cs)
  
  testthat::expect_true(
    dplyr::between(csa$cs_area[1], 0.2, 0.4)
  )
  testthat::expect_true(
    dplyr::between(csa$cs_area[4], 0.2, 0.4)
  )
  
  testthat::expect_equal(
    csa$cs_area[2], 0
  )
  testthat::expect_equal(
    csa$cs_area[3], 0
  )

})

testthat::test_that("cs_area is calculated correctly for a single V shaped set of cross section points that then slopes down and to the right on the right side", {
  
  cs <-
    data.frame(
      hy_id      = c("A", "A", "A", "A", "A", "A", "A", "A", "A", "A"),
      cs_id      = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1),
      pt_id             = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
      cs_lengthm        = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100),
      relative_distance = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9),
      Z = c(10, 9, 8, 5, 7, 6, 8, 9, 3, 1)
    )
  
    csa <- hydrofabric3D::add_cs_area(cs)
    
    testthat::expect_true(
      dplyr::between(csa$cs_area[8], 2.3, 2.5)
    )
    
    testthat::expect_equal(
      csa$cs_area[9], 0
    )
    testthat::expect_equal(
      csa$cs_area[10], 0
    )
    
})

