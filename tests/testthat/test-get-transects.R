library(testthat)
library(sf)
library(geos)

testthat::test_that("get_transects creates multiple cross sections", {
  line <- sf::st_geometry(sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10, 11, 12, 13, 14, 15, 16, 17), ncol = 2, byrow = TRUE)))
  line <- sf::st_as_sf(line)
  line$hy_id = "my_hy_id"
  line <- dplyr::rename(line, "geometry" = "x")
  
  # Test for 2 evenly spaced transects with a bankfull width of 3
  transects <- hydrofabric3D::get_transects(line, bf_width = 3, n = 2)
  # plot(line$geometry)
  # plot(transects$geometry, add = T)
  
  testthat::expect_s3_class(transects, "sf")
  testthat::expect_equal(nrow(transects), 2)
  testthat::expect_equal(as.integer( transects$cs_measure[1]), 24)
  testthat::expect_equal(as.integer( transects$cs_measure[2]), 87)
  
  # Test for a single transect at the midpoint
  transects_single <- hydrofabric3D::get_transects(line, bf_width = 3, n = 1)
  
  # plot(line$geometry)
  # plot(transects_single$geometry, add = T)
  
  testthat::expect_s3_class(transects_single, "sf")
  testthat::expect_equal(nrow(transects_single), 1)
  testthat::expect_equal(as.integer(transects_single$cs_measure), 49)
  testthat::expect_equal(ceiling(transects_single$cs_measure[1]), 50)
  
  
})
