library(testthat)
library(sf)
library(geos)

testthat::test_that("cut_transect creates perpendicular line of given width", {
  
  line <- geos::as_geos_geometry(sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7), ncol = 2, byrow = TRUE)))
  
  # Test for a line with width 0.1
  transect <- hydrofabric3D::cut_transect(line, width = 0.1)

  # plot(transect, col = "green", add = F)
  # plot(line, col = "red", add = T)

  testthat::expect_s3_class(transect, "geos_geometry")
  testthat::expect_equal(length(transect), 1) 
  testthat::expect_equal(sf::st_crs(transect), sf::st_crs(line))
  testthat::expect_equal(sf::st_crs(transect)$epsg, as.character(NA))
  
})
