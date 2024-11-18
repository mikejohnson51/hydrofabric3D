library(testthat)
library(sf)
library(geos)

testthat::test_that("cut_transect creates perpendicular line of given width from geos geometry LINESTRING", {
  
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

testthat::test_that("error cut_transect() on SF LINESTRING because not geos_geometry", {
  
  line <- sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7), ncol = 2, byrow = TRUE))
  testthat::expect_error(hydrofabric3D::cut_transect(line, width = 0.1))
})

testthat::test_that("cut_transect error on empty geos_geom", {
  
  empty_line <- geos::geos_empty()
  
  testthat::expect_error(hydrofabric3D::cut_transect(empty_line, width = 0.1))
  
})

# -------------------------------------------------------------------
# ---- test set of flowlines ----
# -------------------------------------------------------------------

testthat::test_that("run cut_transect() on 10 LINESTRING flowlines", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  flowlines_geos <- 
    flowlines %>% 
    sf::st_cast("LINESTRING") %>% 
    geos::as_geos_geometry() 
  
  for (i in seq_along(flowlines_geos)) {
    transect <- hydrofabric3D::cut_transect(flowlines_geos[i], width = 100)
    
    testthat::expect_s3_class(transect, "geos_geometry")
    testthat::expect_equal(length(transect), 1) 
    testthat::expect_equal(sf::st_crs(transect), sf::st_crs(line))
    testthat::expect_equal(sf::st_crs(transect)$epsg, as.character(NA))
    
    # plot(flowlines_geos[i], col = "red", add = F)
    # plot(transect, col = "green", add = T)
    # plot(transect, col = "green", add = F)
    # plot(flowlines_geos[i], col = "red", add = T)
  }
  
})

testthat::test_that("error cut_transect() because of MULTILINESTRING flowlines instead of LINESTRING", {
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  flowlines_geos <- 
    flowlines %>% 
    geos::as_geos_geometry() 
  
   testthat::expect_error(hydrofabric3D::cut_transect(flowlines_geos[1], width = 0.1))

})
