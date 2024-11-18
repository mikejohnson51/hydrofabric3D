
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()
# 
# -------------------------------------------------------------------
# ---- hydrofabric3D::is_valid_transect_line2() ----
# -------------------------------------------------------------------

testthat::test_that("is_valid_transect_line2() - transect line 1 only intersects flowline 1 time and does NOT interesect transects 3 and 4", {
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # plot(flowlines$x)
  # plot(trans$x[1], add= T)
  # # plot(trans$x[2], add= T)
  # plot(trans$x[3], add= T)
  # plot(trans$x[4], add= T)
  # # plot(trans$x[5], add= T)
  
  # plot(flowlines$x)
  # plot(trans$x[1], col = 'green', lwd = 8, opacity = 0.5, add = T)
  # plot(trans$x[c(1, 3, 4)], lwd = 3, col = "red", add = T)
  # geos::as_geos_geometry(trans$x[1])
  # geos::as_geos_geometry(trans$x[c(3, 4)])
  # 
  # geos::geos_equals(
  #   geos::as_geos_geometry(trans$x[1]),
  #   geos::as_geos_geometry(trans$x[c(1, 3, 4)])
  # )
  # 
  # geos::geos_equals_exact(  
  #   geos::as_geos_geometry(trans$x[1]),
  #   geos::as_geos_geometry(trans$x[c(1, 3, 4)])
  #                           )
  # 
  # transect line 1 only intersects flowline 1 time and does NOT interesect transects 3 and 4
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x[1]),
    geos::as_geos_geometry(trans$x[c(3, 4)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_true(
    is_valid
  )
  
})

# TODO: move each test in here to its own test
testthat::test_that("is_valid_transect_line2() - transect line 2 now intersects with transect line 1, NOT VALID", {
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # plot(flowlines$x)
  # plot(trans$x[1], add= T)
  # plot(trans$x[2], add= T)
  # plot(trans$x[3], add= T)
  # plot(trans$x[4], add= T)
  # plot(trans$x[5], add= T)
  
  plot(flowlines$x)
  plot(trans$x[1], col = 'green', lwd = 8, opacity = 0.5, add = T)
  plot(trans$x[c(2, 3, 4)], lwd = 3, col = "red", add = T)
  
  # TODO: put this in its own test
  # transect line #2 now intersects with transect line 1, NOT VALID
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x[1]),
    geos::as_geos_geometry(trans$x[c(2, 3, 4)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
})

# TODO: move each test in here to its own test
testthat::test_that("is_valid_transect_line2() - transect line #5 does NOT intersect other transects but it DOES intersect flowline 2 times (NOT VALID)", {
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # plot(flowlines$x)
  # plot(trans$x[1], add= T)
  # plot(trans$x[2], add= T)
  # plot(trans$x[3], add= T)
  # plot(trans$x[4], add= T)
  # plot(trans$x[5], add= T)
  
  plot(flowlines$x)
  plot(trans$x[5], col = 'green', lwd = 8, opacity = 0.5, add = T)
  plot(trans$x[c(5)], lwd = 3, col = "red", add = T)
  
  # TODO: put this in its own test
  # transect line #5 does NOT intersect other transects but it DOES intersect flowline 2 times (NOT VALID) 
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x[5]),
    geos::as_geos_geometry(trans$x[c(5)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
})

# TODO: move each test in here to its own test
testthat::test_that("is_valid_transect_line2() - transect line 5 DOES intersects transects 2 and 3 AND it DOES intersect flowline 2 times (NOT VALID) ", {
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # # plot(flowlines$x)
  # # # plot(trans$x[1], add= T)
  # # plot(trans$x[2], add= T)
  # # plot(trans$x[3], add= T)
  # # # plot(trans$x[4], add= T)
  # # plot(trans$x[5], col = "red", add= T)
  # 
  # plot(flowlines$x)
  # plot(trans$x[5], col = 'green', lwd = 8, opacity = 0.5, add = T)
  # plot(trans$x[c(2, 3)], lwd = 3, col = "red", add = T)
  
  # TODO: put this in its own test
  # transect line #5 DOES intersects transects 2 and 3 AND it DOES intersect flowline 2 times (NOT VALID) 
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x[5]),
    geos::as_geos_geometry(trans$x[c(2, 3)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
})

testthat::test_that("is_valid_transect_line2() - check all valid pairings of 2 transect lines that only intersect flowline 1 time", { 
  # transect_lines, 
  # polygons, 
  # flowlines, 
  # crosswalk_id = 'hy_id',  
  # grouping_id = 'mainstem',
  # max_extension_distance = 3000
  
  # flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
  #   dplyr::slice(10)
  # 
  # # flowlines$geom %>% plot()
  # 
  # CROSSWALK_ID       <- "id"
  # NUM_OF_TRANSECTS  <- 5
  # CS_WIDTH <- 300
  # flowlines <-
  #   flowlines %>% 
  #   # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
  #   dplyr::select(
  #     dplyr::any_of(c(CROSSWALK_ID)), 
  #     # tot_drainage_areasqkm,
  #     # bf_width,
  #     geom
  #   ) 
  # 
  # # generate transects
  # transects <- cut_cross_sections(
  #   net = flowlines,
  #   crosswalk_id  = CROSSWALK_ID,  
  #   num = NUM_OF_TRANSECTS,
  #   cs_widths = CS_WIDTH
  # ) %>% 
  #   dplyr::select(
  #     dplyr::any_of(c(CROSSWALK_ID)), cs_id
  #   )
  # 
  # plot(flowlines$geom) 
  # plot(transects$geometry, col = 'red', add = T)
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # plot(flowlines$x)
  # plot(trans$x[1], add= T)
  # plot(trans$x[2], add= T)
  # plot(trans$x[3], add= T)
  # plot(trans$x[4], add= T)
  # plot(trans$x[5], add= T)
  
  valid_transect_pairs <- list(
    c(1, 3),
    c(1, 4),
    c(2, 3),
    c(2, 4),
    c(3, 4)
  )
  
  # for through each of the valid pairs and check that they are valid
  for (valid_pair in valid_transect_pairs) {
    # message(valid_pair)
    
    t1 <- valid_pair[1]
    t2 <- valid_pair[2]
    
    # plot(flowlines$x)
    # plot(trans$x[t1], col = 'green', lwd = 8, opacity = 0.5, add = T)
    # plot(trans$x[t2], lwd = 3, col = "red", add = T)
    
    # transect line 1 only intersects flowline 1 time and does NOT interesect transects 3 and 4
    is_valid <- hydrofabric3D:::is_valid_transect_line2(
      geos::as_geos_geometry(trans$x[t1]),
      geos::as_geos_geometry(trans$x[t2]),
      geos::as_geos_geometry(flowlines)
    )
    
    testthat::expect_true(
      is_valid
    )
    
    
  }
  
})

testthat::test_that("is_valid_transect_line2() - no transect_to_check geometries given and no geometries given for transects to check against, yields FALSE (i.e. NOT VALID transect)", { 
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "AAAAAAAAA")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # plot(flowlines$x)
  # plot(trans$x, add= T)
  
  # TODO: put this in its own test
  # transect line #5 DOES intersects transects 2 and 3 AND it DOES intersect flowline 2 times (NOT VALID) 
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x),
    geos::as_geos_geometry(trans$x),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
})

testthat::test_that("is_valid_transect_line2() - no transect_to_check geometries given but given a set of VALID transects to check against, yields FALSE (i.e. NOT VALID transect)", { 
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline")
  
  # plot(flowlines$x)
  # plot(trans$x, add= T)
  # plot(trans$x[c(1, 3, 4)])
  
  # no input transect checked AGAINST set of valid transects
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x[0]),
    geos::as_geos_geometry(trans$x[c(1, 3, 4)]),
    geos::as_geos_geometry(flowlines)
  )
  
  testthat::expect_false(
    is_valid 
  )
  
})

testthat::test_that("is_valid_transect_line2() - 3 linestrings w/ 0 intersections (yields FALSE, i.e. is NOT valid)", { 
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type == "flowline2")
  
  # plot(dplyr::filter(test_geoms, grepl("flowline", line_type))$x )
  # plot(dplyr::filter(test_geoms, grepl("flowline2", line_type))$x, col = "green", lwd = 3, add = T)
  # plot(trans$x[1], add = T)
  # plot(trans$x[c(3)], add = T)
  
  # no input transect checked AGAINST set of valid transects
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x[1]),
    geos::as_geos_geometry(trans$x[c(3)]),
    geos::as_geos_geometry(flowlines)
  )
  
  
  testthat::expect_false(
    is_valid 
  )
  
})

testthat::test_that("is_valid_transect_line2() - 2 transects that dont touch eachother and go through a flowline only 1 time, there is also 1 more NON CONNECTED flowline (yields TRUE, i.e. IS VALID))", { 
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans     <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type %in% c("flowline", "flowline2"))
  
  # plot(flowlines$x)
  # plot(trans$x[1], col = "red", add = T)
  # plot(trans$x[c(3)], add = T)
  
  # no input transect checked AGAINST set of valid transects
  is_valid <- hydrofabric3D:::is_valid_transect_line2(
    geos::as_geos_geometry(trans$x[1]),
    geos::as_geos_geometry(trans$x[c(3)]),
    geos::as_geos_geometry(flowlines)
  )
  
  
  testthat::expect_true(
    is_valid 
  )
  
})

testthat::test_that("is_valid_transect_line2() - should return FALSE when input is a POINT", {
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans <- 
    test_geoms %>% 
    dplyr::filter(line_type == "transect") %>% 
    dplyr::slice(c(1, 3, 4))
  flowlines <- test_geoms %>% dplyr::filter(line_type %in% c("flowline", "flowline2"))
  
  # Create a POINT geometry
  point_geom <- sf::st_sfc(sf::st_point(c(1, 1)), crs = 5070)
  
  # Test should return FALSE when POINT is passed
  testthat::expect_false(
    hydrofabric3D:::is_valid_transect_line2(
      geos::as_geos_geometry(point_geom), 
      geos::as_geos_geometry(trans$x), 
      geos::as_geos_geometry(flowlines$x)
    )
  )
  
})

testthat::test_that("is_valid_transect_line2() - should return FALSE when input is a MULTILINESTRING", {
  # test_geoms <- make_flowlines_and_transects_test_data()
  # 
  # trans <- 
  #   test_geoms %>% 
  #   dplyr::filter(line_type == "transect") %>% 
  #   dplyr::slice(c(1, 3, 4))
  # flowlines <- test_geoms %>% dplyr::filter(line_type %in% c("flowline", "flowline2"))
  # 
  # # Create a MULTILINESTRING geometry
  # multi_linestring <- sf::st_sfc(sf::st_multilinestring(list(
  #   matrix(c(0, 0, 1, 1), ncol = 2), 
  #   matrix(c(2, 2, 3, 3), ncol = 2)
  # )), crs = 5070)
  # 
  # 
  # plot(flowlines$x)
  # plot(multi_linestring, add = T)
  # plot(trans, add= T)
  # 
  # # Test should return FALSE when MULTILINESTRING is passed
  # testthat::expect_false(
  #   hydrofabric3D:::is_valid_transect_line2(
  #     geos::as_geos_geometry(multi_linestring), 
  #     geos::as_geos_geometry(trans$x), 
  #     geos::as_geos_geometry(flowlines$x)
  #   )
  # )
})

testthat::test_that("is_valid_transect_line2() - should return FALSE when input is a POLYGON", {
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans <- 
    test_geoms %>% 
    dplyr::filter(line_type == "transect") %>% 
    dplyr::slice(c(1, 3, 4))
  
  flowlines <- test_geoms %>% dplyr::filter(line_type %in% c("flowline", "flowline2"))
  
  polygon_geom <- sf::st_sfc(
    sf::st_polygon(
      list(
        matrix(c(
          1, 4,  #
          1, 5,  
          2, 5, 
          2, 4, 
          1, 4  
        ), ncol = 2, byrow = TRUE)
      )
    ),
    crs = 5070
  )
  
  # plot(flowlines$x)
  # plot(polygon_geom, add = T)
  # plot(trans, add= T)
  
  # Test should return FALSE when POLYGON is passed
  testthat::expect_false(
    hydrofabric3D:::is_valid_transect_line2(
      geos::as_geos_geometry(polygon_geom), 
      geos::as_geos_geometry(trans$x), 
      geos::as_geos_geometry(flowlines$x)
    )
  )
})

testthat::test_that("is_valid_transect_line2() - should handle NULL geometries", {
  
  test_geoms <- make_flowlines_and_transects_test_data()
  
  trans <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type %in% c("flowline", "flowline2"))
  
  testthat::expect_error(
    hydrofabric3D:::is_valid_transect_line2(
      NULL, 
      geos::as_geos_geometry(trans$x), 
      geos::as_geos_geometry(flowlines$x)
    ),
    "invalid geometry: geometries cannot be NULL"
  )
})


testthat::test_that("is_valid_transect_line2() - should handle empty geometries by returning FALSE", {
  
  test_geoms <- make_flowlines_and_transects_test_data_all_valid()
  
  # plot(test_geoms$x)
  
  trans <- test_geoms %>% dplyr::filter(line_type == "transect")
  flowlines <- test_geoms %>% dplyr::filter(line_type %in% c("flowline", "flowline2"))
  
  # Create an empty LINESTRING geometry
  empty_geom <- sf::st_sfc(sf::st_linestring(matrix(numeric(0), ncol = 2)), crs = 5070)
  
  # Test should return FALSE when an empty LINESTRING is passed
  testthat::expect_false(
    hydrofabric3D:::is_valid_transect_line2(
      geos::as_geos_geometry(empty_geom), 
      geos::as_geos_geometry(trans$x), 
      geos::as_geos_geometry(flowlines$x)
    )
  )
  
})


testthat::test_that("is_valid_transect_line2() - check real flowlines ", { 
  
  # flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
  #   dplyr::slice(10)
  # 
  # # flowlines$geom %>% plot()
  # 
  # CROSSWALK_ID       <- "id"
  # NUM_OF_TRANSECTS  <- 5
  # CS_WIDTH <- 300
  # flowlines <-
  #   flowlines %>% 
  #   # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
  #   dplyr::select(
  #     dplyr::any_of(c(CROSSWALK_ID)), 
  #     # tot_drainage_areasqkm,
  #     # bf_width,
  #     geom
  #   ) 
  # 
  # # generate transects
  # transects <- cut_cross_sections(
  #   net = flowlines,
  #   crosswalk_id  = CROSSWALK_ID,  
  #   num = NUM_OF_TRANSECTS,
  #   cs_widths = CS_WIDTH
  # ) %>% 
  #   dplyr::select(
  #     dplyr::any_of(c(CROSSWALK_ID)), cs_id
  #   )
  # 
  # plot(flowlines$geom) 
  # plot(transects$geometry, col = 'red', add = T)
  
})

