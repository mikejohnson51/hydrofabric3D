library(testthat)
library(sf)
library(geos)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

testthat::test_that("get_transects correct number of edges get made for a line", {
 #  line <- sf::st_geometry(sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10, 11, 
 #                                  12, 13, 14, 15, 16, 17), ncol = 2, byrow = TRUE)))
 #                                    # convert to geos geometry
 #  line <- geos::as_geos_geometry(line)  
 #  # line <- sf::st_as_sf(line)
 #  # line$hy_id = "my_hy_id"
 #  # line <- dplyr::rename(line, "geometry" = "x")
 #  
 #  NUMBER_OF_TRANSECTS <- 4
 #  # # plot(line$geometry) 
 #  # # Test for 2 evenly spaced transects with a bankfull width of 3
 #  # transects <- hydrofabric3D::get_transects(line, 
 #  #                             bf_width = 3, 
 #  #                             n = NUMBER_OF_TRANSECTS)
 # 
 #  # plot(line$geometry)
 #  # plot(transects$geometry, lwd = 3, add = T)
 #  line <- sf::st_geometry(sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10, 11, 
 #                                  12, 13, 14, 15, 16, 17), ncol = 2, byrow = TRUE)))
 #                                    # convert to geos geometry
 #  line <- geos::as_geos_geometry(line)   
 # 
 #  # vertices of line
 #  vertices <- wk::wk_vertices(line)
 # 
 #  N <- length(vertices)
 #  n = 4
 # N
 #  # length(vertices)  
 #  # rep(
 #  #   seq_along(vertices)[c(-1, -9)],
 #  #   each = 2
 #  # )
 #  # vertices[c(1, 
 #  # rep(
 #  #   seq_along(vertices)[-c(1, N)], each = 2
 #  #    ), 
 #  #     N)]
 #  
 #  # create evenly spaced linestring geometries along line of interest
 #  edges <- geos::as_geos_geometry(
 #    wk::wk_linestring(
 #    vertices[c(1, 
 #              rep(seq_along(vertices)[-c(1, N)], each = 2), 
 #              N)],
 #      feature_id = rep(seq_len(N - 1), each = 2)
 #    )
 #  )
 #    
 #  # # keep all lines except first and last edges
 #  # edges <- edges[-c(1, length(edges))]
 # 
 #  # # get the cumulative length of edges along flowline
 #  edge_lengths <- cumsum(
 #    geos::geos_length(edges)
 #  )
 #  
 #  # total length of linestring
 #  total_length <- edge_lengths[length(edge_lengths)]
 #  
 #  # the below check should be TRUE 
 #  total_length == geos::geos_length(line)
 #  
 #  # keep all lines except first and last edges
 #  edges <- edges[-c(1, length(edges))]
 #  
 #  # # keep all edge lengths except first and last edge lengths
 #  edge_lengths <- edge_lengths[-c(1, length(edge_lengths))]
 #  
 #  # n = 8
 #  
 #  length(edges)
 #  min(length(edges), n)
 # 
 #  # create a sequence of edges along 'line'
 #  if (!is.null(n)) {
 #    if (n == 1) {
 #      # get a single edge at the midpoint
 #      edges <- edges[as.integer(ceiling(length(edges)/ 2))]
 #      
 #      # get the edge length for the single midpoint edge
 #      edge_lengths <- edge_lengths[as.integer(ceiling(length(edge_lengths)/ 2))]
 #      
 #    } else {
 #      # extract edges at intervals of 'n' 
 #      edges <- edges[as.integer(
 #        seq.int(1, length(edges), length.out = min(n, length(edges)))
 #      )
 #      ]
 #      # extract edge lengths at intervals of 'n' (same interval/indices of above edges indexing)
 #      edge_lengths <- edge_lengths[as.integer(
 #        seq.int(1, length(edge_lengths), length.out = min(n, length(edge_lengths)))
 #      )
 #      ]
 #    }
 #  }
 # 
 #  plot(line)
 #  # plot(transects$geometry, lwd = 3, add = T)
 #  plot(edges, col = "red", lwd = 3, add = T)
 # 
 #  bf_width = 3
 #   
 #  # make sure bf_width is the same length as the number of edges
 #  if (length(bf_width) != length(edges)) {
 #    bf_width <- rep(bf_width[1], length(edges))
 #  }
 #  
 #  transects <- geos::geos_empty()
 #  measures  <- vctrs::vec_c()
 #  
 #  for(i in 1:length(edges)){
 #    i = 1
 #    
 #    # message("TRANSECT: ", i)
 #    tran = cut_transect(edges[i], bf_width[i])
 #    
 # 
 #    edge = edges[i]
 #    width = bf_width[i]
 # 
 #    midpoint <- geos::geos_interpolate_normalized(edge, 0.5)
 # 
 #    ep       <- geos::geos_point_end(edge)
 # 
 #    plot(line)
 #    # plot(transects$geometry, lwd = 3, add = T)
 #    plot(edge, col = "red", lwd = 3, add = T) 
 #    plot(midpoint, col = "blue", pch = 19, add = T)
 #    plot(ep, col = "green", pch = 19, add = T)
 #    plot(normal_edge, col = "green", pch = 19, add = T)
 #   ?geos::geos_interpolate_normalized() 
 #  ?wk::wk_transform()
 #    ?wk::wk_affine_compose()
 #     ?wk::wk_affine_translate()
 #     ? wk::wk_transform()
 #    geos::geos_x(midpoint)
 #    geos::geos_y(midpoint)   
 #    wk::wk_affine_translate(dx = -geos::geos_x(midpoint), dy = -geos::geos_y(midpoint))
 #    wk::wk_affine_scale(1 / geos::geos_length(edge), 1 / geos::geos_length(edge))
 #    plot(edge)
 #    wk::wk_transform(
 #                      edge, 
 #                      wk::wk_affine_compose(
 #                        wk::wk_affine_translate(dx = -geos::geos_x(midpoint), dy = -geos::geos_y(midpoint)),
 #                        wk::wk_affine_scale(1 / geos::geos_length(edge), 1 / geos::geos_length(edge))
 #                        # wk::wk_affine_rotate(90)
 #                        )
 #                      )  %>% plot()
 # 
 #    normal_edge <- wk::wk_transform(
 #                      edge, 
 #                      wk::wk_affine_compose(
 #                        wk::wk_affine_translate(dx = -geos::geos_x(midpoint), dy = -geos::geos_y(midpoint)),
 #                        wk::wk_affine_scale(1 / geos::geos_length(edge), 1 / geos::geos_length(edge)),
 #                        wk::wk_affine_rotate(90)
 #                        )
 #                      )
 #    
 #    # return(
 #    # tran = 
 #      wk::wk_set_crs(
 #        wk::wk_transform(
 #          normal_edge,
 #          wk::wk_affine_compose(
 #            wk::wk_affine_scale(width, width),
 #            wk::wk_affine_translate(geos::geos_x(ep), geos::geos_y(ep))
 #            )
 #          ), 
 #        wk::wk_crs(edge)
 #      )
 #    # )
 # 
 #    # # # measure of edge
 #    meas <- edge_lengths[i]
 #    
 #    # # If a MULTIPOINT, then it crosses more the once
 #    if(geos::geos_type(geos::geos_intersection(tran, line)) == "point") {
 #      # message("intersect IS point ")
 #      # Ensure that there are no intersections with previously computed cross sections
 #      if (!any(geos::geos_intersects(tran, transects))) {
 #        # message("----> KEEPING TRANSECT: ", i)
 #        transects <- vctrs::vec_c(transects, tran)
 #        measures  <- vctrs::vec_c(measures, meas)
 #      }
 #    }
 #  }
 #  
 #  # index for only valid transects
 #  is_valid <- !geos::geos_is_empty(transects)
 #  
 #  # is_valid[-1]
 #  # extract only edge lengths of remaining transect lines only valid edge lengths
 #  measures <- measures[is_valid[-1]]
 #  # edge_lengths <- edge_lengths[is_valid[-1]]
 #  
 #  # # calculate cs_measure value
 #  edge_measure <- (measures/total_length) * 100
 #  # edge_lengths <- (edge_lengths/total_length) * 100
 #  
 #  # drop empty geos geometries
 #  transects <- transects[is_valid]
 # 
 #  plot(line)
 #  plot(transects, lwd = 3, add = T)
 #  plot(edges, col = "red", lwd = 3, add = T)
 #  
 #  plot(edges)
 #  testthat::expect_s3_class(transects, "sf")
 #  testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
 #  testthat::expect_equal(as.integer( transects$cs_measure[1]), 24)
 #  testthat::expect_equal(as.integer( transects$cs_measure[2]), 87)
 #  
 #  NUMBER_OF_TRANSECTS <- 1
 #  # Test for a single transect at the midpoint
 #  transects_single <- hydrofabric3D::get_transects(line, bf_width = 3, n = NUMBER_OF_TRANSECTS)
 #  
 #  # plot(line$geometry)
 #  # plot(transects_single$geometry, add = T)
 #  
 #  testthat::expect_s3_class(transects_single, "sf")
 #  testthat::expect_equal(nrow(transects_single), NUMBER_OF_TRANSECTS)
 #  testthat::expect_equal(as.integer(transects_single$cs_measure), 49)
 #  testthat::expect_equal(ceiling(transects_single$cs_measure[1]), 50)
 #  
  
})

testthat::test_that("get_transects creates multiple cross sections", {
  line <- sf::st_geometry(sf::st_linestring(matrix(c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9 ,10, 11, 12, 13, 14, 15, 16, 17), ncol = 2, byrow = TRUE)))
  line <- sf::st_as_sf(line)
  line$hy_id = "my_hy_id"
  line <- dplyr::rename(line, "geometry" = "x")
  
  NUMBER_OF_TRANSECTS <- 2
  plot(line$geometry) 
  # Test for 2 evenly spaced transects with a bankfull width of 3
  transects <- hydrofabric3D::get_transects(line, bf_width = 3, n = NUMBER_OF_TRANSECTS)
  # plot(line$geometry)
  # plot(transects$geometry, add = T)
  class(line)
  
  testthat::expect_s3_class(transects, "sf")
  testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
  testthat::expect_equal(as.integer( transects$cs_measure[1]), 24)
  testthat::expect_equal(as.integer( transects$cs_measure[2]), 87)
  
  NUMBER_OF_TRANSECTS <- 1
  # Test for a single transect at the midpoint
  transects_single <- hydrofabric3D::get_transects(line, bf_width = 3, n = NUMBER_OF_TRANSECTS)
  
  # plot(line$geometry)
  # plot(transects_single$geometry, add = T)
  
  testthat::expect_s3_class(transects_single, "sf")
  testthat::expect_equal(nrow(transects_single), NUMBER_OF_TRANSECTS)
  testthat::expect_equal(as.integer(transects_single$cs_measure), 49)
  testthat::expect_equal(ceiling(transects_single$cs_measure[1]), 50)
  
  
})

testthat::test_that("get 2 transects using get_transects() on each SF MULTILINESTRING flowline in test flowlines dataset", {
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 2
  
  for (i in seq_along(flowlines)) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
   
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
   
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("get 2 transects using get_transects() on each SF LINESTRING flowline in test flowlines dataset", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    sf::st_cast("LINESTRING")
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 2
  
  for (i in seq_along(1:nrow(flowlines))) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("get 10 transects using get_transects() on each SF MULTILINESTRING", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 10
  
  for (i in seq_along(1:nrow(flowlines))) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})
testthat::test_that("get 10 transects using get_transects() on each geos_geometry MULTILINESTRING", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  flowlines_geos <- flowlines %>% geos::as_geos_geometry()
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 10
  
  for (i in seq_along(flowlines_geos)) {
    # message(i)
    transects <- hydrofabric3D::get_transects(flowlines_geos[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_equal(nrow(transects), NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- geos::geos_length(flowlines_geos[i])
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("really high number of transect lines results in less transects being generated because of intersection removals (SF MULTILINESTRING)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  
  # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
  ACCEPTABLE_PCT_DIFF <- 1
  NUMBER_OF_TRANSECTS <- 1000
  
  for (i in seq_along(1:nrow(flowlines))) {
    message(i)
    transects <- hydrofabric3D::get_transects(flowlines$geom[i], bf_width = 50, n = NUMBER_OF_TRANSECTS)
    
    # plot(flowlines$geom[i])
    # plot(transects$geometry, add = T)
    
    testthat::expect_s3_class(transects, "sf")
    testthat::expect_false(nrow(transects) == NUMBER_OF_TRANSECTS)
    
    # full flowline length 
    total_flowline_length <- flowlines$lengthkm[i] * 1000
    
    diff_between_ds_distance_and_cs_measure <- ((transects$ds_distance / total_flowline_length) * 100 ) - transects$cs_measure
    
    # NOTE: expect that the ds_distance should be within 1% of the stated cs_measure proportion
    ds_distance_lines_up_with_cs_measure_percents <- !any(diff_between_ds_distance_and_cs_measure > ACCEPTABLE_PCT_DIFF)
    
    testthat::expect_true(ds_distance_lines_up_with_cs_measure_percents)
    
  }
  
})

testthat::test_that("Error from SF POLYGON)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  polys     <- sf::st_buffer(flowlines, 1) 
  poly      <- polys$geom[1]
  
  testthat::expect_error(hydrofabric3D::get_transects(poly, 4, 3))
})

testthat::test_that("Error from SF MULTIPOLYGON)", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  polys     <- sf::st_buffer(flowlines, 1) 
  poly      <- polys$geom[1] %>% 
    sf::st_cast("MULTIPOLYGON")
  
  testthat::expect_error(hydrofabric3D::get_transects(poly, 4, 3))
  
})

testthat::test_that("Error from character bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], " dsgfd" , 3))
  
})

testthat::test_that("Error from logical bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], TRUE , 3))
  
})
testthat::test_that("Error from list bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], list(x = 43) , 3))
  
})

testthat::test_that("Error from NA bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], NA , 3))
  
})

testthat::test_that("Error from NULL bf_width", {
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  testthat::expect_error(hydrofabric3D::get_transects(flowlines$geom[1], NULL , 3))
  
})