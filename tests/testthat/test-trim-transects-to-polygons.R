
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::trim_transects_to_polygons() ----
# -------------------------------------------------------------------
testthat::test_that("trim_transects_to_polygons() correct output columns - default inputs", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(10)
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # # create testiung polygons by buffering the flowlines
  # create testiung polygons by buffering the flowlines
  big_polygons <- 
    flowlines %>%
    sf::st_buffer(500) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  small_polygons <- 
    flowlines %>%
    sf::st_buffer(250) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id
    )
  
  # plot(polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(transects$geometry, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = big_polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  ) %>% 
    dplyr::select(
      dplyr::any_of(CROSSWALK_ID),
      cs_id
    )
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  trimmed_trans <- hydrofabric3D::trim_transects_by_polygons(
    transect_lines = ext_trans, 
    flowlines = flowlines, 
    polygons = small_polygons, 
    crosswalk_id = CROSSWALK_ID
  ) 
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 4, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 4, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  # # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = F)
  # plot(ext_trans$geometry, col = "green", lwd = 4, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 4, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  # minimum expected cols
  expected_cols <- c(CROSSWALK_ID, 
                     "cs_id"
                     )
  
  has_required_output_cols <- check_required_cols(trimmed_trans, expected_cols = expected_cols)
  testthat::expect_true(has_required_output_cols)
  
})

testthat::test_that("trim_transects_to_polygons() all transects are within polygons and thus get trimmed down and all are retained with correct IDs", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(10)
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # # create testiung polygons by buffering the flowlines
  # create testiung polygons by buffering the flowlines
  big_polygons <- 
    flowlines %>%
    sf::st_buffer(500) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  small_polygons <- 
    flowlines %>%
    sf::st_buffer(250) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
    )
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(transects$geometry, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = big_polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  ) %>% 
    dplyr::select(
      dplyr::any_of(CROSSWALK_ID),
      cs_id,
      cs_lengthm,
      cs_measure
    )
  
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  trimmed_trans <- hydrofabric3D::trim_transects_by_polygons(
    transect_lines = ext_trans, 
    flowlines = flowlines, 
    polygons = small_polygons, 
    crosswalk_id = CROSSWALK_ID
  ) 
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 4, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 4, add = T)
  # plot(flowlines$geom, col = "blue", add = T)
  
  # test all UIDs are retained as desired
  testthat::expect_true(
    has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  testthat::expect_true(
    has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  # test to make sure transects only intersect one flowline and once 
  all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
  testthat::expect_true(
    all_transects_intersect_flowlines_only_once
  )
  
  # test to make sure transects DON'T intersect any other flowlines (except self)
  all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
  testthat::expect_true(
    all_transects_intersect_no_other_transects
  )
  
  
})


testthat::test_that("trim_transects_to_polygons() one set of transects has all trims occur, another set has no trimming, and all the IDs get retained and trimming works as expected ", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(c(9, 10))
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # # create testiung polygons by buffering the flowlines
  # create testiung polygons by buffering the flowlines
  big_polygons <- 
    flowlines %>%
    dplyr::slice(2) %>% 
    sf::st_buffer(200) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  small_polygons <- 
    flowlines %>%
    dplyr::slice(2) %>% 
    sf::st_buffer(150) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
    )
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(transects$geometry, add = T)
  # 
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = big_polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  ) %>% 
    dplyr::select(
      dplyr::any_of(CROSSWALK_ID),
      cs_id,
      cs_lengthm,
      cs_measure
    )
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  # 
  # mapview::mapview(ext_trans, color = "green") +
  #  mapview::mapview(transects, color = "red") +  
  #  mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white") 
  
  
  trimmed_trans <- hydrofabric3D::trim_transects_by_polygons(
    transect_lines = ext_trans, 
    flowlines = flowlines, 
    polygons = small_polygons, 
    crosswalk_id = CROSSWALK_ID
  ) 
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  
  # test all UIDs are retained as desired
  testthat::expect_true(
    has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  testthat::expect_true(
    has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  # test to make sure transects only intersect one flowline and once 
  all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
  testthat::expect_true(
    all_transects_intersect_flowlines_only_once
  )
  
  # test to make sure transects DON'T intersect any other flowlines (except self)
  all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
  testthat::expect_true(
    all_transects_intersect_no_other_transects
  )
  
  
})

testthat::test_that("trim_transects_to_polygons() 2 sets of transects lines get trimmed, one set has all its transects in polygons, 
                    the other set has 3 transect line that intersects with one of the polygons from the other flowline 
                    one of the transects is entirely in the polygon, the other 2 are partitually in the polygon,
                    none of these transects get extended nor trimmed", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(c(9, 10))
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # # create testiung polygons by buffering the flowlines
  # create testiung polygons by buffering the flowlines
  big_polygons <- 
    flowlines %>%
    dplyr::slice(2) %>% 
    sf::st_buffer(500) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  small_polygons <- 
    flowlines %>%
    dplyr::slice(2) %>% 
    sf::st_buffer(200) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
    )
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(transects$geometry, add = T)
  
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = big_polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  ) %>% 
    dplyr::select(
      dplyr::any_of(CROSSWALK_ID),
      cs_id,
      cs_lengthm,
      cs_measure
    )
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)

  # mapview::mapview(ext_trans, color = "green") +
  #  mapview::mapview(transects, color = "red") +
  #  mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white")
  
  
  trimmed_trans <- hydrofabric3D::trim_transects_by_polygons(
    transect_lines = ext_trans, 
    flowlines = flowlines, 
    polygons = small_polygons, 
    crosswalk_id = CROSSWALK_ID
  ) 
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  
  # test all UIDs are retained as desired
  testthat::expect_true(
    has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  testthat::expect_true(
    has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  # test to make sure transects only intersect one flowline and once 
  all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
  testthat::expect_true(
    all_transects_intersect_flowlines_only_once
  )
  
  # test to make sure transects DON'T intersect any other flowlines (except self)
  all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
  testthat::expect_true(
    all_transects_intersect_no_other_transects
  )
  
})

testthat::test_that("trim_transects_to_polygons() a single set of transects with one flowline, and a poylgon for extending/trimming which is NOT entirely covering the flowline,
                    2 transects get extended, but only 1 gets trimmed because that one is intersecting the 'trim polygon', the other extension does NOT intersect the trim polygon", {
                      
    flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
      dplyr::slice(c(9))
    
    # MIN_BF_WIDTH       <- 50
    CROSSWALK_ID       <- "id"
    
    flowlines <-
      flowlines %>% 
      # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
      dplyr::select(
        dplyr::any_of(c(CROSSWALK_ID)), 
        geom
      ) 
    
    # # create testiung polygons by buffering the flowlines
    # create testiung polygons by buffering the flowlines
    big_polygons <- 
      # flowlines %>%
      # dplyr::slice(2) %>% 
      sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
      dplyr::slice(c(10)) %>% 
      sf::st_buffer(500) %>% 
      dplyr::mutate(
        polygon_id = 1:dplyr::n()
      )
    
    small_polygons <- 
      # flowlines %>%
      # dplyr::slice(2) %>% 
      sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
      dplyr::slice(c(10)) %>% 
      sf::st_buffer(200) %>% 
      dplyr::mutate(
        polygon_id = 1:dplyr::n()
      )
    
    # generate transects
    transects <- cut_cross_sections(
      net = flowlines,
      crosswalk_id  = CROSSWALK_ID,  
      cs_widths = 200,
      num = 10
    ) %>% 
      dplyr::select(
        dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
      )
    
    # plot(flowlines$geom, col = "blue", add = F)
    # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
    # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
    # plot(transects$geometry, add = T)
    
    ext_trans <- hydrofabric3D::extend_transects_to_polygons(
      transect_lines = transects, 
      polygons = big_polygons, 
      flowlines = flowlines, 
      crosswalk_id = CROSSWALK_ID,  
      grouping_id = CROSSWALK_ID,
      max_extension_distance = 3000,
      tolerance = NULL,
      keep_lengths = FALSE,
      reindex_cs_ids = FALSE,
      verbose = TRUE
    ) %>% 
      dplyr::mutate(
        is_extended = left_distance > 0 | right_distance > 0
      )
    # %>% 
      # dplyr::select(
        # dplyr::any_of(CROSSWALK_ID),
        # cs_id,
        # cs_lengthm,
        # cs_measure
      # )
    
    # plot(flowlines$geom, col = "blue", add = F)
    # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
    # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
    # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
    # plot(transects$geometry, col = "red",lwd = 2, add = T)
    # # 
    # mapview::mapview(ext_trans, color = "green") +
    #  mapview::mapview(transects, color = "red") +
    #  mapview::mapview(flowlines, color = "dodgerblue") +
    #   mapview::mapview(big_polygons, col.regions = "white")
    
    
    trimmed_trans <- hydrofabric3D::trim_transects_by_polygons(
      transect_lines = ext_trans, 
      flowlines = flowlines,
      # flowlines = sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
      #   dplyr::slice(c(9, 10)),
      polygons = small_polygons, 
      crosswalk_id = CROSSWALK_ID
    ) 
    
    # trimmed_trans %>% 
    #   dplyr::filter(is_extended)
    # 
    
    # plot(flowlines$geom, col = "blue", add = F)
    # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
    # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
    # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
    # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
    # plot(transects$geometry, col = "red",lwd = 2, add = T)
    # 
    
    # mapview::mapview(ext_trans, color = "green") +
    #   mapview::mapview(transects, color = "red") +
    #   mapview::mapview(trimmed_trans, color = "gold") +
    #   mapview::mapview(    trimmed_trans %>% 
    #                          dplyr::filter(is_extended), color = "hotpink") +
    #   mapview::mapview(flowlines, color = "dodgerblue") +
    #   mapview::mapview(big_polygons, col.regions = "white") +
    # mapview::mapview(small_polygons, col.regions = "dodgerblue")
    
    # test all UIDs are retained as desired
    testthat::expect_true(
      has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
    )
    
    testthat::expect_true(
      has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
    )
    
    # test to make sure transects only intersect one flowline and once 
    all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
    testthat::expect_true(
      all_transects_intersect_flowlines_only_once
    )
    
    # test to make sure transects DON'T intersect any other flowlines (except self)
    all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
    testthat::expect_true(
      all_transects_intersect_no_other_transects
    )
    
    # tests that make sure specific transects were and were NOT trimmed 
    was_not_trimmed <- 
      trimmed_trans %>% 
      dplyr::filter(id == "wb-1003265", cs_id == 9) %>% 
      hydrofabric3D::add_length_col("new_length")
    
    was_trimmed <- 
      trimmed_trans %>% 
      dplyr::filter(id == "wb-1003265", cs_id == 10) %>% 
      hydrofabric3D::add_length_col("new_length")
    
    testthat::expect_equal(
      was_not_trimmed$cs_lengthm, 
      was_not_trimmed$new_length
    )
    
    actually_trimmed_transects_are_not_equal_length <- was_trimmed$cs_lengthm != was_trimmed$new_length
    testthat::expect_true(
      actually_trimmed_transects_are_not_equal_length
    )
    
  })


testthat::test_that("trim_transects_to_polygons() a single set of transects with one flowline, 
                    but during trim, there are more flowlines than used to generate transects 
                    but the trim still happens for transects the are cross other flowlines, 
                    this is the case when the given set of transects is violating the multiple flowline intersections property", {
                      
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(c(9))
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # # create testiung polygons by buffering the flowlines
  # create testiung polygons by buffering the flowlines
  big_polygons <- 
    # flowlines %>%
    # dplyr::slice(2) %>% 
    sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(c(10)) %>% 
    sf::st_buffer(500) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  small_polygons <- 
    # flowlines %>%
    # dplyr::slice(2) %>% 
    sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::slice(c(10)) %>% 
    sf::st_buffer(400) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
    )
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(transects$geometry, add = T)
  
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = big_polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  )  
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  # # # 
  # mapview::mapview(ext_trans, color = "green") +
  #  mapview::mapview(transects, color = "red") +
  #  mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white")
  
  
  trimmed_trans <- trim_transects_by_polygons(
    transect_lines = ext_trans, 
    # flowlines = flowlines,
    flowlines = sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>%
      dplyr::slice(c(9, 10)),
    polygons = small_polygons, 
    crosswalk_id = CROSSWALK_ID
  ) 
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(flowlines$geom, col = "blue", add = T)
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  # 
  
  # mapview::mapview(ext_trans, color = "green") +
  #   mapview::mapview(transects, color = "red") +
  #   mapview::mapview(trimmed_trans, color = "gold") +
  #   mapview::mapview(    trimmed_trans %>% 
  #                          dplyr::filter(is_extended), color = "hotpink") +
  #   mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white") +
  # mapview::mapview(small_polygons, col.regions = "dodgerblue")
  
  # test all UIDs are retained as desired
  testthat::expect_true(
    has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  testthat::expect_true(
    has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  # test to make sure transects only intersect one flowline and once 
  all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
  testthat::expect_true(
    all_transects_intersect_flowlines_only_once
  )
  
  # test to make sure transects DON'T intersect any other flowlines (except self)
  all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
  testthat::expect_true(
    all_transects_intersect_no_other_transects
  )
  
  # tests that make sure specific transects were and were NOT trimmed 
  was_not_trimmed <- 
    trimmed_trans %>% 
    dplyr::filter(id == "wb-1003265", cs_id == 3) %>% 
    hydrofabric3D::add_length_col("new_length")
  
  testthat::expect_equal(
    was_not_trimmed$cs_lengthm, 
    was_not_trimmed$new_length
  )
  
  was_trimmed <- 
    trimmed_trans %>% 
    dplyr::filter(id == "wb-1003265", cs_id %in% c(9, 10)) %>% 
    hydrofabric3D::add_length_col("new_length")
  
  actually_trimmed_transects_are_not_equal_length <- all(was_trimmed$cs_lengthm != was_trimmed$new_length)
  
  testthat::expect_true(
    actually_trimmed_transects_are_not_equal_length
  )
  
})
testthat::test_that("trim_transects_to_polygons() complex junction flowlines with all flowlines having a polygon to trim against", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) 
  # dplyr::slice(c(9))
  # plot(flowlines$geom)
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # # create testiung polygons by buffering the flowlines
  # create testiung polygons by buffering the flowlines
  big_polygons <- 
    flowlines %>%
    # dplyr::slice(c(1, 3, 5)) %>%
    # sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
    # dplyr::slice(c(10)) %>% 
    sf::st_buffer(300) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  small_polygons <- 
    flowlines %>%
    # dplyr::slice(c(1, 3, 5)) %>%
    # sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    # dplyr::slice(c(10)) %>% 
    sf::st_buffer(200) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
    )
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(transects$geometry, add = T)
  
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = big_polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  )  
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  
  # mapview::mapview(ext_trans, color = "green") +
  #  mapview::mapview(transects, color = "red") +
  #  mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white")
  
  
  trimmed_trans <- trim_transects_by_polygons(
    transect_lines = ext_trans, 
    flowlines = flowlines,
    # flowlines = sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>%
    #   dplyr::slice(c(9, 10)),
    polygons = small_polygons, 
    crosswalk_id = CROSSWALK_ID
  ) 
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(flowlines$geom, col = "blue", add = T)
  # # plot(flowlines$geom, col = "blue", add = F)
  # # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  
  # mapview::mapview(ext_trans, color = "green") +
  #   mapview::mapview(transects, color = "red") +
  #   mapview::mapview(trimmed_trans, color = "gold") +
  #   mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white") +
  # mapview::mapview(small_polygons, col.regions = "dodgerblue")
  
  # test all UIDs are retained as desired
  testthat::expect_true(
    has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  testthat::expect_true(
    has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  # test to make sure transects only intersect one flowline and once 
  all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
  testthat::expect_true(
    all_transects_intersect_flowlines_only_once
  )
  
  # test to make sure transects DON'T intersect any other flowlines (except self)
  all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
  testthat::expect_true(
    all_transects_intersect_no_other_transects
  )
  
  # tests that make sure specific transects were and were NOT trimmed 
  was_not_trimmed <- 
    trimmed_trans %>% 
    dplyr::filter(id == "wb-1003265", cs_id == 3) %>% 
    hydrofabric3D::add_length_col("new_length")
  
  testthat::expect_equal(
    was_not_trimmed$cs_lengthm, 
    was_not_trimmed$new_length
  )
  
  was_trimmed <- 
    trimmed_trans %>% 
    dplyr::filter(id == "wb-1003265", cs_id %in% c(9, 10)) %>% 
    hydrofabric3D::add_length_col("new_length")
  
  actually_trimmed_transects_are_not_equal_length <- all(was_trimmed$cs_lengthm != was_trimmed$new_length)
  
  testthat::expect_true(
    actually_trimmed_transects_are_not_equal_length
  )
  
})

testthat::test_that("trim_transects_to_polygons() complex junction flowlines with some flowlines having polygons and somenot having polygons", {
                      
    flowlines    <- sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) 
      # dplyr::slice(c(9))
    # plot(flowlines$geom)
    
    # MIN_BF_WIDTH       <- 50
    CROSSWALK_ID       <- "id"
    
    flowlines <-
      flowlines %>% 
      # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
      dplyr::select(
        dplyr::any_of(c(CROSSWALK_ID)), 
        geom
      ) 
    
    # # create testiung polygons by buffering the flowlines
    # create testiung polygons by buffering the flowlines
    big_polygons <- 
      flowlines %>%
      dplyr::slice(c(1, 2)) %>%
      # sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
      # dplyr::slice(c(10)) %>% 
      sf::st_buffer(500) %>% 
      dplyr::mutate(
        polygon_id = 1:dplyr::n()
      )
    
    small_polygons <- 
      flowlines %>%
      dplyr::slice(c(1, 2)) %>%
      # sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
      # dplyr::slice(c(10)) %>% 
      sf::st_buffer(400) %>% 
      dplyr::mutate(
        polygon_id = 1:dplyr::n()
      )
    
    # generate transects
    transects <- cut_cross_sections(
      net = flowlines,
      crosswalk_id  = CROSSWALK_ID,  
      cs_widths = 200,
      num = 10
    ) %>% 
      dplyr::select(
        dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
      )
    
    # plot(flowlines$geom, col = "blue", add = F)
    # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
    # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
    # plot(transects$geometry, add = T)
    
    ext_trans <- hydrofabric3D::extend_transects_to_polygons(
      transect_lines = transects, 
      polygons = big_polygons, 
      flowlines = flowlines, 
      crosswalk_id = CROSSWALK_ID,  
      grouping_id = CROSSWALK_ID,
      max_extension_distance = 3000,
      tolerance = NULL,
      keep_lengths = FALSE,
      reindex_cs_ids = FALSE,
      verbose = TRUE
    )  
    
    # plot(flowlines$geom, col = "blue", add = F)
    # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
    # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
    # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
    # plot(transects$geometry, col = "red",lwd = 2, add = T)
    # # # # 
    # mapview::mapview(ext_trans, color = "green") +
    #  mapview::mapview(transects, color = "red") +
    #  mapview::mapview(flowlines, color = "dodgerblue") +
    #   mapview::mapview(big_polygons, col.regions = "white")
    
    
    trimmed_trans <- trim_transects_by_polygons(
      transect_lines = ext_trans, 
      flowlines = flowlines,
      # flowlines = sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")),
      polygons = small_polygons, 
      crosswalk_id = CROSSWALK_ID
    ) 
    
    # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
    # plot(flowlines$geom, col = "blue", add = T)
    # # plot(flowlines$geom, col = "blue", add = F)
    # # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
    # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
    # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
    # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
    # plot(transects$geometry, col = "red",lwd = 2, add = T)
    # 
    
    # mapview::mapview(ext_trans, color = "green") +
    #   mapview::mapview(transects, color = "red") +
    #   mapview::mapview(trimmed_trans, color = "gold") +
    #   mapview::mapview(    trimmed_trans %>% 
    #                          dplyr::filter(is_extended), color = "hotpink") +
    #   mapview::mapview(flowlines, color = "dodgerblue") +
    #   mapview::mapview(big_polygons, col.regions = "white") +
    # mapview::mapview(small_polygons, col.regions = "dodgerblue")
    
    # test all UIDs are retained as desired
    testthat::expect_true(
      has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
    )
    
    testthat::expect_true(
      has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
    )
    
    # test to make sure transects only intersect one flowline and once 
    all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
    testthat::expect_true(
      all_transects_intersect_flowlines_only_once
    )
    
    # test to make sure transects DON'T intersect any other flowlines (except self)
    all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
    testthat::expect_true(
      all_transects_intersect_no_other_transects
    )
    
    # tests that make sure specific transects were and were NOT trimmed 
    trimmed_trans %>% 
    dplyr::filter(id == "wb-1007682") 
    transects %>% 
      dplyr::filter(id == "wb-1007682") 
  
    was_not_trimmed_has_same_geoms <-  all(
                                        lengths(
                                          sf::st_equals_exact(  
                                            trimmed_trans %>% dplyr::filter(id == "wb-1007682") ,
                                            transects %>% dplyr::filter(id == "wb-1007682"),
                                            par = 0.4
                                            )
                                          ) == 1
                                        )
      
      testthat::expect_true(
        was_not_trimmed_has_same_geoms
      )
      
    was_trimmed_has_diff_geoms <-  any(
                                  lengths(
                                    sf::st_equals_exact(  
                                      trimmed_trans %>% dplyr::filter(id == "wb-1002023") ,
                                      transects %>% dplyr::filter(id == "wb-1002023"),
                                      par = 0.4
                                    )
                                  ) != 1
                                )
    
    testthat::expect_true(
      was_trimmed_has_diff_geoms
    )
    
    
  })

testthat::test_that("trim_transects_to_polygons() 2 seperate networks of flowlines and transects with polygons get trimmed all IDs are kept in tact", {
  
  flowlines <- dplyr::bind_rows(
                      sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")), 
                      sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
                    )
  # dplyr::slice(c(9))
  # plot(flowlines$geom)
  
  # MIN_BF_WIDTH       <- 50
  CROSSWALK_ID       <- "id"
  
  flowlines <-
    flowlines %>% 
    # add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), 
      geom
    ) 
  
  # # create testiung polygons by buffering the flowlines
  # create testiung polygons by buffering the flowlines
  big_polygons <- 
    flowlines %>%
    # dplyr::slice(c(1, 2)) %>%
    # sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
    # dplyr::slice(c(10)) %>% 
    sf::st_buffer(500) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  small_polygons <- 
    flowlines %>%
    # dplyr::slice(c(1, 2)) %>%
    # sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    # dplyr::slice(c(10)) %>% 
    sf::st_buffer(400) %>% 
    dplyr::mutate(
      polygon_id = 1:dplyr::n()
    )
  
  # generate transects
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    cs_widths = 200,
    num = 10
  ) %>% 
    dplyr::select(
      dplyr::any_of(c(CROSSWALK_ID)), cs_id, cs_measure
    )
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(transects$geometry, add = T)
  
  ext_trans <- hydrofabric3D::extend_transects_to_polygons(
    transect_lines = transects, 
    polygons = big_polygons, 
    flowlines = flowlines, 
    crosswalk_id = CROSSWALK_ID,  
    grouping_id = CROSSWALK_ID,
    max_extension_distance = 3000,
    tolerance = NULL,
    keep_lengths = FALSE,
    reindex_cs_ids = FALSE,
    verbose = TRUE
  )  
  
  # plot(flowlines$geom, col = "blue", add = F)
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  # # # # # 
  # mapview::mapview(ext_trans, color = "green") +
  #  mapview::mapview(transects, color = "red") +
  #  mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white")
  
  
  trimmed_trans <- trim_transects_by_polygons(
    transect_lines = ext_trans, 
    flowlines = flowlines,
    # flowlines = sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")),
    polygons = small_polygons, 
    crosswalk_id = CROSSWALK_ID
  ) 
  
  # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = F)
  # plot(flowlines$geom, col = "blue", add = T)
  # # plot(flowlines$geom, col = "blue", add = F)
  # # plot(big_polygons$geom, col = scales::alpha("pink", 0.3), add = T)
  # plot(small_polygons$geom, col = scales::alpha("blue", 0.3), add = T)
  # plot(ext_trans$geometry, col = "green", lwd = 2, add = T)
  # plot(trimmed_trans$geometry, col = "gold",lwd = 5, add = T)
  # plot(transects$geometry, col = "red",lwd = 2, add = T)
  # 
  
  # mapview::mapview(ext_trans, color = "green") +
  #   mapview::mapview(transects, color = "red") +
  #   mapview::mapview(trimmed_trans, color = "gold") +
  #   mapview::mapview(flowlines, color = "dodgerblue") +
  #   mapview::mapview(big_polygons, col.regions = "white") +
  # mapview::mapview(small_polygons, col.regions = "dodgerblue")
  
  # test all UIDs are retained as desired
  testthat::expect_true(
    has_same_uids(transects, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  testthat::expect_true(
    has_same_uids(ext_trans, trimmed_trans, crosswalk_id = CROSSWALK_ID)
  )
  
  # test to make sure transects only intersect one flowline and once 
  all_transects_intersect_flowlines_only_once <- all(lengths(sf::st_intersects(trimmed_trans, flowlines) ) == 1)
  testthat::expect_true(
    all_transects_intersect_flowlines_only_once
  )
  
  # test to make sure transects DON'T intersect any other flowlines (except self)
  all_transects_intersect_no_other_transects <- all(lengths(sf::st_intersects(trimmed_trans)) == 1)
  testthat::expect_true(
    all_transects_intersect_no_other_transects
  )
  
  # tests that make sure specific transects were and were NOT trimmed 
  was_not_trimmed_has_same_geoms <-  all(
    lengths(
      sf::st_equals_exact(  
        trimmed_trans %>% dplyr::filter(id == "wb-1010908", cs_id %in% c(1,2, 4:8)) ,
        transects %>%dplyr::filter(id == "wb-1010908", cs_id %in% c(1,2, 4:8)),
        par = 0.4
      )
    ) == 1
  )
  
  testthat::expect_true(
    was_not_trimmed_has_same_geoms
  )
  
  was_trimmed_has_diff_geoms <-  any(
    lengths(
      sf::st_equals_exact(  
        trimmed_trans %>% dplyr::filter(id == "wb-1002024") ,
        transects %>% dplyr::filter(id == "wb-1002024"),
        par = 0.4
      )
    ) != 1
  )
  
  testthat::expect_true(
    was_trimmed_has_diff_geoms
  )
  
  testthat::expect_true(
    any(
      lengths(
        sf::st_equals_exact(  
          trimmed_trans %>% dplyr::filter(id == "wb-1007682") ,
          transects %>% dplyr::filter(id == "wb-1007682"),
          par = 0.4
        )
      ) != 1
    )
  )
  
  testthat::expect_true(
    any(
      lengths(
        sf::st_equals_exact(  
          trimmed_trans %>% dplyr::filter(id == "wb-1002023") ,
          transects %>% dplyr::filter(id == "wb-1002023"),
          par = 0.4
        )
      ) != 1
    )
  )
  
  testthat::expect_true(
    any(
      lengths(
        sf::st_equals_exact(  
          trimmed_trans %>% dplyr::filter(id == "wb-1003263") ,
          transects %>% dplyr::filter(id == "wb-1003263"),
          par = 0.4
        )
      ) != 1
    )
  )
  
  testthat::expect_true(
    any(
      lengths(
        sf::st_equals_exact(  
          trimmed_trans %>% dplyr::filter(id == "wb-1003267") ,
          transects %>% dplyr::filter(id == "wb-1003267"),
          par = 0.4
        )
      ) != 1
    )
  )
  
})

