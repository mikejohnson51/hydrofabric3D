library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)

source("testing_utils.R")

# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::cut_cross_sections() ----
# -------------------------------------------------------------------

testthat::test_that("flowlines only (set 'id' to NULL) sf dataframe, checking proper default ID", {
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  
  # flowlines <- 
  #   flowlines %>% 
  #   dplyr::select(hy_id = id, geometry = geom)
  # transects <- hydrofabric3D::cut_cross_sections(
  #   net = flowlines,
  #   id = "hy_id",
  #   num = 10
  # )
  # 
  # transects2 <- cut_cross_sections(
  #   net = flowlines,
  #   id = "hy_id",
  #   num = 10
  # )
  # 
  # all(transects == transects2)
  
  start_unique_ids_count <- length(unique(flowlines$id))
  
  flowlines <- dplyr::select(flowlines, geom)
  
  transects <- cut_cross_sections(
    net = flowlines,
    num = 10
  )
  
  end_unique_ids_count <- length(unique(transects$hydrofabric_id))
  
  testthat::expect_true(end_unique_ids_count == start_unique_ids_count)
  
})

testthat::test_that("flowlines with correct 'id' column named 'hy_id' provided", {
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  
  CROSSWALK_ID = "hy_id"
  NUMBER_OF_TRANSECTS = 10
  flowlines <-
    flowlines %>% 
    dplyr::rename(!!sym(CROSSWALK_ID) := id) %>% 
    dplyr::select(dplyr::any_of(CROSSWALK_ID), geom)
  
  start_unique_ids_count <- length(unique(flowlines[[CROSSWALK_ID]]))
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id = CROSSWALK_ID,
    num = NUMBER_OF_TRANSECTS,
    cs_widths  = 5
  )
  
  end_unique_ids_count <- length(unique(transects[[CROSSWALK_ID]]))
  
  testthat::expect_true(end_unique_ids_count == start_unique_ids_count)
  testthat::expect_true(CROSSWALK_ID %in% names(transects))
  
  all_ids_have_correct_number_transects <- 
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::group_by(dplyr::across(dplyr::any_of(CROSSWALK_ID))) %>% 
    dplyr::count() %>% 
    dplyr::mutate(
      correct_number_transects = dplyr::case_when(
        n == NUMBER_OF_TRANSECTS ~ TRUE,
        TRUE                     ~ FALSE
      )
    ) %>% 
    .$correct_number_transects %>% 
    all()
  
  testthat::expect_true(all_ids_have_correct_number_transects)  
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
})

testthat::test_that("flowlines with correct 'crosswalk_id' column named 'hy_id' provided", {
  
  CROSSWALK_IDS = c("hy_id", "crosswalk_id")
  
  for (CROSSWALK_ID in CROSSWALK_IDS) {
    
    NUMBER_OF_TRANSECTS = 10
    flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
    flowlines <-
      flowlines %>% 
      dplyr::rename(!!sym(CROSSWALK_ID) := id) %>% 
      dplyr::select(dplyr::any_of(CROSSWALK_ID), geom)
    
    start_unique_ids_count <- length(unique(flowlines[[CROSSWALK_ID]]))
    
    transects <- cut_cross_sections(
      net = flowlines,
      crosswalk_id = CROSSWALK_ID,
      num = NUMBER_OF_TRANSECTS,
      cs_widths  = 5
    )
    
    end_unique_ids_count <- length(unique(transects[[CROSSWALK_ID]]))
    
    testthat::expect_true(end_unique_ids_count == start_unique_ids_count)
    testthat::expect_true(CROSSWALK_ID %in% names(transects))
    
    all_ids_have_correct_number_transects <- 
      transects %>% 
      sf::st_drop_geometry() %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(CROSSWALK_ID))) %>% 
      dplyr::count() %>% 
      dplyr::mutate(
        correct_number_transects = dplyr::case_when(
          n == NUMBER_OF_TRANSECTS ~ TRUE,
          TRUE                     ~ FALSE
        )
      ) %>% 
      .$correct_number_transects %>% 
      all()
    
    testthat::expect_true(all_ids_have_correct_number_transects)  
    testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  } 
})

testthat::test_that("5 meter long transects distance check", {
  
  NUMBER_OF_TRANSECTS = 10
  EXPECTED_WIDTH = 5
  
  CROSSWALK_ID = 'id'
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  flowlines <-
    flowlines %>% 
    dplyr::rename(!!sym(CROSSWALK_ID) := id) %>% 
    dplyr::select(dplyr::any_of(CROSSWALK_ID), geom)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id = CROSSWALK_ID,
    num = NUMBER_OF_TRANSECTS,
    cs_widths  = EXPECTED_WIDTH
  )
  
  is_correct_length_transects <- (
    transects %>% 
      sf::st_length() %>% 
      as.numeric() %>% 
      round() %>% 
      as.integer() == EXPECTED_WIDTH
  ) %>% 
    all()
  
  testthat::expect_true(is_correct_length_transects)
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  
})


testthat::test_that("check correct output columns for transects", {
  
  NUMBER_OF_TRANSECTS = 10
  EXPECTED_WIDTH = 5
  
  CROSSWALK_ID = NULL
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  flowlines <-
    flowlines %>%
    # dplyr::rename(!!sym(CROSSWALK_ID) := id) %>% 
    dplyr::select(geom)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id = CROSSWALK_ID,
    num = NUMBER_OF_TRANSECTS,
    cs_widths  = EXPECTED_WIDTH
  )
  
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  
  is_correct_length_transects <- (
    transects %>% 
      sf::st_length() %>% 
      as.numeric() %>% 
      round() %>% 
      as.integer() == EXPECTED_WIDTH
  ) %>% 
    all()
  
  testthat::expect_true(is_correct_length_transects)
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  
})


testthat::test_that("flowline only (no other input cols) sf dataframe, checking transect number and no self intersections", {
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  CROSSWALK_ID <- "hydrofabric_id"
  
  flowline <-
    flowlines[10, ] %>%
    dplyr::select(geom)
  
  transects <- cut_cross_sections(
    net = flowline,
    num = 10
  )
  
  testthat::expect_true(nrow(transects) == 10)
  
  has_no_self_intersctions <- all(lengths(sf::st_intersects(transects)) == 1)
  testthat::expect_true(has_no_self_intersctions)
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  
  transects <- cut_cross_sections(
    net = flowline,
    num = 20
  )
  
  # plot(transects$geometry)
  # plot(flowline$geom, col = "red", add = T)
  
  # NOTE: there is a kink in the flowline that CORRECTLY causes a transect to be removed
  testthat::expect_true(nrow(transects) == 20)
  # mapview::mapview(transects) + flowline
  
  has_no_self_intersctions <- all(lengths(sf::st_intersects(transects)) == 1)
  testthat::expect_true(has_no_self_intersctions)
  
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  
  transects <- cut_cross_sections(
    net = flowline,
    num = 100
  )
  
  testthat::expect_true(nrow(transects) == 96)
  
  has_no_self_intersctions <- all(lengths(sf::st_intersects(transects)) == 1)
  testthat::expect_true(has_no_self_intersctions)
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  # trans <- cut_cross_sections(
  #   net = flowline,
  #   cs_widths = 2500,
  #   num = 1,
  #   rm_self_intersect = T
  # ) 
  # plot(flowline, add = F)
  # trans %>% .$geometry %>% plot(add = T) 
  # trans[lengths(sf::st_intersects(trans)) == 1, ] 
  # trans[lengths(sf::st_intersects(trans, flowline)) == 1, ]
  # 
  # mapview::mapview(trans) + flowline
  # cut_cross_sections()
  # 
  # transects <- hydrofabric3D::cut_cross_sections(
  #   net               = flowline
  #   # crosswalk_id                = NULL,
  #   # cs_widths         = c(10) 
  # )
  
})

testthat::test_that("flowline w/ only 2 points results 
                    in a single transect line when no densification or smoothing", {
  
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
    hydrofabric3D::add_powerlaw_bankful_width(
      total_drainage_area_sqkm_col = "tot_drainage_areasqkm",
      min_bf_width = 50
    ) %>%
    dplyr::select(
      id,
      lengthkm,
      mainstem,
      tot_drainage_areasqkm,
      geometry = geom
    ) %>%   
    dplyr::filter(
      id %in% c("wb-1002023")
      # id %in% c("wb-1010908","wb-1002024", "wb-1002023")
    ) %>% 
    sf::st_simplify(dTolerance = 50)
  
  # dense_lines <- force_min_npts_per_flowlines(flowlines)
  
  # flowlines %>% 
  #   mapview::npts(by_feature = T)
  
  # mapview::mapview(dense_lines, color = "red") +
  #   mapview::mapview(flowlines, color = "green")
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowlines,
    crosswalk_id      = "id",
    cs_widths         = 50,     # cross section width of each "id" linestring ("hy_id")
    num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = FALSE,                          # smooth lines
    densify           = NULL,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  testthat::expect_true(
    nrow(transects) == 1
  )
  
  # mapview::mapview(transects, color = "red") +
  #   mapview::mapview(flowlines, color = "green")
  
})
testthat::test_that("flowline w/ only 3 points results 
                    in a max of 4 transects (no densification or smoothing)", {
                      
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
    hydrofabric3D::add_powerlaw_bankful_width(
      total_drainage_area_sqkm_col = "tot_drainage_areasqkm",
      min_bf_width = 50
    ) %>%
    dplyr::select(
      id,
      lengthkm,
      mainstem,
      tot_drainage_areasqkm,
      geometry = geom
    ) %>%   
    dplyr::filter(
      # id %in% c("wb-1002023")
      # id %in% c("wb-1010908","wb-1002024", "wb-1002023")
      id %in% c("wb-1010908")
    ) %>% 
    sf::st_simplify(dTolerance = 50)
  
  # dense_lines <- force_min_npts_per_flowlines(flowlines)
  
  # flowlines %>%
  #   mapview::npts(by_feature = T)
  # mapview::mapview(dense_lines, color = "red") +
  #   mapview::mapview(flowlines, color = "green")
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowlines,
    crosswalk_id      = "id",
    cs_widths         = 50,     # cross section width of each "id" linestring ("hy_id")
    num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = FALSE,                          # smooth lines
    densify           = NULL,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  testthat::expect_true(
    nrow(transects) == 4
  )
  
  # mapview::mapview(transects, color = "red") +
  #   mapview::mapview(flowlines, color = "green")
  
})

testthat::test_that("flowline w/ only 4 points results 
                    in 1 transect line (no densification or smoothing)", {
      
      flowlines <-
        sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
        hydrofabric3D::add_powerlaw_bankful_width(
          total_drainage_area_sqkm_col = "tot_drainage_areasqkm",
          min_bf_width = 50
        ) %>%
        dplyr::select(
          id,
          lengthkm,
          mainstem,
          tot_drainage_areasqkm,
          geometry = geom
        ) %>%   
        dplyr::filter(
          # id %in% c("wb-1002023")
          # id %in% c("wb-1010908","wb-1002024", "wb-1002023")
          id %in% c("wb-1010908")
        ) %>% 
        sf::st_simplify(dTolerance = 35)
      
      # dense_lines <- force_min_npts_per_flowlines(flowlines)
      
      # flowlines %>%
      #   mapview::npts(by_feature = T)
      
      # mapview::mapview(dense_lines, color = "red") +
      #   mapview::mapview(flowlines, color = "green")
      
      transects <- hydrofabric3D::cut_cross_sections(
        net               = flowlines,
        crosswalk_id      = "id",
        cs_widths         = 50,     # cross section width of each "id" linestring ("hy_id")
        num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
        smooth            = FALSE,                          # smooth lines
        densify           = NULL,                             # densify linestring points
        rm_self_intersect = TRUE,                          # remove self intersecting transects
        fix_braids        = FALSE
      )
      
      testthat::expect_true(
        nrow(transects) == 1
      )
      
})

testthat::test_that("flowline w/ only 5 points results 
                    in 2 transect lines (no densification or smoothing)", {
                      
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
    hydrofabric3D::add_powerlaw_bankful_width(
      total_drainage_area_sqkm_col = "tot_drainage_areasqkm",
      min_bf_width = 50
    ) %>%
    dplyr::select(
      id,
      lengthkm,
      mainstem,
      tot_drainage_areasqkm,
      geometry = geom
    ) %>%   
    dplyr::filter(
      # id %in% c("wb-1002023")
      # id %in% c("wb-1010908","wb-1002024", "wb-1002023")
      id %in% c("wb-1010908")
    ) %>% 
    sf::st_simplify(dTolerance = 27)
  
  # flowlines %>%
  #   mapview::npts(by_feature = T)
  
  # mapview::mapview(dense_lines, color = "red") +
  #   mapview::mapview(flowlines, color = "green")
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowlines,
    crosswalk_id      = "id",
    cs_widths         = 50,     # cross section width of each "id" linestring ("hy_id")
    num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = FALSE,                          # smooth lines
    densify           = NULL,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  testthat::expect_true(
    nrow(transects) == 2
  )
  
})


testthat::test_that("flowline w/ only 7 points results 
                    in max of 4 transect lines (no densification or smoothing)", {
                      
    flowlines <-
      sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
      hydrofabric3D::add_powerlaw_bankful_width(
        total_drainage_area_sqkm_col = "tot_drainage_areasqkm",
        min_bf_width = 50
      ) %>%
      dplyr::select(
        id,
        lengthkm,
        mainstem,
        tot_drainage_areasqkm,
        geometry = geom
      ) %>%   
      dplyr::filter(
        # id %in% c("wb-1002023")
        # id %in% c("wb-1010908","wb-1002024", "wb-1002023")
        id %in% c("wb-1010908")
      ) 
    # %>% 
      # sf::st_simplify(dTolerance = 27)

    # flowlines %>%
    #   mapview::npts(by_feature = T)
    
    # mapview::mapview(dense_lines, color = "red") +
    #   mapview::mapview(flowlines, color = "green")
    
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowlines,
      crosswalk_id      = "id",
      cs_widths         = 50,     # cross section width of each "id" linestring ("hy_id")
      num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
      smooth            = FALSE,                          # smooth lines
      densify           = NULL,                             # densify linestring points
      rm_self_intersect = TRUE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    
    testthat::expect_true(
      nrow(transects) == 4
    )
    
  })

testthat::test_that("number of transects equals the number of points on a line
                    minus 2 (for the end points) and then minus 1 (2 pts create 1 transect)
                    (asssuming no densification or smoothing)", {
    
    flowlines <- dplyr::bind_rows(
                      sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")),
                      sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")),
                      sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg")) %>% 
                        dplyr::select(id = comid) %>% 
                        dplyr::mutate(id = as.character(id))
                    ) %>% 
      hydroloom::rename_geometry("geometry") %>% 
      dplyr::select(
        id
      ) %>%
      dplyr::filter(
        # id %in% c("wb-1002023")
        # id %in% c("wb-1010908","wb-1002024", "wb-1002023")
        # id %in% c("wb-1007682")
      )
    
    for (i in 1:nrow(flowlines)) {
     
      message(i)
      # i <- 94
      fl <- flowlines[i, ]
      
      pt_count <- geos::geos_num_coordinates(fl)
      
      # pt_counts <- 
      #   fl %>%
      #   mapview::npts(by_feature = T)
      
      num_trans <- max(pt_count) + 4
      
      transects <- hydrofabric3D::cut_cross_sections(
        net               = fl,
        crosswalk_id      = "id",
        cs_widths         = 0.01,     # cross section width of each "id" linestring ("hy_id")
        num               = num_trans,                            # number of cross sections per "id" linestring ("hy_id")
        smooth            = FALSE,                          # smooth lines
        densify           = NULL,                             # densify linestring points
        rm_self_intersect = FALSE,                          # remove self intersecting transects
        fix_braids        = FALSE
      )
      
      # mapview::mapview(transects, color = "red") +
      #   mapview::mapview(fl, color = "green")
      
      # prep_flowlines(fl) %>%
      #   mapview::npts()
      # transects %>% 
      #   sf::st_drop_geometry() %>% 
      #   dplyr::group_by(id) %>% 
      #   dplyr::count(id) %>% 
      #   dplyr::ungroup()
      
      # if flowlines have more than 4 points, then its the equation to know how many transects get made
      # 2, 3, 4 points are all special edge cases
      if (pt_count > 4) {
        expected_num_of_transects <- (pt_count - 2) - 1
      } else if (pt_count %in% c(2, 4)) {
        expected_num_of_transects <- 1
      } else if(pt_count == 3) {
        expected_num_of_transects <- 4
      }
      
      testthat::expect_true(
        nrow(transects) == expected_num_of_transects
      )
       
    }
    
    # flowlines %>%
    #   mapview::npts(by_feature = T)
})


testthat::test_that("transects pass validate_transects with varying number of prescribed transects (1-15 cuts)
                    with rm_self_intersection = TRUE", {
                      
    flowlines <- dplyr::bind_rows(
      sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")),
      sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")),
      sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg")) %>% 
        dplyr::select(id = comid) %>% 
        dplyr::mutate(id = as.character(id))
    ) %>% 
      hydroloom::rename_geometry("geometry") %>% 
      dplyr::select(
        id
      ) %>% 
      sf::st_cast("MULTILINESTRING")
    
    for (i in 1:15) {
    
      transects <- hydrofabric3D::cut_cross_sections(
        net               = flowlines,
        crosswalk_id      = "id",
        cs_widths         = 0.01,     # cross section width of each "id" linestring ("hy_id")
        num               = i,                            # number of cross sections per "id" linestring ("hy_id")
        smooth            = FALSE,                          # smooth lines
        densify           = NULL,                             # densify linestring points
        rm_self_intersect = TRUE,                          # remove self intersecting transects
        fix_braids        = FALSE
      )
      
      # mapview::mapview(transects, color = "red") +
      #   mapview::mapview(fl, color = "green")
      testthat::expect_true(
        hydrofabric3D::validate_transects(transects, "id")
      )
  
    }
})

testthat::test_that("transects that cross other transects are removed when rm_self_intersecct is TRUE", {
                      
      flowlines <-
        sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg")) %>% 
        dplyr::arrange(hydroseq) %>% 
        dplyr::select(id = comid) %>% 
        dplyr::mutate(id = as.character(id)) %>% 
        hydroloom::rename_geometry("geometry") %>% 
        dplyr::select(
          id
        ) %>% 
        sf::st_cast("MULTILINESTRING") %>% 
        dplyr::slice(1:50)
      
        
        transects <- hydrofabric3D::cut_cross_sections(
          net               = flowlines,
          crosswalk_id      = "id",
          cs_widths         = 500,     # cross section width of each "id" linestring ("hy_id")
          num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
          smooth            = FALSE,                          # smooth lines
          densify           = NULL,                             # densify linestring points
          rm_self_intersect = TRUE,                          # remove self intersecting transects
          fix_braids        = FALSE
        )
        
        # plot(flowlines$geometry)
        # plot(transects$geometry, add = T)
        
        testthat::expect_true(
            all(
              lengths(sf::st_intersects(transects)) == 1
            )
        )
       
    })

testthat::test_that("transects that cross other transects are ketp when rm_self_intersecct is FALSE", {
  
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg")) %>% 
    dplyr::arrange(hydroseq) %>% 
    dplyr::select(id = comid) %>% 
    dplyr::mutate(id = as.character(id)) %>% 
    hydroloom::rename_geometry("geometry") %>% 
    dplyr::select(
      id
    ) %>% 
    sf::st_cast("MULTILINESTRING") %>% 
    dplyr::slice(1:50)
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowlines,
    crosswalk_id      = "id",
    cs_widths         = 500,     # cross section width of each "id" linestring ("hy_id")
    num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = FALSE,                          # smooth lines
    densify           = NULL,                             # densify linestring points
    rm_self_intersect = FALSE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, add = T)
  
  testthat::expect_true(
    any(lengths(sf::st_intersects(transects)) > 1)
  )
  
})

testthat::test_that("braided flowlines, transects never cross flowlines more than once
                    whether rm_self_intersect is TRUE or FALSE
                    ", {
  
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg")) %>% 
    dplyr::arrange(hydroseq) %>% 
    dplyr::select(id = comid) %>% 
    dplyr::mutate(id = as.character(id)) %>% 
    hydroloom::rename_geometry("geometry") %>% 
    dplyr::select(
      id
    ) %>% 
    sf::st_cast("MULTILINESTRING") %>% 
    dplyr::slice(1:50)
  
  for (width in c(1000, 2500)) {
    
    # rm_self_intersect = TRUE
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowlines,
      crosswalk_id      = "id",
      cs_widths         = width,     # cross section width of each "id" linestring ("hy_id")
      num               = 20,                            # number of cross sections per "id" linestring ("hy_id")
      smooth            = FALSE,                          # smooth lines
      densify           = NULL,                             # densify linestring points
      rm_self_intersect = TRUE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    
    # plot(flowlines$geometry)
    # plot(transects$geometry, add = T)
    
    testthat::expect_true(
      all(
        lengths(sf::st_intersects(transects, flowlines)) == 1
      )
    )
  }
  
  for (width in c(1000, 2500)) {
    # rm_self_intersect = FALSE
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowlines,
      crosswalk_id      = "id",
      cs_widths         = width,     # cross section width of each "id" linestring ("hy_id")
      num               = 20,                            # number of cross sections per "id" linestring ("hy_id")
      smooth            = FALSE,                          # smooth lines
      densify           = NULL,                             # densify linestring points
      rm_self_intersect = FALSE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
  
    # plot(flowlines$geometry)
    # plot(transects$geometry, add = T)
    
    testthat::expect_true(
      all(
        lengths(sf::st_intersects(transects, flowlines)) == 1
      )
    )
    
  }
  
  
})

testthat::test_that("junction flowlines, transects never cross flowlines more than once
                    whether rm_self_intersect is TRUE or FALSE", {
                      
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "junction_flowlines.gpkg")) %>% 
    dplyr::mutate(id = as.character(id)) %>% 
    hydroloom::rename_geometry("geometry") %>% 
    dplyr::select(
      id
    ) %>% 
    sf::st_cast("MULTILINESTRING") 
  
  for (width in c(1000, 2500)) {
    
    # rm_self_intersect = TRUE
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowlines,
      crosswalk_id      = "id",
      cs_widths         = width,     # cross section width of each "id" linestring ("hy_id")
      num               = 20,                            # number of cross sections per "id" linestring ("hy_id")
      smooth            = FALSE,                          # smooth lines
      densify           = NULL,                             # densify linestring points
      rm_self_intersect = TRUE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    
    # plot(flowlines$geometry)
    # plot(transects$geometry, add = T)
    
    testthat::expect_true(
      all(
        lengths(sf::st_intersects(transects, flowlines)) == 1
      )
    )
  }
  
  for (width in c(1000, 2500)) {
    # rm_self_intersect = FALSE
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowlines,
      crosswalk_id      = "id",
      cs_widths         = width,     # cross section width of each "id" linestring ("hy_id")
      num               = 20,                            # number of cross sections per "id" linestring ("hy_id")
      smooth            = FALSE,                          # smooth lines
      densify           = NULL,                             # densify linestring points
      rm_self_intersect = FALSE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    
    # plot(flowlines$geometry)
    # plot(transects$geometry, add = T)
    
    testthat::expect_true(
      all(
        lengths(sf::st_intersects(transects, flowlines)) == 1
      )
    )
    
  }
  
  
})

testthat::test_that("flowlines.gpkg, transects never cross flowlines more than once
                    whether rm_self_intersect is TRUE or FALSE", {
  
  flowlines <-
    sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) %>% 
    dplyr::mutate(id = as.character(id)) %>% 
    hydroloom::rename_geometry("geometry") %>% 
    dplyr::select(
      id
    ) %>% 
    sf::st_cast("MULTILINESTRING") 
  
  for (width in c(1000, 2500)) {
    
    # rm_self_intersect = TRUE
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowlines,
      crosswalk_id      = "id",
      cs_widths         = width,     # cross section width of each "id" linestring ("hy_id")
      num               = 20,                            # number of cross sections per "id" linestring ("hy_id")
      smooth            = FALSE,                          # smooth lines
      densify           = NULL,                             # densify linestring points
      rm_self_intersect = TRUE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    
    # plot(flowlines$geometry)
    # plot(transects$geometry, add = T)
    
    testthat::expect_true(
      all(
        lengths(sf::st_intersects(transects, flowlines)) == 1
      )
    )
  }
  
  for (width in c(1000, 2500)) {
    # rm_self_intersect = FALSE
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowlines,
      crosswalk_id      = "id",
      cs_widths         = width,     # cross section width of each "id" linestring ("hy_id")
      num               = 20,                            # number of cross sections per "id" linestring ("hy_id")
      smooth            = FALSE,                          # smooth lines
      densify           = NULL,                             # densify linestring points
      rm_self_intersect = FALSE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    
    # plot(flowlines$geometry)
    # plot(transects$geometry, add = T)
    
    testthat::expect_true(
      all(
        lengths(sf::st_intersects(transects, flowlines)) == 1
      )
    )
    
  }
  
  
})




testthat::test_that("cut 2 transects on a single flowline", {
  # flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  # flowline <- flowlines[10, ] 
  # 
  # flowline <- 
  #   flowline %>% 
  #   dplyr::mutate(bf_width = calc_powerlaw_bankful_width(tot_drainage_areasqkm))
  # 
  # transects <- hydrofabric3D::cut_cross_sections(
  #   net               = flowline,
  #   crosswalk_id                = "id",
  #   cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "id" linestring ("hy_id")
  #   num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
  #   smooth            = TRUE,                          # smooth lines
  #   densify           = 3,                             # densify linestring points
  #   rm_self_intersect = TRUE,                          # remove self intersecting transects
  #   fix_braids        = FALSE
  # )
  # 
  # # plot(transects$geometry)
  # # plot(flowline, add = T)
  # 
  # # test that the number of rows is right and all cs IDs are present
  # testthat::expect_equal(nrow(transects), 10)
  # testthat::expect_equal(transects$cs_id, c(1:10))
  # 
  # testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  # 
  # # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  # testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
  # testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
  # # testthat::expect_equal(as.character(transects$cs_lengthm)[1], "50")
  # 
  # testthat::expect_lte(max(transects$cs_measure), 100)
  # testthat::expect_gte(min(transects$cs_measure), 0)
})

testthat::test_that("cut 10 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
  # flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
  # 
  # transects <- hydrofabric3D::cut_cross_sections(
  #   net               = flowlines,
  #   crosswalk_id                = "id",
  #   cs_widths         = pmax(50, flowlines$bf_width * 11),     # cross section width of each "id" linestring ("hy_id")
  #   num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
  #   smooth            = TRUE,                          # smooth lines
  #   densify           = 3,                             # densify linestring points
  #   rm_self_intersect = TRUE,                          # remove self intersecting transects
  #   fix_braids        = FALSE
  # )
  # 
  # # plot(transects$geometry)
  # # plot(flowline, add = T)
  # 
  # # test that the number of rows is right and all cs IDs are present
  # testthat::expect_equal(nrow(transects), 10)
  # testthat::expect_equal(transects$cs_id, c(1:10))
  # # test correct column names
  # testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  # 
  # # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  # testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
  # testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
  # # testthat::expect_equal(as.character(transects$cs_lengthm)[1], "50")
  # 
  # testthat::expect_lte(max(transects$cs_measure), 100)
  # testthat::expect_gte(min(transects$cs_measure), 0)
})

# -------------------------------------------------------------------
# ---- hydrofabric::cut_cross_sections() ----
# -------------------------------------------------------------------
# create test data (hy_id = "wb-1004970" from nextgen flowlines)
coords <- matrix(c(968520.8, 1381795, 968471.3, 1381851, 968420.6, 1381874, 
                   968418.1, 1381897, 968436.2, 1381961, 968426.9, 1382022, 
                   968412.6, 1382036,  968211.2, 1382114, 968197.2, 1382148, 
                   968172.4, 1382166,  968029.8, 1382217, 967972.7, 1382319, 
                   967936.7, 1382369,  967835.1, 1382461, 967831.7, 1382514, 
                   967836.6, 1382538, 967764.9, 1382589,  967741.8, 1382615, 
                   967695.0, 1382625, 967639.9, 1382619,  967108.0, 1382436, 
                   967072.6, 1382434,  967038.1, 1382448,  966982.6, 1382491, 
                   966947.4, 1382534,  966945.7, 1382549, 966932.3, 1382555, 
                   966886.3, 1382694,  966876.6, 1382781,  966930.3, 1382957, 
                   966926.8, 1382988,  966873.1, 1383015, 966851.8, 1383046, 
                   966807.0, 1383062, 966779.4, 1383172), 
                 ncol = 2, byrow = TRUE)

# create linestring and Sf dataframe
linestring_geom <- sf::st_linestring(as.matrix(coords))
flowline <- sf::st_as_sf(
  data.frame(hy_id = "wb-1004970", 
             tot_drainage_areasqkm = 3.90825,
             geom = sf::st_geometry(linestring_geom)),
  crs = 5070
  )

# lengthkm and bankful width (power law equation using total draineage area (sq. km))
flowline <- 
  flowline %>% 
  dplyr::mutate(
    lengthkm = as.numeric(sf::st_length(geometry))/1000,
    bf_width = exp(0.700    + 0.365* log(tot_drainage_areasqkm))
  ) %>% 
  dplyr::select(
    hy_id,
    lengthkm,
    tot_drainage_areasqkm,
    bf_width,
    geometry
  )

testthat::test_that("cut 10 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
  CROSSWALK_ID                = "hy_id"
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    crosswalk_id                = CROSSWALK_ID,
    cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "crosswalk_id" linestring ("hy_id")
    num               = 10,                            # number of cross sections per "crosswalk_id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
    
  # plot(transects$geometry)
  # plot(flowline, add = T)
    
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 10)
  testthat::expect_equal(transects$cs_id, c(1:10))
  
  # test correct column names 
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  # testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", "lengthm", "sinuosity","geometry"))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
  # testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
  # testthat::expect_equal(as.character(transects$cs_lengthm)[1], "50")
  
  testthat::expect_lte(max(transects$cs_measure), 100)
  testthat::expect_gte(min(transects$cs_measure), 0)
})

testthat::test_that("cut 20 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
  CROSSWALK_ID              = "hy_id"
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    crosswalk_id                = CROSSWALK_ID,
    cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "crosswalk_id" linestring ("hy_id")
    num               = 20,                            # number of cross sections per "crosswalk_id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  # transects
  # plot(flowline$geometry[1])
  # plot(transects$geometry, add = T)
  
  # test correct column names 
  testthat::expect_true(check_transect_output_cols(transects, CROSSWALK_ID))
  # testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", "lengthm", "sinuosity","geometry"))
   
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 20)
  testthat::expect_equal(transects$cs_id, c(1:20))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
  # testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 50-2, 50+2)))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 50-2, 50+2)))

  # expect cs_measure values to be between 0-100
  testthat::expect_lte(max(transects$cs_measure), 100)
  testthat::expect_gte(min(transects$cs_measure), 0)
  
  
})

testthat::test_that("cut 100 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    crosswalk_id                = "hy_id",
    cs_widths         = 100,     # cross section width of each "crosswalk_id" linestring ("hy_id")
    num               = 100,                            # number of cross sections per "crosswalk_id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  # transects
  # plot(flowline$geometry[1])
  # plot(transects$geometry, add = T)
  # 
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 69)
  testthat::expect_equal(transects$cs_id, c(1:69))
  
  # test correct column names 
  testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", 
                                              # "lengthm", 
                                              "sinuosity","geometry"))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 100-2, 100+2))
  # testthat::expect_true(dplyr::between(transects$lengthm[1], 100-2, 100+2))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 100-2, 100+2)))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 100-2, 100+2)))
  # testthat::expect_equal(as.character(transects$cs_lengthm)[1], "50")
  
  # expect cs_measure values to be between 0-100
  testthat::expect_lte(max(transects$cs_measure), 100)
  testthat::expect_gte(min(transects$cs_measure), 0)
  
  
})

testthat::test_that("huge cs_lengthm with remove intersections)", {
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    crosswalk_id                = "hy_id",
    cs_widths         = 2500,     # cross section width of each "crosswalk_id" linestring ("hy_id")
    num               = 50,                            # number of cross sections per "crosswalk_id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  # transects
  # plot(flowline$geometry[1])
  # plot(transects$geometry, add = T)
  
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 9)
  testthat::expect_equal(transects$cs_id, c(1:9))
  
  # test correct column names 
  testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", 
                                              # "lengthm", 
                                              "sinuosity","geometry"))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 2500-2, 2500+2))
  # testthat::expect_true(dplyr::between(transects$lengthm[1], 2500-2, 2500+2))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 2500-2, 2500+2)))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 2500-2, 2500+2)))
  
  # expect cs_measure values to be between 0-100
  testthat::expect_lte(max(transects$cs_measure), 100)
  testthat::expect_gte(min(transects$cs_measure), 0)
  
  
})

testthat::test_that("error on invalid num argument)", {
  
  testthat::expect_error(
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    crosswalk_id                = "hy_id",
    cs_widths         = 50,    
    num               = "bad inputs", 
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  ))
})

testthat::test_that("error on invalid net argument)", {
  
  testthat::expect_error(
    transects <- hydrofabric3D::cut_cross_sections(
      net               = data.frame(),
      crosswalk_id                = "hy_id",
      cs_widths         = 50,    
      num               = 10, 
      smooth            = TRUE,                          # smooth lines
      densify           = 3,                             # densify linestring points
      rm_self_intersect = TRUE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    )
})

testthat::test_that("error on invalid 'crosswalk_id' argument value (logical))", {
  
  testthat::expect_error(
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowline,
      crosswalk_id                = FALSE,
      cs_widths         = 50,    
      num               = 10, 
      smooth            = TRUE,                          # smooth lines
      densify           = 3,                             # densify linestring points
      rm_self_intersect = TRUE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
  )
})

# testthat::test_that("cut 20 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
#   
#   transects <- hydrofabric3D::cut_cross_sections(
#     net               = flowline,
#     crosswalk_id                = "hy_id",
#     cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "crosswalk_id" linestring ("hy_id")
#     num               = 100,                            # number of cross sections per "crosswalk_id" linestring ("hy_id")
#     smooth            = TRUE,                          # smooth lines
#     densify           = 3,                             # densify linestring points
#     rm_self_intersect = TRUE,                          # remove self intersecting transects
#     fix_braids        = FALSE
#   )
#   
#   transects
#   plot(flowline$geometry[1])
#   plot(transects$geometry, add = T)
#   
#   testthat::expect_equal(nrow(transects), 17)
#   # testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_widths", "cs_measure", "ds_distance", "lengthm", "sinuosity","geometry"))
#   testthat::expect_equal(as.numeric(sf::st_length( transects$geometry)), rep(50, 17))
#   
# })
