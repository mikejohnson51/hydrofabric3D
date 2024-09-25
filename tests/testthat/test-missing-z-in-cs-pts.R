
library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")

# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::cross_section_pts() ----
# -------------------------------------------------------------------
testthat::test_that("check that missing NA value is identified and added as a NA Z value for the correct transect", {
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) 

  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
              cs             = transects,
              crosswalk_id   = ID_COL,
              points_per_cs  = POINTS_PER_CS,
              min_pts_per_cs = MIN_PTS_PER_CS,
              dem            = DEM_PATH
            ) 
  
  invalid_cs <-     
    cs_pts %>% 
    dplyr::filter(
      cs_id %in% CS_IDS_MISSING_Z
    ) 
  
  correct_transects_missing_z <- 
    invalid_cs  %>% 
    dplyr::pull(Z) %>% 
    is.na() %>% 
    any()
  
  testthat::expect_true(correct_transects_missing_z)
  
  all_cs_have_atleast_min_num_pts <- 
    all(
      cs_pts %>% 
        sf::st_drop_geometry() %>% 
        add_tmp_id(x = ID_COL) %>% 
        dplyr::group_by(tmp_id) %>% 
        dplyr::count() %>% 
        dplyr::pull(n) >= MIN_PTS_PER_CS
    )
  
  testthat::expect_true(all_cs_have_atleast_min_num_pts)
  
})

# TODO: this test / behavior needs improving, if there are missing Z values, 
# TODO: how should we classify those points AND the neighboring points that actually do have Z values
# TODO: Right now this test passes, but maybe it shouldn't...
testthat::test_that("classify_points() - (1 transects) - cs_pts that have 1+ NA values, only a bottom is classified?", {
  
  TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
  ID_COL              <- "hy_id"
  CS_IDS_MISSING_Z    <- c(66)
  
  # Cross section point inputs
  DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
  POINTS_PER_CS       <- NULL
  MIN_PTS_PER_CS      <- 10
  
  transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) %>% 
                    dplyr::filter(cs_id %in% CS_IDS_MISSING_Z)
  
  # mapview::mapview(raster::raster(DEM_PATH)) + transects
  
  cs_pts <- hydrofabric3D::cross_section_pts(
    cs             = transects,
    crosswalk_id   = ID_COL,
    points_per_cs  = POINTS_PER_CS,
    min_pts_per_cs = MIN_PTS_PER_CS,
    dem            = DEM_PATH
  ) 
  
  classified_pts <- hydrofabric3D::classify_points(cs_pts, crosswalk_id = ID_COL)
 
  has_bottom_point_type <- c("bottom") %in% classified_pts$point_type
  testthat::expect_true(has_bottom_point_type)
  
})


# testthat::test_that("check that missing NA value is identified and added as a NA Z value", {
  
  # flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines_missing_depth.gpkg")) 
  # # flowlines    <- dplyr::slice(flowlines, 1)
  # 
  # dem <- raster::raster(DEM_PATH)
  # 
  # MIN_BF_WIDTH       <- 25
  # ID_COL             <- "hy_id"
  # NUM_OF_TRANSECTS   <- 150
  # 
  # # Cross section point inputs
  # DEM_PATH          <- testthat::test_path("testdata", "dem_missing_depth.tif")
  # POINTS_PER_CS     <- NULL
  # MIN_PTS_PER_CS    <- 10
  # 
  # flowlines <- 
  #   flowlines %>% 
  #   prep_flowlines_for_transect_cuts(ID_COL, MIN_BF_WIDTH)
  # transects <- 
  #   flowlines %>% 
  #   hydrofabric3D::cut_cross_sections(
  #     id = ID_COL,
  #     num = NUM_OF_TRANSECTS,
  #     cs_widths  = flowlines$bf_width
  #   ) 
  # transects_missing_depth <- 
  #   transects %>% 
  #   dplyr::filter(cs_id %in% c(64, 65, 66, 67, 68))
  # sf::write_sf(transects_missing_depth)
    # dplyr::select(dplyr::any_of(ID_COL),
                  # cs_id,
                  # cs_lengthm
                  # )
  # mapview::mapview(dem) + transects  
  # })

# library(sf)
# library(dplyr)
# library(geos)
# library(terra)

# cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
# cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_11.gpkg")
# cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_classified_11.gpkg")
  # dplyr::rename(hy_id = id)
# flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
# transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_11.gpkg")

# library(sf)
# library(dplyr)
# library(geos)
# library(terra)
# 
# # NOTE: Need nextgen flowlines for VPU 13
# net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_11.gpkg")
# 
# # flowlines with known issues (missing Z data in the DEM)
# missing_Z_ids <- c("wb-2146599")
# # missing_Z_ids <- c("wb-2131572", "wb-2146599")
# 
# neighbor_ids <- dplyr::pull(
#                 tidyr::pivot_longer(
#                   dplyr::select(
#                     sf::st_drop_geometry(
#                     dplyr::mutate(  
#                       dplyr::filter(net, id %in% missing_Z_ids), 
#                       to_id = gsub("nex-", "wb-", foi$toid)
#                       )
#                     ), 
#                     id, to_id
#                   ),
#                     cols      = c(id, to_id), 
#                     names_to  = "name", 
#                     values_to = "id"
#                     ), 
#                 id
#               )
#     
# foi <- 
#   net %>% 
#   dplyr::filter(id %in% neighbor_ids) %>% 
#   dplyr::select(id, 
#                 lengthkm, mainstem, tot_drainage_areasqkm, 
#                 geometry = geom
#                 )
# 
# sf::write_sf(foi, "tests/testthat/testdata/invalid_flowlines.gpkg")

# TODO: problematic hy_ids in VPU 13 (have NA valid_banks / has_relief)
# which(transects_to_check$hy_id %in% c("wb-2131572", "wb-2146599"))
