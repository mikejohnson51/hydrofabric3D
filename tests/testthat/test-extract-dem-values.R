library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::extract_dem_values() ----
# -------------------------------------------------------------------
testthat::test_that("extract values from DEM are correct", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>%
    # dplyr::slice(1:3) %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::rename(!!sym(ID_COL) := id) %>% 
    dplyr::select(
      dplyr::any_of(ID_COL), 
      tot_drainage_areasqkm,
      bf_width,
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <- dplyr::select(transects,
                             dplyr::any_of(ID_COL),
                             cs_id,
                             cs_lengthm
  )
  
  cs <- hydrofabric3D:::add_points_per_cs(
    cs              = transects,
    points_per_cs   = NULL,
    min_pts_per_cs  = 10,
    dem             = DEM_PATH
  )
  
  extracted_pts <- 
    cs %>% 
    extract_dem_values(
      crosswalk_id = ID_COL,
      dem = DEM_PATH
    )
  
  # Make sure the Z and relative distance columns are added 
  has_Z_and_relative_distance_cols <- all(c("Z", "relative_distance") %in% names(extracted_pts))
  testthat::expect_true(has_Z_and_relative_distance_cols)
  
  avg_Z <- 
    extracted_pts %>% 
    dplyr::group_by(hy_id, cs_id) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::summarise(
      meanZ = mean(Z)
    )
 
  testthat::expect_true(
    dplyr::between(dplyr::filter(avg_Z, hy_id == "wb-1003259", cs_id == 1)$meanZ, 
                   265, 
                   266)
   ) 
  testthat::expect_true(
        dplyr::between(dplyr::filter(avg_Z, hy_id == "wb-1003259", cs_id == 2)$meanZ, 
                       244, 
                       245)
        )
  
  testthat::expect_true(
    dplyr::between(dplyr::filter(avg_Z, hy_id == "wb-1003259", cs_id == 3)$meanZ, 
                   223, 
                   224)
  )
  
})

testthat::test_that("extract_dem_values throws errors when transects (cs) input is missing some / all required columns ", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>%
    # dplyr::slice(1:3) %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::rename(!!sym(ID_COL) := id) %>% 
    dplyr::select(
      dplyr::any_of(ID_COL), 
      tot_drainage_areasqkm,
      bf_width,
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  )
  
  transects <- 
    transects %>% 
    dplyr::select(dplyr::any_of(ID_COL),
                  cs_id,
                  cs_lengthm
                  ) %>% 
    hydrofabric3D:::add_points_per_cs(
      points_per_cs    = 10,
      min_pts_per_cs   = MIN_PTS_PER_CS, 
      dem              = DEM_PATH
    ) %>% 
    dplyr::relocate(points_per_cs)
  
  testthat::expect_error(
      hydrofabric3D:::extract_dem_values(
        cs             = dplyr::select(transects,
                                       cs_id
        ),
        crosswalk_id   = ID_COL,
        dem            = DEM_PATH
      )
    )
  
  testthat::expect_error(
    hydrofabric3D:::extract_dem_values(
      cs             = dplyr::select(transects,
                                     dplyr::any_of(ID_COL),
                                     cs_lengthm
      ),
      crosswalk_id   = ID_COL,
      dem            = DEM_PATH
    )
  )
  
  testthat::expect_error(
    hydrofabric3D:::extract_dem_values(
      cs             = dplyr::select(transects,
                                     dplyr::any_of(ID_COL),
                                     cs_lengthm, points_per_cs
      ),
      crosswalk_id   = ID_COL,
      dem            = DEM_PATH
    )
  )
  
  })

testthat::test_that("error giving empty geometries", {
  
  flowlines    <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # flowlines    <- dplyr::slice(flowlines, 1)
  
  MIN_BF_WIDTH       <- 50
  ID_COL             <- "hy_id"
  NUM_OF_TRANSECTS   <- 3
  
  # Cross section point inputs
  DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
  POINTS_PER_CS     <- NULL
  MIN_PTS_PER_CS    <- 10
  
  flowlines <-
    flowlines %>% 
    dplyr::slice(1) %>%
    # dplyr::slice(1:3) %>% 
    add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
    dplyr::rename(!!sym(ID_COL) := id) %>% 
    dplyr::select(
      dplyr::any_of(ID_COL), 
      tot_drainage_areasqkm,
      bf_width,
      geom
    ) 
  
  transects <- cut_cross_sections(
    net = flowlines,
    id  = ID_COL,  
    num = NUM_OF_TRANSECTS
  ) 
  
  transects <- hydrofabric3D:::add_points_per_cs(
    cs               = transects, 
    points_per_cs    = 10,
    min_pts_per_cs   = MIN_PTS_PER_CS, 
    dem              = DEM_PATH
  ) %>% 
    dplyr::relocate(points_per_cs)
  
  # transects %>% sf::st_buffer(5)
  empty_sf_geoms <-
    geos::geos_empty() %>% 
    sf::st_as_sf() %>% 
    sf::st_set_crs(sf::st_crs(transects)) %>% 
    sf::st_as_sfc()
  
  sf::st_geometry(transects) <- rep(empty_sf_geoms, nrow(transects))
  
  testthat::expect_error(
    hydrofabric3D:::extract_dem_values(
      transects, 
      ID_COL, 
      DEM_PATH
    )
  )

})