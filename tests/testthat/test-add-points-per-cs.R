

library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)

# -------------------------------------------------------------------
# ---- hydrofabric3D::add_points_per_cs() ----
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

# flowline
# flowline$geometry %>% plot()

testthat::test_that("cut 10 transects along single flowline, then add cross section points column, NULL points, and 10 min", {

  dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
  # 
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    id                = "hy_id",
    cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "id" linestring ("hy_id")
    num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  # # # add points per cross sections 
  # testthat::expect_error(hydrofabric3D:::add_points_per_cs(
  #   cs             = transects,
  #   points_per_cs  = NULL,
  #   min_pts_per_cs = 10,
  #   dem            = dem
  # ))

})