library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)

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
    
  # plot(transects$geometry)
  # plot(flowline, add = T)
    
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 10)
  testthat::expect_equal(transects$cs_id, c(1:10))
  # test correct column names 
  testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", "lengthm", "sinuosity","geometry"))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
  testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
  # testthat::expect_equal(as.character(transects$cs_lengthm)[1], "50")
  
  testthat::expect_lte(max(transects$cs_measure), 100)
  testthat::expect_gte(min(transects$cs_measure), 0)
})

testthat::test_that("cut 20 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    id                = "hy_id",
    cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "id" linestring ("hy_id")
    num               = 20,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  # transects
  # plot(flowline$geometry[1])
  # plot(transects$geometry, add = T)
  
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 20)
  testthat::expect_equal(transects$cs_id, c(1:20))
  
  # test correct column names 
  testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", "lengthm", "sinuosity","geometry"))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
  testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 50-2, 50+2)))
  testthat::expect_true(all(dplyr::between(transects$cs_lengthm, 50-2, 50+2)))

  # expect cs_measure values to be between 0-100
  testthat::expect_lte(max(transects$cs_measure), 100)
  testthat::expect_gte(min(transects$cs_measure), 0)
  
  
})

testthat::test_that("cut 100 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
  
  transects <- hydrofabric3D::cut_cross_sections(
    net               = flowline,
    id                = "hy_id",
    cs_widths         = 100,     # cross section width of each "id" linestring ("hy_id")
    num               = 100,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  transects
  plot(flowline$geometry[1])
  plot(transects$geometry, add = T)
  
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 69)
  testthat::expect_equal(transects$cs_id, c(1:69))
  
  # test correct column names 
  testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", "lengthm", "sinuosity","geometry"))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 100-2, 100+2))
  testthat::expect_true(dplyr::between(transects$lengthm[1], 100-2, 100+2))
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
    id                = "hy_id",
    cs_widths         = 2500,     # cross section width of each "id" linestring ("hy_id")
    num               = 50,                            # number of cross sections per "id" linestring ("hy_id")
    smooth            = TRUE,                          # smooth lines
    densify           = 3,                             # densify linestring points
    rm_self_intersect = TRUE,                          # remove self intersecting transects
    fix_braids        = FALSE
  )
  
  transects
  plot(flowline$geometry[1])
  plot(transects$geometry, add = T)
  
  # test that the number of rows is right and all cs IDs are present
  testthat::expect_equal(nrow(transects), 9)
  testthat::expect_equal(transects$cs_id, c(1:9))
  
  # test correct column names 
  testthat::expect_equal(names(transects),  c("hy_id","cs_id","cs_lengthm", "cs_measure", "ds_distance", "lengthm", "sinuosity","geometry"))
  
  # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
  testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 2500-2, 2500+2))
  testthat::expect_true(dplyr::between(transects$lengthm[1], 2500-2, 2500+2))
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
    id                = "hy_id",
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
      id                = "hy_id",
      cs_widths         = 50,    
      num               = 10, 
      smooth            = TRUE,                          # smooth lines
      densify           = 3,                             # densify linestring points
      rm_self_intersect = TRUE,                          # remove self intersecting transects
      fix_braids        = FALSE
    )
    )
})

testthat::test_that("error on invalid net argument)", {
  
  testthat::expect_error(
    transects <- hydrofabric3D::cut_cross_sections(
      net               = flowline,
      id                = NULL,
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
#     id                = "hy_id",
#     cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "id" linestring ("hy_id")
#     num               = 100,                            # number of cross sections per "id" linestring ("hy_id")
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
