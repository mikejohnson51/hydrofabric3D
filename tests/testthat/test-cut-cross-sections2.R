library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)
# devtools::load_all()

# # -------------------------------------------------------------------
# # ---- hydrofabric::cut_cross_sections() ----
# # -------------------------------------------------------------------
# check_transect_output_cols <- function(transects, id = "hydrofabric_id") {
#   
#   if(is.null(id)) {
#     id = "hydrofabric_id"
#   }
#   
#   expected_cols <- c(id, "cs_id","cs_lengthm", "cs_measure", "ds_distance", 
#                      "lengthm", "sinuosity", "geometry")
#                      
#   return(
#     all(expected_cols %in% names(transects)) && length(expected_cols) == length(names(transects))
#     )
#   }
# 
# testthat::test_that("flowlines only (set 'id' to NULL) sf dataframe, checking proper default ID", {
#   flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#   
#   # flowlines <- 
#   #   flowlines %>% 
#   #   dplyr::select(hy_id = id, geometry = geom)
#   # transects <- hydrofabric3D::cut_cross_sections(
#   #   net = flowlines,
#   #   id = "hy_id",
#   #   num = 10
#   # )
#   # 
#   # transects2 <- cut_cross_sections(
#   #   net = flowlines,
#   #   id = "hy_id",
#   #   num = 10
#   # )
#   # 
#   # all(transects == transects2)
#   
#   start_unique_ids_count <- length(unique(flowlines$id))
#   
#   flowlines <- dplyr::select(flowlines, geom)
#   
#   transects <- cut_cross_sections(
#     net = flowlines,
#     num = 10
#   )
#   
#   end_unique_ids_count <- length(unique(transects$hydrofabric_id))
#   
#   testthat::expect_true(end_unique_ids_count == start_unique_ids_count)
#   
# })
# 
# testthat::test_that("flowlines with correct 'id' column named 'hy_id' provided", {
#   flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#   
#   id_col = "hy_id"
#   NUMBER_OF_TRANSECTS = 10
#   flowlines <-
#     flowlines %>% 
#     dplyr::rename(!!sym(id_col) := id) %>% 
#     dplyr::select(dplyr::any_of(id_col), geom)
#   
#   start_unique_ids_count <- length(unique(flowlines[[id_col]]))
#   
#   transects <- cut_cross_sections(
#     net = flowlines,
#     id = id_col,
#     num = NUMBER_OF_TRANSECTS,
#     cs_widths  = 5
#   )
#   
#   end_unique_ids_count <- length(unique(transects[[id_col]]))
#   
#   testthat::expect_true(end_unique_ids_count == start_unique_ids_count)
#   testthat::expect_true(id_col %in% names(transects))
#   
#   all_ids_have_correct_number_transects <- 
#     transects %>% 
#     sf::st_drop_geometry() %>% 
#     dplyr::group_by(dplyr::across(dplyr::any_of(id_col))) %>% 
#     dplyr::count() %>% 
#     dplyr::mutate(
#       correct_number_transects = dplyr::case_when(
#         n == NUMBER_OF_TRANSECTS ~ TRUE,
#         TRUE                     ~ FALSE
#       )
#     ) %>% 
#     .$correct_number_transects %>% 
#     all()
#   
#   testthat::expect_true(all_ids_have_correct_number_transects)  
# 
# })
# testthat::test_that("flowlines with correct 'id' column named 'hy_id' provided", {
#   
#   ID_COLS = c("hy_id", "id")
#   
#   for (id_col in ID_COLS) {
#     
#     NUMBER_OF_TRANSECTS = 10
#     flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#     flowlines <-
#       flowlines %>% 
#       dplyr::rename(!!sym(id_col) := id) %>% 
#       dplyr::select(dplyr::any_of(id_col), geom)
#     
#     start_unique_ids_count <- length(unique(flowlines[[id_col]]))
#     
#     transects <- cut_cross_sections(
#       net = flowlines,
#       crosswalk_id = id_col,
#       num = NUMBER_OF_TRANSECTS,
#       cs_widths  = 5
#     )
#     
#     end_unique_ids_count <- length(unique(transects[[id_col]]))
#     
#     testthat::expect_true(end_unique_ids_count == start_unique_ids_count)
#     testthat::expect_true(id_col %in% names(transects))
#     
#     all_ids_have_correct_number_transects <- 
#       transects %>% 
#       sf::st_drop_geometry() %>% 
#       dplyr::group_by(dplyr::across(dplyr::any_of(id_col))) %>% 
#       dplyr::count() %>% 
#       dplyr::mutate(
#         correct_number_transects = dplyr::case_when(
#           n == NUMBER_OF_TRANSECTS ~ TRUE,
#           TRUE                     ~ FALSE
#         )
#       ) %>% 
#       .$correct_number_transects %>% 
#       all()
#     
#     testthat::expect_true(all_ids_have_correct_number_transects)  
#     testthat::expect_true(check_transect_output_cols(transects, id_col))
#   } 
# })
# 
# testthat::test_that("5 meter long transects distance check", {
#   
#     NUMBER_OF_TRANSECTS = 10
#     EXPECTED_WIDTH = 5
#     
#     id_col = 'id'
#     
#     flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#     flowlines <-
#       flowlines %>% 
#       dplyr::rename(!!sym(id_col) := id) %>% 
#       dplyr::select(dplyr::any_of(id_col), geom)
#     
#     transects <- cut_cross_sections(
#       net = flowlines,
#       crosswalk_id = id_col,
#       num = NUMBER_OF_TRANSECTS,
#       cs_widths  = EXPECTED_WIDTH
#     )
#     
#     is_correct_length_transects <- (
#                           transects %>% 
#                             sf::st_length() %>% 
#                             as.numeric() %>% 
#                             round() %>% 
#                             as.integer() == EXPECTED_WIDTH
#                           ) %>% 
#                           all()
#                            
#       testthat::expect_true(is_correct_length_transects)
#       testthat::expect_true(check_transect_output_cols(transects, id_col))
#     
# })
# 
# 
# testthat::test_that("check correct output columns for transects", {
#   
#     NUMBER_OF_TRANSECTS = 10
#     EXPECTED_WIDTH = 5
#     
#     id_col = NULL
#     
#     flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#     flowlines <-
#       flowlines %>%
#       # dplyr::rename(!!sym(id_col) := id) %>% 
#       dplyr::select(geom)
#     
#     transects <- cut_cross_sections(
#       net = flowlines,
#       crosswalk_id = id_col,
#       num = NUMBER_OF_TRANSECTS,
#       cs_widths  = EXPECTED_WIDTH
#     )
#     
#     testthat::expect_true(check_transect_output_cols(transects, id_col))
#     
#     is_correct_length_transects <- (
#                           transects %>% 
#                             sf::st_length() %>% 
#                             as.numeric() %>% 
#                             round() %>% 
#                             as.integer() == EXPECTED_WIDTH
#                           ) %>% 
#                           all()
#                            
#       testthat::expect_true(is_correct_length_transects)
#       testthat::expect_true(check_transect_output_cols(transects, id_col))
#     
# })
# 
# 
# testthat::test_that("flowline only (no other input cols) sf dataframe, checking transect number and no self intersections", {
#   flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#   id_col <- "hydrofabric_id"
#   flowline <-
#     flowlines[10, ] %>%
#     dplyr::select(geom)
#   
#   transects <- cut_cross_sections(
#     net = flowline,
#     num = 10
#   )
#   
#   testthat::expect_true(nrow(transects) == 10)
#   
#   has_no_self_intersctions <- all(lengths(sf::st_intersects(transects)) == 1)
#   testthat::expect_true(has_no_self_intersctions)
#   testthat::expect_true(check_transect_output_cols(transects, id_col))
#   transects <- cut_cross_sections(
#     net = flowline,
#     num = 20
#   )
#   
#   testthat::expect_true(nrow(transects) == 20)
#   
#   has_no_self_intersctions <- all(lengths(sf::st_intersects(transects)) == 1)
#   testthat::expect_true(has_no_self_intersctions)
#   testthat::expect_true(check_transect_output_cols(transects, id_col))
#   transects <- cut_cross_sections(
#     net = flowline,
#     num = 100
#   )
#   
#   testthat::expect_true(nrow(transects) == 96)
#   
#   has_no_self_intersctions <- all(lengths(sf::st_intersects(transects)) == 1)
#   testthat::expect_true(has_no_self_intersctions)
#   testthat::expect_true(check_transect_output_cols(transects, id_col))
#   # trans <- cut_cross_sections(
#   #   net = flowline,
#   #   cs_widths = 2500,
#   #   num = 1,
#   #   rm_self_intersect = T
#   # ) 
#   # plot(flowline, add = F)
#   # trans %>% .$geometry %>% plot(add = T) 
#   # trans[lengths(sf::st_intersects(trans)) == 1, ] 
#   # trans[lengths(sf::st_intersects(trans, flowline)) == 1, ]
#   # 
#   # mapview::mapview(trans) + flowline
#   # cut_cross_sections()
#   # 
#   # transects <- hydrofabric3D::cut_cross_sections(
#   #   net               = flowline
#   #   # id                = NULL,
#   #   # cs_widths         = c(10) 
#   # )
#   
# })
# testthat::test_that("cut 2 transects on a single flowline", {
#   # flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#   # flowline <- flowlines[10, ] 
#   # 
#   # flowline <- 
#   #   flowline %>% 
#   #   dplyr::mutate(bf_width = calc_powerlaw_bankful_width(tot_drainage_areasqkm))
#   # 
#   # transects <- hydrofabric3D::cut_cross_sections(
#   #   net               = flowline,
#   #   id                = "id",
#   #   cs_widths         = pmax(50, flowline$bf_width * 11),     # cross section width of each "id" linestring ("hy_id")
#   #   num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
#   #   smooth            = TRUE,                          # smooth lines
#   #   densify           = 3,                             # densify linestring points
#   #   rm_self_intersect = TRUE,                          # remove self intersecting transects
#   #   fix_braids        = FALSE
#   # )
#   # 
#   # # plot(transects$geometry)
#   # # plot(flowline, add = T)
#   # 
#   # # test that the number of rows is right and all cs IDs are present
#   # testthat::expect_equal(nrow(transects), 10)
#   # testthat::expect_equal(transects$cs_id, c(1:10))
#   # 
#   # testthat::expect_true(check_transect_output_cols(transects, id_col))
#   # 
#   # # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
#   # testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
#   # testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
#   # # testthat::expect_equal(as.character(transects$cs_lengthm)[1], "50")
#   # 
#   # testthat::expect_lte(max(transects$cs_measure), 100)
#   # testthat::expect_gte(min(transects$cs_measure), 0)
# })
# 
# testthat::test_that("cut 10 transects along single flowline & remove intersects (power law bankful widths, smooth, densify 3)", {
#   # flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg")) 
#   # 
#   # transects <- hydrofabric3D::cut_cross_sections(
#   #   net               = flowlines,
#   #   id                = "id",
#   #   cs_widths         = pmax(50, flowlines$bf_width * 11),     # cross section width of each "id" linestring ("hy_id")
#   #   num               = 10,                            # number of cross sections per "id" linestring ("hy_id")
#   #   smooth            = TRUE,                          # smooth lines
#   #   densify           = 3,                             # densify linestring points
#   #   rm_self_intersect = TRUE,                          # remove self intersecting transects
#   #   fix_braids        = FALSE
#   # )
#   # 
#   # # plot(transects$geometry)
#   # # plot(flowline, add = T)
#   # 
#   # # test that the number of rows is right and all cs IDs are present
#   # testthat::expect_equal(nrow(transects), 10)
#   # testthat::expect_equal(transects$cs_id, c(1:10))
#   # # test correct column names
#   # testthat::expect_true(check_transect_output_cols(transects, id_col))
#   # 
#   # # Expect cs_lengthm and lengthm are within 2 units of expected value # TODO: might not want to check for equivalency with floating point numbers...
#   # testthat::expect_true(dplyr::between(transects$cs_lengthm[1], 50-2, 50+2))
#   # testthat::expect_true(dplyr::between(transects$lengthm[1], 50-2, 50+2))
#   # # testthat::expect_equal(as.character(transects$cs_lengthm)[1], "50")
#   # 
#   # testthat::expect_lte(max(transects$cs_measure), 100)
#   # testthat::expect_gte(min(transects$cs_measure), 0)
# })
