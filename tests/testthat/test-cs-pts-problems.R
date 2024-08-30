# 
# library(testthat)
# library(dplyr)
# library(sf)
# # # library(hydrofabric3D)
# 
# source("testing_utils.R")
# # source("tests/testthat/testing_utils.R")
# 
# # devtools::load_all()
# 
# # -------------------------------------------------------------------
# # ---- hydrofabric::cross_section_pts() ----
# # -------------------------------------------------------------------
# # ----------------------------------------
# 
# library(sf)
# library(dplyr)
# library(geos)
# library(terra)
# 
# # # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
# # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_11.gpkg")
# # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_classified_11.gpkg")
# 
# # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_classified_11_2.gpkg")
# 
# net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_11.gpkg") 
#   # dplyr::rename(hy_id = id)
# 
# flowlines <- 
#   net %>% 
#   dplyr::filter(mainstem %in% c(203930))
#   # dplyr::filter(hy_id %in% c("wb-2131572"), mainstem %in% c(203930))
# 
# # flowlines$id == "wb-2131572"
# 
# # # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
# transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_11.gpkg")
# # 
# transects <-
#   transects %>%
#   dplyr::filter(hy_id %in% flowlines$id)
# 
# MIN_BF_WIDTH       <- 50
# ID_COL             <- "hy_id"
# NUM_OF_TRANSECTS   <- 10
# 
# # Cross section point inputs
# # DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
# DEM_PATH = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/1/TIFF/USGS_Seamless_DEM_1.vrt"
# POINTS_PER_CS     <- NULL
# MIN_PTS_PER_CS    <- 10
# PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF = 0.01
# EXTENSION_PCT = 0.5
# 
# # flowlines <-
# #   flowlines %>% 
# #   # dplyr::slice(1) %>%
# #   # dplyr::slice(1:3) %>% 
# #   add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
# #   dplyr::rename(!!sym(ID_COL) := id) %>% 
# #   dplyr::select(
# #     dplyr::any_of(ID_COL), 
# #     tot_drainage_areasqkm,
# #     bf_width,
# #     geom
# #   ) 
# # 
# # transects <- cut_cross_sections(
# #   net = flowlines,
# #   id  = ID_COL,  
# #   num = NUM_OF_TRANSECTS,
# #   cs_widths = flowlines$bf_width,
# #   smooth            = TRUE,                          # smooth lines
# #   densify           = 3,                             # densify linestring points
# #   rm_self_intersect = TRUE,
# #   fix_braids = FALSE,
# #   add = TRUE
# # )
# 
# system.time({
#   
#   # get cross section point elevations
#   start_cs_pts <- hydrofabric3D::cross_section_pts(
#     
#     cs             = transects,
#     crosswalk_id   = "hy_id",
#     points_per_cs  = NULL,
#     min_pts_per_cs = 10,
#     dem            = DEM_PATH
#   )
#   
# })
# 
# start_cs_pts %>% 
#   dplyr::group_by(hy_id, cs_id) %>% 
#   dplyr::filter(any(is.na(Z)))
# 
# cs_pts2 <- 
#   start_cs_pts %>% 
#   # dplyr::group_by(hy_id, cs_id) %>% 
#   # dplyr::filter(!any(is.na(Z))) %>% 
#   # dplyr::ungroup() %>% 
#   drop_incomplete_cs_pts("hy_id") %>% 
#   hydrofabric3D::classify_points(
#     crosswalk_id             = "hy_id", 
#     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
#   )  
# 
# # start_cs_pts %>% drop_incomplete_cs_pts("hy_id")
# 
# system.time({
#   fixed_pts <- hydrofabric3D::get_improved_cs_pts(
#     cs_pts         = cs_pts2,    # cross section points generated from hydrofabric3D::cross_section_pts()
#     net            = dplyr::rename(flowlines, hy_id = id),    # original flowline network
#     # net            = flines,    # original flowline network
#     transects      = transects, # original transect lines
#     crosswalk_id   = "hy_id",
#     points_per_cs  = NULL, 
#     min_pts_per_cs = 10, # number of points per cross sections
#     dem            = DEM_PATH, # DEM to extract points from
#     scale          = EXTENSION_PCT, # How far to extend transects if the points need to be rechecked
#     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF, # percent of cross sections length to be needed in relief calculation to consider cross section to "have relief"
#     fix_ids = FALSE,
#     verbose = TRUE
#   )
# })
# 
# # ------------------------------------------------------------------------------------------------------
# # get_improved_cs_pts()
# # ------------------------------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------------
# 
# get_improved_cs_pts = function(
#     cs_pts         = NULL,   
#     net            = NULL,
#     transects      = NULL,
#     crosswalk_id   = NULL,
#     points_per_cs  = NULL,
#     min_pts_per_cs = 10,
#     dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
#     scale          = 0.5,
#     pct_of_length_for_relief = 0.01,
#     fix_ids        = FALSE,
#     verbose        = TRUE
# ) {
#   # ----------------------------------------
#   
#   cs_pts         = cs_pts2   # cross section points generated from hydrofabric3D::cross_section_pts()
#   net            = dplyr::rename(flowlines, hy_id = id)    # original flowline network
#   # net            = flines,    # original flowline network
#   transects      = transects # original transect lines
#   crosswalk_id   = "hy_id"
#   points_per_cs  = NULL
#   min_pts_per_cs = 10 # number of points per cross sections
#   dem            = DEM_PATH # DEM to extract points from
#   scale          = EXTENSION_PCT # How far to extend transects if the points need to be rechecked
#   pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF # percent of cross sections length to be needed in relief calculation to consider cross section to "have relief"
#   fix_ids = FALSE
#   verbose = TRUE
#   
#   # library(sf)
#   # library(dplyr)
#   # library(geos)
#   # library(terra)
#   # 
#   # # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
#   # # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_11.gpkg")
#   # cs_pts <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_cs_pts_classified_11.gpkg")
#   # net <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_11.gpkg") %>%
#   #   dplyr::rename(hy_id = id)
#   # # flowlines <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_flines_06.gpkg")
#   # transects <- sf::read_sf("/Users/anguswatters/Desktop/test_improve_transects_11.gpkg")
#   # 
#   # points_per_cs  = NULL
#   # min_pts_per_cs = 10
#   # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
#   # scale          = 0.5
#   # pct_of_length_for_relief = 0.01
#   # fix_ids        = FALSE
#   # crosswalk_id = "hy_id"
#   # verbose        = TRUE
#   # devtools::load_all()
#   # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
#   # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines.gpkg")
#   # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects.gpkg")
#   
#   # ----------------------------------------
#   
#   # make a unique ID if one is not given (NULL 'id')
#   if(is.null(crosswalk_id)) {
#     # net <- add_hydrofabric_id(net) 
#     crosswalk_id  <- 'hydrofabric_id'
#   }
#   
#   if (!crosswalk_id %in% names(cs_pts)) {
#     stop("crosswalk_id '", crosswalk_id, "' is not a column in 'cs_pts' input,\n", 
#          "Please provide a valid 'crosswalk_id' that crosswalks 'cs_pts' to 'transects' and 'net'")
#   }
#   
#   if (!crosswalk_id %in% names(net)) {
#     stop("crosswalk_id '", crosswalk_id, "' is not a column in 'net' input,\n", 
#          "Please provide a valid 'crosswalk_id' that crosswalks 'net' to 'cs_pts' and 'transects'")
#   }
#   
#   if (!crosswalk_id %in% names(transects)) {
#     stop("crosswalk_id '", crosswalk_id, "' is not a column in 'transects' input,\n", 
#          "Please provide a valid 'crosswalk_id' that crosswalks 'transects' to 'cs_pts' and 'net'")
#   }
#   
#   # add a "tmp_id" column to easily index transects by hy_id and cs_id 
#   transects <- hydrofabric3D::add_tmp_id(transects, x = get(crosswalk_id))
#   
#   # rename geometry column to "geom" 
#   cs_pts     <- nhdplusTools::rename_geometry(cs_pts, "geometry")
#   net        <- nhdplusTools::rename_geometry(net, "geometry")
#   transects  <- nhdplusTools::rename_geometry(transects, "geometry")
#   # transects <- nhdplusTools::rename_geometry(transects, "geom")
#   
#   ### ### ## ## ### ## ### ##  ### ### ## ## ### ## ### ##
#   
#   # remove any cross sections points that might be missing Z values (i.e. NA Z values)
#   cs_pts <- drop_incomplete_cs_pts(cs_pts, crosswalk_id)
#   
#   if (verbose) { message("Determining points to reevaluate...") }
#   
#   # add valid_banks and has_relief columns to transects data
#   transects <- 
#     transects %>% 
#     dplyr::left_join(
#       dplyr::ungroup(
#         dplyr::slice(
#           dplyr::group_by(
#             dplyr::select(sf::st_drop_geometry(cs_pts), 
#                           # hy_id, 
#                           dplyr::any_of(crosswalk_id),
#                           cs_id, 
#                           valid_banks, has_relief),
#             # group by the crosswalk_id and cs_id
#             dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))
#             # hy_id, cs_id
#           ), 
#           1)
#       ),
#       by = c(crosswalk_id, "cs_id")
#       # by = c("hy_id", "cs_id")
#     )
#   
#   # if there are no transects that need rectification, return the original cs_pts early with a "is_extended" flag
#   if (!needs_rectification(transects)) {
#     
#     cs_pts <- 
#       cs_pts %>% 
#       dplyr::mutate(
#         is_extended = FALSE
#       ) %>% 
#       dplyr::relocate(geometry, .after = dplyr::last_col())
#     
#     return(cs_pts)
#   }
#   
#   # 0. Split the data into valid and invalid transects
#   # 1. Go through invalid transects
#   # 2. Try to EXTEND, 
#   # 3. and then UPDATE --> (only IF the extended transect does NOT violate any of the intersection rules)
#   # If ALL of the below intersection conditions are TRUE then a given extended transect line will get replace the old transect geometry 
#   # Intersection rules: 
#   # - Newly extended transect intersects with its flowlines AT MOST 1 time
#   # - Newly extended transect does NOT intersect with any of the other NEWLY EXTENDED transect lines
#   # - Newly extended transect does NOT intersect with any of the ORIGINAL transect lines
#   
#   # system.time({
#   
#   # NOTE: extend_invalid_transect_sides() returns the "transects" object with updated attributes for any
#   # extensions that were made (geometries, cs_lengthm, "is_extended" flag) and keeps all the rest of the remaining data in place
#   extended_transects  <- extend_invalid_transect_sides(
#     transects_to_check  = transects, 
#     net                 = net, 
#     crosswalk_id        = crosswalk_id,
#     scale               = scale,
#     direction           = "both",
#     verbose             = verbose
#   )
#   
#   # })
#   
#   # all(hydrofabric3D::add_tmp_id(extended_transects)$tmp_id %in% hydrofabric3D::add_tmp_id(transects)$tmp_id)
#   # all(hydrofabric3D::add_tmp_id(extended_transects)$tmp_id %in% hydrofabric3D::add_tmp_id(extended_transects2)$tmp_id)
#   
#   # Remove unextendable transects from extended_geoms 
#   extended_transects <- dplyr::filter(extended_transects, is_extended)
#   
#   # nrow(extended_transects) + nrow(good_to_go_transects) == nrow(transects)
#   
#   # add cross section points to extended cross sections
#   extended_transects <- add_points_per_cs(
#     cs             = extended_transects,
#     # cs             = trans_to_extend,
#     # cs             = dplyr::slice(extended_geoms , 1:100),
#     points_per_cs  = points_per_cs,
#     min_pts_per_cs = min_pts_per_cs,
#     dem            = dem
#   )
#   
#   if (verbose) { message("Extracting new DEM values..")}
#   
#   # extract DEM values for newly extended cross sections
#   extended_pts <- extract_dem_values(
#     cs           = extended_transects, 
#     crosswalk_id = crosswalk_id, 
#     dem          = dem
#   )
#   
#   # Drop the old valid_banks and has_relief columns
#   extended_pts <- dplyr::select(extended_pts, -valid_banks, -has_relief)
#   
#   # add a tmp_id for joining and filtering 
#   # extended_pts <- add_tmp_id(extended_pts)
#   
#   # remove any cross sections points that might be missing Z values (i.e. NA Z values)
#   extended_pts <- drop_incomplete_cs_pts(extended_pts, crosswalk_id)
#   
#   # get_unique_tmp_ids(extended_pts)[!get_unique_tmp_ids(extended_pts) %in% get_unique_tmp_ids(fin_extended_pts) ]
#   # get_unique_tmp_ids(cs_pts) %>% length() 
#   
#   if (verbose) { message("Double checking new extended cross section DEM values for flatness") }
#   
#   # reclassify the cross sections to look for any improvments in the points bank/relief validity
#   reclassified_pts <- classify_points(
#     extended_pts,
#     # fin_extended_pts,
#     crosswalk_id             = crosswalk_id,
#     pct_of_length_for_relief = pct_of_length_for_relief
#   )
#   
#    # extended_pts %>% 
#   #   dplyr::filter(hy_id %in% c("wb-2131572")) %>% 
#   #   sf::st_drop_geometry() %>% 
#   #   dplyr::select(hy_id, cs_id, pt_id, Z, relative_distance, cs_lengthm)
#   # 
#   # extended_pts %>% 
#   #   dplyr::filter(hy_id %in% c("wb-2131572")) %>% 
#   #   dplyr::pull(relative_distance)
#   # 
#   # df <- data.frame(
#   #   hy_id = rep("wb-2131572", 17),
#   #   cs_id = rep(4, 17),
#   #   pt_id = 1:17,
#   #   Z = c(132, 132, 131, 130, 128, 128, 125, 124, NA, 111, 111, 120, 122, 121, 122, 118, 118),
#   #   relative_distance = c(0.0000, 30.9646, 61.9292, 92.8938, 123.8584, 154.8230, 185.7876, 216.7522, 247.7168, 278.6814,
#   #                         309.6460, 340.6106, 371.5752, 402.5398, 433.5044, 464.4690, 495.4336),
#   #   cs_lengthm = rep(495, 17)
#   # )
#   
#   # add tmp id for convenience
#   reclassified_pts <- hydrofabric3D::add_tmp_id(reclassified_pts, x = get(crosswalk_id))
#   
#   # Find "validity score" values which just represents a cross sections bank and relief validity as either (0, 1, or 2)
#   #  Score 0 = FALSE banks & FALSE relief
#   #  Score 1 = Either TRUE banks OR relief
#   #  Score 2 = Both TRUE banks & TRUE relief
#   # ---> We get this score for the old and the new set of extended cross sections and 
#   # then take the points in the new data that showed improvement from the original cross section. 
#   # The cross section points that did NOT show improvment remain untouched in the original data
#   old_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(cs_pts, crosswalk_id, "old_validity_score"), x = get(crosswalk_id))
#   new_validity_scores <- hydrofabric3D::add_tmp_id(calc_validity_scores(reclassified_pts, crosswalk_id, "new_validity_score"), x = get(crosswalk_id))
#   
#   # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
#   check_for_improvement <- dplyr::left_join(
#     dplyr::select(
#       dplyr::filter(old_validity_scores, tmp_id %in% unique(new_validity_scores$tmp_id)),  
#       dplyr::any_of(crosswalk_id), cs_id, old_validity_score
#     ), 
#     dplyr::select(
#       new_validity_scores, 
#       dplyr::any_of(crosswalk_id), cs_id, new_validity_score
#     ),
#     by = c(crosswalk_id, "cs_id")
#     # by = c("hy_id", "cs_id")
#   ) %>% 
#     dplyr::mutate(
#       improved = dplyr::case_when(
#         new_validity_score > old_validity_score ~ TRUE,
#         TRUE                                    ~ FALSE
#       )
#     ) %>% 
#     dplyr::select(dplyr::any_of(crosswalk_id), cs_id, improved)
#   
#   # check_for_improvement %>% 
#   #   dplyr::filter(hy_id == "wb-2131572", cs_id == 4)
#   
#   # List of unique hy_id/cs_ids (tmp_id) that showed improvement after extension, if valid banks or relief was addded (or both),
#   # then the cross section "showed improvement", and the new values will be put into the output cross section dataset
#   extended_ids_to_keep <- 
#     check_for_improvement %>% 
#     dplyr::filter(improved) %>% 
#     get_unique_tmp_ids(x = get(crosswalk_id))
#   
#   # ids_to_add_to_good_set <- 
#   #   check_for_improvement %>% 
#   #   dplyr::filter(!improved) %>% 
#   #   get_unique_tmp_ids()
#   
#   # add a tmp_id for joining and filtering 
#   extended_pts <- add_tmp_id(extended_pts, x = get(crosswalk_id))
#   
#   # TODO: Left off here to add back and remove old data 03/05/2024
#   pts_to_keep <- dplyr::filter(extended_pts, 
#                                tmp_id %in% extended_ids_to_keep)
#   # pts_to_keep <- dplyr::filter(extended_pts2, !tmp_id %in% ids_to_add_to_good_set)
#   # pts_to_move_to_good_set <- dplyr::filter(extended_pts2, tmp_id %in% ids_to_add_to_good_set)
#   
#   # Reclassify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
#   pts_to_keep             <- classify_points(
#     pts_to_keep,
#     crosswalk_id = crosswalk_id,
#     pct_of_length_for_relief = pct_of_length_for_relief
#   )
#   # pts_to_keep             <- hydrofabric3D::classify_points(pts_to_keep, pct_of_length_for_relief = pct_of_length_for_relief)
#   
#   # pts_to_keep %>% 
#   #   dplyr::filter(is_extended)
#   
#   # add is_extended logical if does not exist
#   if (!"is_extended" %in% names(pts_to_keep)) {
#     pts_to_keep$is_extended = TRUE
#   }
#   
#   # remove the IDs of newly updated cross section points from the original data, then 
#   # bind the new version of these points to the rest of the original data
#   final_pts <-
#     cs_pts %>%  
#     hydrofabric3D::add_tmp_id(x = get(crosswalk_id)) %>% 
#     dplyr::filter(
#       !tmp_id %in% extended_ids_to_keep
#     ) %>% 
#     dplyr::mutate(
#       is_extended = FALSE
#     ) %>% 
#     dplyr::bind_rows(
#       hydrofabric3D::add_tmp_id(pts_to_keep, x = get(crosswalk_id))
#     ) %>% 
#     dplyr::select(-tmp_id) 
#   
#   # start_ids <- unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id)
#   # end_ids <- unique(final_pts$tmp_id)
#   # 
#   # length(unique(final_pts$tmp_id))
#   # length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
#   # length(unique(final_pts$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
#   # length(unique(hydrofabric3D::add_tmp_id(final_pts)$tmp_id)) == length(unique(hydrofabric3D::add_tmp_id(cs_pts)$tmp_id))
#   
#   # rename geometry column to "geom" 
#   final_pts <- nhdplusTools::rename_geometry(final_pts, "geometry")
#   
#   # TODO: this should probably be removed and just kept as its own separete function and use outside of this function
#   # If TRUE then the cs_ids are renumbered to make sure each hy_id has cross sections
#   # that are numbered (1 - number of cross sections) on the hy_id
#   if (fix_ids) {
#     if (verbose) { message("Renumbering cross section IDs...") }
#     final_pts <- renumber_cs_ids(df = final_pts, crosswalk_id = crosswalk_id)
#   }
#   
#   # then move the geometry column to the last column
#   final_pts <- move_geometry_to_last(final_pts)
#   # final_pts <- dplyr::relocate(final_pts, geom, .after = dplyr::last_col())
#   
#   # final_pts %>%
#   #   get_unique_tmp_ids() %>% 
#   #   length()
#   # 
#   # cs_pts %>%
#   #   get_unique_tmp_ids() %>% 
#   #   length()
#   
#   return(final_pts)
# }
# # ------------------------------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------------
# 
# df <- data.frame(
#   hy_id = rep("wb-2131572", 17),
#   cs_id = rep(4, 17),
#   pt_id = 1:17,
#   Z = c(132, 132, 131, 130, 128, 128, 125, 124, NA, 111, 111, 120, 122, 121, 122, 118, 118),
#   relative_distance = c(0.0000, 30.9646, 61.9292, 92.8938, 123.8584, 154.8230, 185.7876, 216.7522, 247.7168, 278.6814,
#                         309.6460, 340.6106, 371.5752, 402.5398, 433.5044, 464.4690, 495.4336),
#   cs_lengthm = rep(495, 17)
# )
# 
# classify_points <- function(
#     cs_pts, 
#     crosswalk_id = NULL,
#     pct_of_length_for_relief = 0.01
# ){
#   
#   . <-  L <-  L1 <-  L2  <-  R  <-  R1 <-  R2  <- Z  <-  Z2 <-  anchor <-  b1  <- b2  <- cs_lengthm  <- count_left <- 
#     count_right  <-  cs_id <-  hy_id <-  in_channel_pts  <- lengthm <-  low_pt  <- max_bottom  <- mean_dist <-  mid_bottom  <- min_bottom  <- pt_id <- relative_distance <-  third <- NULL
#   # TODO: maybe relief_to_length_ratio is more intuitive than pct_of_length_for_relief ????
#   # relief_to_length_ratio = 0.01
#   
#   # cs_pts2 <- cs_pts
#   # crosswalk_id = "hy_id"
#   # pct_of_length_for_relief = 0.01
#   
#   # make a unique ID if one is not given (NULL 'id')
#   if(is.null(crosswalk_id)) {
#     # cs  <- add_hydrofabric_id(cs) 
#     crosswalk_id  <- 'hydrofabric_id'
#   }
#   
#   REQUIRED_COLS <- c(crosswalk_id, "cs_id", "pt_id", "cs_lengthm", "relative_distance")
#   # REQUIRED_COLS <- c(id, "cs_id")
#   
#   if (!all(REQUIRED_COLS %in% names(cs_pts))) {
#     
#     missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(cs_pts))]
#     
#     stop("'cs_pts' is missing one or more of the required columns:\n > ", 
#          paste0(missing_cols, collapse = "\n > "))
#   }
#   
#   # type checking
#   if (!is.numeric(pct_of_length_for_relief)) {
#     stop("Invalid argument type, 'pct_of_length_for_relief' must be of type 'numeric', given type was '",   
#          class(pct_of_length_for_relief), "'")
#   }
#   
#   # Make sure pct_of_length_for_relief is valid percentage value (greater than 0)
#   if (pct_of_length_for_relief < 0 ) {
#     stop("Invalid value 'pct_of_length_for_relief' of ", pct_of_length_for_relief, ", 'pct_of_length_for_relief' must be greater than or equal to 0")
#   } 
#   
#   # # remove any columns that already exist
#   cs_pts <- dplyr::select(cs_pts, 
#                           !dplyr::any_of(c("class", "point_type", "bottom", "left_bank", "right_bank", "valid_banks", "has_relief"))
#   )
#   
#   # required cols that will be selected from the classified_pts object and in this order
#   req_cols       <- c(crosswalk_id, "cs_id", "pt_id", "Z", "relative_distance", 
#                       "cs_lengthm", "class", "point_type")
#   
#   # any starting columns in the original data 
#   starting_cols  <- names(cs_pts)
#   
#   # name and order of columns to select with
#   cols_to_select <- c(req_cols, starting_cols[!starting_cols %in% req_cols])
#   
#   df <- data.frame(
#     hy_id = rep("wb-2131572", 17),
#     cs_id = rep(4, 17),
#     pt_id = 1:17,
#     Z = c(132, 132, 131, 130, 128, 128, 125, 124, NA, 111, 111, 120, 122, 121, 122, 118, 118),
#     relative_distance = c(0.0000, 30.9646, 61.9292, 92.8938, 123.8584, 154.8230, 185.7876, 216.7522, 247.7168, 278.6814,
#                           309.6460, 340.6106, 371.5752, 402.5398, 433.5044, 464.4690, 495.4336),
#     cs_lengthm = rep(495, 17)
#   )
#   
#   df <- data.frame(
#     hy_id = rep("wb-2131572", 17),
#     cs_id = rep(4, 17),
#     pt_id = 1:17,
#     Z = c(132, 132, 131, 130, 128, 128, 
#           NA, NA, NA, NA, NA, NA, 
#           122, 121, 122, 118, 118
#           ),
#     relative_distance = c(0.0000, 30.9646, 61.9292, 92.8938, 123.8584, 154.8230, 185.7876, 216.7522, 247.7168, 278.6814,
#                           309.6460, 340.6106, 371.5752, 402.5398, 433.5044, 464.4690, 495.4336),
#     cs_lengthm = rep(495, 17)
#   )
#   
#   plot(df$Z~df$relative_distance)
#   
#   Z = c(132, 132, 131, 130, 128, 128, 
#         NA, NA, NA, NA, NA, NA, 
#         122, 121, 122, 118, 118)
#   
#   Z
#   
#   dplyr::filter(df) %>%
#     dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
#     # dplyr::filter(!is.na(Z)) %>% 
#     dplyr::filter(!any(is.na(Z))) %>% 
#     dplyr::ungroup() 
#     # dplyr::mutate(
#       # Z2             = zoo::rollmean(Z, 3)
#     # ) 
#   
#   dplyr::filter(df) %>%
#   dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
#   # dplyr::filter(!is.na(Z)) %>% 
#     dplyr::filter(!any(is.na(Z))) %>% 
#   dplyr::mutate(
#       third          = ceiling(dplyr::n() / 3),
#       mean_dist      = mean(diff(relative_distance)),
#       in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
#       b1             = ceiling(in_channel_pts / 2),
#       b2             = in_channel_pts - b1,
#       low_pt         = min(Z[third[1]:(2*third[1] - 1)], na.rm = TRUE),
#       class          = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
#                               "bottom", 
#                               "bank"
#       ),
#       # ) %>% dplyr::relocate(third, mean_dist, in_channel_pts, b1, b2, low_pt, class)
#       Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()])
#       ) %>% 
#   dplyr::relocate(third, mean_dist, in_channel_pts, b1, b2, low_pt, class, Z2, Z)
#   
#   # create classifications for points
#   # classified_pts <-
#     # dplyr::filter(cs_pts) %>%
#     dplyr::filter(df) %>%
#     dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>% 
#     # dplyr::group_by(hy_id, cs_id) %>%
#     dplyr::filter(!is.na(Z)) %>% 
#       dplyr::mutate(
#         pt_id = 1:dplyr::n()
#       ) %>% 
#     dplyr::mutate(
#       third          = ceiling(dplyr::n() / 3),
#       mean_dist      = mean(diff(relative_distance)),
#       in_channel_pts = ceiling(cs_lengthm[1] / mean_dist),
#       b1             = ceiling(in_channel_pts / 2),
#       b2             = in_channel_pts - b1,
#       low_pt         = min(Z[third[1]:(2*third[1] - 1)], na.rm = TRUE),
#       class          = ifelse(Z <= low_pt & dplyr::between(pt_id, third[1], (2*third[1] - 1)), 
#                               "bottom", 
#                               "bank"
#       ),
#       # ) %>% dplyr::relocate(third, mean_dist, in_channel_pts, b1, b2, low_pt, class)
#       Z2             = c(Z[1], zoo::rollmean(Z, 3), Z[dplyr::n()]),
#       Z              = ifelse(class == "bottom", Z, Z2),
#       min_bottom     = which(class == "bottom")[1],
#       # ) %>% dplyr::relocate(class)
#       mid_bottom     = which(class == "bottom")[ceiling(length(which(class == "bottom"))/2)],
#       max_bottom     = which(class == "bottom")[length(which(class == "bottom"))],
#       L1             = pmax(1, mid_bottom - b1),
#       L2             = pmax(1, mid_bottom - b2),
#       R1             = pmin(mid_bottom + b2, n()),
#       R2             = pmin(mid_bottom + b1, n()),
#       anchor         = ifelse(Z[R2] < Z[L1], 2, 1),
#       L              = pmax(third, ifelse(anchor == 1, L1, L2)),
#       R              = pmin(2*third[1], ifelse(anchor == 1, R1, R2)),
#       count_left     = min_bottom - L,
#       count_right    = R - max_bottom,
#       L              = ifelse(count_left == 0, L - count_right, L),
#       R              = ifelse(count_right == 0, R + count_left, R),
#       class          = ifelse(dplyr::between(pt_id, L[1], R[1]) & class != 'bottom', "channel", class),
#       class          = ifelse(class == 'bank' & pt_id <= L[1], "left_bank", class),
#       class          = ifelse(class == 'bank' & pt_id >= R[1], "right_bank", class)
#     ) %>%
#     dplyr::ungroup() %>% 
#       dplyr::relocate(third, mean_dist, in_channel_pts, b1, b2, low_pt, class)
#     # dplyr::mutate(point_type = class) %>% 
#     # dplyr::select(dplyr::any_of(cols_to_select))
#   # dplyr::select(dplyr::all_of(cols_to_select))      # Stricter, requires ALL of the columns to be present or it will throw an error
#   
#   # classified_pts[cols_to_select]                       # Another method for selecting columns....
#   
#   # get bank validity attributes for each hy_id/cs_id
#   # - Uses the count of point types per cross section and checks Z to make sure that a "bottom" point is
#   #   in each cross section and each "bottom" point has a valid left and right bank)
#   bank_validity_df <- get_bank_attributes(classified_pts, crosswalk_id)
#   
#   # classified_pts %>%
#   # dplyr::filter(hy_id %in% c('wb-1003260')) %>%
#   # hydrofabric3D::plot_cs_pts(color = "point_type")
#   
#   # # Or add bank attributes 
#   # banked_pts <- add_bank_attributes(output_pts)
#   
#   # get relief data, determine if a cross section has relief within X% percentage of the cross sections length
#   relief_df <- get_relief(
#     classified_pts, 
#     crosswalk_id             = crosswalk_id,
#     pct_of_length_for_relief = pct_of_length_for_relief, 
#     detailed                 = FALSE
#   )
#   
#   # join the bank validity attributes with the relief values
#   validity_checks <- dplyr::left_join(
#     bank_validity_df, 
#     relief_df, 
#     by = c(crosswalk_id, "cs_id")  
#     # by = c("hy_id", "cs_id")
#   )
#   
#   # join the new validity check values to the classified points
#   classified_pts <- 
#     classified_pts %>% 
#     dplyr::left_join(
#       validity_checks,
#       by = c(crosswalk_id, "cs_id")  
#       # by = c("hy_id", "cs_id")
#     ) 
#   
#   # move the geometry column to the last column (if one exists)
#   classified_pts <- move_geometry_to_last(classified_pts)
#   
#   return(classified_pts)
#   
# }
# # ------------------------------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------------
# # ------------------------------------------------------------------------------------------------------
# 
# # 
# # points_per_cs  = NULL
# # min_pts_per_cs = 10
# # dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"
# # scale          = 0.5
# # pct_of_length_for_relief = 0.01
# # fix_ids        = FALSE
# # crosswalk_id = "hy_id"
# # verbose        = TRUE
# # devtools::load_all()
# # sf::write_sf(cs_pts, "/Users/anguswatters/Desktop/test_improve_cs_pts_06.gpkg")
# # sf::write_sf(flines, "/Users/anguswatters/Desktop/test_improve_flines.gpkg")
# # sf::write_sf(transects, "/Users/anguswatters/Desktop/test_improve_transects.gpkg")
# 
# # transects_to_check  = transects %>% dplyr::slice(52000:54000)
# 
# # net                 = net
# # net2 <- net %>% dplyr::filter(hy_id %in% transects_to_check$hy_id)
# # crosswalk_id        = crosswalk_id
# # scale               = scale
# # direction           = "both"
# # verbose             = verbose
# 
# # ----------------------------------------
# MISSING_Z_CS_DATA <- data.frame(
#   hy_id = rep("wb-2131572", 17),
#   cs_id = rep(4, 17),
#   pt_id = 1:17,
#   Z = c(132, 132, 131, 130, 128, 128, 125, 124, NA, 111, 111, 120, 122, 121, 122, 118, 118),
#   relative_distance = c(0.0000, 30.9646, 61.9292, 92.8938, 123.8584, 154.8230, 185.7876, 216.7522, 247.7168, 278.6814,
#                         309.6460, 340.6106, 371.5752, 402.5398, 433.5044, 464.4690, 495.4336),
#   cs_lengthm = rep(495, 17)
# )
# 
# testthat::test_that("check that missing NA value is identified and added as a NA Z value for the correct transect", {
#   MIN_BF_WIDTH       <- 50
#   ID_COL             <- "hy_id"
#   NUM_OF_TRANSECTS   <- 3
#   
#   # Cross section point inputs
#   DEM_PATH          <- testthat::test_path("testdata", "dem_flowlines.tif")
#   POINTS_PER_CS     <- NULL
#   MIN_PTS_PER_CS    <- 10
#   
#   flowlines <-
#     flowlines %>% 
#     dplyr::slice(1) %>%
#     # dplyr::slice(1:3) %>% 
#     add_powerlaw_bankful_width("tot_drainage_areasqkm", MIN_BF_WIDTH) %>%  
#     dplyr::rename(!!sym(ID_COL) := id) %>% 
#     dplyr::select(
#       dplyr::any_of(ID_COL), 
#       tot_drainage_areasqkm,
#       bf_width,
#       geom
#     ) 
#   
#   transects <- cut_cross_sections(
#     net = flowlines,
#     id  = ID_COL,  
#     num = NUM_OF_TRANSECTS
#   )
#   
#   TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH <- testthat::test_path("testdata", "transects_missing_depth.gpkg")
#   ID_COL              <- "hy_id"
#   CS_IDS_MISSING_Z    <- c(66)
#   
#   # Cross section point inputs
#   DEM_PATH            <- testthat::test_path("testdata", "dem_missing_depth.tif")
#   POINTS_PER_CS       <- NULL
#   MIN_PTS_PER_CS      <- 10
#   
#   transects    <- sf::read_sf(TRANSECTS_MISSING_DEPTH_TEST_DATA_PATH) 
#   
#   # mapview::mapview(raster::raster(DEM_PATH)) + transects
#   
#   cs_pts <- hydrofabric3D::cross_section_pts(
#     cs             = transects,
#     crosswalk_id   = ID_COL,
#     points_per_cs  = POINTS_PER_CS,
#     min_pts_per_cs = MIN_PTS_PER_CS,
#     dem            = DEM_PATH
#   ) 
# 
#   })
