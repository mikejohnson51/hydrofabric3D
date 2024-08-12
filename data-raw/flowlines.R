pacman::p_load(sf, dplyr)

# flowlines for a mainstem "1359733" in VPU 06 from Nextgen v20.1
flowlines <- sf::read_sf("tests/testthat/testdata/flowlines.gpkg")

usethis::use_data(flowlines, overwrite = TRUE)

# tmp_path <- "/Users/anguswatters/Desktop/test_flines_06.gpkg"
# 
# flines <- sf::read_sf(tmp_path)
# flines %>%
#   sf::st_drop_geometry() %>%
#   dplyr::group_by(mainstem) %>%
#   dplyr::count() %>%
#   dplyr::arrange(-n) %>%
#   dplyr::filter(n <= 10)
# 
# # 1359733
# test_flowlines <-
#   flines %>%
#   dplyr::filter(mainstem == "1359733") %>%
#   dplyr::mutate(
#     lengthkm = as.numeric(sf::st_length(.)) / 1000
#     ) %>% 
#   dplyr::select(
#     id = hy_id,
#     lengthkm,
#     mainstem,
#     tot_drainage_areasqkm
#     )
# sf::write_sf(test_flowlines, "tests/testthat/testdata/flowlines.gpkg")
# 
# test_flowlines[10, ]
# 
# # # create test data (hy_id = "wb-1004970" from nextgen flowlines)
# coords <- matrix(c(968520.8, 1381795, 968471.3, 1381851, 968420.6, 1381874,
#                    968418.1, 1381897, 968436.2, 1381961, 968426.9, 1382022,
#                    968412.6, 1382036,  968211.2, 1382114, 968197.2, 1382148,
#                    968172.4, 1382166,  968029.8, 1382217, 967972.7, 1382319,
#                    967936.7, 1382369,  967835.1, 1382461, 967831.7, 1382514,
#                    967836.6, 1382538, 967764.9, 1382589,  967741.8, 1382615,
#                    967695.0, 1382625, 967639.9, 1382619,  967108.0, 1382436,
#                    967072.6, 1382434,  967038.1, 1382448,  966982.6, 1382491,
#                    966947.4, 1382534,  966945.7, 1382549, 966932.3, 1382555,
#                    966886.3, 1382694,  966876.6, 1382781,  966930.3, 1382957,
#                    966926.8, 1382988,  966873.1, 1383015, 966851.8, 1383046,
#                    966807.0, 1383062, 966779.4, 1383172),
#                  ncol = 2, byrow = TRUE)
# 
# # # create linestring and Sf dataframe
# linestring_geom <- sf::st_linestring(as.matrix(coords))
# flowline <- sf::st_as_sf(
#   data.frame(hy_id                 = "wb-1004970",
#              tot_drainage_areasqkm = 3.90825,
#              geometry = sf::st_geometry(linestring_geom)),
#   crs = 5070
# )
# # 
# 
# 
# 
# # # lengthkm and bankful width (power law equation using total draineage area (sq. km))
# flowline <-
#   flowline %>%
#   dplyr::mutate(
#     lengthkm = as.numeric(sf::st_length(geometry))/1000,
#     bf_width = calc_powerlaw_bankful_width(tot_drainage_areasqkm)
#   ) %>%
#   dplyr::select(
#     hy_id,
#     lengthkm,
#     tot_drainage_areasqkm,
#     bf_width,
#     geometry
#   )
# 
#   unlink(nhdplusTools::get_vaa_path())
# flines <- nhdplusTools::get_nhdplus(comid = 101)
# get_vaa(c('totdasqkm'), download = T, updated_network = T)
# 
# linestring = 
#   findNLDI(comid = 101, nav = "UT", find = "flowlines")$UT_flowlines %>%
#   st_transform(5070) %>%
#   mutate(comid = as.numeric(nhdplus_comid)) %>%
#   left_join(
#     get_vaa(c('totdasqkm'),
#             download = T
#             ), 
#     
#     by = "comid"
#     ) %>%
#   mutate(dist_m = add_lengthkm(.) * 1000)
# 
# usethis::use_data(linestring)