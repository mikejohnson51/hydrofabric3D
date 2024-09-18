pacman::p_load(sf, dplyr)

# braided_flowlines.gpkg for a VPU 06 COMID 22180848
braided_flowlines<- sf::read_sf("tests/testthat/testdata/braided_flowlines.gpkg")

usethis::use_data(braided_flowlines, overwrite = TRUE)
 
# ref_path <- "/Users/anguswatters/Desktop/lynker-spatial/00_reference_features/gpkg/06_reference_features.gpkg"
# ref_fl <- sf::read_sf(ref_path, layer = "flowlines")
# 
# nav <- nhdplusTools::navigate_network(start = 22180848, 
#                                      mode = "UT", 
#                                      distance_km = 100,
#                                      network = ref_fl)
# 
# sf::write_sf(nav, "tests/testthat/testdata/braided_flowlines.gpkg")