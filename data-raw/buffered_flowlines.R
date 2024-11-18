# pacman::p_load(sf, dplyr)
# 
# # flowlines for a mainstem "1359733" in VPU 06 from Nextgen v20.1
# flowlines <- sf::read_sf("tests/testthat/testdata/flowlines.gpkg")
# # flowlines$geom %>% plot()
# 
# buff <- 
#   flowlines %>% 
#   sf::st_buffer(200) 
#   # .$geom %>% mapview::mapview() + flowlines
# 
# usethis::use_data(buff, overwrite = TRUE)
