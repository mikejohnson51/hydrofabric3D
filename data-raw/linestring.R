pacman::p_load(dataRetrieval, nhdplusTools, hydrofab, sf, dplyr)

linestring = findNLDI(comid = 101, nav = "UT", find = "flowlines")$UT_flowlines %>% 
  st_transform(5070) %>% 
  mutate(comid = as.numeric(nhdplus_comid)) %>% 
  left_join(get_vaa(c('totdasqkm')), by = "comid") %>% 
  mutate(dist_m = add_lengthkm(.) * 1000)

usethis::use_data(linestring)

