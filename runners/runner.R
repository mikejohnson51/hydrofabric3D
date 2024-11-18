library(hydrofabric)
#library(terrain_sliceR)

{
  cat = get_nhdplus(comid = linestring$nhdplus_comid, realization = "catchment")
  tmp = dap("/vsis3/nextgen-hydrofabric/DEM-products/usfs_riparian.tif", AOI =cat)[[1]]
  dd  = execute_zonal(tmp, geom = cat, fun = 'sum', ID = 'featureid', join = F)
  out = left_join(linestring, dd, by = c("comid" = "featureid")) %>% 
  mutate(
    area =  USFS_Riparian_CONUS * prod(res(tmp[[1]])),
    average_tw = area / dist_m
    ) 

  cs = cut_cross_sections(net = out,
                          crosswalk_id = "comid", 
                          cs_widths = 2* out$average_tw,
                          num = ceiling(out$dist_m / 250),
                          smooth = TRUE,
                          densify = 3,
                          rm_self_intersect = TRUE) 
  
  cs2 = cs[lengths(st_intersects(cs, out)) == 1,]
}

pts = cross_section_pts(cs, dem = '/Volumes/Transcend/ngen/DEM-products/dem.vrt')
pts = classify_points(pts)

library(ggplot2)
filter(pts, hy_id == 101, cs_id == 5) %>% 
  ggplot() +
  geom_line(aes(x = relative_distance, y = Z)) + 
  geom_point(aes(x = relative_distance, y = Z, color = class), size = 2)


cc = filter(pts, hy_id == 101, cs_id == 5) 
bb = AOI::bbox_get(cc)

de = dap('/Volumes/Transcend/ngen/DEM-products/dem.vrt',
    AOI = bb)

plot(de[[1]])
plot(cc, add = T)

mapview::mapview(cc)

range(cc$Z) %>% diff()
p = "data/tester.gpkg"

sf::write_sf(cs, p, "transects")
sf::write_sf(linestring, p, "flowpaths")
sf::write_sf(pts, p, "transect_points")
