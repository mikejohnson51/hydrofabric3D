library(dplyr); library(terra); library(foreach)

fl = sf::read_sf('/Volumes/Transcend/ngen/CONUS-hydrofabric/05_nextgen/nextgen_01.gpkg', "flowpaths") %>% 
  mutate(bf_width = exp(0.700	 + 0.365* log(tot_drainage_areasqkm))) 

n.cores <- parallel::detectCores() - 1
doParallel::registerDoParallel(cores = n.cores)
foreach::getDoParRegistered()


xx = unique(fl$mainstem)
print(length(xx))

system.time({
  lines = foreach(i = 1:length(xx)) %dopar% {
    input = filter(fl, mainstem == xx[i])
      cut_cross_sections(net = input,
                         id = "id", 
                         bf_widths = pmax(50, input$bf_width * 7),
                         num = 10) 
  }
  
  pts = foreach(i = 1:length(lines)) %dopar% {
    cross_section_pts(lines[[i]], dem = '/Volumes/Transcend/ngen/DEM-products/dem.vrt')
  }
  
})

601 / 60
  
ll = bind_rows(lines)
nrow(ll) / nrow(fl)
pp = bind_rows(pts)
nrow(pp)

l = bind_rows(lines)
p = bind_rows(pts)
p2 = classify_points(p)

sf::write_sf(l, '/Volumes/Transcend/ngen/CONUS-hydrofabric/05_nextgen/3d_channel_01.gpkg', "transects")
sf::write_sf(p2, '/Volumes/Transcend/ngen/CONUS-hydrofabric/05_nextgen/3d_channel_01.gpkg', "transect_points")
