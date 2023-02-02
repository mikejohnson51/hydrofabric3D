roughness_map = data.frame(
  nlcd = c(11,12,21,22,23,24,31,41,42,43,51,52,71,72,73,74,81,82,90,95),
  description = c("Open Water", "Perennial Ice/Snow", 
                  "Developed Open Space", "Developed, Low Intensity", "Developed, Medium Intensity", "Developed High Intensity", 
                  "Barren Land (Rock/Sand/Clay)", 
                  "Deciduous Forest", "Evergreen Forest", "Mixed Forest",
                  "Dwarf Scrub", "Shrub/Scrub", 
                  "Grassland/Herbaceous", "Sedge/Herbaceous",  "Lichens", "Moss",
                  "Pasture/Hay", "Cultivated Crops",
                  "Woody Wetlands", "Emergent Herbaceous Wetlands"),
  min_n = c(.025, NA, .03, .06, .08, .12, .023, .10, .08, .08, .025, .07, .025, .025, NA, NA, .025, .020, .045, .05),
  max_n = c(0.05, NA, .05, .12, .16, .20, .030, .20, .16, .20, .05, .16, .05, .05, NA, NA, .05, .05, .15, .085)
)
