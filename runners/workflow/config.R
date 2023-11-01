# load required packages
pacman::p_load(
  logger,
  archive,
  aws.s3,
  terrainSliceR,
  sf,
  terra,
  glue
)

# load root directory 
source("runners/workflow/root_dir.R")
# source("runners/workflow/download_nextgen.R")

sf_use_s2(FALSE)

# name of bucket with nextgen data
nextgen_bucket <- "lynker-spatial"

# directory to copy nextgen bucket data too
nextgen_dir   <- glue::glue('{base_dir}/pre-release/')

# cross section data model data directories
# flowline_dir  <- glue::glue('{base_dir}/01_flowlines/')
transects_dir <- glue::glue('{base_dir}/01_transects/')
cs_pts_dir    <- glue::glue('{base_dir}/02_cs_pts/')
# upload_dir    <- glue::glue('{base_dir}/03_uploads/')

# final output directory with geopackages per VPU
final_dir  = glue::glue('{base_dir}/cross_sections/')

# 
# dir.create(flowline_dir,  showWarnings = FALSE)
dir.create(transects_dir, showWarnings = FALSE)
dir.create(cs_pts_dir,    showWarnings = FALSE)
dir.create(final_dir,     showWarnings = FALSE)