# Running this script goes and pulls the desired NextGen geopackage datasets from http://www.lynker-spatial.com/, saves them into a directory within "base_dir"
# base_dir is defined within runners/workflow/root_dir.R

library(logger)

source("runners/workflow/root_dir.R")

s3_bucket <- "s3://lynker-spatial/"

# nextgen bucket name
prerelease_key     <- "s3://lynker-spatial/pre-release/"

# directory to copy nextgen bucket data too
nextgen_dir   <- glue::glue('{base_dir}/pre-release/')

# create the directory if it does NOT exist
if(!dir.exists(nextgen_dir)) {
  logger::log_info("\n\nDirectory does not exist at: \n\t'{nextgen_dir}'\nCreating directory at: \n\t'{nextgen_dir}'")
  dir.create(nextgen_dir)
}

model_attr_dir <-  glue::glue('{base_dir}/model_attributes/')

# create the directory if it does NOT exist
if(!dir.exists(model_attr_dir)) {
  logger::log_info("\n\nDirectory does not exist at: \n\t'{model_attr_dir}'\nCreating directory at: \n\t'{model_attr_dir}'")
  dir.create(model_attr_dir)
}

# list objects in S3 bucket, and regular expression match to nextgen_.gpkg pattern
command <- paste0('#!/bin/bash
            # AWS S3 Bucket and Directory information
            S3_BUCKET="', prerelease_key, '"
            DESTINATION_DIR=', nextgen_dir, '
            
            # Regular expression pattern to match object keys
            PATTERN="^nextgen_[0-9][0-9][A-Za-z]*\\.gpkg$"
            
            # AWS CLI command to list objects in the S3 bucket and use grep to filter them
            S3_OBJECTS=$(aws s3 ls "$S3_BUCKET" | awk \'{print $4}\' | grep -E "$PATTERN")
            
            echo "$S3_OBJECTS"'
                  )

# Run the script using backticks
bucket_keys <- system(command, intern = TRUE)

# Parse the selected S3 object keys and copy them to the destination directory
for (key in bucket_keys) {
  
  copy_cmd <- paste0('aws s3 cp ', prerelease_key, key, " ", nextgen_dir, key)
  logger::log_info("Copying S3 object:\n{prerelease_key}{key}")
  
  system(copy_cmd)
  
  logger::log_info("Download '{key}' complete!")
  logger::log_info("------------------")
}



# Parse the selected S3 object keys and copy them to the destination directory
copy_cmd <- paste0('aws s3 cp ', s3_bucket, "v20/3D/model_attributes/nextgen_3D_12.parquet ", model_attr_dir, "nextgen_3D_12.parquet")
logger::log_info("Copying S3 object:\n{s3_bucket}v20/3D/model_attributes/nextgen_3D_12.parquet")
system(copy_cmd)
  
logger::log_info("Download '{key}' complete!")
logger::log_info("------------------")

