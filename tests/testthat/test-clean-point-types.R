library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::clean_point_types() ----
# -------------------------------------------------------------------
# TODO:
testthat::test_that("standard point types, no changes", {
  
  pt_types <- c("left_bank", "left_bank", "channel", "channel", "bottom", "bottom", "channel", "right_bank")
  
  testthat::expect_true(
    all(hydrofabric3D::clean_point_types(pt_types) == pt_types)
  )
  
})

testthat::test_that("left bank between 2 channel points, convert 'left_bank' -> 'channel'", {
  
  pt_types <- c("left_bank", "channel", "left_bank", "channel", "bottom", "bottom", "channel", "right_bank")
  
  clean <- hydrofabric3D::clean_point_types(pt_types)
  
  expected_pt_types <- c("left_bank", "channel", "channel", "channel", "bottom", "bottom", "channel", "right_bank")
  
  testthat::expect_true(
    all(clean == expected_pt_types)
  )
  
})

testthat::test_that("2 left bank pts between 2 channel points, convert 'left_bank' -> 'channel'", {
  
  pt_types <- c("left_bank", "channel", "left_bank", "left_bank", "channel" , "bottom", "bottom", "channel", "right_bank")
  
  clean <- hydrofabric3D::clean_point_types(pt_types)
  
  expected_pt_types <- c("left_bank", "channel", "channel", "channel", "channel" , "bottom", "bottom", "channel", "right_bank")
  
  testthat::expect_true(
    all(clean == expected_pt_types)
  )
  
})

testthat::test_that("1 left_bank between 2 bottom, convert 'left_bank' -> 'bottom'", {
  
  pt_types <- c("left_bank", "channel", "channel", "bottom", "left_bank" , "bottom", "bottom", "channel", "right_bank")
  
  clean <- hydrofabric3D::clean_point_types(pt_types)
  
  expected_pt_types <- c("left_bank", "channel", "channel", "bottom", "bottom" , "bottom", "bottom", "channel", "right_bank")
  
  testthat::expect_true(
    all(clean == expected_pt_types)
  )
  
})

testthat::test_that("3 left_bank between 2 bottom, convert 'left_bank' -> 'bottom'", {
  
  pt_types <- c("left_bank", "channel", "channel", "bottom", "left_bank", "left_bank",
                "left_bank" , "bottom", "bottom", "channel", "right_bank")
  
  clean <- hydrofabric3D::clean_point_types(pt_types)
  
  expected_pt_types <- c("left_bank", "channel", "channel", "bottom", "bottom", "bottom",
                         "bottom" , "bottom", "bottom", "channel", "right_bank")
  
  
  testthat::expect_true(
    all(clean == expected_pt_types)
  )
  
})

testthat::test_that("1 right_bank between 2 bottom, convert 'right_bank' -> 'bottom'", {
  
  pt_types <- c("left_bank", "channel", "channel", "bottom", "bottom" , "right_bank", "bottom", "channel", "right_bank")
  
  clean <- hydrofabric3D::clean_point_types(pt_types)
  
  expected_pt_types <- c("left_bank", "channel", "channel", "bottom", "bottom" , "bottom", "bottom", "channel", "right_bank")
  
  testthat::expect_true(
    all(clean == expected_pt_types)
  )
  
})

testthat::test_that("3 right_bank between 2 bottom, convert 'right_bank' -> 'bottom'", {
  
  pt_types <- c("left_bank", "channel", "channel", "bottom", "right_bank" , "right_bank", 
                "right_bank", "bottom", "channel", "right_bank")
  
  clean <- hydrofabric3D::clean_point_types(pt_types)
  
  expected_pt_types <- c("left_bank", "channel", "channel", "bottom", "bottom" , 
                         "bottom", "bottom", "bottom", "channel", "right_bank")
  
  
  testthat::expect_true(
    all(clean == expected_pt_types)
  )
  
})
