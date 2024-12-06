library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()
# 
# -------------------------------------------------------------------
# ---- hydrofabric3D:::classify_derivatives() ----
# -------------------------------------------------------------------

testthat::test_that("test length of classify_derivatives output with input vector of length 1", {
            
            depths <- c(1)
            # length(depths)
            # plot(depths)    
            
            classes = hydrofabric3D:::classify_derivatives(
                depths
            )  
            
            testthat::expect_equal(length(classes), length(depths))
})

testthat::test_that("test length of classify_derivatives output with input vector of length 2", {
            
            depths <- c(1, 2)
            # length(depths)
            # plot(depths)    
            
            classes = hydrofabric3D:::classify_derivatives(
                depths
            )  
            
            testthat::expect_equal(length(classes), length(depths))
})


testthat::test_that("test length of classify_derivatives output with input vector of length 0", {
            
            depths <- c(     )
            # length(depths)
            # plot(depths)    
            
            classes = hydrofabric3D:::classify_derivatives(
                depths
            )  
            
            testthat::expect_equal(length(classes), length(depths))
})

testthat::test_that("test length of classify_derivatives output with input vector of length 3", {
            
            depths <- c(1, 2, 3)
            # length(depths)
            # plot(depths)    
            
            classes = hydrofabric3D:::classify_derivatives(
                depths
            )  
            
            testthat::expect_equal(length(classes), length(depths))
})

testthat::test_that("test length of classify_derivatives output with input vector of length 4", {
            
            depths <- c(1, 2, 3, 4)
            # length(depths)
            # plot(depths)    
            
            classes = hydrofabric3D:::classify_derivatives(
                depths
            )  
            
            testthat::expect_equal(length(classes), length(depths))
})

testthat::test_that("test length of classify_derivatives output with input vector of length 5", {
            
            depths <- c(1, 2, 3, 4, 5)
            # length(depths)
            # plot(depths)    
            
            classes = hydrofabric3D:::classify_derivatives(
                depths
            )  
            
            testthat::expect_equal(length(classes), length(depths))
})


testthat::test_that("derivative classification for a simple U shape", {

    depths <- c(10, 8, 7, 5, 3, 1, 1, 3, 5, 7, 8, 10 ) 
    
    classes = hydrofabric3D:::classify_derivatives(
        depths
    )  

    expected_classes <- c('flat', 'concave_up_decreasing', 'concave_down_decreasing', 
                        'linear', 'linear', 'concave_up_decreasing', 'flat', 
                        'linear', 'linear', 'concave_down_increasing', 
                        'concave_up_increasing', 'concave_down_increasing')
    
    testthat::expect_equal(classes, expected_classes)
    
})

testthat::test_that("derivative classification for a length 5 V shape", {

    depths <- c(5, 3, 1, 3, 5) 
    # length(depths)
    # plot(depths)    
    
    classes = hydrofabric3D:::classify_derivatives(
        depths
    )  
    
    expected_classes <- c('flat', 'linear', 'concave_up_decreasing', 
                        'linear', 'concave_down_increasing')
    
    testthat::expect_equal(classes, expected_classes)
    
})

testthat::test_that("derivative classification for length 5 flat shape", {

    depths <- c(5, 5, 5, 5, 5) 
    # length(depths)
    # plot(depths)    
    
    classes = hydrofabric3D:::classify_derivatives(
        depths
    )  
    
    expected_classes <- c('flat', 'flat', 'flat', 'flat', 'flat')
    
    testthat::expect_equal(classes, expected_classes)
    
})

testthat::test_that("derivative classification for length 5 concave up shape", {

    depths <- c(1, 2, 3, 4, 5) 
    length(depths)
    plot(depths)    
    
    classes = hydrofabric3D:::classify_derivatives(
        depths
    )  
    
    expected_classes <- c('flat', 'linear', 'linear', 
                        'linear', 'concave_down_increasing')
    
    testthat::expect_equal(classes, expected_classes)
    
})

testthat::test_that("derivative classifcaitn for length 5 vertical line", {

    depths <- c(1, 1, 1, 1, 1) 
    # length(depths)
    # plot(depths)    
    
    classes = hydrofabric3D:::classify_derivatives(
        depths
    )  
    
    expected_classes <- c('flat', 'flat', 'flat', 'flat', 'flat')
    
    testthat::expect_equal(classes, expected_classes)
    
})
