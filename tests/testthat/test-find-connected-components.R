library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::find_connected_components() ----
# -------------------------------------------------------------------

testthat::test_that("find_connected_components(), 10 connected SF linestrings (1 group)", {
  
  ID_COL        <- "id"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL))  
  
  NUM_FLOWLINES <- nrow(flowlines)
  NUM_CONNECTED_COMPONENTS <- 1
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # get the connected components graph
  cc <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
  
  has_correct_cols <- find_connected_components_has_min_output_cols(cc, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # check for correct component IDs
  component_ids <- unique(cc$component_id)
  expected_component_ids <- sort(unique(1:NUM_CONNECTED_COMPONENTS))
  
  has_expected_component_ids <- all(component_ids %in% expected_component_ids)
  testthat::expect_true(has_expected_component_ids)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
})

testthat::test_that("find_connected_components(), 10 SF linestrings (disconnected into 2 groups)", {
  
  ID_COL        <- "id"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  FLOWLINE_INDICES <- c(1:5, 7:10)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  # plot(flowlines$geom)
  NUM_FLOWLINES <- nrow(flowlines)
  NUM_CONNECTED_COMPONENTS <- 2
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # get the connected components graph
  cc <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
  
  has_correct_cols <- find_connected_components_has_min_output_cols(cc, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # check for correct component IDs
  component_ids <- unique(cc$component_id)
  expected_component_ids <- sort(unique(1:NUM_CONNECTED_COMPONENTS))
  
  has_expected_component_ids <- all(component_ids %in% expected_component_ids)
  testthat::expect_true(has_expected_component_ids)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
})

testthat::test_that("find_connected_components(), 10 SF linestrings (disconnected into 5 groups)", {
  
  ID_COL        <- "id"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  FLOWLINE_INDICES <- c(1,
                        3,
                        5,
                        7, 8,
                        10
                        )
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  # plot(flowlines$geom)
  NUM_FLOWLINES <- nrow(flowlines)
  NUM_CONNECTED_COMPONENTS <- 5
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # get the connected components graph
  cc <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
  
  has_correct_cols <- find_connected_components_has_min_output_cols(cc, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # check for correct component IDs
  component_ids <- unique(cc$component_id)
  expected_component_ids <- sort(unique(1:NUM_CONNECTED_COMPONENTS))
  
  has_expected_component_ids <- all(component_ids %in% expected_component_ids)
  testthat::expect_true(has_expected_component_ids)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
})

testthat::test_that("find_connected_components(), 8 SF linestrings w/ 1 braid in middle (1 group)", {
  
  ID_COL        <- "comid"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  # FLOWLINE_INDICES <- c(1, 4, 7)
  # FLOWLINE_INDICES <- c(1, 88, 341, 343, 344, 345, 388, 392, 393, 394)
  FLOWLINE_INDICES <- c(1, 341, 343, 344, 345, 388, 392, 394)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  flowlines[[ID_COL]] <- as.character(flowlines[[ID_COL]])
  # flowlines$geom %>% plot()
  
  # plot(flowlines$geom)
  NUM_FLOWLINES <- nrow(flowlines)
  NUM_CONNECTED_COMPONENTS <- 1
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # get the connected components graph
  cc <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
  
  has_correct_cols <- find_connected_components_has_min_output_cols(cc, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # check for correct component IDs
  component_ids <- unique(cc$component_id)
  expected_component_ids <- sort(unique(1:NUM_CONNECTED_COMPONENTS))
  
  has_expected_component_ids <- all(component_ids %in% expected_component_ids)
  testthat::expect_true(has_expected_component_ids)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
})




testthat::test_that("22 flowlines with 2 braids (3 loops), network is disconnected (SF linestrings)", {
  
  ID_COL        <- "comid"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  # FLOWLINE_INDICES <- c(1, 4, 7)
  # FLOWLINE_INDICES <- c(1, 88, 341, 343, 344, 345, 388, 392, 393, 394)
  FLOWLINE_INDICES1 <- c(1, 341, 343, 344, 345, 388, 392, 394)
  
  # indices of a braid
  FLOWLINE_INDICES2 <- c(334, 335, 336, 337, 338, 339, 381, 382, 383, 387, 384, 389)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))  %>% 
    # dplyr::select(dplyr::any_of(ID_COL)) %>% 
    # dplyr::slice(FLOWLINE_INDICES)
    dplyr::mutate(
      n = 1:dplyr::n(),
      braid_id = dplyr::case_when(
        n %in% FLOWLINE_INDICES1 ~ "braid_1",
        n %in% FLOWLINE_INDICES2 ~ "braid_2",
        TRUE                     ~ "no_braid"
      )
    ) %>% 
    # dplyr::relocate(n, braid_id) %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    dplyr::select(dplyr::any_of(ID_COL)) 
  
  flowlines[[ID_COL]] <- as.character(flowlines[[ID_COL]])
  
  # plot(flowlines$geom)
  NUM_FLOWLINES <- nrow(flowlines)
  NUM_CONNECTED_COMPONENTS <- 2
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # get the connected components graph
  cc <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
  
  has_correct_cols <- find_connected_components_has_min_output_cols(cc, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # check for correct component IDs
  component_ids <- unique(cc$component_id)
  expected_component_ids <- sort(unique(1:NUM_CONNECTED_COMPONENTS))
  
  has_expected_component_ids <- all(component_ids %in% expected_component_ids)
  testthat::expect_true(has_expected_component_ids)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
})

testthat::test_that("find_connected_components(), test CONNECTED generated linestrings", {
  # ID_COL <- "ex_id"
  NUM_CONNECTED_COMPONENTS <- 1
  
  ID_COL_LIST       <- c("ex_id", "comid", "id", "hy_id")
  MAX_NUM_OF_LINES  <- 10
  
  for (ID_COL in ID_COL_LIST) {
    
    TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
    message(paste0(ID_COL, " - ", TO_ID_COL))
    
    for (i in 1:MAX_NUM_OF_LINES) {
      # i =5 
      # message(i)
  
      # connected    <- get_test_lines(NUM_OF_LINES, connected = TRUE)
      connected    <- get_test_lines(i, id_col_name = ID_COL, connected = TRUE)
      
      # make node topology dataframe 
      nodes    <- hydrofabric3D:::get_node_topology(connected, ID_COL)
      
      # get the connected components graph
      cc    <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
      
      # Check output columns
      testthat::expect_true(  find_connected_components_has_min_output_cols(cc, ID_COL))
      
      # check for correct component IDs (CONNECTED)
      component_ids <- unique(cc$component_id)
      expected_component_ids <- sort(unique(1:NUM_CONNECTED_COMPONENTS))
      
      has_expected_component_ids <- all(component_ids %in% expected_component_ids)
      testthat::expect_true(has_expected_component_ids)
      
      # IDs from the original data
      start_ids <- unique(connected[[ID_COL]])
      
      # IDs in the ending node dataframe
      end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
      end_ids  <- end_ids[end_ids != 0]
      
      # that all IDs from the flowlines are in the end node dataframe
      has_all_ids <- all(start_ids %in% end_ids)
      testthat::expect_true(has_all_ids)
    }
    
  }
})

testthat::test_that("find_connected_components(), test 10 DISCONNECTED generated linestrings w/ 4 different ID column names", {
  ID_COL_LIST      <- c("ex_id", "comid", "id", "hy_id")
  MAX_NUM_OF_LINES <- 10
  
  for (ID_COL in ID_COL_LIST) {
    
    TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
    message(paste0(ID_COL, " - ", TO_ID_COL))
    
    for (i in 1:MAX_NUM_OF_LINES) {
      
      message(i)
     
      disconnected <- get_test_lines(i, id_col_name = ID_COL, connected = FALSE)
      # disconnected
      # plot(disconnected$geometry, col = "red", add = F)
      
      # make node topology dataframe 
      nodes <- hydrofabric3D:::get_node_topology(disconnected, ID_COL)
      
      # get the connected components graph
      cc <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
      
      # Check output columns
      testthat::expect_true(  find_connected_components_has_min_output_cols(cc, ID_COL))
      
      # check for correct component IDs (DISCONNECTED)
      component_ids <- unique(cc$component_id)
      expected_component_ids <- sort(unique(1:i))
      
      has_expected_component_ids <- all(component_ids %in% expected_component_ids)
      testthat::expect_true(has_expected_component_ids)
      
      # IDs from the original data
      start_ids <- unique(disconnected[[ID_COL]])
      
      # IDs in the ending node dataframe
      end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
      end_ids  <- end_ids[end_ids != 0]
      
      # that all IDs from the flowlines are in the end node dataframe
      has_all_ids <- all(start_ids %in% end_ids)
      testthat::expect_true(has_all_ids)
      
      }
  }
  
})

# TODO: test should hit in error
testthat::test_that("find_connected_components(), 0 SF linestrings" , {
  
  ID_COL        <- "comid"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  FLOWLINE_INDICES <- c(0)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  flowlines[[ID_COL]] <- as.character(flowlines[[ID_COL]])
  # flowlines$geom %>% plot()
  
  # # plot(flowlines$geom)
  # NUM_FLOWLINES <- nrow(flowlines)
  # NUM_CONNECTED_COMPONENTS <- 1
  # 
  # # make node topology dataframe 
  # nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  # 
  # # get the connected components graph
  # cc <- hydrofabric3D:::find_connected_components(nodes, ID_COL)
  # 
  # has_correct_cols <- find_connected_components_has_min_output_cols(cc, ID_COL)
  # testthat::expect_true(has_correct_cols)
  # 
  # # check for correct component IDs
  # component_ids <- unique(cc$component_id)
  # expected_component_ids <- sort(unique(1:NUM_CONNECTED_COMPONENTS))
  # 
  # has_expected_component_ids <- all(component_ids %in% expected_component_ids)
  # testthat::expect_true(has_expected_component_ids)
  # 
  # # IDs from the original data
  # start_ids <- unique(flowlines[[ID_COL]])
  # 
  # # IDs in the ending node dataframe
  # end_ids  <- unique(c(cc[[ID_COL]], cc[[TO_ID_COL]]))
  # end_ids  <- end_ids[end_ids != 0]
  # 
  # # that all IDs from the flowlines are in the end node dataframe
  # has_all_ids <- all(start_ids %in% end_ids)
  # testthat::expect_true(has_all_ids)
  
})
