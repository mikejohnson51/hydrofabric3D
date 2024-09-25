library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()


# -------------------------------------------------------------------
# ---- hydrofabric::get_node_topology() ----
# -------------------------------------------------------------------

testthat::test_that("1-10 (1:1, 1:2, ... 1:10) connected SF linestrings (ID and node checks)", {
  
  ID_COL        <- "id"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL))  
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  for (i in 1:NUM_FLOWLINES) {
    # message(i)
    
    flines_subset <- 
      flowlines %>% 
      dplyr::slice(1:i)
 
    # make node topology dataframe 
    nodes <- hydrofabric3D:::get_node_topology(flines_subset, ID_COL)
    
    has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
    testthat::expect_true(has_correct_cols)
    
    # nodes
    # flowlines %>%
    #   ggplot2::ggplot() +
    #   ggplot2::geom_sf(ggplot2::aes(color = id)) +
    #   ggplot2::geom_sf_label(ggplot2::aes(label = id))
    # plot(flowlines$geom)
    
    # IDs from the original data
    start_ids <- unique(flines_subset[[ID_COL]])
    
    # IDs in the ending node dataframe
    end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
    end_ids  <- end_ids[end_ids != 0]
    
    # that all IDs from the flowlines are in the end node dataframe
    has_all_ids <- all(start_ids %in% end_ids)
    testthat::expect_true(has_all_ids)
    
    # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
    node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
    expected_nodes_ids <- 0:i
    
    correct_node_ids <- all(node_ids == expected_nodes_ids )
    testthat::expect_true(correct_node_ids)
  }
  
})

testthat::test_that("3 singular and disconnected SF linestrings", {
  
  ID_COL        <- "id"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  FLOWLINE_INDICES <- c(1, 4, 7)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  # plot(flowlines$geom)
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  all_zero_to_ids <- all(nodes[[TO_ID_COL]] == 0)
  testthat::expect_true(all_zero_to_ids)
  
  all_zero_to_nodes <- all(nodes$tonode == 0)
  testthat::expect_true(all_zero_to_nodes)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
})

testthat::test_that("3 disconnected groups of 2 SF linestrings each w/ 1 neighbor", {
  
  ID_COL        <- "id"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  FLOWLINE_INDICES <- c(1, 2, 
                        4, 5, 
                        9, 10)
  
  # number of disconnected parts of the network (i.e. flowlines that do not connect with other parts of a the dataset are "disconnected")
  FLOWLINE_GROUPS  <- 3
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  # mapview::mapview(flowlines)
  # flowlines %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = id)) +
  #   ggplot2::geom_sf_label(ggplot2::aes(label = id))
  # plot(flowlines$geom)
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)

  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  has_expected_num_of_zero_to_ids   <- sum(nodes[[TO_ID_COL]] == 0) == FLOWLINE_GROUPS
  testthat::expect_true(has_expected_num_of_zero_to_ids)
  
  has_expected_num_of_zero_to_nodes <- sum(nodes$tonode == 0) == FLOWLINE_GROUPS
  testthat::expect_true(has_expected_num_of_zero_to_nodes)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
})


testthat::test_that("2 disconnected groups of 1 and 3 SF linestrings", {
  
  ID_COL        <- "id"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  FLOWLINE_INDICES <- c(1, 2, 3 ,
                       10
                        )
  
  # number of disconnected parts of the network (i.e. flowlines that do not connect with other parts of a the dataset are "disconnected")
  FLOWLINE_GROUPS  <- 2
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  # mapview::mapview(flowlines)
  # flowlines %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = id)) +
  #   ggplot2::geom_sf_label(ggplot2::aes(label = id))
  # plot(flowlines$geom)
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  has_expected_num_of_zero_to_ids   <- sum(nodes[[TO_ID_COL]] == 0) == FLOWLINE_GROUPS
  testthat::expect_true(has_expected_num_of_zero_to_ids)
  
  has_expected_num_of_zero_to_nodes <- sum(nodes$tonode == 0) == FLOWLINE_GROUPS
  testthat::expect_true(has_expected_num_of_zero_to_nodes)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
})

testthat::test_that("10 flowlines with 1 braid (1 loop) in the middle (SF linestrings)", {
  
  ID_COL        <- "comid"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  # FLOWLINE_INDICES <- c(1, 4, 7)
  # FLOWLINE_INDICES <- c(1, 88, 341, 343, 344, 345, 388, 392, 393, 394)
  FLOWLINE_INDICES <- c(1, 341, 343, 344, 345, 388, 392, 394)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>% 
    dplyr::slice(FLOWLINE_INDICES)
  
  flowlines[[ID_COL]] <- as.character(flowlines[[ID_COL]])
   
  # plot(flowlines$geom)
  
    # dplyr::slice(1:40)
  # flowlines
  # braided <- hydrofabric3D::add_braid_ids(flowlines, ID_COL) %>% 
  #   dplyr::mutate(
  #     has_braid = braid_id != "no_braid"
  #   )
  # mapview::mapview(braided, zcol = "has_braid")
  # which(braided$comid %in% c(22180836, 22180848, 22180838, 22180816, 22180814))
  # c(1, 88, 341, 343, 344, 345, 388, 392, 393, 394)
  # which(braided$braid_id =="braid_17")
  # braided %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = has_braid)) 
  # flowlines %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = get(ID_COL))) +
  #   ggplot2::geom_sf_label(ggplot2::aes(label = get(ID_COL)))
  # braided
  
  # hydrofabric3D::find_braids(flowlines, ID_COL)
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
 
  # count of each node number (ignoring ZEROs) 
  fromnode_counts <- table(nodes$fromnode[nodes$fromnode != 0]) 
  tonode_counts   <- table(nodes$tonode[nodes$tonode != 0])
  # fromnode_counts <- table(nodes$fromnode) 
  # tonode_counts   <- table(nodes$tonode)
  
  # its expected for a braided network to have these duplicates, something is wrong if they dont
  has_duplicate_fromnodes <- any(fromnode_counts > 1)
  has_duplicate_tonodes   <- any(tonode_counts > 1)
  
  testthat::expect_true(has_duplicate_fromnodes)
  testthat::expect_true(has_duplicate_tonodes)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
})

testthat::test_that("12 flowlines with 1 braid (2 loops) in the middle (SF linestrings)", {
  ID_COL        <- "comid"
  TO_ID_COL     <- hydrofabric3D:::as_to_id(ID_COL)
  
  # indices of a braid
  FLOWLINE_INDICES <- c(334, 335, 336, 337, 338, 339, 381, 382, 383, 387, 384, 389)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) %>%
    # dplyr::select(dplyr::any_of(ID_COL), hydroseq, dnhydroseq, totdasqkm) 
    dplyr::slice(FLOWLINE_INDICES)
  
  flowlines[[ID_COL]] <- as.character(flowlines[[ID_COL]])
  
  # plot(flowlines$geom)
  
  # dplyr::slice(1:40)
  # flowlines
  # braided <- hydrofabric3D::add_braid_ids(flowlines, ID_COL) %>%
  #   dplyr::mutate(
  #     has_braid = braid_id != "no_braid"
  #   )
  # 
  # braided <- 
  #   braided %>% 
  #   dplyr::group_by(braid_id) %>% 
  #   dplyr::mutate(
  #     min_hydroseq = min(dnhydroseq),
  #     max_hydroseq = max(hydroseq)
  #   ) %>% 
  #   dplyr::relocate(min_hydroseq, max_hydroseq) %>% 
  #   dplyr::ungroup() %>% 
  #   sf::st_as_sf()
  
  # mapview::mapview(braided, zcol = "has_braid")
  # which(braided$comid %in% c(22180776, 22180806))
  # which(braided$braid_id =="braid_15, braid_16")
  # braided %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = has_braid)) 
  # flowlines %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = get(ID_COL))) +
  #   ggplot2::geom_sf_label(ggplot2::aes(label = get(ID_COL)))
  # braided
  # hydrofabric3D::find_braids(flowlines, ID_COL)
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # count of each node number (ignoring ZEROs) 
  fromnode_counts <- table(nodes$fromnode[nodes$fromnode != 0]) 
  tonode_counts   <- table(nodes$tonode[nodes$tonode != 0])
  # fromnode_counts <- table(nodes$fromnode) 
  # tonode_counts   <- table(nodes$tonode)
  
  # its expected for a braided network to have these duplicates, something is wrong if they dont
  has_duplicate_fromnodes <- any(fromnode_counts > 1)
  has_duplicate_tonodes   <- any(tonode_counts > 1)
  
  testthat::expect_true(has_duplicate_fromnodes)
  testthat::expect_true(has_duplicate_tonodes)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
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
  
  # dplyr::slice(1:40)
  # flowlines
  # braided <- hydrofabric3D::add_braid_ids(flowlines, ID_COL) %>% 
  #   dplyr::mutate(
  #     has_braid = braid_id != "no_braid"
  #   )
  # mapview::mapview(braided, zcol = "has_braid")
  # which(braided$comid %in% c(22180836, 22180848, 22180838, 22180816, 22180814))
  # c(1, 88, 341, 343, 344, 345, 388, 392, 393, 394)
  # which(braided$braid_id =="braid_17")
  # braided %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = has_braid)) 
  # flowlines %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = get(ID_COL))) +
  #   ggplot2::geom_sf_label(ggplot2::aes(label = get(ID_COL)))
  # braided
  
  # hydrofabric3D::find_braids(flowlines, ID_COL)
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # count of each node number (ignoring ZEROs) 
  fromnode_counts <- table(nodes$fromnode[nodes$fromnode != 0]) 
  tonode_counts   <- table(nodes$tonode[nodes$tonode != 0])
  # fromnode_counts <- table(nodes$fromnode) 
  # tonode_counts   <- table(nodes$tonode)
  
  # its expected for a braided network to have these duplicates, something is wrong if they dont
  has_duplicate_fromnodes <- any(fromnode_counts > 1)
  has_duplicate_tonodes   <- any(tonode_counts > 1)
  
  testthat::expect_true(has_duplicate_fromnodes)
  testthat::expect_true(has_duplicate_tonodes)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
})

testthat::test_that("508 flowlines with 17 braid (17 loops) in the middle (SF linestrings)", {
  ID_COL         <- "comid"
  TO_ID_COL      <- hydrofabric3D:::as_to_id(ID_COL)
  NUM_OF_BRAIDS  <- 17
  # indices of a braid
  # FLOWLINE_INDICES <- c(334, 335, 336, 337, 338, 339, 381, 382, 383, 387, 384, 389)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))  %>% 
    dplyr::select(dplyr::any_of(ID_COL)) 
    # dplyr::select(dplyr::any_of(ID_COL), hydroseq, dnhydroseq, totdasqkm) 
    # dplyr::slice(FLOWLINE_INDICES)
  
  flowlines[[ID_COL]] <- as.character(flowlines[[ID_COL]])
  # plot(flowlines$geom)
  
  # braided <- hydrofabric3D::add_braid_ids(flowlines, ID_COL) %>%
  #   dplyr::mutate(
  #     has_braid = braid_id != "no_braid"
  #   )
  # 
  # length(unique(unlist(strsplit(braided$braid_id[braided$braid_id != "no_braid"], ", "))))
  # NUM_OF_BRAIDS <- 17
  
  # flowlines %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = get(ID_COL))) +
  #   ggplot2::geom_sf_label(ggplot2::aes(label = get(ID_COL)))
  # braided
  # hydrofabric3D::find_braids(flowlines, ID_COL)
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # count of each node number (ignoring ZEROs) 
  fromnode_counts <- table(nodes$fromnode[nodes$fromnode != 0]) 
  tonode_counts   <- table(nodes$tonode[nodes$tonode != 0])
  # fromnode_counts <- table(nodes$fromnode) 
  # tonode_counts   <- table(nodes$tonode)
  
  # its expected for a braided network to have these duplicates, something is wrong if they dont
  has_duplicate_fromnodes <- any(fromnode_counts > 1)
  has_duplicate_tonodes   <- any(tonode_counts > 1)
  
  testthat::expect_true(has_duplicate_fromnodes)
  testthat::expect_true(has_duplicate_tonodes)
  
  # should have a single zero tonode (going out of network)
  tonode_zero_count <- sum(nodes$tonode == 0)
  
  has_single_zero_tonode <- tonode_zero_count == 1
  testthat::expect_true(has_single_zero_tonode)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
})

testthat::test_that("Node topology with braided flowlines removed from network (0 braids / loops) (SF linestrings)", {
  ID_COL         <- "comid"
  TO_ID_COL      <- hydrofabric3D:::as_to_id(ID_COL)
  NUM_OF_BRAIDS  <- 17
  # indices of a braid
  # FLOWLINE_INDICES <- c(334, 335, 336, 337, 338, 339, 381, 382, 383, 387, 384, 389)
  # flowlines$divergence
  # sum(flowlines$divergence == 0)
  
  flowlines <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))  %>% 
    dplyr::filter(divergence !=2) %>%
    dplyr::select(dplyr::any_of(ID_COL)) 
  
  flowlines[[ID_COL]] <- as.character(flowlines[[ID_COL]])
  # plot(flowlines$geom)
  
  # braided <- hydrofabric3D::add_braid_ids(flowlines, ID_COL) %>%
  #   dplyr::mutate(
  #     has_braid = braid_id != "no_braid"
  #   )
  # 
  # length(unique(unlist(strsplit(braided$braid_id[braided$braid_id != "no_braid"], ", "))))
  # NUM_OF_BRAIDS <- 17
  
  # flowlines %>%
  #   ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = get(ID_COL))) +
  #   ggplot2::geom_sf_label(ggplot2::aes(label = get(ID_COL)))
  # braided
  # hydrofabric3D::find_braids(flowlines, ID_COL)
  
  NUM_FLOWLINES <- nrow(flowlines)
  
  # make node topology dataframe 
  nodes <- hydrofabric3D:::get_node_topology(flowlines, ID_COL)
  
  # check to IDs and to nodes are ALL ZERO (each flowline is isolated with no neighbors)
  has_correct_cols <- get_node_topology_has_min_output_cols(nodes, ID_COL)
  testthat::expect_true(has_correct_cols)
  
  # count of each node number (ignoring ZEROs) 
  fromnode_counts <- table(nodes$fromnode[nodes$fromnode != 0]) 
  tonode_counts   <- table(nodes$tonode[nodes$tonode != 0])
  # fromnode_counts <- table(nodes$fromnode)
  # tonode_counts   <- table(nodes$tonode)
  
  # its expected for a braided network to have these duplicates, something is wrong if they dont
  has_duplicate_fromnodes <- any(fromnode_counts > 1)
  has_duplicate_tonodes   <- any(tonode_counts > 1)
  
  # flowlines %>% 
  #   dplyr::filter(flowlines, 
  #     comid %in% nodes$comid[nodes$tonode != 0][tonode_counts > 1]
  #     ) %>% mapview::mapview()
  # mapview::mapview(
  #   dplyr::filter(flowlines, 
  #               comid %in% nodes$comid[nodes$tonode != 0][tonode_counts > 1]
  # ), color = "red") +   
  # mapview::mapview(
  #   dplyr::filter(flowlines, 
  #                 !comid %in% nodes$comid[nodes$tonode != 0][tonode_counts > 1]
  #   ), color = "green")
  
  # DOES NOT have duplicate FROM NODES 
  testthat::expect_false(has_duplicate_fromnodes)
  
  # DOES have duplicate TO NODES
  testthat::expect_true(has_duplicate_tonodes)
  
  # should have a single zero tonode (going out of network)
  tonode_zero_count <- sum(nodes$tonode == 0)
  
  has_single_zero_tonode <- tonode_zero_count == 1
  testthat::expect_true(has_single_zero_tonode)
  
  # IDs from the original data
  start_ids <- unique(flowlines[[ID_COL]])
  
  # IDs in the ending node dataframe
  end_ids  <- unique(c(nodes[[ID_COL]], nodes[[TO_ID_COL]]))
  end_ids  <- end_ids[end_ids != 0]
  
  # that all IDs from the flowlines are in the end node dataframe
  has_all_ids <- all(start_ids %in% end_ids)
  testthat::expect_true(has_all_ids)
  
  # test that the node IDs are 0 through the NUMBER OF FLOWLINES 
  node_ids <- sort(unique(c(nodes$fromnode, nodes$tonode)))
  expected_nodes_ids <- 0:NUM_FLOWLINES
  
  correct_node_ids <- all(node_ids == expected_nodes_ids)
  testthat::expect_true(correct_node_ids)
  
})






