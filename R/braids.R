utils::globalVariables(
  c(".", "hy_id", "cs_id", "pt_id", "Z", "middle_index", "point_type", "minZ", 
    "maxZ", "minZ_bottom", "maxZ_left_bank", "maxZ_right_bank", "valid_left_bank", 
    "valid_right_bank", "bottom", "left_bank", "right_bank", "valid_banks", 
    "relative_distance", "cs_lengthm", "default_middle", "has_relief", 
    "max_relief", "braid_id", "geometry",
    
    "comid", "fromnode", "tonode", 
    "tocomid", "divergence", "cycle_id", "node", "braid_vector", "totdasqkm", 
    "changed", "relative_position", "head_distance", "tail_distance", 
    "component_id", "cs_measure", "ds_distance", "along_channel", "euclid_dist", 
    "sinuosity", "points_per_cs", "Z_at_bottom", "lower_bound", "upper_bound", 
    "ge_bottom", "is_near_bottom", "pts_near_bottom", "total_valid_pts", 
    "pct_near_bottom", 
    "member_braids",  "braid_members", "diff_pts", "is_extended", 
    "new_cs_id", "split_braid_ids",
    
    "braid_length", 
    "id", 
    "lengthm", 
    "check_z_values", 
    "geom", 
    "is_same_Z", 
    "is_multibraid", 
    "channel", "unique_count",
    "left_bank_count", "right_bank_count", "channel_count", "bottom_count", 
    "terminalID",
    "tmp_id",
    "make_geoms_to_cut_plot",
    "Y", "improved", "length_vector_col", "median", "min_ch", "new_validity_score",
    "old_validity_score", "transects", "validity_score", "x",
    "A", "DEPTH", "DINGMAN_R", "TW", "X", "X_end", "X_start", "Y_end", "Y_start",
    "ahg_a", "ahg_index", "ahg_x", "ahg_y", 
    "bottom_end", "bottom_length", "bottom_midpoint", 
    "bottom_start", "cs_partition", "distance_interval", "fixed_TW", 
    "has_new_DEPTH", "has_new_TW", "ind", "is_dem_point", "left_max", 
    "left_start", "max_right_position", "new_DEPTH", "new_TW", "next_X_is_missing", "next_Y_is_missing",
    "parabola", "partition", "prev_X_is_missing", 
    "prev_Y_is_missing", "right_start", "right_start_max", "start_or_end", "start_pt_id",
    "cs_source", 
    "partition_lengthm", "left_fema_index", "right_fema_index", 
    "left_is_within_fema", "right_is_within_fema", "left_distance", "right_distance",
    "new_cs_lengthm", 
    "crosswalk_id", "extend_invalid_transects2",
    "anchors", "deriv_type", "edge", "extension_distance", 
    "left_is_extended", "right_is_extended", "to_node", "verbose", 
    "toindid", "indid", "toid", "is", "internal_is_braided2"
  )
)

# -----------------------------------------------------------------------------
# ---- braid detection and processing ---- 
# -----------------------------------------------------------------------------

#' Find braided sections of a network and return the unique crosswalk_ids for each idenfied braid 
#'
#' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#'
#' @param network The network object representing the river network.
#' @param crosswalk_id unique ID column name 
#' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#'
#' @return dataframe or sf dataframe with added braid_id
#' 
#' @examples
#'  \dontrun{
#' net <- nhdplusTools::navigate_network(
#'  start       = 101,
#'  mode        = "UT",
#'  distance_km = 100
#'  ) 
#'  
#' # drop most of the columns in the network dataset
#' net <- dplyr::select(net, comid, divergence, totdasqkm, fromnode, tonode, terminalpa)
#'
#' # get a dataframe of COMIDs and braid IDs
#' braids <- find_braids(network = net, crosswalk_id = "comid")
#' 
#'
#' # returns original data with each braid_id represented
#' # by its individual COMIDs (may contain duplicate COMIDs)
#' nested_braids = find_braids(network   = net,
#'                        crosswalk_id = "comid",
#'                        nested    = FALSE
#'                        )
#' }
#' @export
find_braids <- function(
    network,
    crosswalk_id = NULL,
    nested       = TRUE,
    verbose      = FALSE
) {
  # library(dplyr)
  # library(sf) 
  # 
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # verbose = TRUE
  # nested      = TRUE
  # verbose     = TRUE
  
  # get a list of braid IDs and comids within each braid
  braids <- get_braid_list(
    network      = network, 
    crosswalk_id = crosswalk_id,
    verbose      = verbose
  )
  
  # process multibraided portions of braid list returned above 
  braids <- process_braid_list(
    braids        = braids,
    crosswalk_id  = crosswalk_id,
    nested        = nested,
    verbose       = verbose
  )
  
  # # process multibraided portions of braid list returned above 
  # braids <- process_braids(
  #   network = network, 
  #   braids  = braids,
  #   add     = add,
  #   nested  = nested,
  #   version = version,
  #   verbose = verbose
  # )
  
  return(braids)
  
}

#' Find braids and add to a dataframe/sf dataframe
#' Adds a 'braid_id' and 'is_multibraid' columns to an sf dataframe containing a crosswalk_id and sf linestring geometires
#' @param network The network object representing the river network.
#' @param crosswalk_id unique ID column name 
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#'
#' @return dataframe or sf dataframe with added braid_id
#' @importFrom dplyr mutate left_join
#' @export
add_braid_ids <- function(
    network,
    crosswalk_id = NULL,
    verbose      = FALSE
) {
  # library(dplyr)
  # library(sf) 
  # verbose = TRUE
  # 
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # 
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # verbose = TRUE
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # # 
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # verbose     = TRUE
  
  # find all braids that each crosswalk_id is a part of 
  braids <- find_braids(
    network      = network, 
    crosswalk_id = crosswalk_id,
    nested       = TRUE,
    verbose      = verbose
  )
  
  # # if braid data should be added to original data
  # join back with original data
  network <- dplyr::mutate(
    dplyr::left_join(
      network,
      braids,
      by = crosswalk_id
    ),
    braid_id      = ifelse(is.na(braid_id), "no_braid", braid_id),
    is_multibraid = ifelse(is.na(is_multibraid), FALSE, is_multibraid)
  )
  
  return(network)
  
}


# -----------------------------------------------------------------------------
# ---- Get a list of braids (IDs contained in each braid) ----
# -----------------------------------------------------------------------------

#' Create a list of braid IDs containing crosswalk_ids in each braid 
#'
#' Find and uniquely identify braids in a network of flowlines, given an sf dataframe containing crosswalk_id and sf linestring geometries, 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#'
#' @param network The network object representing the river network.
#' @param crosswalk_id unique ID column name 
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' 
#' @return list of braid IDs and COMIDs within each braid
#' @importFrom dplyr filter left_join any_of select
#' @examples
#'  \dontrun{
#' net <- nhdplusTools::navigate_network(
#'  start       = 101,
#'  mode        = "UT",
#'  distance_km = 100
#'  ) 
#' 
#' net <- dplyr::select(net, comid, divergence, totdasqkm, fromnode, tonode, terminalpa)
#'
#' # get a dataframe of COMIDs and braid IDs
#' braids <- get_braid_list(network = net, crosswalk_id = "comid")
#' }
#' @export 
get_braid_list <- function(
    network,
    crosswalk_id = NULL,
    verbose = FALSE
) {
  
  # library(dplyr)
  # library(sf) 
  # verbose = TRUE
  
  # terminal_id = NULL
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # verbose = TRUE
  
  # # Test data for braids
  # library(dplyr)
  # library(sf)
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # 
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # recycle     = FALSE
  # verbose      = TRUE
  
  # rm(is_cycle, visit, topo_map, detect_cycle, prev, node)
  
  # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
  # net <-  net2 %>%
  #   dplyr::select(comid, fromnode, tonode, streamcalc, divergence, terminalpa) %>%
  #   dplyr::filter(!comid %in% c(1079041, 1078529, 1078505, 1078403, 1078391, 1078491, 1078485, 1078483))
  # network <- net %>%
  #   dplyr::select(-terminalpa)
  # terminal_id = NULL
  # recycle     = FALSE
  # network <- net
  # terminal_id = "terminalpa"
  # verbose     = FALSE
  # mapview::mapview(network)
  # network %>% 
  #   # dplyr::filter(divergence != 2) %>% 
  #   # mapview::mapview()
  #   # .$geom %>% plot()
  #   dplyr::select(id = comid, divergence) %>% 
  #   hydroloom::hy() %>% 
  #   hydroloom::make_attribute_topology(min_distance = 5) %>% 
  #   hydroloom::make_node_topology(add_div = T) %>% 
  #   hydroloom::add_toids(return_dendritic = F) %>% 
  #   hydroloom::check_hy_graph(loop_check = T)
  
  # get the "to" version of the given 'crosswalk_id' (i.e. "to_<crosswalk_id>")
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  
  # graph <- get_node_topology(network, crosswalk_id)
  network <- dplyr::select(network,
                           dplyr::any_of(crosswalk_id)
                           # fromnode, tonode,
                           # divergence,
                           # geometry = geom
  ) 
  
  # network <-
  #   network %>%
  #   dplyr::filter(
  #     !comid %in% c(22179332)
  #   )
  # 
  # mapview::mapview(network)
  
  graph <- get_node_topology(network, crosswalk_id)
  
  # graph
  # network <- dplyr::select(network,
  #                 dplyr::any_of(crosswalk_id)
  #                 # fromnode, tonode,
  #                 # divergence,
  #                 # geometry = geom
  #                 )
  
  # network <- find_connected_components2(network = network, 
  #                                       crosswalk_id = crosswalk_id, 
  #                                       add = TRUE)
  
  components <- find_connected_components(graph = graph, 
                                          crosswalk_id = crosswalk_id,
                                          verbose = verbose
  )
  
  # components$component_id %>% unique()
  
  # join the component ID onto the network dataset
  network <- 
    network %>% 
    dplyr::left_join(
      dplyr::select(components, 
                    dplyr::any_of(crosswalk_id), 
                    component_id
      ),
      by = crosswalk_id
    )
  
  # TODO: plot connected components
  # network %>% 
  #   dplyr::mutate(
  #     component_id = as.character(component_id)
  #   ) %>% 
  #   ggplot2::ggplot() + 
  #   ggplot2::geom_sf(ggplot2::aes(color = component_id))
  
  # components$component_id %>% unique() %>% length()
  # components$comid %>% unique() %>% length()
  # components$to_comid %>% unique() %>% length()
  
  # join the component ID to the graph
  graph <- 
    graph %>% 
    dplyr::left_join(
      dplyr::select(components, 
                    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), 
                    component_id
      ),
      by = c(crosswalk_id, to_crosswalk_id)
    )
  
  # unique(network$component_id) %>% length()
  
  # list of the uniqueu component IDs 
  component_ids <- unique(graph$component_id)
  # component_ids <- unique(network$component_id)
  
  # loop through each of the unique 'component_id' and get a list of braids for each distinct componenet
  braid_list <- lapply(1:length(component_ids), function(i) {
    if(verbose) {
      # message strings
      component_id_msg     <- paste0("Component ID: ", component_ids[i], "(", i, "/", length(component_ids), ")")
      msg_delim            <- paste0(rep("-", ceiling(nchar(component_id_msg) * 1.5)), collapse = "")
      component_row_count  <- nrow(dplyr::filter(graph, 
                                                 component_id %in% component_ids[i]))
      message(component_id_msg)
      message(" > rows in component ID ", component_ids[i], ": ", component_row_count)
    }
    
    # get list of braids for distinct network
    # filter network down to the iterations component (components[i])
    b_list <- internal_get_braid_list(
      graph = dplyr::filter(graph, 
                            component_id %in% component_ids[i]), 
      crosswalk_id = crosswalk_id,
      start   = NULL,
      verbose = verbose
    )
    
    if (verbose) { message(msg_delim) }
    
    return(b_list)
    
  })
  
  # remove duplicate braids IDs and re number braid IDs
  braid_list <- reduce_braid_list(braid_list)
  
  # braid_list
  
  # braid_list <- reduce_braid_list(braid_list)
  # braid_plots <- list()
  # for (i in seq_along(braid_list)) {
  #   b <- braid_list[[i]]
  #   message(b)
  # braid_plots[[i]] <-
  #     network %>%
  #     dplyr::mutate(
  #       is_braid = dplyr::case_when(
  #         comid %in% b  ~ TRUE,
  #         TRUE       ~ FALSE
  #       )
  #     ) %>%
  #     nhdplusTools::rename_geometry("geometry") %>%
  #     ggplot2::ggplot() +
  #     ggplot2::geom_sf(ggplot2::aes(color = is_braid))
  # }
  # mapview::mapview(dplyr::filter(network2, comid %in% braid_list[[8]]),  color = 'green') + 
  # mapview::mapview(network2, 
  #                  zcol = "is_braid") 
  
  return(braid_list)
  
}

#' Find the connected components in a NHDPlus flowlines Network
#' 
#' @description Determine how many different, unconnected/seperate sets of flowlines are within a set of NHDPlus flowlines. 
#' The input 'network' dataset must contain a comid, tonode, fromnode, and then (optionally) divergence and terminalpa attributes.
#' Used internally within 'get_braid_list' and 'find_braids()' function to make sure each connected set of flowlines is addressed and braids are searched for in each seperated component.
#' @param graph data.frame from get_node_topology()
#' @param crosswalk_id unique ID column name 
#' @param verbose logical print status updates?
#' 
#' @noRd
#' @keywords internal
#' @return data.frame containing the distance between pairs of network outlets.
#' @importFrom dplyr select left_join mutate bind_rows any_of
#' @importFrom sf st_drop_geometry
#' @importFrom fastmap fastmap faststack
#' @importFrom stats setNames
find_connected_components <- function(
    graph,
    crosswalk_id = NULL,
    verbose = FALSE
) {
  
  # ----------------------------
  # ----------------------------
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # # crosswalk_id = "id"
  # 
  
  # 
  # # graph <- get_node_topology(network, crosswalk_id)
  # network <- dplyr::select(network,
  #                          dplyr::any_of(crosswalk_id)
  #                          # fromnode, tonode,
  #                          # divergence,
  #                          # geometry = geom
  # ) 
  # 
  # # ids <-  c(22179638, 22179668, 22179680, 22179688, 22179690, 22179700, 22179666, 22179682, 22179674)
  # ids <-  c(22179638, 22179668, 
  #           # 22179680, 
  #           22179688, 22179690, 22179700, 22179666, 22179682, 22179674)
  # network <-
  #   network %>%
  #   dplyr::filter(
  #     comid %in% ids
  #     # !comid %in% c(22179332 )
  #     # !comid %in% c(22179332, 22180488)
  #   )
  # # mapview::mapview(network)
  # 
  # graph <- get_node_topology(network, crosswalk_id)
  # start <- NULL
  # c(22179638, 22179668, 22179680, 22179688, 22179690, 22179700, 22179666, 22179682, 22179674)
  # sum(graph$tonode == 0)
  # sum(graph$fromnode == 1)
  # # sum(graph$tonode == 1)
  # dplyr::left_join(
  #   network, 
  #   graph, 
  #   by = "comid"
  # ) %>% 
  #   mapview::mapview()
  # 
  # ----------------------------
  # ----------------------------
  
  # # lower case names
  # names(network) <- tolower(names(network))
  # 
  # # turn network into a directed graph
  # dag <- create_dag(network)
  # graph <- create_dag(network)
  
  # # get the fromnode associated with a given COMID 'start'
  # start_node <- id_to_node(graph, crosswalk_id, start)
  # # start_node
  # 
  # if(verbose) {
  #   message("Starting braid detection at ", 
  #           ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
  # }
  
  # get the "to" version of the given 'crosswalk_id' (i.e. "to_<crosswalk_id>")
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  # drop graph geometry
  graph <- sf::st_drop_geometry(graph)
  
  # stash comids and from IDs
  id_map <- dplyr::select(
    graph,
    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)),
    fromnode,
    tonode
  )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # Questionable at this point, other option is the code below that uses 
  # single_cycles() function (COMMENTED OUT CODE BELOW)
  graph <- renode_circuits(graph, crosswalk_id, verbose = FALSE)
  
  # # make an undirected graph
  graph <- make_undirected(graph)
  
  # make a topology hashmap to use in DFS 
  topo_map <- make_topo_map(
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode,
    directed = FALSE
  )
  
  # keep track of visited nodes
  marked <- fastmap::fastmap()
  
  # set all marked values to FALSE
  marked$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i){ FALSE }),
    unique(c(graph$fromnode, graph$tonode))
  )
  )
  
  # start DFS traversal from root node
  final <- list()
  
  # nodes to start 
  start_nodes <- graph$fromnode[graph$tonode == 0]
  
  # TODO: not sure if this is 100% going to work as intended 
  # TODO: in the case that there are no tonodes == 0... (CHECK BACK ON THIS)
  # in case no fromnodes = 0, just pick the first node....
  if (length(start_nodes) < 1) {
    start_nodes <- graph$fromnode[1]
  }
  
  
  for (i in seq_along(start_nodes)) {
    
    # message("starting DFS at node ", root)
    root = as.character(start_nodes[i])
    
    # output result object
    res <- list()
    
    # initialize stack
    stack <- fastmap::faststack()
    
    # push root to top of stack
    stack$push(root)
    
    # counter
    count = 0
    
    # while stack is non empty
    while (stack$size() > 0) {
      
      count = count + 1
      
      # message("stack size: ", stack$size())
      # message("count: ", count)
      # message("Stack top to bottom:\n--------------------\n",
      #         # rev(paste0(" - ", c(unlist(stack$as_list())), sep = "\n"))
      #         rev(paste0(" - ", c(unlist(stack$as_list())), sep = " "))
      #         )
      
      # pop from top of stack
      node <- stack$pop()
      
      # convert node to character for fastmap
      node <- as.character(node)
      
      # message("Popped ", node, " from top of stack")
      
      # if vertex hasn't been visited
      if (!marked$get(node)) {
        
        # message("!!!!!!! THIS IS THE VISITING PORTION OF THE DFS ALGO !!!!!!!")
        # Process the current node, add to result object
        res[[count]] <- topo_map$get(node)
        
        # set vertex to marked
        marked$set(node, TRUE)
        # marked$get(node)
        
        # neighbors of current vertex
        neighbors <- topo_map$get(node)$to_node
        
        # message("Neighbors of node ", node , " are:\n",  paste0(" --> ", c(neighbors), sep = "\n"))
        
        # iterate through neighbors and add to stack if NOT VISITED
        for (n in neighbors) {
          # message("neighbor n: ", n)
          
          if (!marked$get(n) & n != 0) {
            # message("**** neighbor ", n, " HAS NOT BEEN VISITED ****")
            # message("Pushing ", n, " onto stack")
            
            stack$push(n)
            
          }
          # message("*************")
        }
      }
      # message("ONTO NEXT ITERATION OF WHILE LOOP")
      # message("====================================")
    }
    
    res_df <- 
      res %>% 
      dplyr::bind_rows() %>% 
      dplyr::mutate(
        component_id = i
      )
    
    final[[i]] <- res_df
    
  }
  
  # merge all the resulting dataframes
  final <- 
    final %>%
    dplyr::bind_rows() 
  
  # get the fromnode out of the 'edge' column
  final$fromnode <- unlist(lapply(final$edge, function(s) {
    strsplit(s, "-")[[1]][1] 
  }))
  
  final <- dplyr::left_join(
    dplyr::mutate(
      id_map, 
      fromnode = as.character(fromnode),
      tonode   = as.character(tonode)
    ),
    dplyr::select(
      final, fromnode, tonode = to_node, component_id
    ),
    by = c("fromnode", "tonode")
  ) %>%  
    dplyr::select(
      component_id,  
      dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), 
      fromnode, tonode
    )
  
  if(verbose) {
    message("Found ", length(unique(final$component_id)), " sets of connected componenets in graph")
  }
  
  return(final)
  
}

#' Create a node topology from sf linestrings / edge network topology.
#'
#' This function creates a node topology dataframe given an sf linestring network with a unique identifer.
#'
#' @param x A data frame or SF linestring dataframe representing a hydrologic network with crosswalk_id and linestring geometries (for SF dataframes) or a crosswalk_id with to_crosswalk_id columns 
#' @param crosswalk_id unique ID column name 
#' @noRd
#' @keywords internal
#' @return dataframe, graph representation of the input network.
#' @importFrom dplyr select tibble mutate left_join distinct
#' @importFrom tidyr separate_rows
#' @importFrom hydroloom hy align_names make_attribute_topology make_node_topology add_toids
#' @importFrom sf st_drop_geometry
#' @seealso \code{\link{hydroloom::add_toids}}, \code{\link{hydroloom::make_attribute_topology}}, \code{\link{hydroloom::make_node_topology}}
get_node_topology <- function(
    x,
    crosswalk_id    = NULL
) {
  
  # *************************************
  # Example data:
  # *************************************
  
  # to_crosswalk_id = "toid"
  # x <- dplyr::select(network, dplyr::any_of(crosswalk_id))
  # network,
  # crosswalk_id = crosswalk_id
  # x <- network
  # x = dplyr::select(network, dplyr::any_of(crosswalk_id))
  # crosswalk_id = "comid"
  # crosswalk_id = crosswalk_id
  # stash_nodes <- x %>% dplyr::select(comid, fromnode, tonode) %>% sf::st_drop_geometry()
  # *************************************

  # make a unique ID if one is not given (NULL 'id')
  if(is.null(crosswalk_id)) {
    x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id)
  
  # validate input graph
  is_valid <- validate_df(x, REQUIRED_COLS, "x")
  
  is_empty_df <- nrow(x) == 0
  
  if(is_empty_df) {
    stop("Input dataframe/sf dataframe 'x' is empty, node topology can not be generated from empty dataset")
  }
  
  # temporary change crosswalk_id to a standard "id" for hydroloom
  names(x)[names(x) == crosswalk_id]    <- "id"
  
  topo <-
    x %>%
    dplyr::select(id) %>%
    # dplyr::select(network, dplyr::any_of(crosswalk_id)) %>% 
    # dplyr::select(id = comid) %>%
    hydroloom::hy() %>% 
    hydroloom::align_names() %>% 
    hydroloom::make_attribute_topology(min_distance = 5)
  
  topo <-
    topo %>%
    hydroloom::make_node_topology(add_div = TRUE) %>%
    hydroloom::add_toids(return_dendritic = FALSE) %>%
    # dplyr::mutate(divergence = 1) %>%
    # hydroloom::add_toids(return_dendritic = TRUE) %>%
    dplyr::tibble()
  
  # # remove repeated IDs
  # topo <- topo[!duplicated(topo$id), ]
  # # re generate node topology with duplicate IDs removed
  # topo <-
  #   topo %>% 
  #   dplyr::select(-fromnode, -tonode) %>% 
  #   hydroloom::make_node_topology(add = TRUE)
  
  suppressWarnings({
    # make an adjacency list
    adj_list <- 
      topo %>%
      hydroloom::make_index_ids()  %>%
      .$to_list %>%
      # index_ids$to_list %>%
      dplyr::mutate(toindid = sapply(toindid, function(x) paste(x, collapse = ","))) %>% 
      tidyr::separate_rows(toindid, sep = ",") %>%
      dplyr::mutate(
        toindid = as.integer(trimws(toindid))
      )  
  })
  
  # TODO: delete this, for reversing graph then making undirected version
  # reverse_edges <- 
  #   adj_list %>%
  #   dplyr::rename(fromid = indid, toid = toindid) %>%
  #   dplyr::mutate(temp_from = fromid, fromid = toid, toid = temp_from) %>%
  #   dplyr::select(-temp_from) %>% 
  #   dplyr::rename(indid = fromid, toindid = toid)
  
  # # bind original and reverse edges, then drop remove duplicates
  # undir <- dplyr::bind_rows(adj_list_long, reverse_edges) %>%
  #   dplyr::distinct(indid, toindid, .keep_all = TRUE)
  
  # select id, fromnode and tonode
  adj <- 
    adj_list %>% 
    dplyr::select(id, fromnode = indid , tonode = toindid)
  
  suppressWarnings({
    adj <- 
      adj %>% 
      dplyr::left_join(
        dplyr::select(adj, toid = id, fromnode),
        by = c("tonode" = "fromnode")
      ) %>% 
      dplyr::distinct() %>% 
      dplyr::select(id, toid, fromnode, tonode) %>% 
      dplyr::mutate(
        toid   = ifelse(is.na(toid), 0, toid),
        tonode = ifelse(is.na(tonode), 0, tonode)
      )
  })
  
  # change "id" and "toid" back to "crosswalk_id" and "to_crosswalk_id"
  names(adj)[names(adj) == "id"]     <- crosswalk_id
  names(adj)[names(adj) == "toid"]   <- as_to_id(crosswalk_id)
  
  return(adj)
  
}


#' Artificially create cycles from portions of a network/graph that contain a circuit
#' (2 nodes and 2 unique edges between those nodes).
#'
#' This function is necessary because circuits do NOT get recognized by cycle detection
#' DFS implementation. By applying this function, we can artificially create cycles
#' from circuits that SHOULD represent a braided section of a river.
#'
#' @param graph A data frame representing the network/graph with columns 'fromnode', 'tonode', '<crosswalk_id>', and 'to_<crosswalk_id>'.
#' @param crosswalk_id character, unique ID column name 
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' 
#' @noRd
#' @keywords internal
#' @return A modified data frame representing the network/graph with artificial cycles created from circuits.
#' @importFrom dplyr group_by add_count filter ungroup slice select bind_rows mutate any_of tibble
#' @examples
#'  \dontrun{
#' graph <- data.frame(
#'   fromnode = c(1, 2, 2, 3),
#'   tonode = c(2, 3, 3, 4),
#'   comid = c("A", "B", "C", "D"),
#'   tocomid = c("B", "C", "D", "E")
#' )
#' crosswalk_id = "comid"
#' renode_circuits(graph, crosswalk_id)
#' }
renode_circuits <- function(graph, crosswalk_id = NULL, verbose = FALSE) {
  # crosswalk_id = "comid"
  # graph <- data.frame(
  #     fromnode = c(1, 2, 2, 3),
  #     tonode = c(2, 3, 3, 4),
  #     comid = c("A", "B", "C", "D"),
  #     to_comid = c("B", "C", "D", "E")
  #   )
  
  # make a unique ID if one is not given (NULL 'id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  REQUIRED_COLS <- c(crosswalk_id, to_crosswalk_id, "fromnode", "tonode")
  
  # validate input graph
  is_valid <- validate_df(graph, REQUIRED_COLS, "graph")
  
  # REQUIRED_COLS <- c(crosswalk_id, to_crosswalk_id, "fromnode", "tonode")
  # if (!all(REQUIRED_COLS %in% names(graph))) {
  #   missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(graph))]
  #   stop("'graph' is missing one or more of the required columns:\n > ", 
  #        paste0(missing_cols, collapse = "\n > "))
  # }
  
  # unique nodes in graph
  unodes <- unique(c(graph$fromnode, graph$tonode)) 
  
  # unodes <- unique(c(unique(graph$fromnode), unique(graph$tonode)))
  single_cycles <-
    # network %>%
    graph %>%
    dplyr::group_by(fromnode, tonode) %>% 
    dplyr::add_count() %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::ungroup()
  
  if(nrow(single_cycles) == 0) {
    if(verbose) { message("No circuits found") }
    return(graph)
    
  }
  
  # get one of the crosswalk_ids for single_cycles and reworked node topology
  renodes <- 
    single_cycles %>%
    dplyr::group_by(fromnode, tonode) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(
      dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), 
      fromnode, tonode
    )
  # dplyr::select(comid, tocomid, fromnode, tonode)
  
  # get the number of new nodes needed that don't overlap with current unique nodes in graph
  new_nodes <- seq(max(unodes)+1,  max(unodes) + nrow(renodes))
  
  # Inject a new fake node between each unique edge
  # to artificially create a cycle that can be detected in DFS function
  renodes <- dplyr::bind_rows(
    dplyr::tibble(
      !!crosswalk_id    := as.character(renodes[[crosswalk_id]]),
      !!to_crosswalk_id := paste0(renodes[[crosswalk_id]], "_v2"),
      # comid = as.character(renodes$comid),
      # tocomid = paste0(renodes$comid, "_v2"),
      fromnode = renodes$fromnode,
      tonode = new_nodes
    ),
    
    dplyr::tibble(
      !!crosswalk_id := paste0(renodes[[crosswalk_id]], "_v2"),
      !!to_crosswalk_id := as.character(renodes[[to_crosswalk_id]]),
      # comid = as.character(renodes$comid),
      # tocomid = paste0(renodes$comid, "_v2"),
      fromnode = renodes$fromnode,
      tonode = new_nodes
    ),
    
    dplyr::mutate(
      dplyr::select(
        dplyr::filter(
          single_cycles, 
          !.data[[crosswalk_id]] %in% renodes[[crosswalk_id]]
        ),
        dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), fromnode, tonode
      ),
      !!crosswalk_id    := as.character(.data[[crosswalk_id]]),
      !!to_crosswalk_id := as.character(.data[[to_crosswalk_id]])
    )
  )
  
  # remove original versions of single cycle nodes and replace with reworked nodes
  regraph <- 
    dplyr::bind_rows(
      renodes, 
      dplyr::mutate(
        dplyr::filter(
          dplyr::select(graph, dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), fromnode, tonode),
          !fromnode %in% single_cycles$fromnode
        ),
        !!crosswalk_id    := as.character(.data[[crosswalk_id]]),
        !!to_crosswalk_id := as.character(.data[[to_crosswalk_id]])
        # comid   = as.character(comid),
        # tocomid = as.character(tocomid)
      )
    )
  
  return(regraph)
}

#' Convert a Directed Acyclic Graph (DAG) into an Undirected Graph
#'
#' This function takes a Directed Acyclic Graph (DAG), typically the output of the `get_node_topology()` function,
#' and converts it into an undirected graph. The purpose of this conversion is to prepare the graph for
#' cycle and braid detection functions, as they usually require an undirected graph.
#'
#' @param graph A data frame representing the Directed Acyclic Graph (DAG) with columns 'fromnode' and 'tonode'.
#' 
#' @noRd
#' @keywords internal
#' @return An undirected graph represented as a data frame with columns 'fromnode' and 'tonode'.
#' @importFrom dplyr select tibble bind_rows
#' @importFrom sf st_drop_geometry
#' @examples
#'  \dontrun{
#' # direccted graph without cycles
#' graph <- data.frame(
#'  fromnode = c(1, 2, 3, 3),
#'  tonode   = c(2, 3, 4, 5)
#'  )
#'  # make undirected graph
#'  undirected <- make_undirected(graph)
#'  # directed graph containing a cycle
#'  graph <- data.frame(
#'  fromnode = c(1, 2, 3, 3, 4),
#'  tonode   = c(2, 3, 4, 5, 5)
#'  )
#'  # make undirected graph
#'  undirected <- make_undirected(graph)
#'  }
make_undirected <- function(graph) {
  
  REQUIRED_COLS <- c("fromnode", "tonode")
  
  # validate input graph
  is_valid <- validate_df(graph, REQUIRED_COLS, "graph")
  
  # if (!all(REQUIRED_COLS %in% names(graph))) {
  #   missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(graph))]
  #   stop("'graph' is missing one or more of the required columns:\n > ", 
  #        paste0(missing_cols, collapse = "\n > "))
  # }
  
  # get to and from nodes
  adj <- 
    graph %>% 
    dplyr::select(fromnode, tonode) %>%
    sf::st_drop_geometry() %>% 
    dplyr::tibble()
  
  # opposite topology from directed graph
  rev_edges <- dplyr::select(
    adj, 
    fromnode = tonode,
    tonode   = fromnode
    # comid = tocomid,
    # tocomid = comid,
  )
  
  # add back edges
  adj <- dplyr::bind_rows(adj, rev_edges)
  
  return(adj)
  
}

#' Make a topology fastmap object from lists of to and from nodes representing a network
#' 
#' @param from_nodes numeric or character vector containing from node topology 
#' @param to_nodes numeric or character vector containing to node topology 
#' @param directed logical whether edge strings should be directed connection string ('->') or undirected connection string ('-'). If TRUE, edge strings will seperate nodes with a directed connection string ('->'). Default is TRUE.
#' @param ... Additional vectors (optional). Additional vectors should be mapped to the from node keys in the hashmap, must be the same length as from_nodes and to_nodes vectors.
#' 
#' @noRd
#' @keywords internal
#' @return fastmap with from_nodes as keys and to_nodes as values in the hashmap
#' @importFrom fastmap fastmap
make_topo_map <- function(
    from_nodes,
    to_nodes,
    directed = TRUE,
    ...
) {
  
  # Convert input arguments to character vectors
  from_nodes <- as.character(from_nodes)
  to_nodes <- as.character(to_nodes)
  
  # Create a list of additional arguments
  add_args <- list(...)
  
  # # initialize hashmap
  topo_map <- fastmap::fastmap()
  
  for (i in 1:length(from_nodes)) {
    #   # message("iteration ", i, "/", length(from_nodes))
    
    from_node <- from_nodes[i]
    to_node   <- to_nodes[i]
    
    # check if from_node has been added to hashmap already
    if (!topo_map$has(from_node)) {
      
      # # edge string
      # edge_str <- paste0(from_node, "->", to_node)
      
      # edge string
      if(directed) {
        edge_str <- paste0(from_node, "->", to_node)
      } else {
        edge_str <- paste0(from_node, "-", to_node)
      }
      
      map_entry <- list(
        to_node     = to_node,
        edge        = edge_str,
        count       = 1
      )
      
      if(length(add_args) > 0) {
        for (n in 1:length(names(add_args))) {
          key = names(add_args)[n]
          map_entry[[key]] <-add_args[[key]][i]
        }
      }
      
      # set new entry in topology map
      topo_map$set(from_node, map_entry)
      
    } else {
      
      # # edge string
      # edge_str <- paste0(from_node, "->", to_node)
      
      # edge string
      if(directed) {
        edge_str <- paste0(from_node, "->", to_node)
      } else {
        edge_str <- paste0(from_node, "-", to_node)
      }
      
      map_entry <- list(
        to_node     = c(topo_map$get(from_node)$to_node, to_node),
        edge        = c(topo_map$get(from_node)$edge, edge_str),
        count       = c(topo_map$get(from_node)$count + 1)
      )
      
      if(length(add_args) > 0) {
        
        for (n in 1:length(names(add_args))) {
          key = names(add_args)[n]
          map_entry[[key]] <- c(topo_map$get(from_node)[[key]], add_args[[key]][i])
        }
        
      }
      
      #  Update the existing entry in topo_map
      topo_map$set(from_node, map_entry)
    }
  }
  
  # return(topo_map)
  return(topo_map)
}

#' Create a list of braid IDs containing COMIDs in each braid for a single continguous network
#'
#' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' Internal function for use in 'get_braid_list()'
#' @param graph graph representation of hydrologic network. Output of get_node_topology().
#' @param crosswalk_id unique ID column name 
#' @param start Optional argument specifying the starting point for braid detection.
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#'
#' @noRd
#' @keywords internal
#' @return list of braid IDs with COMIDs within each braid
#' @importFrom dplyr select filter any_of
#' @importFrom sf st_drop_geometry
#' @importFrom stats setNames
internal_get_braid_list  <- function(
    graph,
    crosswalk_id = NULL,
    start        = NULL,
    verbose      = FALSE
) {
  
  # ---------------------------------------------------------------
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # 
  # # make directed graph an undirected graph
  # graph <- make_undirected(graph)
  # hydrofabric3D:::find_cycles(graph)
  
  # net_init    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # network %>% 
  #   dplyr::filter(hydroseq >= 1385417, hydroseq <= 1385423 | 
  #                   hydroseq %in% c(1390468, 1390467, 1390468, 1390469, 1390470, 1390471, 1390472, 1390463, 1390464)
  #   ) %>% .$geometry %>% plot()
  
  # network <-
  #   net_init %>%
  #   dplyr::filter(hydroseq >= 1385418, hydroseq <= 1385422 |
  #                   hydroseq %in% c(1390468, 1390467, 1390468,
  #                                   1390469,
  #                                   1390470, 1390471, 1390472
  #                                   # 1390463, 1390464)
  # ) 
  # hydrofabric3D:::internal_get_braid_list(
  # graph = dplyr::filter(graph, 
  #                       component_id %in% component_ids[i])
  # crosswalk_id = crosswalk_id
  # start   = NULL
  # verbose = verbose
  
  
  # ---------------------------------------------------------------
  
  # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
  if(nrow(graph) <= 1) {
    
    if(verbose) {
      message("No cycles found, returning NULL")
    }
    
    return(NULL)
  }
  
  # # lower case names
  # names(network) <- tolower(names(network))
  
  # get "to_<crosswalk_id>" string column name 
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  # # # turn network into a directed graph
  # graph <- get_node_topology(
  #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
  #   # network,
  #   crosswalk_id = crosswalk_id
  # )
  # network$geometry %>% plot()
  
  # graph <- get_node_topology(
  #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
  #   # network,
  #   crosswalk_id = crosswalk_id,
  #   deduplicate  = FALSE
  #   )
  
  # graph <- hydrofabric3D:::create_dag(network)
  
  # get the fromnode associated with a given COMID 'start'
  start_node <- id_to_node(graph, crosswalk_id, start)
  
  if(verbose) {
    message("Starting braid detection at ", 
            ifelse(is.null(start), paste0("node: ", start_node), paste0(crosswalk_id, ": ", start)))
  }
  
  # drop graph geometry
  graph <- sf::st_drop_geometry(graph)
  # graph <- sf::st_drop_geometry(graph)
  
  # stash comids and from IDs
  id_map <- dplyr::select(
    graph,
    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)),
    # dplyr::any_of(c(crosswalk_id, to_crosswalk_id, "comid", "tocomid")),
    # dag,
    # comid,
    # id,
    fromnode,
    tonode
  )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # Questionable at this point, other option is the code below that uses 
  # single_cycles() function (COMMENTED OUT CODE BELOW)
  
  graph <- renode_circuits(graph, crosswalk_id = crosswalk_id, verbose = FALSE)
  # graph <- hydrofabric3D:::renode_circuits3(graph, verbose = FALSE)
  # graph <- hydrofabric3D:::renode_circuits3(dplyr::rename(graph, tocomid = to_comid), verbose = FALSE)
  
  # make an undirected graph
  graph <- make_undirected(graph)
  # undir <- make_undirected(graph)
  # graph <- hydrofabric3D:::make_undirected(graph)
  
  # find cycles in undirected graph (proxy for braids)
  cycles <- find_cycles(
    graph     = graph,
    # graph = distinct(graph),
    # graph     = undir,
    start     = start_node,
    edge      = FALSE,
    verbose   = verbose
  )
  
  # if(cycles$size() == 0 & is.null(single_braids)) {
  if(is.null(cycles)) {
    # message("No cycles found, returning NULL")
    return(NULL)
  }
  
  # remove added nodes from renode_circuits3()
  braids <- lapply(cycles, function(i) { 
    i[i %in% id_map$fromnode] 
  })
  
  # # match fromnodes from id_map to nodes identified in each cycle
  braids <- lapply(1:length(braids), function(k) {
    # get crosswalk_id of each braid
    id_map[id_map$fromnode %in% braids[[k]], ] %>%
      dplyr::filter(tonode %in% braids[[k]]) %>%
      .[[crosswalk_id]]
  })
  
  # set braid_id names
  braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
  
  # hydrofabric3D:::reduce_braid_list(braids)
  # for (i in seq_along(braids)) {
  #   message(i)
  #   b <- braids[i]
  #   braid_name <- names(b)
  #   
  #   # b
  #   net2 <- 
  #     network %>% 
  #     dplyr::mutate(
  #       braid_id = "no_braid"
  #     ) %>% 
  #     dplyr::mutate(
  #       braid_id = dplyr::case_when(
  #         comid %in% b[[1]] ~ paste0(braid_id, "_", braid_name),
  #         # comid %in% b[[1]] ~ braid_name,
  #         TRUE ~ "no_braid"
  #       )
  #     )
  #   
  # }
  
  # network %>%
  #   # dplyr::mutate(
  #   #   braid_id = "no_braid"
  #   # ) %>%
  #   dplyr::mutate(
  #     braid_id = dplyr::case_when(
  #       # comid %in% b[[1]] ~ paste0(braid_id, "_", braid_name),
  #       # comid %in% braids$braid_1 & comid %in% braids$braid_2 ~ "b1 + b2",
  #       comid %in% braids$braid_1 ~ "b1",
  #       comid %in% braids$braid_2 ~ "b2",
  #       comid %in% braids$braid_3 ~ "b3",
  #       comid %in% braids$braid_4 ~ "b4",
  #       # comid %in% braids$braid_1 | comid %in% braids$braid_2 ~ "b1 + b2",
  #       TRUE ~ "no_braid"
  #     )
  #   ) %>%
  
  # net2 %>%
  # ggplot2::ggplot() +
  #   ggplot2::geom_sf(ggplot2::aes(color = braid_id)) 
  #   ggplot2::facet_wrap(~braid_id)
  
  return(braids)
  
}

#' Create a list of braid IDs containing COMIDs in each braid for a single continguous network
#' Note: This function replicates internal_get_braid_list() but given a network of flowlines, rather than the graph output of 'get_node_topology()'
#' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' Internal function for use in 'get_braid_list()'
#' @param graph graph representation of hydrologic network. Output of get_node_topology().
#' @param crosswalk_id unique ID column name 
#' @param start Optional argument specifying the starting point for braid detection.
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#'
#' @noRd
#' @keywords internal
#' @return list of braid IDs with COMIDs within each braid
#' @importFrom dplyr select filter any_of
#' @importFrom sf st_drop_geometry
#' @importFrom stats setNames
internal_get_network_braid_list <- function(
    network,
    crosswalk_id = NULL,
    start        = NULL,
    verbose      = FALSE
) {
  
  # library(dplyr)
  # library(sf)
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # # 
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # network <- net2
  # start = NULL
  # verbose      = TRUE
  
  # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
  if(nrow(network) <= 1) {
    
    if(verbose) {
      message("No cycles found, returning NULL")
    }
    
    return(NULL)
  }
  
  # # check valid return_as input
  # if(!return_as %in% c("list", "dataframe")) {
  #   stop("Invalid 'return_as' argument '",
  #        return_as, "'\n'return_as' must be: 'list' or 'dataframe'")
  # }
  
  # # check relationship argument is valid
  # if(return_as == "dataframe") {
  #   if(!relationship %in% c("one-to-one", "one-to-many")) {
  #     stop("Invalid 'relationship' argument '",
  #          relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
  #   }
  # }
  
  # get the "to" version of the given 'crosswalk_id' (i.e. "to_<crosswalk_id>")
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  # create node topology graph of network 
  graph <- get_node_topology(network, crosswalk_id)
  
  # get the fromnode associated with a given crosswalk_id 'start'
  start_node <- id_to_node(graph, crosswalk_id, start)
  
  # drop graph geometry
  graph <- sf::st_drop_geometry(graph)
  
  # stash comids and from IDs
  id_map <- dplyr::select(
    graph,
    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)),
    fromnode,
    tonode
  )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # Questionable at this point, other option is the code below that uses 
  # single_cycles() function (COMMENTED OUT CODE BELOW)
  graph <- renode_circuits(graph, crosswalk_id, verbose = FALSE)
  
  # # make an undirected graph
  graph <- make_undirected(graph)
  
  # find cycles in undirected graph (proxy for braids)
  cycles <- find_cycles(
    graph     = graph,
    start     = start_node,
    edge      = FALSE,
    verbose   = verbose
  )
  
  # if(cycles$size() == 0 & is.null(single_braids)) {
  if(is.null(cycles)) {
    # message("No cycles found, returning NULL")
    return(NULL)
  }
  
  # remove added nodes from renode_circuits3()
  braids <- lapply(cycles, function(i) { 
    i[i %in% id_map$fromnode] 
  })
  
  # match fromnodes from id_map to nodes identified in each cycle
  braids <- lapply(1:length(braids), function(k) {
    # get crosswalk_id of each braid
    id_map[id_map$fromnode %in% braids[[k]], ] %>%
      dplyr::filter(tonode %in% braids[[k]]) %>%
      .[[crosswalk_id]]
  })
  
  # set braid_id names
  braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
  
  return(braids)
}

#' Collapse and rename braid IDs in a list of lists
#' Internal function. Ensures there are no duplicate braid_ids, making 
#' each braid ID unique. and collapsing the original list into a single list with list names being the braid_id
#' Collapse, deduplicate, and rename braid IDs in a list of lists
#' @description Internal function. Ensures there are no duplicate braid_ids, making each braid ID unique. and collapsing the original list into a single list with list names being the braid_id
#' @param x list, with each element either being NULL or a list with braid_id names (i.e. "braid_1", "braid_2", etc.)
#'  
#' @noRd
#' @keywords internal
#' @return list with unique braid IDs / COMIDs
reduce_braid_list <- function(x) {
  
  # x <- braid_list
  
  # Remove NULL values using indexing
  x <- x[!sapply(x, is.null)]
  
  # return the empty list if braid list is ALL NULL values
  if (inherits(x, "list") & length(x) == 0) {
    return(x)
  }
  
  # sort the comids in each braid
  # collapse the comids into an "_" underscore seperate string
  # collapse all underscore seperated comids into a single vector and remove the duplicates (i.e. unique())
  new_braids <- unique(
    unlist(
      lapply(1:length(x), function(i) {
        sapply(1:length(x[[i]]), function(k) {
          
          paste0(
            as.character(
              sort(
                Reduce(c, x[[i]][k])
                # x[[i]][k]
              )
            ),
            collapse = "_"
          )
        })
      })
    )
  )
  
  # split strings by the underscore
  new_braids <- strsplit(new_braids, "_")
  
  # convert to integer
  new_braids <- sapply(new_braids, as.integer)
  
  # assign new braid_ids as names to list
  names(new_braids) <- paste0("braid_", 1:length(new_braids))
  
  return(new_braids)
}

# -----------------------------------------------------------------------------
# ---- Process get_braid_list() output list ----
# -----------------------------------------------------------------------------

#' Convert a list of braid IDs and COMIDs into a dataframe (v2)
#' Internal function, used to process the outputs of 'get_braid_list()' within 
#' the 'find_braids()' function. The function takes a list of braid IDs and associated COMIDs and puts the list into a dataframe, 
#' and optionally adds the information back to the original 'network' dataset.
#' @param network The network object representing the river network.
#' @param braids list of braid IDs and COMIDs within each braid
#' @param crosswalk_id unique ID column name 
#' @param add Logical indicating whether to add braid information to the original network data.
#' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
#' @param version integer, which version number to use (either 1 or 2)
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' 
#' @noRd
#' @keywords internal
#' @return sf dataframe or dataframe with a braid_id mapping for each COMID
#' @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate left_join relocate select
process_braids <- function(network, 
                            braids, 
                            crosswalk_id = NULL,
                            add     = FALSE, 
                            nested  = TRUE, 
                            verbose = FALSE
) {
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  verbose = TRUE
  
  # get a list of braid IDs and comids within each braid
  braids <- get_braid_list(
    network     = network, 
    crosswalk_id = crosswalk_id,
    # terminal_id = terminal_id,
    verbose     = verbose
  )
  
  # # process multibraided portions of braid list returned above 
  # braids <- process_braids(
  #   network = network, 
  #   braids  = braids,
  #   add     = add,
  #   nested  = nested,
  #   version = version,
  #   verbose = verbose
  # )
  # network = network 
  # braids  = braids
  add     = FALSE
  nested  = TRUE
  # verbose = TRUE
  
  # rename braids to numeric values
  names(braids) <- 1:length(braids)
  
  # create a new list of braid_ids that has the braid_ids of 
  # all other braids connected to the given braid_id
  # the first braid_id in any of the vectors in "multis" will be the reference/main braid_id and
  # the following braid_ids will be the rest of the multibraided system
  multis <- find_multibraids(braids)
  
  # # any vector with more than one braid_id is a multibraid
  # is_multi <- unname(lengths(multis) > 1)
  
  # # add a logical value that says whether a COMID is part of a multibraided system,
  # # or is a flowline in more than one braid (multiple braid_ids) --> column "is_multibraid"
  
  # format braids into a dataframe
  braids <- lapply(1:length(braids), function(k) {
    
    # get crosswalk_id of each braid
    data.frame(
      comid         = braids[[k]],
      braid_id      = names(braids[k]),
      member_braids = paste0(multis[[names(braids[k])]], collapse = ", "),
      is_multibraid = length(multis[[names(braids[k])]]) > 1
    )
    
  }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::tibble()
  
  # if nested is NOT TRUE, then COMIDs (flowlines) may appear more than once in output dataset
  # ----> (i.e. a COMID/flowline can be a part of more than one braid_id and thus will have a row for "braid_1" and "braid_2")
  # if each comid should have a list of all the braids it is a part of
  # if relationship == "one-to-one" then COMIDs may appear more than once
  # in dataset as a COMID may be apart of more than one braid
  # if(relationship == "one-to-many") {
  if(!nested) {
    
    # replace numeric braid_id with "braid_<number>"
    braids$braid_id <- paste0("braid_", braids$braid_id)
    
    # drop the member braids column
    braids <- dplyr::select(braids, -member_braids)
    
  } else { # if nested is TRUE, then each COMID is unique and has a braid_id column with a comma seperated character string of all relevent braid_ids
    
    # if braid_ids should be nested so that each COMID has a braid_id column that reflects ALL braid_ids related/connected to it
    braids <- 
      braids %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
      # dplyr::group_by(comid)  %>% 
      dplyr::summarise(
        braid_id = paste0(member_braids, collapse = ", ")
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        braid_id = lapply(strsplit(braid_id, ", "), function(x) {
          paste0("braid_", sort(as.numeric(unique(x))))
        })
      )
    
    # if more than one braid_id in "braid_id" column, then it is a multibraid 
    braids$is_multibraid <- lengths(braids$braid_id) > 1
    
    # collapse braid_ids into comma seperated character string
    braids$braid_id <- Reduce(c, 
                              lapply(braids$braid_id, function(k) {
                                paste0(k, collapse = ", ")
                              })
    )
  }
  
  # # add a logical value that says whether a COMID is part of a multibraided system,
  # # or is a flowline in more than one braid (multiple braid_ids)
  # braids$is_multibraid <- braids$comid %in% multibraids
  
  # if braid data should be added to original data
  if(add) {
    
    # join back with original data
    braids <- dplyr::mutate(
      dplyr::left_join(
        network,
        braids,
        by = "comid"
      ),
      braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
    )
    # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
  } 
  
  # set no_braid values to FALSE
  braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
                                 FALSE,
                                 braids$is_multibraid
  )
  
  # move braid_id and is_multibraided columns to front of dataframe
  braids <- dplyr::relocate(braids, dplyr::any_of(crosswalk_id), braid_id, is_multibraid)
  # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
  
  return(braids)
  
}

#' Convert a list of braid IDs and crosswalk_ids into a dataframe (v3)
#' Internal function, used to process the outputs of 'get_braid_list()' within 
#' the 'find_braids()' function. The function takes a list of braid IDs and associated crosswalk_ids and puts the list into a dataframe
#' @param braids list of braid IDs and crosswalk_ids within each braid
#' @param crosswalk_id unique ID column name 
#' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate crosswalk_ids as a single COMID could be a part of multiple braids (braid_id)
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' 
#' @noRd
#' @keywords internal
#' @return sf dataframe or dataframe with a braid_id mapping for each COMID
#' @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate select any_of across
process_braid_list <- function( 
    braids, 
    crosswalk_id = NULL,
    nested  = TRUE, 
    verbose = FALSE
) {
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # 
  # # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # # crosswalk_id = "id"
  # verbose = TRUE
  # 
  # # get a list of braid IDs and crosswalk_ids within each braid
  # braids <- get_braid_list(
  #   network     = network, 
  #   crosswalk_id = crosswalk_id,
  #   # terminal_id = terminal_id,
  #   verbose     = verbose
  # )
  # crosswalk_id = "comid"
  
  # # process multibraided portions of braid list returned above 
  # braids <- process_braids(
  #   network = network, 
  #   braids  = braids,
  #   add     = add,
  #   nested  = nested,
  #   version = version,
  #   verbose = verbose
  # )
  # braids  = braids
  # nested  = TRUE
  # verbose = TRUE
  
  if (length(braids) < 1) {
    return(
      dplyr::tibble(
        !!crosswalk_id := character(),
        braid_id        = character(),
        is_multibraid   = logical()
      )
    )
  }
  
  # rename braids to numeric values
  names(braids) <- 1:length(braids)
  
  # create a new list of braid_ids that has the braid_ids of 
  # all other braids connected to the given braid_id
  # the first braid_id in any of the vectors in "multis" will be the reference/main braid_id and
  # the following braid_ids will be the rest of the multibraided system
  multis <- find_multibraids(braids)
  
  # # any vector with more than one braid_id is a multibraid
  # is_multi <- unname(lengths(multis) > 1)
  
  # # add a logical value that says whether a COMID is part of a multibraided system,
  # # or is a flowline in more than one braid (multiple braid_ids) --> column "is_multibraid"
  
  # format braids into a dataframe
  braids <- lapply(1:length(braids), function(k) {
    
    # get crosswalk_id of each braid
    dplyr::tibble(
      !!crosswalk_id := braids[[k]],
      braid_id        = names(braids[k]),
      member_braids   = paste0(multis[[names(braids[k])]], collapse = ", "),
      is_multibraid   = length(multis[[names(braids[k])]]) > 1
    )
    
    # # get crosswalk_id of each braid
    # data.frame(
    #   comid         = braids[[k]],
    #   braid_id      = names(braids[k]),
    #   member_braids = paste0(multis[[names(braids[k])]], collapse = ", "),
    #   is_multibraid = length(multis[[names(braids[k])]]) > 1
    # )
    
  }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::tibble()
  
  # Should output be a nested (list of braid_ids) or NOT nested (single braid_id for each row)
  #  - if each crosswalk_id should have a list of all the braids it is a part of 
  # OR 
  #  - if each crosswalk_id should have multiple rows, 
  # representing the different braids it is a part of 
  # ----> (i.e. a crosswalk_id/flowline can be a part of more than one braid_id and thus will have a row for "braid_1" and "braid_2")
  
  # if nested is TRUE, then each crosswalk_id is unique and has a braid_id column with a comma seperated character string of all relevent braid_ids
  if(nested) {
    
    # if braid_ids should be nested so that each crosswalk_id has a braid_id column that reflects ALL braid_ids related/connected to it
    braids <- 
      braids %>% 
      dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
      # dplyr::group_by(comid)  %>% 
      dplyr::summarise(
        braid_id = paste0(member_braids, collapse = ", ")
      ) %>% 
      dplyr::ungroup() %>% 
      dplyr::mutate(
        braid_id = lapply(strsplit(braid_id, ", "), function(x) {
          paste0("braid_", sort(as.numeric(unique(x))))
        })
      )
    
    # if more than one braid_id in "braid_id" column, then it is a multibraid 
    braids$is_multibraid <- lengths(braids$braid_id) > 1
    
    # collapse braid_ids into comma seperated character string
    braids$braid_id <- Reduce(c, 
                              lapply(braids$braid_id, function(k) {
                                paste0(k, collapse = ", ")
                              })
    )
    
  } else {    # if nested is NOT TRUE, then crosswalk_ids (flowlines) may appear more than once in output dataset
    
    # if relationship == "one-to-one" then crosswalk_ids may appear more than once
    # in dataset as a crosswalk_id may be apart of more than one braid
    
    # replace numeric braid_id with "braid_<number>"
    braids$braid_id <- paste0("braid_", braids$braid_id)
    
    # drop the member braids column
    braids <- dplyr::select(braids, -member_braids)
  }
  
  # # add a logical value that says whether a COMID is part of a multibraided system,
  # # or is a flowline in more than one braid (multiple braid_ids)
  # braids$is_multibraid <- braids$comid %in% multibraids
  
  # # if braid data should be added to original data
  # if(add) {
  #   
  #   # join back with original data
  #   braids <- dplyr::mutate(
  #     dplyr::left_join(
  #       network,
  #       braids,
  #       by = "comid"
  #     ),
  #     braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
  #   )
  #   # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
  # } 
  # 
  # set no_braid values to FALSE
  braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
                                 FALSE,
                                 braids$is_multibraid
  )
  
  # move crosswalk_id, braid_id and is_multibraided columns to front of dataframe
  braids <- braids[ , c(crosswalk_id, "braid_id", "is_multibraid")]
  # braids <- dplyr::relocate(braids, dplyr::any_of(crosswalk_id), braid_id, is_multibraid)
  
  return(braids)
  
}

# 
# # Convert a list of braid IDs and COMIDs into a dataframe (v1)
# # Internal function, used to process the outputs of 'get_braid_list()' within 
# # the 'find_braids()' function. The function takes a list of braid IDs and associated COMIDs and puts the list into a dataframe, 
# # and optionally adds the information back to the original 'network' dataset.
# # @param network The network object representing the river network.
# # @param braids list of braid IDs and COMIDs within each braid
# # @param add Logical indicating whether to add braid information to the original network data.
# # @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
# # @param verbose Logical indicating whether to display verbose messages during the braid detection process.
# # 
# # @noRd
# # @keywords internal
# # @return sf dataframe or dataframe with a braid_id mapping for each COMID
# # @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate left_join relocate select
# process_braids2 <- function(network, 
#                             braids, 
#                             add     = FALSE, 
#                             nested  = TRUE, 
#                             verbose = FALSE
# ) {
#   
#   # network = network 
#   # braids  = braids
#   # add     = FALSE
#   # nested  = TRUE
#   # verbose = TRUE
#   
#   # get comids of multibraids
#   multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
#   
#   # format braids into a dataframe
#   braids <- lapply(1:length(braids), function(k) {
#     
#     # get crosswalk_id of each braid
#     data.frame(
#       comid    = braids[[k]],
#       braid_id = names(braids[k])
#     )
#   }) %>% 
#     dplyr::bind_rows() %>% 
#     dplyr::tibble()
#   
#   # if each comid should have a list of all the braids it is a part of
#   # if relationship == "one-to-one" then COMIDs may appear more than once
#   # in dataset as a COMID may be apart of more than one braid
#   # if(relationship == "one-to-many") {
#   if(nested) { 
#     
#     braids <-
#       braids %>%
#       dplyr::group_by(comid) %>% 
#       dplyr::summarise(
#         braid_id = paste0(braid_id, collapse = ", ")
#       ) %>% 
#       dplyr::ungroup()
#     
#   }
#   
#   # add a logical value that says whether a COMID is part of a multibraided system,
#   # or is a flowline in more than one braid (multiple braid_ids)
#   braids$is_multibraid <- braids$comid %in% multibraids
#   
#   # if braid data should be added to original data
#   if(add) {
#     
#     # join back with original data
#     braids <- dplyr::mutate(
#       dplyr::left_join(
#         network,
#         braids,
#         by = "comid"
#       ),
#       braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#     )
#     # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#   } 
#   
#   # set no_braid values to FALSE
#   braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#                                  FALSE, braids$is_multibraid)
#   
#   # move braid_id and is_multibraided columns to front of dataframe
#   braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#   # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#   
#   return(braids)
#   
#   
# }
# 
# # Convert a list of braid IDs and crosswalk_ids into a dataframe (v1)
# # Internal function, used to process the outputs of 'get_braid_list()' within 
# # the 'find_braids()' function. The function takes a list of braid IDs and associated crosswalk_ids and puts the list into a dataframe
# # @param braids list of braid IDs and crosswalk_ids within each braid
# # @param crosswalk_id unique ID column name 
# # @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate crosswalk_ids as a single COMID could be a part of multiple braids (braid_id)
# # @param verbose Logical indicating whether to display verbose messages during the braid detection process.
# # 
# # @noRd
# # @keywords internal
# # @return sf dataframe or dataframe with a braid_id mapping for each crosswalk_id
# # @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate select any_of across
# process_braid_list2 <- function(
#     braids, 
#     crosswalk_id = NULL,
#     nested  = TRUE, 
#     verbose = FALSE
# ) {
#   
#   # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
#   # crosswalk_id = "comid"
#   # 
#   # # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
#   # # crosswalk_id = "id"
#   # verbose = TRUE
#   # 
#   # # get a list of braid IDs and crosswalk_ids within each braid
#   # braids <- get_braid_list(
#   #   network     = network, 
#   #   crosswalk_id = crosswalk_id,
#   #   # terminal_id = terminal_id,
#   #   verbose     = verbose
#   # )
#   # crosswalk_id = "comid"
#   # # network = network 
#   # # braids  = braids
#   # # add     = FALSE
#   # nested  = FALSE
#   # verbose = TRUE
#   
#   # get crosswalk_ids of multibraids
#   multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
#   
#   # format braids into a dataframe
#   braids <- lapply(1:length(braids), function(k) {
#     # k = 1
#     # get crosswalk_id of each braid
#     dplyr::tibble(
#       !!crosswalk_id := braids[[k]],
#       braid_id        = names(braids[k])
#     )
#     # data.frame(
#     #   crosswalk_id  = braids[[k]],
#     #   braid_id      = names(braids[k])
#     # )
#     
#   }) %>% 
#     dplyr::bind_rows() %>% 
#     dplyr::tibble()
#   
#   # if each crosswalk_id should have a list of all the braids it is a part of
#   # if relationship == "one-to-one" then crosswalk_ids may appear more than once
#   # in dataset as a crosswalk_id may be apart of more than one braid
#   # if(relationship == "one-to-many") {
#   if(nested) { 
#     
#     braids <-
#       braids %>%
#       dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
#       # dplyr::group_by(comid) %>% 
#       dplyr::summarise(
#         braid_id = paste0(braid_id, collapse = ", ")
#       ) %>% 
#       dplyr::ungroup()
#     
#   }
#   
#   # add a logical value that says whether a crosswalk_id is part of a multibraided system,
#   # or is a flowline in more than one braid (multiple braid_ids)
#   if (!is.null(multibraids)) {
#     braids$is_multibraid <- braids[[crosswalk_id]] %in% multibraids
#   } else {
#     # multibraid is NULL (all braids are NOT multibraids)
#     braids$is_multibraid <- FALSE
#   }
#   
#   # # if braid data should be added to original data
#   # if(add) {
#   #   
#   #   # join back with original data
#   #   braids <- dplyr::mutate(
#   #     dplyr::left_join(
#   #       network,
#   #       braids,
#   #       by = "comid"
#   #     ),
#   #     braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#   #   )
#   #   # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#   # } 
#   
#   # set no_braid values to FALSE
#   braids$is_multibraid <- ifelse(
#     is.na(braids$is_multibraid), 
#     FALSE, 
#     braids$is_multibraid
#   )
#   
#   # move crosswalk_id, braid_id and is_multibraided columns to front of dataframe
#   braids <- braids[ , c(crosswalk_id, "braid_id", "is_multibraid")]
#   # braids <- dplyr::relocate(braids, dplyr::any_of(crosswalk_id), braid_id, is_multibraid)
#   
#   return(braids)
#   
#   
# }
# # Convert a list of braid IDs and COMIDs into a dataframe
# # Internal function, used to process the outputs of 'get_braid_list()' within 
# # the 'find_braids()' function. The function takes a list of braid IDs and associated COMIDs and puts the list into a dataframe, 
# # and optionally adds the information back to the original 'network' dataset.
# # @param network The network object representing the river network.
# # @param braids list of braid IDs and COMIDs within each braid
# # @param add Logical indicating whether to add braid information to the original network data.
# # @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
# # @param version integer, which version number to use (either 1 or 2)
# # @param verbose Logical indicating whether to display verbose messages during the braid detection process.
# # 
# # @noRd
# # @keywords internal
# # @return sf dataframe or dataframe with a braid_id mapping for each COMID
# # @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate left_join relocate select
# process_braids_v1 <- function(network, 
#                               braids, 
#                               add     = FALSE, 
#                               nested  = TRUE, 
#                               version = 1, 
#                               verbose = FALSE
# ) {
#   
#   # network = network 
#   # braids  = braids
#   # add     = FALSE
#   # nested  = TRUE
#   # version = 2
#   # verbose = TRUE
#   
#   if(!version %in% c(1, 2)) {
#     stop("Invalid 'version' argument, 'version' must be equal to 1 or 2")
#   }
#   
#   ### process braids (version 1) ###
#   if(version == 1) {
#     
#     # get comids of multibraids
#     multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
#     
#     # format braids into a dataframe
#     braids <- lapply(1:length(braids), function(k) {
#       
#       # get crosswalk_id of each braid
#       data.frame(
#         comid    = braids[[k]],
#         braid_id = names(braids[k])
#       )
#     }) %>% 
#       dplyr::bind_rows() %>% 
#       dplyr::tibble()
#     
#     # if each comid should have a list of all the braids it is a part of
#     # if relationship == "one-to-one" then COMIDs may appear more than once
#     # in dataset as a COMID may be apart of more than one braid
#     # if(relationship == "one-to-many") {
#     if(nested) { 
#       
#       braids <-
#         braids %>%
#         dplyr::group_by(comid) %>% 
#         dplyr::summarise(
#           braid_id = paste0(braid_id, collapse = ", ")
#         ) %>% 
#         dplyr::ungroup()
#       
#     }
#     
#     # add a logical value that says whether a COMID is part of a multibraided system,
#     # or is a flowline in more than one braid (multiple braid_ids)
#     braids$is_multibraid <- braids$comid %in% multibraids
#     
#     # if braid data should be added to original data
#     if(add) {
#       
#       # join back with original data
#       braids <- dplyr::mutate(
#         dplyr::left_join(
#           network,
#           braids,
#           by = "comid"
#         ),
#         braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#       )
#       # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#     } 
#     
#     # set no_braid values to FALSE
#     braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#                                    FALSE, braids$is_multibraid)
#     
#     # move braid_id and is_multibraided columns to front of dataframe
#     braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#     # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#     
#     return(braids)
#     
#     
#   } else { ### process braids (version 2) ###
#     
#     # rename braids to numeric values
#     names(braids) <- 1:length(braids)
#     
#     # create a new list of braid_ids that has the braid_ids of 
#     # all other braids connected to the given braid_id
#     # the first braid_id in any of the vectors in "multis" will be the reference/main braid_id and
#     # the following braid_ids will be the rest of the multibraided system
#     multis <- find_multibraids(braids)
#     
#     # # any vector with more than one braid_id is a multibraid
#     # is_multi <- unname(lengths(multis) > 1)
#     
#     # # add a logical value that says whether a COMID is part of a multibraided system,
#     # # or is a flowline in more than one braid (multiple braid_ids) --> column "is_multibraid"
#     
#     # format braids into a dataframe
#     braids <- lapply(1:length(braids), function(k) {
#       
#       # get crosswalk_id of each braid
#       data.frame(
#         comid         = braids[[k]],
#         braid_id      = names(braids[k]),
#         member_braids = paste0(multis[[names(braids[k])]], collapse = ", "),
#         is_multibraid = length(multis[[names(braids[k])]]) > 1
#       )
#       
#     }) %>% 
#       dplyr::bind_rows() %>% 
#       dplyr::tibble()
#     
#     # if nested is NOT TRUE, then COMIDs (flowlines) may appear more than once in output dataset
#     # ----> (i.e. a COMID/flowline can be a part of more than one braid_id and thus will have a row for "braid_1" and "braid_2")
#     # if each comid should have a list of all the braids it is a part of
#     # if relationship == "one-to-one" then COMIDs may appear more than once
#     # in dataset as a COMID may be apart of more than one braid
#     # if(relationship == "one-to-many") {
#     if(!nested) {
#       
#       # replace numeric braid_id with "braid_<number>"
#       braids$braid_id <- paste0("braid_", braids$braid_id)
#       
#       # drop the member braids column
#       braids <- dplyr::select(braids, -member_braids)
#       
#     } else { # if nested is TRUE, then each COMID is unique and has a braid_id column with a comma seperated character string of all relevent braid_ids
#       
#       # if braid_ids should be nested so that each COMID has a braid_id column that reflects ALL braid_ids related/connected to it
#       braids <- 
#         braids %>% 
#         dplyr::group_by(comid)  %>% 
#         dplyr::summarise(
#           braid_id = paste0(member_braids, collapse = ", ")
#         ) %>% 
#         dplyr::ungroup() %>% 
#         dplyr::mutate(
#           braid_id = lapply(strsplit(braid_id, ", "), function(x) {
#             paste0("braid_", sort(as.numeric(unique(x))))
#           })
#         )
#       
#       # if more than one braid_id in "braid_id" column, then it is a multibraid 
#       braids$is_multibraid <- lengths(braids$braid_id) > 1
#       
#       # collapse braid_ids into comma seperated character string
#       braids$braid_id <- Reduce(c, 
#                                 lapply(braids$braid_id, function(k) {
#                                   paste0(k, collapse = ", ")
#                                 })
#       )
#     }
#     
#     # # add a logical value that says whether a COMID is part of a multibraided system,
#     # # or is a flowline in more than one braid (multiple braid_ids)
#     # braids$is_multibraid <- braids$comid %in% multibraids
#     
#     # if braid data should be added to original data
#     if(add) {
#       
#       # join back with original data
#       braids <- dplyr::mutate(
#         dplyr::left_join(
#           network,
#           braids,
#           by = "comid"
#         ),
#         braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#       )
#       # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#     } 
#     
#     # set no_braid values to FALSE
#     braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#                                    FALSE,
#                                    braids$is_multibraid
#     )
#     
#     # move braid_id and is_multibraided columns to front of dataframe
#     braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#     # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#     
#     return(braids)
#   }
#   
# }

#' Create a list of all overlapping braid IDs for each element in a list of ID vectors
#' 
#' Given a list of braid_ids with each list element being a vector of IDs within the braid_id,
#' create a list of all the overlapping braid_ids such that a braid that is overlapping 
#' with other braids, will now contain the braid_id of its nearby/overlapping braid_ids
#'
#' @param x named list of vectors
#' 
#' @noRd
#' @keywords internal
#' @return list of the same length as input list with each list element containing a vector of all overlapping names in the rest of the list 'x' 
find_multibraids <- function(x) {
  
  # list to store the names of the list elements whose vectors have overlapping values
  overlaps <- list()
  # i = 1
  # go through original list of IDs and find overlaps
  for (i in seq_along(x)) {
    
    # sapply(x, function(vec) {
    #   vec %in% x[[i]]
    #   }
    # )
    
    overlap_names <- names(x)[
      sapply(x, function(vec) any(vec %in% x[[i]]))
    ]
    
    # remove self braid_id and then reinsert it at the front of vector
    overlap_names <- c(names(x)[i], 
                       overlap_names[names(x)[i] != overlap_names]
    )
    
    overlaps[[names(x)[i]]] <- overlap_names
  }
  
  return(overlaps)
  
}

# -----------------------------------------------------------------------------
# ---- Braid boolean check functiona ----
# -----------------------------------------------------------------------------

#' Detect whether a braid exists in a hydrologic network
#' Check if if a hydrologic network dataset contains any braids. If multiple discontinuous networks are within the 'network' data. 
#' The function will try to infer the distinct networks and then check for braids in each component (using find_connected_components()).
#' @param network sf data.frame of linestrings with a unique <crosswalk_id> attribute.
#' @param crosswalk_id unique ID column name 
#' @param recycle logical, whether the return logical vector should be recycled to the length of the number of unique networks (disconnected networks/outlets/terminalpa). 
#' If FALSE (default), the function returns TRUE if ANY of the networks contain a braid. Otherwise, if TRUE, the function attempts to distinguish the different/separate network components and 
#' returns a logical vector the length of the number of connected components in the network.
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' 
#' @return logical, If TRUE, at least one braid was detected in network, FALSE if no braids were found. If multiple components are found OR a terminal_id column is given, each unique network is checked for braiding (recycles to length of unique "terminal_id")
#' @importFrom dplyr filter select left_join any_of
#' @importFrom stats setNames
#' @export
is_braided <- function(
    network,
    crosswalk_id = NULL,
    recycle      = FALSE,
    verbose      = FALSE
) {
  
  # # Test data for braids
  # library(dplyr)
  # library(sf)
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  # 
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # recycle     = FALSE
  # verbose      = TRUE
  
  # rm(is_cycle, visit, topo_map, detect_cycle, prev, node)
  
  # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
  # net <-  net2 %>%
  #   dplyr::select(comid, fromnode, tonode, streamcalc, divergence, terminalpa) %>%
  #   dplyr::filter(!comid %in% c(1079041, 1078529, 1078505, 1078403, 1078391, 1078491, 1078485, 1078483))
  # network <- net %>%
  #   dplyr::select(-terminalpa)
  # terminal_id = NULL
  # recycle     = FALSE
  # network <- net
  # terminal_id = "terminalpa"
  # verbose     = FALSE
  
  # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
  if(nrow(network) <= 1) {
    return(FALSE)
  }
  
  # make a unique ID if one is not given (NULL 'id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  # get "to_<crosswalk_id>" string column name 
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  REQUIRED_COLS <- c(crosswalk_id)
  # REQUIRED_COLS <- c(crosswalk_id, "fromnode", "tonode")
  
  # validate input graph
  is_valid <- validate_df(network, REQUIRED_COLS, "network")
  
  # # # turn network into a directed graph
  # graph <- get_node_topology(
  #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
  #   # network,
  #   crosswalk_id = crosswalk_id
  # )
  # network$geometry %>% plot()
  
  # graph <- get_node_topology(
  #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
  #   # network,
  #   crosswalk_id = crosswalk_id,
  #   deduplicate  = FALSE
  #   )
  
  graph <- get_node_topology(network, crosswalk_id)
  # graph <- hydrofabric3D:::create_dag(network)
  
  # Create a terminal ID by finding all connected componenets in network
  # determine which flowlines are part of which contiguous set of linestrings 
  # (creates a unique identifier for each seperated network of flowlines)
  components <- find_connected_components(graph = graph, 
                                          crosswalk_id = crosswalk_id,
                                          verbose = verbose
  )
  
  # components$component_id %>% unique()
  
  # # join the component ID onto the network dataset
  # network <- 
  #   network %>% 
  #   dplyr::left_join(
  #     dplyr::select(components, 
  #                   dplyr::any_of(crosswalk_id), 
  #                   component_id
  #     ),
  #     by = crosswalk_id
  #   )
  
  # join the component ID to the graph
  graph <- 
    graph %>% 
    dplyr::left_join(
      dplyr::select(components, 
                    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)), 
                    component_id
      ),
      by = c(crosswalk_id, to_crosswalk_id)
    )
  
  # list of the uniqueu component IDs 
  component_ids <- unique(graph$component_id)
  
  # loop through each of the unique 'component_id' and check whether each component has a braid or not
  braid_checks <- sapply(1:length(component_ids), function(i) {
    
    if(verbose) {
      # message strings
      component_id_msg     <- paste0("Component ID: ", component_ids[i], "(", i, "/", length(component_ids), ")")
      msg_delim            <- paste0(rep("-", ceiling(nchar(component_id_msg) * 1.5)), collapse = "")
      component_row_count  <- nrow(dplyr::filter(graph, 
                                                 component_id %in% component_ids[i]))
      message(component_id_msg)
      message(" > rows in component ID ", component_ids[i], ": ", component_row_count)
    }
    
    # get list of braids for distinct network
    # filter graph down to the iterations component (component_ids[i])
    is_a_braid <- internal_is_braided2(
      graph        = dplyr::filter(graph,  
                                   component_id %in% component_ids[i]), 
      crosswalk_id = crosswalk_id,
      start        = NULL,
      verbose      = verbose
    )
    
    if (verbose) { message(msg_delim) }
    
    return(is_a_braid)
    
  })
  
  # if recycle == TRUE add the component_id as a name to the logical vector
  if (recycle) {
    # add names to braid_checks logical vector if recycle == TRUE
    braid_checks <- stats::setNames(braid_checks, component_ids)
    
    return(braid_checks)
  }
  
  # if ANY are braids are found, return TRUE
  braid_checks <- any(braid_checks)
  
  return(braid_checks)
  
}

#' Internal function for detecting whether a braid exists in a network of flowlines 
#' Function is used within is_braided() to determine if a network is braided (TRUE) or not (FALSE)
#' @param graph data.frame from get_node_topology()
#' @param crosswalk_id unique ID column name 
#' @param start integer crosswalk_id to start braid detection from. A start crosswalk_id should be provided in cases where network is made up of disconnected network components (i.e. a start COMID will detect all braids in each connected flowlines network) Default is NULL and braid checking will start from the flowline that has a "to_<crosswalk_id>" value of "0", or a random flowline otherwise. 
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' 
#' @noRd
#' @keywords internal
#' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found
#' @importFrom dplyr select any_of 
#' @importFrom sf st_drop_geometry
#' @importFrom fastmap fastmap
#' @importFrom stats setNames
internal_is_braided <- function(
    graph,
    crosswalk_id = NULL,
    start   = NULL,
    verbose = FALSE
) {
  
  # graph = dplyr::filter(graph,  
  #                         component_id %in% component_ids[i])
  # crosswalk_id = "comid"
  # start   = NULL
  # verbose = verbose
  # library(dplyr)
  # library(sf)
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  #
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # start = NULL
  # verbose      = TRUE
  
  # network = dplyr::filter(network,   component_id %in% components[i])
  # start   = NULL
  # verbose = verbose
  # network = dplyr::filter(network,
  #                         component_id %in% components[i])
  # start   = NULL
  # verbose = verbose
  
  # if 'graph' has 1 or 0 rows, no braids are possible, return NULL 
  if(nrow(graph) <= 1) {
    return(FALSE)
  }
  
  
  # get "to_<crosswalk_id>" string column name 
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  # get the fromnode associated with a given COMID 'start'
  start_node <- id_to_node(graph, crosswalk_id, start)
  
  if(verbose) {
    message("Starting braid detection at ", 
            ifelse(is.null(start), paste0("node: ", start_node), paste0(crosswalk_id, ": ", start)))
  }
  
  # drop graph geometry
  graph <- sf::st_drop_geometry(graph)
  # graph <- sf::st_drop_geometry(graph)
  
  # stash crosswalk_ids and from IDs
  id_map <- dplyr::select(
    graph,
    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)),
    # dplyr::any_of(c(crosswalk_id, to_crosswalk_id, "comid", "tocomid")),
    # dag,
    # comid,
    # id,
    fromnode,
    tonode
  )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # Questionable at this point, other option is the code below that uses 
  # single_cycles() function (COMMENTED OUT CODE BELOW)
  
  graph <- renode_circuits(graph = graph, crosswalk_id = crosswalk_id, verbose = FALSE)
  # graph <- hydrofabric3D:::renode_circuits3(graph, verbose = FALSE)
  # graph <- hydrofabric3D:::renode_circuits3(dplyr::rename(graph, tocomid = to_comid), verbose = FALSE)
  
  # make an undirected graph
  graph <- make_undirected(graph)
  
  # make hashmap for tonode/fromnode topology for traversing graph
  topo_map <- make_topo_map(
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode,
    directed   = FALSE
  )
  
  # keep track of visited nodes
  visit <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  visit$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i)
    { FALSE }
    ),
    unique(c(graph$fromnode, graph$tonode))
  )
  )
  
  # initialize is_cycle value
  is_cycle <- FALSE
  
  # Depth first search function for detecting cycles 
  detect_cycle <- function(node, prev) {
    
    message("--------------")
    message("node: ", node)
    message("prev: ", prev)
    message("is_cycle: ", is_cycle)
    message("--------------")
    
    # if is_cycle is TRUE, return
    if(is_cycle) {
      # message("!!!!! is_cycle IS TRUE RETURNING !!!!!")
      return()
    }
    
    # visit node
    visit$set(node, TRUE)
    
    # get the neighbors of current node
    neighbors <- topo_map$get(node)$to_node
    
    # message("Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    for (i in neighbors) {
      # message("NEIGHBOR: ", i)  
      if(
        # isTRUE(visit$get(i)) & i != prev
        visit$get(i) && i != prev
      ) {
        
        message("!!!!!!!!! neighbor ", i, " HAS BEEN VISITED and i != prev (", i, " != ", prev, ")")
        message("!!!!!!!!! SET is_cycle to TRUE")
        is_cycle <<- TRUE
        
        return()
        
      }
      # if neighbor "i" has NOT been visited yet, call DFS on neighbor "i" and node as "prev"
      if(!visit$get(i)) {
        # message("----> VISITING NEIGHBOR ", i)
        detect_cycle(node = i, prev = node)
      }
    }
  }
  
  # detect if any cycles in network
  detect_cycle(node = start_node,  prev = "none")
  
  return(is_cycle)
}


#' Internal function for detecting whether a braid exists in a network of flowlines 
#' Function is used within is_braided() to determine if a network is braided (TRUE) or not (FALSE)
#' @param network data.frame with comid, tonode, fromnode, and (optionally) divergence and terminalpa attributes.
#' @param crosswalk_id unique ID column name 
#' @param start integer crosswalk_id to start braid detection from. A start crosswalk_id should be provided in cases where network is made up of disconnected network components (i.e. a start COMID will detect all braids in each connected flowlines network) Default is NULL and braid checking will start from the flowline that has a "to_<crosswalk_id>" value of "0", or a random flowline otherwise. 
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' 
#' @noRd
#' @keywords internal
#' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found
#' @importFrom sf st_drop_geometry
#' @importFrom fastmap fastmap
#' @importFrom stats setNames
internal_is_network_braided <- function(
    network,
    crosswalk_id = NULL,
    start   = NULL,
    verbose = FALSE
) {
  
  # library(dplyr)
  # library(sf)
  
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id = "comid"
  #
  # network    <- sf::read_sf(testthat::test_path("testdata", "nextgen_braided_flowlines.gpkg"))
  # crosswalk_id = "id"
  # start = NULL
  # verbose      = TRUE
  
  # network = dplyr::filter(network,   component_id %in% components[i])
  # start   = NULL
  # verbose = verbose
  # network = dplyr::filter(network,
  #                         component_id %in% components[i])
  # start   = NULL
  # verbose = verbose
  
  # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
  if(nrow(network) <= 1) {
    return(FALSE)
  }
  
  # make a unique ID if one is not given (NULL 'id')
  if(is.null(crosswalk_id)) {
    # x             <- add_hydrofabric_id(x)
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  # get "to_<crosswalk_id>" string column name 
  to_crosswalk_id <- as_to_id(crosswalk_id)
  
  REQUIRED_COLS <- c(crosswalk_id, "fromnode", "tonode")
  
  # validate input graph
  is_valid <- validate_df(network, REQUIRED_COLS, "network")
  
  # # # turn network into a directed graph
  # graph <- get_node_topology(
  #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
  #   # network,
  #   crosswalk_id = crosswalk_id
  # )
  # network$geometry %>% plot()
  
  # graph <- get_node_topology(
  #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
  #   # network,
  #   crosswalk_id = crosswalk_id,
  #   deduplicate  = FALSE
  #   )
  
  graph <- get_node_topology(network, crosswalk_id)
  # graph <- hydrofabric3D:::create_dag(network)
  
  # get the fromnode associated with a given COMID 'start'
  start_node <- id_to_node(graph, crosswalk_id, start)
  
  if(verbose) {
    message("Starting braid detection at ", 
            ifelse(is.null(start), paste0("node: ", start_node), paste0(crosswalk_id, ": ", start)))
  }
  
  # drop graph geometry
  graph <- sf::st_drop_geometry(graph)
  # graph <- sf::st_drop_geometry(graph)
  
  # stash crosswalk_ids and from IDs
  id_map <- dplyr::select(
    graph,
    dplyr::any_of(c(crosswalk_id, to_crosswalk_id)),
    # dplyr::any_of(c(crosswalk_id, to_crosswalk_id, "comid", "tocomid")),
    # dag,
    # comid,
    # id,
    fromnode,
    tonode
  )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # Questionable at this point, other option is the code below that uses 
  # single_cycles() function (COMMENTED OUT CODE BELOW)
  
  graph <- renode_circuits(graph, crosswalk_id = crosswalk_id, verbose = FALSE)
  # graph <- hydrofabric3D:::renode_circuits3(graph, verbose = FALSE)
  # graph <- hydrofabric3D:::renode_circuits3(dplyr::rename(graph, tocomid = to_comid), verbose = FALSE)
  
  # make an undirected graph
  graph <- make_undirected(graph)
  
  # make hashmap for tonode/fromnode topology for traversing graph
  topo_map <- make_topo_map(
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode,
    directed   = FALSE
  )
  
  # keep track of visited nodes
  visit <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  visit$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i)
    { FALSE }
    ),
    unique(c(graph$fromnode, graph$tonode))
  )
  )
  
  # initialize is_cycle value
  is_cycle <- FALSE
  
  # Depth first search function for detecting cycles 
  detect_cycle <- function(node, prev) {
    
    # message("--------------")
    # message("node: ", node)
    # message("prev: ", prev)
    # message("is_cycle: ", is_cycle)
    # message("--------------")
    
    # if is_cycle is TRUE, return
    if(is_cycle) {
      # message("!!!!! is_cycle IS TRUE RETURNING !!!!!")
      return()
    }
    
    # visit node
    visit$set(node, TRUE)
    
    # get the neighbors of current node
    neighbors <- topo_map$get(node)$to_node
    
    # message("Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    for (i in neighbors) {
      # message("NEIGHBOR: ", i)  
      if(
        # isTRUE(visit$get(i)) & i != prev
        visit$get(i) && i != prev
      ) {
        
        # message("!!!!!!!!! neighbor ", i, " HAS BEEN VISITED and i != prev (", i, " != ", prev, ")")
        # message("!!!!!!!!! SET is_cycle to TRUE")
        is_cycle <<- TRUE
        
        return()
        
      }
      # if neighbor "i" has NOT been visited yet, call DFS on neighbor "i" and node as "prev"
      if(!visit$get(i)) {
        # message("----> VISITING NEIGHBOR ", i)
        detect_cycle(node = i, prev = node)
      }
    }
  }
  
  # detect if any cycles in network
  detect_cycle(node = start_node,  prev = "none")
  
  return(is_cycle)
}

# -----------------------------------------------------------------------------
# ---- ID / crosswalk_id / Node / graph utils ----
# -----------------------------------------------------------------------------

#' Convert a string to a "to_<id>"
#' Internal helper function
#' @param id character string to prefix with "to_" 
#'
#' @noRd
#' @keywords internal
#' @return character string of "id" prefixed by "to_"
as_to_id <- function(id) {
  
  to_id <- paste0("to_", id)
  
  return(to_id)
  
}

#' Convert a crosswalk_id to its corresponding fromnode in a Network
#'
#' @param network A data frame representing the network of NHDPlus flowlines with columns 'comid', 'tocomid', 'fromnode', and 'tonode'.
#' @param crosswalk_id character, colkumn name of unique ID column
#' @param id character unique ID value
#' 
#' @noRd
#' @keywords internal
#' @return The fromnode corresponding to the given COMID in the network.
id_to_node <- function(
    network, 
    crosswalk_id = NULL,
    id   = NULL
) {
  
  is_valid <- validate_df(network, c(crosswalk_id, "fromnode", "tonode"), "network")
  
  # if a start COMID is given
  if(!is.null(id)) {
    
    # if start COMID is NOT found in the network, throw error
    if(!id %in% network[[crosswalk_id]]) { stop(id, "is not a ", crosswalk_id, " in network")}
    
    # find fromnode of id to start cycle detection at
    start_node <- as.character(network$fromnode[network[[crosswalk_id]] == id])
    
  } else {
    
    # select the from node that has a tonode that is NOT in fromnodes
    start_node <- as.character(
      network$fromnode[!network$tonode %in% network$fromnode]
    )
    # if ("0" %in% network$tocomid) {
    #   # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
    #   start_node <- as.character(network$fromnode[network$tocomid == "0"])[1]
    # } else {
    #   start_node <- as.character(network$fromnode)[1]
    # }
  }
  
  # if no return, just use the first fromnode
  if (length(start_node) == 0) {
    start_node <- as.character(network$fromnode[1])
  }
  
  # In case there is more than one start_node, select the FIRST element in start_node
  start_node <- start_node[1]
  
  return(start_node)
  
}

#' Get a valid starting node from a graph
#'
#' @param graph dataframe, sf dataframe, with fromnode and tonode columns
#' @param start character, node in 'fromnode' column of graph
#'
#' @return character, node 
#' @export
get_start_node <- function(graph, start = NULL) {
  
  is_valid <- validate_df(graph, c("fromnode", "tonode"), "graph")
  
  # if a start COMID/Node is given
  if(!is.null(start)) {
    
    # if start COMID is NOT found in the network, throw error
    if(!start %in% graph$fromnode) { stop(start, " node not found in graph")}
    
    # if(verbose) { message("Starting braid detection at COMID: ", start)  }
    
    # find fromnode of comid to start cycle detection at
    # start_node <- as.character(graph$fromnode[graph$comid == start])
    start_node <- as.character(start)
    
  } else {
    # If no "start" is given, pick a node to start at
    if ("0" %in% graph$tonode) {
      
      # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
      start_node <- as.character(graph$fromnode[graph$tonode == "0"])[1]
      
      # if(verbose) {message("Starting braid detection at COMID: ", graph$comid[graph$tocomid == "0"][1])}
      
    } else {
      
      # if(verbose) { message("No 'tocomid' value equal to '0' found", 
      # "\nStarting braid detection at COMID: ", graph$comid[graph$fromnode == as.character(graph$fromnode)[1]] ) }
      start_node <- as.character(graph$fromnode)[1]
    }
  }
  
  return(start_node)
  
}

#' Convert edge string seperating nodes by a "->" to just a "to_node" value
#'
#' @param edge character
#' @param to logical whether to return the tonode (TRUE) or fromnode(FALSE)
#' @param directed logical, whether the edge string is directed/TRUE ("->") or undirected/FALSE ("-")
#' 
#' @noRd
#' @keywords internal
#' @return character of "to" or "from" node
edge_to_node <- function(edge, 
                         to       = TRUE, 
                         directed = TRUE
) {
  
  # set connection string if directed or undirected
  if(directed) {
    connect_str <- "->"
  } else {
    connect_str <- "-"
  }
  
  # if to node should be returned
  if(to) {
    # edge string
    edge_str <- sub(paste0(".*", connect_str), "", edge)
    # return(sub(".*->", "", edge))
    
    # if from node should be returned
  } else {
    # edge string
    edge_str <- sub(paste0(connect_str, ".*"), "", edge)
    # return(sub("->.*", "", edge))
  }
  
  return(edge_str)
  
}

#' Identify the indices of big multibraids 
#'
#' @param x list of vectors with each list element containing a vector crosswalk_ids representing a braid
#' 
#' @noRd
#' @keywords internal
#' @return boolean indices of the multibraids
id_multibraids <- function(x) {
  
  # overlapped braids
  overlaps <- find_overlaps(
    lst            = x,
    no_overlap_val = FALSE, 
    rm_no_overlap  = FALSE, 
    verbose        = FALSE
  ) 
  
  overlaps <- Reduce(c, overlaps)
  
  # big_cycles <- sort(
  #   unique(
  #     Reduce(c, overlaps)
  #   )
  # )
  # 
  # # remove 0 values
  # big_cycles <- big_cycles[big_cycles != 0]
  
  return(overlaps)
  
}

#' Find overlaping elements in a list of vectors
#' Given a list of vectors, return the indexes of the list elements that have duplicates in another list element. 
#' The value of each returned list element represents the index of the other list element that has duplicated vector values in the original list element
#' @param lst list
#' @param no_overlap_val numeric, character, or logical value to represent non overlapping values. Default is FALSE
#' @param rm_no_overlap logical, whether to remove list elements with no overlap. Default is FALSE, thus overlapping elements are not removed
#' @param verbose logical, whether to print messages or not. Default is FALSE
#' 
#' @noRd
#' @keywords internal
#' @return list with each value representing the index of the other list element that has duplicated vector values
find_overlaps <- function(
    lst, 
    no_overlap_val = FALSE,
    rm_no_overlap  = FALSE,
    verbose        = FALSE
) {
  # lst <- braids
  # lst <- braids
  # no_overlap_val = FALSE
  # rm_no_overlap  = FALSE
  
  overs <- lapply(seq_along(lst), function(i) {
    
    matches <- which(
      sapply(seq_along(lst), function(j) {
        i != j && all(lst[[i]] %in% lst[[j]])
      }))
    
    if (length(matches) > 0) {
      matches
    } else {
      no_overlap_val
    }
    
  })
  
  # whether to remove list elements with no overlap
  if(rm_no_overlap) {
    
    # if no overlap is NULL
    if(is.null(no_overlap_val)) {
      
      overs <- overs[!sapply(overs, is.null)]
      
      # no overlap is NOT NULL use  no_overlap_val to remove value
    } else {
      
      overs <- overs[!sapply(overs, function(j) {j == no_overlap_val})]
      
    }
  }
  
  # if no overlaps found return NULL
  if (length(overs) == 0) {
    
    if(verbose) { message("No overlaps found") }
    
    return(NULL)
    
  } 
  
  return(overs)
  
  # overs <- vector("list", length(lst))
  # for (i in 1:length(lst)) {
  #   matches <- c()
  #   for (j in 1:length(lst)) {
  #     if (i != j && all(lst[[i]] %in% lst[[j]])) {
  #       matches <- c(matches, j)
  #     }} if (length(matches) > 0) { overs[[i]] <- matches } else {  overs[[i]] <- FALSE}}
  # return(overs)
}

# -----------------------------------------------------------------------------
# ---- Braid thresholder / unpacker ----
# -----------------------------------------------------------------------------

# Apply flowline braid length threshold to braided network dataset 
# Return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset
# x: braided flowlines
# originals: not braided flowlines from the same network
# threshold: braid_threshold numeric value to remove braids with a total braid flowline length greater than 'threshold'

#' Apply flowline braid length threshold to braided network dataset 
#' Internal function
#' @param x sf object of braided flowlines (output of find_braids())
#' @param crosswalk_id unique ID column name 
#' @param originals sf object, non braided flowlines from the same network
#' @param threshold numeric, remove braids with a total braid flowline length greater than 'threshold'
#' @param new_braid_ids character, what to name braid_id column for braids that were thresholded and removed. Default is "no_braid".
#' @param verbose logical, whether to output progress messages. If TRUE (default), messages are outputted
#' 
#' @noRd
#' @keywords internal
#' @return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset, a list of 2 sf objects containing the updated braids with braids removed that are greater than the threshold value, and an sf object containing the original remaining network linestrings
#' @importFrom dplyr group_by mutate ungroup filter bind_rows
#' @importFrom sf st_length
#' @importFrom hydroloom rename_geometry 
braid_thresholder <- function(x, 
                              crosswalk_id    = NULL,
                              originals,
                              threshold = NULL, 
                              new_braid_ids = "no_braid",
                              verbose   = TRUE
) {
  # ************************************************************************
  # ************************************************************************
  # braids2 <- braids
  # network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  # crosswalk_id    = "comid"
  # # flowlines    <- dplyr::slice(flowlines, 1)
  # network <- dplyr::select(network, dplyr::any_of(crosswalk_id))
  # method          = "crosswalk_id"
  # precision       = 1
  # rm_intersects   = TRUE
  # # set geometry name of network to "geometry"
  # network <- nhdplusTools::rename_geometry(network, "geometry")
  # # add braid_id column to network
  # braids <- add_braid_ids(
  #   network      = network, 
  #   crosswalk_id = crosswalk_id,
  #   verbose      = FALSE
  # )
  # 
  # # x <- dplyr::filter(braids, braid_id != "no_braid") 
  # # dplyr::filter(braids, braid_id == "no_braid") 
  # # dplyr::filter(braids, braid_id != "no_braid") 
  # # braids
  # 
  # # not braided flowlines
  # not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # 
  # # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <- dplyr::filter(braids, braid_id != "no_braid") 
  # # braids <- dplyr::filter(braids, braid_id != "no_braid") 
  # x <- only_braids
  # crosswalk_id    = "comid"
  # originals = not_braids
  # threshold = 20000
  # 
  # new_braid_ids = "no_braid"
  # verbose   = TRUE
  
  ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### ###### 
  ###### BRAID LENGTH CHECKING
  # TODO: Refactor braid_thresholder Code to use a prescription ID
  # braid_sizes <- braid_lengths(braids, keep_geom = TRUE)
  # hist(braid_sizes$braid_length)
  
  # if (!is.null(braid_threshold)) {
  #   
  #   # remove braids that have a total flowline length greater than braid_threshold
  #   braids <- braid_thresholder(
  #     x         = braids, 
  #     originals = not_braids, 
  #     threshold = braid_threshold,
  #     verbose   = TRUE
  #   )
  #   
  #   # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
  #   not_braids <- braids$not_braids
  #   braids     <- braids$braids
  #   
  # }
  
  # ************************************************************************
  # ************************************************************************
  
  # input check for input 'x'
  if(is.null(x)) {
    stop("missing 'x' input argument")
  }
  
  # input check for input 'originals'
  if(is.null(originals)) {
    stop("missing 'originals' input argument")
  }
  
  # input check for input 'threshold'
  if(is.null(threshold)) {
    stop("missing 'threshold' input argument")
  }
  
  # force rename geometries
  x <- hydroloom::rename_geometry(x, "geometry")
  
  # unpack nested braid_id column0
  unpacked <- unpack_braids(x = x, crosswalk_id = crosswalk_id)
  
  # calculate total length of all the linestrings in each braid_id
  unpacked <-
    unpacked %>% 
    dplyr::group_by(braid_id) %>%
    # dplyr::group_by(braid_id, braid_members) %>% 
    dplyr::mutate(
      braid_length = as.numeric(
        sum(sf::st_length(geometry), na.rm = T))
    ) %>%
    dplyr::ungroup()
  # dplyr::mutate(braid_length = sf::st_length(geometry)) %>%
  # dplyr::mutate(braid_length = as.numeric(sum(braid_length, na.rm = T))) %>%
  
  # check to make sure some braids are over threshold and can be removed, if NOT then just return original data
  if (all(unpacked$braid_length <= threshold)) {
    
    message("Removing: 0 braids from braided dataset\n", 
            "Keeping: All braids as all braids have total flowline lengths less than or equal to threshold value: ", threshold)
    
    return(
      list(
        braids     = x,
        not_braids = originals
        )
    )
    
  } 
  
  # # table of TRUEs and FALSE for braids to keep/remove given 'threshold'
  # threshold_tbl <- table(unpacked$braid_length <= threshold)
  # if(verbose) { message("Removing: ",  threshold_tbl["FALSE"],  
  # " braids from braided dataset\nKeeping: ", threshold_tbl["TRUE"],
  #           " braids that have total flowline lengths less than or equal to threshold value: ", threshold)}
  
  # comids to keep (total length of braid linestrings is less than or equal to braid_threshold value)
  to_keep <- dplyr::filter(unpacked, braid_length <= threshold)[[crosswalk_id]]
  
  # # Extract the list of "braid_id" that are getting removed from output braids 
  drop_braids <- unique(dplyr::filter(x, !.data[[crosswalk_id]] %in% to_keep)$braid_id)
  # drop_braids <- unique(dplyr::filter(x, !comid %in% to_keep)$braid_id)
  
  drop_braids <- unique(unlist(strsplit(drop_braids, ", ") ))
  
  # # COMIDs that are too large, add them back to the "not_braids" data
  # to_drop <- dplyr::filter(x, !comid %in% to_keep)
  
  # keep track of keeping and removing count
  orig_nrows <- nrow(originals)
  x_nrows    <- nrow(x)
  
  # add the "too big braid COMIDs" back to original "not_braids" data 
  # and set these comids braid_ids to "no_braid" and is_multibraid = FALSE
  originals <- dplyr::bind_rows(
                originals,
                dplyr::mutate(
                  dplyr::filter(
                    x, !.data[[crosswalk_id]] %in% to_keep
                  ),
                  braid_id      = new_braid_ids,
                  # braid_id      = "no_braid",
                  # braid_id      = "thresholded",
                  is_multibraid = FALSE
                )
              )
  
  new_orig_nrows <- nrow(originals)
  
  # filter out braid_ids/COMIDs that are too big
  x <- dplyr::filter(x, .data[[crosswalk_id]] %in% to_keep)
  
  # # Drop "braid_id" IDs values that were removed via threshold length value
  updated_braid_ids <- strsplit(x$braid_id, ", ")
  updated_braid_ids <- lapply(updated_braid_ids, function(vec)
    paste0(
      vec[(!vec %in% drop_braids)],
      collapse = ", "
    )
  )
  
  # replace old "braid_id" with updated thresholded "braid_id"
  x$braid_id <- unlist(updated_braid_ids)
  
  # updating count of keeping and removing 
  new_orig_nrows <- nrow(originals)
  new_x_nrows    <- nrow(x)
  
  if(verbose) {
    message("Removing: ", new_orig_nrows - orig_nrows, 
            " braids from braided dataset\nKeeping: ",   new_x_nrows,
            " braids that have total flowline lengths less than or equal to threshold value: ", threshold)
  }
  
  return(
      list(
        braids     = x,
        not_braids = originals
      )
    )
  
}

#' Utility function that takes the output from 'find_braids(nested = TRUE)' and unpacks/unnests braid_id column
#' 
#' Unnests the comma separated braid_id column into individual rows for each braid ID/crosswalk_id pairing. 
#' This function will unnest the nested braid_ids column that is the output of calling 'find_braids()' with 'nested = TRUE', essentially giving the output of running 'nested = FALSE'.
#' 
#' @param x dataframe or sf dataframe containing a "comid" and "braid_id" column
#' @param crosswalk_id unique ID column name 
#' @param into_list logical, if TRUE, unpacked braid_ids and corresponding crosswalk_ids are returned as a list. Default is FALSE
#'
#' @return dataframe or list
#' @importFrom dplyr mutate rename
#' @importFrom tidyr unnest 
#' @examples
#'  \dontrun{
#' # get a NHDPlus flowlines network
#' net <- nhdplusTools::navigate_network(start = 101, mode = "UT", distance_km = 100)
#' 
#' # Drop some columns for clarity
#' net <- dplyr::select(net, comid, divergence, fromnode, tonode)
#' 
#' # locate braids in the network and specify nested braid_id return
#' braids <- find_braids(net, add = TRUE, nested = TRUE)
#' 
#' # unnest the nested braid_id column (explode the list column into individual rows)
#' unpacked <- unpack_braids(braids)
#' }
#' @export
unpack_braids <- function(x, 
                          crosswalk_id = NULL, 
                          into_list = FALSE
                          ) {
  
  # make a unique ID if one is not given (NULL 'id')
  if(is.null(crosswalk_id)) {
    crosswalk_id  <- 'hydrofabric_id'
  }
  
  REQUIRED_COLS <- c(crosswalk_id, "braid_id")
  
  # validate input graph
  is_valid <- validate_df(x, REQUIRED_COLS, "x")
  
  # auto set to FALSE if anything other than TRUE or FALSE is given 
  if (!inherits(into_list, "logical") || is.na(into_list)) {
    into_list <- FALSE
  }
  
  # # TODO: Not using tidyr::unnest(), can drop tidyr dependency now i think
  # unnested_braids <-
  #   x %>%
  #   # braids %>% 
  #   dplyr::mutate(split_braid_ids = strsplit(braid_id, ", ")) %>% 
  #   dplyr::mutate(
  #     ids_length = sapply(split_braid_ids, length)
  #   ) %>% 
  #   dplyr::slice(rep(1:dplyr::n(), ids_length)) %>% 
  #   dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "braid_id")))) %>% 
  #   dplyr::mutate(
  #     n = 1:dplyr::n()
  #   ) %>% 
  #   dplyr::mutate(
  #     new_braid_id = split_braid_ids[[1]][n]
  #   ) %>% 
  #   dplyr::ungroup() %>%
  #   dplyr::rename(
  #     braid_members = braid_id, 
  #     braid_id      = new_braid_id
  #   ) %>% 
  #   dplyr::select(-split_braid_ids, -n, -ids_length)
  
  # TODO: Using tidyr::unnest()
  unnested_braids <-
    x %>%
    # braids %>%
    dplyr::mutate(split_braid_ids = strsplit(braid_id, ", ")) %>%
    # dplyr::mutate(split_braid_ids = stringr::str_split(braid_id, ", ")) %>%
    tidyr::unnest(cols = c(split_braid_ids)) %>%
    dplyr::rename(
      braid_members = braid_id,
      braid_id      = split_braid_ids
    )
  #   # dplyr::relocate(dplyr::any_of(crosswalk_id), braid_id, braid_members)

  # reorder the columns to put crosswalk_id, braid_id, and braid_members at the front of the dataframe
  unnested_braids <- reorder_cols(unnested_braids, c(crosswalk_id, "braid_id", "braid_members"))
  
  if(into_list) {
    unnested_braids <- split(unnested_braids[[crosswalk_id]], unnested_braids$braid_id) 
  }
  
  return(unnested_braids)
  
}

# -----------------------------------------------------------------------------
# ---- Cycle detection ----
# -----------------------------------------------------------------------------

#' Find cycles in a graph/network starting from a specified node or the first available node
#'
#' This function performs a depth-first search (DFS) on a graph/network to find cycles. It starts
#' the search from a specified node or the first available node. If a start node is provided, it
#' checks if the node exists in the graph and throws an error if not found. The function returns
#' the cycles found as a list of nodes in each cycle
#' @param graph A data frame representing the graph/network with columns 'fromnode', 'tonode', '<crosswalk_id', and 'to_<crosswalk_id>'. Graph must be converted to an undirected graph via make_undirected() 
#' @param start The starting node for the cycle detection. Default is NULL.
#' @param edge logicl, whether to return edges (node pairings) or nodes. If TRUE, 'node-node' character string will be returned, otherwise a starting node is returned. Default is FALSE. 
#' @param verbose Logical indicating whether to print verbose messages during the process. Default is FALSE.
#' 
#' @noRd
#' @keywords internal
#' @return A list of cycles found in the graph/network 
#' @importFrom dplyr relocate mutate bind_rows group_by summarise ungroup
#' @importFrom fastmap fastmap
#' @importFrom vctrs vec_c
#' @importFrom stats setNames
#' @examples
#'  \dontrun{
#' # directed graph with a directed cycle 3-4-5-3
#' graph <- data.frame(
#'   fromnode = c(1, 2, 3, 4, 5),
#'   tonode = c(2, 3, 4, 5, 3),
#'   comid = c("A", "B", "C", "D", "E"),
#'   tocomid = c("B", "C", "D", "E", "C")
#'   )
#'   
#' # start at node 1
#' find_cycles(graph, start   = 1, verbose = T)
#' 
#' # example directed graph with cycle 3-4-5-3 that needs to 
#' # be coerced into an undirected graph to detect cycles (via make_undirected() function)
#' graph = data.frame(
#'   fromnode = c(1, 2, 3, 3, 4),
#'   tonode   = c(2, 3, 4, 5, 5),
#'   comid    = c("A", "B", "C", "C", "D"),
#'   tocomid  = c("B", "C", "D", "E", "E")
#'   )
#'   
#' # make directed graph an undirected graph 
#' graph <- make_undirected(graph)
#' 
#' # start at node 1
#' find_cycles(graph, start = 1, verbose = T)
#' 
#' # no specified start node
#' find_cycles(graph, start = NULL, verbose = T)
#' }
find_cycles <- function(
    graph,
    start     = NULL,
    edge      = FALSE,
    verbose   = FALSE
) {
  
  # graph     = graph
  # # graph = distinct(graph),
  # # graph     = undir,
  # start     = start_node
  # return_as = "list"
  # edge      = FALSE
  # wide      = TRUE
  
  # verbose   = verbose
  # graph     = graph
  # # graph     = undir,
  # start     = start_node
  # return_as = "list"
  # edge      = FALSE
  # wide      = TRUE
  # verbose   = verbose
  
  # validate graph and columns 
  is_valid <- validate_df(graph, c("fromnode", "tonode"), "graph")
  
  # validate edge argument is valid logical type
  if (!inherits(edge, "logical")) {
    edge <- FALSE
  }
  
  # get / check for a valid start node 
  start_node <- get_start_node(graph, start)
  
  # if(verbose) {
  #   message("Starting braid detection at node: ", start_node)
  # }
  
  # make a topology hashmap to use in DFS 
  topo_map <- make_topo_map(
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode,
    directed   = FALSE
    # from_nodes = graph$tonode,
    # to_nodes   = graph$fromnode
  )
  
  
  # # keep track of visited nodes
  visits <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  visits$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
           function(i){
             0
           }),
    unique(c(graph$fromnode, graph$tonode))
  )
  )
  
  # keep track of visited nodes
  # visits <- get_visits_set(graph$fromnode, graph$tonode, 0)
  
  # keep track of cycles nodes
  cycles <- fastmap::fastmap()
  
  # # # set all marked values to FALSE
  # cycles$mset(.list = stats::setNames(lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), function(i){ 0 }),
  #                         unique(c(ungraph$fromnode, ungraph$tonode))))
  
  # keep track of visited nodes
  prev_nodes <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  prev_nodes$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
           function(i){ 0 }),
    unique(c(graph$fromnode, graph$tonode))
  )
  )
  
  # # keep track of visited nodes
  # prev_nodes <- get_visits_set(graph$fromnode, graph$tonode, 0)
  
  # cycle number count
  count <- 1
  
  # Depth first search function for marking cycles in undirected graph
  dfs <- function(n, p) {
    # message("node: ", n)
    # message("node ", n, " visit value: ", visits$get(n))
    
    # node has been fully visited
    if (visits$get(n) == 2) {
      return()
    }
    
    # node is in process of visitation
    if (visits$get(n) == 1) {
      
      # map/list to store current level nodes
      v <- vctrs::vec_c()
      
      curr <- p
      
      # keep track of fromnodes
      if(!edge) {
        
        # fromnodes vector
        v <- vctrs::vec_c(v, vctrs::vec_c(curr))
        
      } else {
        # keep track of edges
        v <- vctrs::vec_c(v,
                          vctrs::vec_c(topo_map$get(curr)$edge)
        )
      }
      
      # while parent of n is not equal to itself
      while (curr != n) {
        
        # update curr value to previous values of curr
        
        # make edge string
        if(edge) {
          edges <- paste0(prev_nodes$get(curr), "-", curr)
        } 
        
        curr  <- prev_nodes$get(curr)
        
        # add cycle vertices to v
        # keep track of fromnodes
        if(!edge) {
          
          # fromnodes vector
          v <- vctrs::vec_c(v, vctrs::vec_c(curr))
          
        } else {
          # keep track of edges
          v <- vctrs::vec_c(v, vctrs::vec_c(edges))
        }
      }
      
      # add vertices for current cycle number to cycles hashmap 
      cycles$set(as.character(count), v)
      
      # incremenet count
      count <<- count + 1
      
      return()
      
    }
    
    # set parents of node n 
    prev_nodes$set(n, p)
    
    # mark node as partially visited
    visits$set(n, 1)
    
    # get neighbors of node n
    neighbors <- topo_map$get(n)$to_node
    # neighbors <- edge_to_node(topo_map$get(n)$edge)
    
    # message("Neighbors of node: ",  n, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    # iterate through neighbors of node n
    for (vert in neighbors) {
      
      if(vert == prev_nodes$get(n)) {
        # message("vert ", vert, " equals parent ", prev_nodes$get(n), " SKIPPING")
        next
      }
      
      # message("---- RUNNING DFS ON ----\n ---> vert = ", 
      # vert, "---> n = ", n)
      
      dfs(vert, n)
      
      # message("***********")
    }
    # mark node as fully visited
    visits$set(n, 2)
    
    # message("=============================")
  }
  
  # Start DFS on a node
  dfs(start_node, "no_previous")
  
  if (is.null(cycles) || cycles$size() == 0) {
    # if (cycles$size() == 0) {
    
    if(verbose) {
      message("No cycles found, returning NULL")
    }
    
    return(NULL)
  }
  
  # extract cycles hashmap as a list
  result <- cycles$as_list()
  
  return(result)
  
}

#' Find cycles in a graph/network starting from a specified node or the first available node and return as dataframe
#'
#' This function performs a depth-first search (DFS) on a graph/network to find cycles. It starts
#' the search from a specified node or the first available node. If a start node is provided, it
#' checks if the node exists in the graph and throws an error if not found. The function returns
#' the cycles found as a dataframe denoting any nodes / edges that are part of a cycle with a cycle_id.
#' @param graph A data frame representing the graph/network with columns 'fromnode', 'tonode'. Graph must be converted to an undirected graph via make_undirected() 
#' @param start character, node in 'fromnode' to start searching for cycles
#' @param edge logical, return the cycles in edge-edge or node format. Default is FALSE which returns node format
#' @param wide logical, return dataframe in long form (a single cycle ID for every row in output) or wide format (list of cycle IDs for each node/edge). Default is FALSE, returns long format with a single cycle ID in each row
#' @param verbose logical, output messages or not
#'
#' @return A dataframe of cycles found in the graph/network 
#' @noRd
#' @keywords internal
find_cycles_df <- function(
    graph,
    start     = NULL,
    edge      = FALSE,
    wide      = TRUE,
    verbose   = FALSE
) {
  
  # validate input graph
  is_valid <- validate_df(graph, c("fromnode", "tonode"), "graph")
  
  # extract cycles as a list
  result   <- find_cycles(graph, start, edge = edge, verbose = verbose)
  
  if(edge) {
    cycles_df <- cycle_edge_list_to_cycle_edge_df(result, wide = wide)
    
  } else {
    cycles_df <- cycle_node_list_to_cycle_node_df(result, wide = wide)
  }
  
  
  return(cycles_df)
  
}

#' Convert output of find_cycles() to a dataframe
#' Converts a node cycle list to a dataframe of nodes and cycle IDs, internal helper function.
#' @param edge_cycles_list list of edges in each cycle_id (output of find_cycles())
#' @param wide logical, return data as a wide dataframe (multiple cycle_ids in a list in each row), or long dataframe (1 cycle_id for each row representing a node)
#' @importFrom dplyr bind_rows tibble group_by summarise ungroup relocate mutate
#' @return tibble, dataframe
cycle_edge_list_to_cycle_edge_df <- function(edge_cycles_list, 
                                             wide = FALSE) {
  
  edge_cycles_df <- lapply(1:length(edge_cycles_list), function(k) {
    # get IDs of each node
    dplyr::relocate(
      dplyr::mutate(
        data.frame(
          edge     = edge_cycles_list[[k]],
          cycle_id = names(edge_cycles_list[k])
          # node     = cycles$as_list()[[k]],
          # cycle_id = names(cycles$as_list()[k])
        ),
        fromnode = edge_to_node(edge, to = FALSE,  directed = FALSE),
        tonode   = edge_to_node(edge, to = TRUE,  directed = FALSE)
      ),
      edge, fromnode, tonode, cycle_id
    )
  })  %>% 
    dplyr::bind_rows() %>% 
    dplyr::tibble()
  
  
  # make each edge have its own row, with a list of cycle_ids in each row
  if(wide) {
    
    edge_cycles_df <- 
      edge_cycles_df %>%
      dplyr::group_by(edge, fromnode, tonode) %>%
      dplyr::summarise(
        cycle_id = paste0(cycle_id, collapse = ", ")
      ) %>%
      dplyr::ungroup()
  }
  
  return(edge_cycles_df)
  
}

#' Convert output of find_cycles() to a dataframe
#' Converts a node cycle list to a dataframe of nodes and cycle IDs, internal helper function.
#' @param node_cycles_list list of nodes in each cycle_id (output of find_cycles())
#' @param wide logical, return data as a wide dataframe (multiple cycle_ids in a list in each row), or long dataframe (1 cycle_id for each row representing a node)
#' @importFrom dplyr bind_rows tibble group_by summarise ungroup
#' @return tibble, dataframe
cycle_node_list_to_cycle_node_df <- function(node_cycles_list, 
                                             wide = FALSE) {
  
  node_cycles_df <- lapply(1:length(node_cycles_list), function(k) {
    # get IDs of each node
    data.frame(
      node     = node_cycles_list[[k]],
      cycle_id = names(node_cycles_list[k])
      # node     = cycles$as_list()[[k]],
      # cycle_id = names(cycles$as_list()[k])
    )
  })  %>% 
    dplyr::bind_rows() %>% 
    dplyr::tibble()
  
  # make each node have its own row, with a list of cycle_ids in each row
  if(wide) {
    
    node_cycles_df <- 
      node_cycles_df %>%
      dplyr::group_by(node) %>%
      dplyr::summarise(
        cycle_id = paste0(cycle_id, collapse = ", ")
      ) %>%
      dplyr::ungroup()
  }
  
  return(node_cycles_df)
  
}
