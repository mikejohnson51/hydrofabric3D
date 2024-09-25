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
#' braids <- find_braids(network = net, add = FALSE)
#'
#' # add braid_id column to original dataset
#' braid_df = find_braids(network   = net,
#'                        nested    = TRUE,
#'                        )
#'
#' # returns original data with each braid_id represented
#' # by its individual COMIDs (may contain duplicate COMIDs)
#' nested_braids = find_braids(network   = net,
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

#' Convert a list of braid IDs and COMIDs into a dataframe
#' Internal function, used to process the outputs of 'get_braid_list()' within 
#' the 'find_braids()' function. The function takes a list of braid IDs and associated COMIDs and puts the list into a dataframe, 
#' and optionally adds the information back to the original 'network' dataset.
#' @param network The network object representing the river network.
#' @param braids list of braid IDs and COMIDs within each braid
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
                           add     = FALSE, 
                           nested  = TRUE, 
                           version = 1, 
                           verbose = FALSE
) {
  
  # network = network 
  # braids  = braids
  # add     = FALSE
  # nested  = TRUE
  # version = 2
  # verbose = TRUE
  
  if(!version %in% c(1, 2)) {
    stop("Invalid 'version' argument, 'version' must be equal to 1 or 2")
  }
  
  ### process braids (version 1) ###
  if(version == 1) {
    
    # get comids of multibraids
    multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
    
    # format braids into a dataframe
    braids <- lapply(1:length(braids), function(k) {
      
      # get crosswalk_id of each braid
      data.frame(
        comid    = braids[[k]],
        braid_id = names(braids[k])
      )
    }) %>% 
      dplyr::bind_rows() %>% 
      dplyr::tibble()
    
    # if each comid should have a list of all the braids it is a part of
    # if relationship == "one-to-one" then COMIDs may appear more than once
    # in dataset as a COMID may be apart of more than one braid
    # if(relationship == "one-to-many") {
    if(nested) { 
      
      braids <-
        braids %>%
        dplyr::group_by(comid) %>% 
        dplyr::summarise(
          braid_id = paste0(braid_id, collapse = ", ")
        ) %>% 
        dplyr::ungroup()
      
    }
    
    # add a logical value that says whether a COMID is part of a multibraided system,
    # or is a flowline in more than one braid (multiple braid_ids)
    braids$is_multibraid <- braids$comid %in% multibraids
    
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
                                   FALSE, braids$is_multibraid)
    
    # move braid_id and is_multibraided columns to front of dataframe
    braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
    # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
    
    return(braids)
    
    
  } else { ### process braids (version 2) ###
    
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
        dplyr::group_by(comid)  %>% 
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
    braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
    # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
    
    return(braids)
  }
  
}

#' Convert a list of braid IDs and COMIDs into a dataframe (v1)
#' Internal function, used to process the outputs of 'get_braid_list()' within 
#' the 'find_braids()' function. The function takes a list of braid IDs and associated COMIDs and puts the list into a dataframe, 
#' and optionally adds the information back to the original 'network' dataset.
#' @param network The network object representing the river network.
#' @param braids list of braid IDs and COMIDs within each braid
#' @param add Logical indicating whether to add braid information to the original network data.
#' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' 
#' @noRd
#' @keywords internal
#' @return sf dataframe or dataframe with a braid_id mapping for each COMID
#' @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate left_join relocate select
process_braids1 <- function(network, 
                            braids, 
                            add     = FALSE, 
                            nested  = TRUE, 
                            verbose = FALSE
) {
  
  # network = network 
  # braids  = braids
  # add     = FALSE
  # nested  = TRUE
  # verbose = TRUE
  
  # get comids of multibraids
  multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
  
  # format braids into a dataframe
  braids <- lapply(1:length(braids), function(k) {
    
    # get crosswalk_id of each braid
    data.frame(
      comid    = braids[[k]],
      braid_id = names(braids[k])
    )
  }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::tibble()
  
  # if each comid should have a list of all the braids it is a part of
  # if relationship == "one-to-one" then COMIDs may appear more than once
  # in dataset as a COMID may be apart of more than one braid
  # if(relationship == "one-to-many") {
  if(nested) { 
    
    braids <-
      braids %>%
      dplyr::group_by(comid) %>% 
      dplyr::summarise(
        braid_id = paste0(braid_id, collapse = ", ")
      ) %>% 
      dplyr::ungroup()
    
  }
  
  # add a logical value that says whether a COMID is part of a multibraided system,
  # or is a flowline in more than one braid (multiple braid_ids)
  braids$is_multibraid <- braids$comid %in% multibraids
  
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
                                 FALSE, braids$is_multibraid)
  
  # move braid_id and is_multibraided columns to front of dataframe
  braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
  # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
  
  return(braids)
  
  
}


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
process_braids2 <- function(network, 
                            braids, 
                            crosswalk_id = NULL,
                            add     = FALSE, 
                            nested  = TRUE, 
                            verbose = FALSE
) {
  
  network    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
  crosswalk_id = "comid"
  
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

#' Convert a list of braid IDs and crosswalk_ids into a dataframe (v1)
#' Internal function, used to process the outputs of 'get_braid_list()' within 
#' the 'find_braids()' function. The function takes a list of braid IDs and associated crosswalk_ids and puts the list into a dataframe
#' @param braids list of braid IDs and crosswalk_ids within each braid
#' @param crosswalk_id unique ID column name 
#' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate crosswalk_ids as a single COMID could be a part of multiple braids (braid_id)
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' 
#' @noRd
#' @keywords internal
#' @return sf dataframe or dataframe with a braid_id mapping for each crosswalk_id
#' @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate select any_of across
process_braid_list1 <- function(
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
  # # network = network 
  # # braids  = braids
  # # add     = FALSE
  # nested  = FALSE
  # verbose = TRUE
  
  # get crosswalk_ids of multibraids
  multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
  
  # format braids into a dataframe
  braids <- lapply(1:length(braids), function(k) {
    # k = 1
    # get crosswalk_id of each braid
    dplyr::tibble(
      !!crosswalk_id := braids[[k]],
      braid_id        = names(braids[k])
    )
    # data.frame(
    #   crosswalk_id  = braids[[k]],
    #   braid_id      = names(braids[k])
    # )
    
  }) %>% 
    dplyr::bind_rows() %>% 
    dplyr::tibble()
  
  # if each crosswalk_id should have a list of all the braids it is a part of
  # if relationship == "one-to-one" then crosswalk_ids may appear more than once
  # in dataset as a crosswalk_id may be apart of more than one braid
  # if(relationship == "one-to-many") {
  if(nested) { 
    
    braids <-
      braids %>%
      dplyr::group_by(dplyr::across(dplyr::any_of(crosswalk_id))) %>% 
      # dplyr::group_by(comid) %>% 
      dplyr::summarise(
        braid_id = paste0(braid_id, collapse = ", ")
      ) %>% 
      dplyr::ungroup()
    
  }
  
  # add a logical value that says whether a crosswalk_id is part of a multibraided system,
  # or is a flowline in more than one braid (multiple braid_ids)
  if (!is.null(multibraids)) {
    braids$is_multibraid <- braids[[crosswalk_id]] %in% multibraids
  } else {
    # multibraid is NULL (all braids are NOT multibraids)
    braids$is_multibraid <- FALSE
  }
  
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
  
  # set no_braid values to FALSE
  braids$is_multibraid <- ifelse(
    is.na(braids$is_multibraid), 
    FALSE, 
    braids$is_multibraid
  )
  
  # move crosswalk_id, braid_id and is_multibraided columns to front of dataframe
  braids <- braids[ , c(crosswalk_id, "braid_id", "is_multibraid")]
  # braids <- dplyr::relocate(braids, dplyr::any_of(crosswalk_id), braid_id, is_multibraid)
  
  return(braids)
  
  
}

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

#' Utility function that takes the output from 'find_braids(nested = TRUE)' and unpacks/unnests braid_id column
#' 
#' Unnests the comma seperated braid_id column into individual rows for each braid ID/comid pairing. 
#' This function will unnest the nested braid_ids column that is the output of calling 'find_braids()' with 'nested = TRUE', essentially giving the output of running 'nested = FALSE'.
#' 
#' @param braids dataframe or sf dataframe containing a "comid" and "braid_id" column
#' @param into_list logical, if TRUE, unpacked braid_ids and corresponding crosswalk_ids are returned as a list. Default is FALSE
#'
#' @return dataframe or list
#' @importFrom dplyr mutate rename relocate
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
#' unpacked <- unnpack_braids(braids)
#' }
#' @export
unnpack_braids <- function(braids, into_list = FALSE) {
  
  if(!"braid_id" %in% tolower(names(braids))) {
    stop("'braids' input does not contain 'braid_id' column ")
  }
  
  unnested_braids <- 
    braids %>% 
    # dplyr::mutate(split_braid_ids = stringr::str_split(braid_id, ", ")) %>%
    dplyr::mutate(split_braid_ids = strsplit(braid_id, ", ")) %>%
    tidyr::unnest(cols = c(split_braid_ids)) %>% 
    dplyr::rename(
      braid_members = braid_id, 
      braid_id      = split_braid_ids
    ) %>% 
    dplyr::relocate(comid, braid_id, braid_members)
  
  if(into_list) {
    unnested_braids <- split(unnested_braids$comid, unnested_braids$braid_id) 
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

#' Validate that a dataframe is valid type (inherits from a dataframe) and has the given required columns
#'
#' @param x dataframe, sf, or tibble
#' @param cols character vector of columns that are required in 'x'
#' @param obj_name character string, optional name of object to display in error messages. If not obj_name is given, obj_name defaults to 'x'
#'
#' @return logical, TRUE value if all conditions are met and valid, otherwise an error is raised
#' @export
validate_df <- function(x, cols, obj_name = NULL) {
  
  if(is.null(obj_name) | !inherits(obj_name, "character")) {
    obj_name <- "x"
  }
  
  if (!inherits(x, "data.frame")) {
    stop("Invalid '", obj_name, "' type of '",  class(x), "' \n '", 
         obj_name ,"' must inherit from 'data.frame', 'tibble', or 'sf'")
  }
  
  if (!all(cols %in% names(x))) {
    
    missing_cols <- cols[which(!cols %in% names(x))]
    
    stop("'", obj_name, "' is missing one or more of the required columns:\n > ", 
         paste0(missing_cols, collapse = "\n > "))
  }
  
  return(TRUE)
  
}


# ---- OLD BRAID CODE ------------------------------------

#' 
#' #' Find braids and add to a dataframe/sf dataframe
#' #'
#' #' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' #'
#' #' @param network The network object representing the river network.
#' #' @param terminal_id character, column name containing a unique identifier, delineating separate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
#' #' @param add Logical indicating whether to add braid information to the original network data.
#' #' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
#' #' @param version integer, version number of multibraid classification and grouping algorithm, must be either 1 or 2. Default is 2.
#' #' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' #'
#' #' @return dataframe or sf dataframe with added braid_id
#' #' 
#' #' @examples
#' #'  \dontrun{
#' #' net <- nhdplusTools::navigate_network(
#' #'  start       = 101,
#' #'  mode        = "UT",
#' #'  distance_km = 100
#' #'  ) 
#' #'  
#' #' # drop most of the columns in the network dataset
#' #' net <- dplyr::select(net, comid, divergence, totdasqkm, fromnode, tonode, terminalpa)
#' #'
#' #' # get a dataframe of COMIDs and braid IDs
#' #' braids <- find_braids(network = net, add = FALSE)
#' #'
#' #' # add braid_id column to original dataset
#' #' braid_df = find_braids(network   = net,
#' #'                        add       = TRUE,
#' #'                        nested    = TRUE,
#' #'                        )
#' #'
#' #' # returnal original data with each braid_id represented
#' #' # by its individual COMIDs (may contain duplicate COMIDs)
#' #' nested_braids = find_braids(network   = net,
#' #'                        add       = TRUE,
#' #'                        nested    = FALSE
#' #'                        )
#' #' # if a column exists that uniquely identifies different contiguous networks,
#' #' # the column name can be given to 'terminal_id' to delianiate seperate networks within 'net'
#' #' sep_braids = find_braids(network     = net,
#' #'                          terminal_id = "terminalpa",
#' #'                          add         = TRUE,
#' #'                          nested      = FALSE
#' #'                        )
#' #' }
#' #' @export
#' find_braids <- function(
#'     network,
#'     terminal_id = NULL,
#'     add         = FALSE,
#'     nested      = TRUE,
#'     version     = 2,
#'     verbose     = FALSE
#' ) {
#' 
#'   # network = net2
#'   # terminal_id = NULL
#'   # add         = FALSE
#'   # nested      = TRUE
#'   # version     = 1
#'   # verbose     = TRUE
#'   
#'   # get a list of braid IDs and comids within each braid
#'   braids <- get_braid_list(
#'     network     = network, 
#'     terminal_id = terminal_id,
#'     verbose     = verbose
#'   )
#'   
#'   # process multibraided portions of braid list returned above 
#'   braids <- process_braids(
#'     network = network, 
#'     braids  = braids,
#'     add     = add,
#'     nested  = nested,
#'     version = version,
#'     verbose = verbose
#'   )
#'   
#'   return(braids)
#'   
#' }
#' 
#' #' Find braids and add to a dataframe/sf dataframe
#' #' Deprecated, old version.
#' #' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' #'
#' #' @param network The network object representing the river network.
#' #' @param terminal_id character, column name containing a unique identifier, delineating seperate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
#' #' @param add Logical indicating whether to add braid information to the original network data.
#' #' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
#' #' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return dataframe or sf dataframe with added braid_id
#' #' @examples
#' #'  \dontrun{
#' #' net <- nhdplusTools::navigate_network(
#' #'  start       = 101,
#' #'  mode        = "UT",
#' #'  distance_km = 100
#' #'  ) %>%
#' #' dplyr::select( comid, divergence, totdasqkm, fromnode, tonode)
#' #'
#' #' # get a dataframe of COMIDs and braid IDs
#' #' braids <- find_braids(network = net, add = FALSE)
#' #'
#' #' # add braid_id column to original dataset
#' #' braid_df = find_braids(network   = net,
#' #'                        add       = TRUE,
#' #'                        nested    = TRUE,
#' #'                        )
#' #'
#' #' # returnal original data with each braid_id represented
#' #' # by its individual COMIDs (may contain duplicate COMIDs)
#' #' nested_braids = find_braids(network   = net,
#' #'                        add       = TRUE,
#' #'                        nested    = FALSE
#' #'                        )
#' #' # if a column exists that uniquely identifies different contiguous networks,
#' #' # the column name can be given to 'terminal_id' to delianiate seperate networks within 'net'
#' #' sep_braids = find_braids(network     = net,
#' #'                          terminal_id = "terminalpa",
#' #'                          add         = TRUE,
#' #'                          nested      = FALSE
#' #'                        )
#' #' }
#' #' @importFrom dplyr bind_rows tibble select filter group_by summarise ungroup mutate left_join relocate
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom stats setNames
#' find_braids2 <- function(
#'     network,
#'     terminal_id = NULL,
#'     add         = FALSE,
#'     nested      = TRUE,
#'     verbose     = FALSE
#'     ) {
#'   
#'   # network = net2
#'   # terminal_id = NULL
#'   # add         = FALSE
#'   # nested      = TRUE
#'   # verbose     = T
#'   # # ne
#'   # # nhdplusTools::navigate_network(start = 101, mode = "UT")
#'   # network <- ref_net %>%
#'   #   dplyr::filter(
#'   #     terminalpa %in% unique(counts$terminalpa)[1:5]
#'   #     # terminalpa %in% c(2010288, 1962269, 1853299, 1852198, 1852502, 1852542, 1852774)
#'   #     # terminalpa %in% c(1852198, 1852502, 1852542, 1852774)
#'   #     ) 
#'   
#'   # get a list of braid IDs and comids within each braid
#'   braids <- get_braid_list(
#'                 network     = network, 
#'                 terminal_id = terminal_id,
#'                 verbose     = verbose
#'                 )
#' 
#'   # get comids of multibraids
#'   multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
#'   
#'   # format braids into a dataframe
#'   braids <- lapply(1:length(braids), function(k) {
#'     
#'     # get COMIDs of each braid
#'     data.frame(
#'       comid    = braids[[k]],
#'       braid_id = names(braids[k])
#'     )
#'   }) %>% 
#'     dplyr::bind_rows() %>% 
#'     dplyr::tibble()
#'   
#'   # if each comid should have a list of all the braids it is a part of
#'   # if relationship == "one-to-one" then COMIDs may appear more than once
#'   # in dataset as a COMID may be apart of more than one braid
#'   # if(relationship == "one-to-many") {
#'   if(nested) { 
#'     
#'     braids <-
#'       braids %>%
#'       dplyr::group_by(comid) %>% 
#'       dplyr::summarise(
#'         braid_id = paste0(braid_id, collapse = ", ")
#'       ) %>% 
#'       dplyr::ungroup()
#'     
#'   }
#'   
#'   # add a logical value that says whether a COMID is part of a multibraided system,
#'   # or is a flowline in more than one braid (multiple braid_ids)
#'   braids$is_multibraid <- braids$comid %in% multibraids
#'   
#'   # if braid data should be added to original data
#'   if(add) {
#'     
#'     # join back with original data
#'     braids <- dplyr::mutate(
#'                     dplyr::left_join(
#'                       network,
#'                       braids,
#'                       by = "comid"
#'                     ),
#'                     braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#'                   )
#'     # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#'   } 
#'   
#'   # set no_braid values to FALSE
#'   braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#'                                  FALSE, braids$is_multibraid)
#'   
#'   # move braid_id and is_multibraided columns to front of dataframe
#'   braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#'   # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#'   
#'   return(braids)
#'   
#' }
#' 
#' #' Convert a list of braid IDs and COMIDs into a dataframe
#' #' Internal function, used to process the outputs of 'get_braid_list()' within 
#' #' the 'find_braids()' function. The function takes a list of braid IDs and associated COMIDs and puts the list into a dataframe, 
#' #' and optionally adds the information back to the original 'network' dataset.
#' #' @param network The network object representing the river network.
#' #' @param braids list of braid IDs and COMIDs within each braid
#' #' @param add Logical indicating whether to add braid information to the original network data.
#' #' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
#' #' @param version integer, which version number to use (either 1 or 2)
#' #' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return sf dataframe or dataframe with a braid_id mapping for each COMID
#' #' @importFrom dplyr bind_rows tibble group_by summarise ungroup mutate left_join relocate select
#' process_braids <- function(network, braids, add, nested, version = 1, verbose) {
#'   
#'   if(!version %in% c(1, 2)) {
#'     stop("Invalid 'version' argument, 'version' must be equal to 1 or 2")
#'   }
#'   
#'   ### process braids (version 1) ###
#'   if(version == 1) {
#'     
#'     # get comids of multibraids
#'     multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
#'     
#'     # format braids into a dataframe
#'     braids <- lapply(1:length(braids), function(k) {
#'       
#'       # get COMIDs of each braid
#'       data.frame(
#'         comid    = braids[[k]],
#'         braid_id = names(braids[k])
#'       )
#'     }) %>% 
#'       dplyr::bind_rows() %>% 
#'       dplyr::tibble()
#'     
#'     # if each comid should have a list of all the braids it is a part of
#'     # if relationship == "one-to-one" then COMIDs may appear more than once
#'     # in dataset as a COMID may be apart of more than one braid
#'     # if(relationship == "one-to-many") {
#'     if(nested) { 
#'       
#'       braids <-
#'         braids %>%
#'         dplyr::group_by(comid) %>% 
#'         dplyr::summarise(
#'           braid_id = paste0(braid_id, collapse = ", ")
#'         ) %>% 
#'         dplyr::ungroup()
#'       
#'     }
#'     
#'     # add a logical value that says whether a COMID is part of a multibraided system,
#'     # or is a flowline in more than one braid (multiple braid_ids)
#'     braids$is_multibraid <- braids$comid %in% multibraids
#'     
#'     # if braid data should be added to original data
#'     if(add) {
#'       
#'       # join back with original data
#'       braids <- dplyr::mutate(
#'         dplyr::left_join(
#'           network,
#'           braids,
#'           by = "comid"
#'         ),
#'         braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#'       )
#'       # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#'     } 
#'     
#'     # set no_braid values to FALSE
#'     braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#'                                    FALSE, braids$is_multibraid)
#'     
#'     # move braid_id and is_multibraided columns to front of dataframe
#'     braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#'     # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#'     
#'     return(braids)
#'     
#'     
#'   } else { ### process braids (version 2) ###
#'     
#'     
#'     # rename braids to numeric values
#'     names(braids) <- 1:length(braids)
#'     
#'     # create a new list of braid_ids that has the braid_ids of 
#'     # all other braids connected to the given braid_id
#'     # the first braid_id in any of the vectors in "multis" will be the reference/main braid_id and
#'     # the following braid_ids will be the rest of the multibraided system
#'     multis <- find_multibraids(braids)
#'     
#'     # # any vector with more than one braid_id is a multibraid
#'     # is_multi <- unname(lengths(multis) > 1)
#'     
#'     # # add a logical value that says whether a COMID is part of a multibraided system,
#'     # # or is a flowline in more than one braid (multiple braid_ids) --> column "is_multibraid"
#'     
#'     # format braids into a dataframe
#'     braids <- lapply(1:length(braids), function(k) {
#'       
#'       # get COMIDs of each braid
#'       data.frame(
#'         comid         = braids[[k]],
#'         braid_id      = names(braids[k]),
#'         member_braids = paste0(multis[[names(braids[k])]], collapse = ", "),
#'         is_multibraid = length(multis[[names(braids[k])]]) > 1
#'       )
#'       
#'     }) %>% 
#'       dplyr::bind_rows() %>% 
#'       dplyr::tibble()
#'     
#'     # if nested is NOT TRUE, then COMIDs (flowlines) may appear more than once in output dataset
#'     # ----> (i.e. a COMID/flowline can be a part of more than one braid_id and thus will have a row for "braid_1" and "braid_2")
#'     # if each comid should have a list of all the braids it is a part of
#'     # if relationship == "one-to-one" then COMIDs may appear more than once
#'     # in dataset as a COMID may be apart of more than one braid
#'     # if(relationship == "one-to-many") {
#'     if(!nested) {
#'       
#'       # replace numeric braid_id with "braid_<number>"
#'       braids$braid_id <- paste0("braid_", braids$braid_id)
#'       
#'       # drop the member braids column
#'       braids <- dplyr::select(braids, -member_braids)
#'       
#'     } else { # if nested is TRUE, then each COMID is unique and has a braid_id column with a comma seperated character string of all relevent braid_ids
#'       
#'       # if braid_ids should be nested so that each COMID has a braid_id column that reflects ALL braid_ids related/connected to it
#'       braids <- 
#'         braids %>% 
#'         dplyr::group_by(comid)  %>% 
#'         dplyr::summarise(
#'           braid_id = paste0(member_braids, collapse = ", ")
#'         ) %>% 
#'         dplyr::ungroup() %>% 
#'         dplyr::mutate(
#'           braid_id = lapply(strsplit(braid_id, ", "), function(x) {
#'             paste0("braid_", sort(as.numeric(unique(x))))
#'           })
#'         )
#'       
#'       # if more than one braid_id in "braid_id" column, then it is a multibraid 
#'       braids$is_multibraid <- lengths(braids$braid_id) > 1
#'       
#'       # collapse braid_ids into comma seperated character string
#'       braids$braid_id <- Reduce(c, 
#'                                 lapply(braids$braid_id, function(k) {
#'                                   paste0(k, collapse = ", ")
#'                                 })
#'       )
#'     }
#'     
#'     # # add a logical value that says whether a COMID is part of a multibraided system,
#'     # # or is a flowline in more than one braid (multiple braid_ids)
#'     # braids$is_multibraid <- braids$comid %in% multibraids
#'     
#'     # if braid data should be added to original data
#'     if(add) {
#'       
#'       # join back with original data
#'       braids <- dplyr::mutate(
#'         dplyr::left_join(
#'           network,
#'           braids,
#'           by = "comid"
#'         ),
#'         braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#'       )
#'       # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#'     } 
#'     
#'     # set no_braid values to FALSE
#'     braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#'                                    FALSE,
#'                                    braids$is_multibraid
#'     )
#'     
#'     # move braid_id and is_multibraided columns to front of dataframe
#'     braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#'     # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#'     
#'     return(braids)
#'   }
#'   
#' }
#' 
#' #' Create a list of braid IDs containing COMIDs in each braid 
#' #'
#' #' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' #'
#' #' @param network The network object representing the river network.
#' #' @param terminal_id character, column name containing a unique identifier, delineating seperate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
#' #' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' #' 
#' #' @return list of braid IDs and COMIDs within each braid
#' #' @importFrom dplyr filter
#' #' @examples
#' #'  \dontrun{
#' #' net <- nhdplusTools::navigate_network(
#' #'  start       = 101,
#' #'  mode        = "UT",
#' #'  distance_km = 100
#' #'  ) 
#' #' 
#' #' net <- dplyr::select(net, comid, divergence, totdasqkm, fromnode, tonode, terminalpa)
#' #'
#' #' # get a dataframe of COMIDs and braid IDs
#' #' braids <- get_braid_list(network = net)
#' #' }
#' #' @export 
#' get_braid_list <- function(
#'     network,
#'     terminal_id = NULL,
#'     verbose = FALSE
#' ) {
#'   
#'   # if no terminal_ids column name is given, create one by finding all connected componenets in network
#'   if(is.null(terminal_id)) {
#'     
#'     # determine which flowlines are part of which contiguous set of linestrings 
#'     # (creates a unique identifier for each seperated network of flowlines)
#'     network <- find_connected_components(network = network, add = TRUE)
#'     
#'     
#'   } else { # if a terminal_id IS provided, the column name is checked and changed to "component_id"
#'     
#'     # check that 'terminal_id' is a column in 'network', if not throw an error
#'     if(!terminal_id %in% names(network)) {
#'       stop("Invalid terminal_id argument '", terminal_id, "', terminal_id must be a column in 'network'")
#'     }
#'     
#'     if(verbose) { message("User provided 'terminal_id': '", terminal_id, "'")}
#'     
#'     # name of the "tocomid" column in network that should be removed
#'     term_col <- names(network)[names(network) %in% terminal_id]
#'     
#'     # replace term_col column name with "terminalID"
#'     names(network) <- gsub(term_col, "component_id", names(network)) 
#'     
#'   }
#'   
#'   # unique(network$terminalpa) %>% length()
#'   # unique(network$component_id) %>% length()
#'   
#'   # list of the uniqueu component IDs 
#'   components <- unique(network$component_id)
#'   
#'   # loop through each of the unique 'component_id' and get a list of braids for each distinct componenet
#'   braid_list <- lapply(1:length(components), function(i) {
#'     
#'     if(verbose) {
#'       message(i, "/", length(components))
#'       message("component[i]: ", components[i])
#'       message("# rows in component ", components[i], ": ", nrow(
#'         dplyr::filter(network, component_id %in% components[i])
#'       ))
#'       message('=====================')
#'     }
#'     
#'     # get list of braids for distinct network
#'     # filter network down to the iterations component (components[i])
#'     internal_get_braid_list(
#'       network = dplyr::filter(network, 
#'                               component_id %in% components[i]), 
#'       start   = NULL,
#'       verbose = verbose
#'     )
#'     
#'   })
#'   
#'   # remove duplicate braids IDs and re number braid IDs
#'   braid_list <- reduce_braid_list(braid_list)
#'   
#'   return(braid_list)
#'   
#'   # network %>% get_terminal_ids(add = F)
#'   # roots <- network %>% get_start_ids()
#'   # parts <- find_connected_components(network, add = T)
#'   # parts$component_id %>% unique()
#'   # network %>% 
#'   #   dplyr::mutate(terminalpa = as.character(terminalpa)) %>% 
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::geom_sf(ggplot2::aes(color = terminalpa))
#'   # parts %>% 
#'   #   dplyr::mutate(component_id = as.character(component_id)) %>% 
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::geom_sf(ggplot2::aes(color = component_id))
#'   # mapview::mapview(dplyr::filter(parts, component_id == "1"), color = "dodgerblue") +
#'   #   mapview::mapview(dplyr::filter(parts, component_id == "2"), color = "gold") +
#'   # mapview::mapview(dplyr::filter(parts, component_id == "3"), color = "red") +
#'   # mapview::mapview(dplyr::filter(parts, component_id == "4"), color = "green") + 
#'   # mapview::mapview(dplyr::filter(parts, component_id == "5"), color = "hotpink") +
#'   #   mapview::mapview(dplyr::filter(parts, component_id == "6"), color = "cyan")
#'   # 
#'   # parts %>% 
#'   # ggplot2::ggplot() +
#'   #   ggplot2::geom_sf(ggplot2::aes(color = uterms))
#'   
#' }
#' 
#' #' Create a list of braid IDs containing COMIDs in each braid for a single continguous network
#' #'
#' #' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' #' Internal function for use in 'get_braid_list()'
#' #' @param network The network object representing the river network.
#' #' @param start Optional argument specifying the starting point for braid detection.
#' #' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' #'
#' #' @noRd
#' #' @keywords internal
#' #' @return list of braid IDs with COMIDs within each braid
#' #' @importFrom dplyr select filter
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom stats setNames
#' internal_get_braid_list <- function(
#'     network,
#'     start        = NULL,
#'     verbose      = FALSE
#' ) {
#'   
#'   # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
#'   if(nrow(network) <= 1) {
#'     
#'     if(verbose) {
#'       message("No cycles found, returning NULL")
#'     }
#'     
#'     return(NULL)
#'   }
#'   
#'   # # check valid return_as input
#'   # if(!return_as %in% c("list", "dataframe")) {
#'   #   stop("Invalid 'return_as' argument '",
#'   #        return_as, "'\n'return_as' must be: 'list' or 'dataframe'")
#'   # }
#'   
#'   # # check relationship argument is valid
#'   # if(return_as == "dataframe") {
#'   #   if(!relationship %in% c("one-to-one", "one-to-many")) {
#'   #     stop("Invalid 'relationship' argument '",
#'   #          relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
#'   #   }
#'   # }
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # turn network into a directed graph
#'   dag <- create_dag(network)
#'   
#'   # get the fromnode associated with a given COMID 'start'
#'   start_node <- comid_to_node(dag, start)
#'   
#'   if(verbose) {
#'     message("Starting braid detection at ", 
#'             ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
#'   }
#'   
#'   # drop graph geometry
#'   dag <- sf::st_drop_geometry(dag)
#'   # graph <- sf::st_drop_geometry(graph)
#'   
#'   # stash comids and from IDs
#'   comid_map <- dplyr::select(
#'     # graph,
#'     dag,
#'     comid,
#'     fromnode,
#'     tonode
#'   )
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identfied as a cycle and thus a braid
#'   # Questionable at this point, other option is the code below that uses 
#'   # single_cycles() function (COMMENTED OUT CODE BELOW)
#'   regraph <- renode_circuits(dag, verbose = FALSE)
#'   
#'   # ##### - GET_SINGLE_CYCLES CODE -
#'   # # check for single cycles (2 nodes A,B that have unique edges)
#'   # # (i.e. 2 comids w/ exact same fromnodes/tonodes)
#'   # single_cycles <- get_single_cycles(dag)
#'   # # if cycles were found
#'   # if(!is.null(cycles)) {
#'   #   # match each fromnode with original COMIDs
#'   #   braids <- sapply(1:length(cycles), function(k) {
#'   #     # get COMIDs of each braid
#'   #     comid_map[comid_map$fromnode %in% cycles[[k]], ]$comid
#'   #   })
#'   #   # if single cycles are found in network
#'   #   if(!is.null(single_cycles)) {
#'   #     braids <- c(braids, unname(single_cycles))
#'   #   }
#'   #   # if NO cycles were found
#'   # } else {
#'   #   # if single cycles are found in network
#'   #   if(!is.null(single_cycles)) {
#'   #     braids <- c(unname(single_cycles))
#'   #   } else {
#'   #     message("No braids found, returning NULL")
#'   #     return(NULL)
#'   #   }
#'   # }
#'   
#'   # make an undirected graph
#'   undir <- make_undirected(regraph)
#'   
#'   # find cycles in undirected graph (proxy for braids)
#'   cycles <- find_cycles(
#'     graph     = undir,
#'     start     = start_node,
#'     return_as = "list",
#'     edge      = FALSE,
#'     wide      = TRUE,
#'     verbose   = verbose
#'   )
#'   
#'   # if(cycles$size() == 0 & is.null(single_braids)) {
#'   if(is.null(cycles)) {
#'     # message("No cycles found, returning NULL")
#'     return(NULL)
#'   }
#'   
#'   # remove added nodes from renode_circuits()
#'   braids <- lapply(cycles, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   braids <- lapply(1:length(braids), function(k) {
#'     # get COMIDs of each braid
#'     comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#'       dplyr::filter(tonode %in% braids[[k]]) %>%
#'       .$comid
#'   })
#'   
#'   # set braid_id names
#'   braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
#'   
#'   return(braids)
#'   
#'   # # if multibraids should be untangled
#'   # if(untangle) {
#'   #   if(verbose) { message("Attempting to seperate multibraids...") }
#'   #   braids <- seperate_braids(network, braids)
#'   # }
#'   
#'   # # if data should be returned as a 2 column dataframe with comid and braid_ids
#'   # if(return_as == "dataframe") {
#'   #   
#'   #   # get comids of multibraids
#'   #   multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
#'   #   
#'   #   # format braids into a dataframe
#'   #   braids <- lapply(1:length(braids), function(k) {
#'   #     # braids_df <- lapply(1:length(braids), function(k) {
#'   #     
#'   #     # get COMIDs of each braid
#'   #     data.frame(
#'   #       comid    = braids[[k]],
#'   #       braid_id = names(braids[k])
#'   #     )
#'   #   }) %>% 
#'   #     dplyr::bind_rows() %>% 
#'   #     dplyr::tibble()
#'   #   
#'   #   # if each comid should have a list of all the braids it is a part of
#'   #   # if relationship == "one-to-one" then COMIDs may appear more than once
#'   #   # in dataset as a COMID may be apart of more than one braid
#'   #   # if(relationship == "one-to-many") {
#'   #   if(nested) { 
#'   #     
#'   #     braids <-
#'   #       braids %>%
#'   #       dplyr::group_by(comid) %>% 
#'   #       dplyr::summarise(
#'   #         braid_id = paste0(braid_id, collapse = ", ")
#'   #       ) %>% 
#'   #       dplyr::ungroup()
#'   #     
#'   #   }
#'   #   
#'   #   # add a logical value that says whether a COMID is part of a multibraided system,
#'   #   # or is a flowline in more than one braid (multiple braid_ids)
#'   #   braids$is_multibraid <- braids$comid %in% multibraids
#'   #   
#'   #   # if braid data should be added to original data
#'   #   if(add) {
#'   #     
#'   #     # join back with original data
#'   #     braids <-
#'   #       dplyr::mutate(
#'   #         dplyr::left_join(
#'   #           network,
#'   #           braids,
#'   #           by = "comid"
#'   #         ),
#'   #         braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#'   #       )
#'   #     # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#'   #   } 
#'   #   
#'   #   # set no_braid values to FALSE
#'   #   braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#'   #                                  FALSE, braids$is_multibraid)
#'   #   
#'   #   # move braid_id and is_multibraided columns to front of dataframe
#'   #   braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#'   #   # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#'   # }
#'   # return(braids)
#'   
#' }
#' 
#' #' Create a list of braid IDs containing COMIDs in each braid for a single continguous network
#' #'
#' #' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' #' Internal function for use in 'get_braid_list()'
#' #' @param network The network object representing the river network.
#' #' @param start Optional argument specifying the starting point for braid detection.
#' #' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' #' 
#' #' @return list of braid IDs with COMIDs within each braid
#' #' @importFrom dplyr select filter
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom stats setNames
#' #' @export
#' unique_braids <- function(
#'     network,
#'     start        = NULL,
#'     verbose      = FALSE
#' ) {
#'   
#'   # network <- net2
#'   # start = NULL
#'   
#'   # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
#'   if(nrow(network) <= 1) {
#'     
#'     if(verbose) {
#'       message("No cycles found, returning NULL")
#'     }
#'     
#'     return(NULL)
#'   }
#'   
#'   # # check valid return_as input
#'   # if(!return_as %in% c("list", "dataframe")) {
#'   #   stop("Invalid 'return_as' argument '",
#'   #        return_as, "'\n'return_as' must be: 'list' or 'dataframe'")
#'   # }
#'   
#'   # # check relationship argument is valid
#'   # if(return_as == "dataframe") {
#'   #   if(!relationship %in% c("one-to-one", "one-to-many")) {
#'   #     stop("Invalid 'relationship' argument '",
#'   #          relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
#'   #   }
#'   # }
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # turn network into a directed graph
#'   dag <- create_dag(network)
#'   
#'   # get the fromnode associated with a given COMID 'start'
#'   start_node <- comid_to_node(dag, start)
#'   
#'   if(verbose) {
#'     message("Starting braid detection at ", 
#'             ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
#'   }
#'   
#'   # drop graph geometry
#'   dag <- sf::st_drop_geometry(dag)
#'   # graph <- sf::st_drop_geometry(graph)
#'   
#'   # stash comids and from IDs
#'   comid_map <- dplyr::select(
#'     # graph,
#'     dag,
#'     comid,
#'     fromnode,
#'     tonode
#'   )
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identfied as a cycle and thus a braid
#'   # Questionable at this point, other option is the code below that uses 
#'   # single_cycles() function (COMMENTED OUT CODE BELOW)
#'   regraph <- renode_circuits(dag, verbose = FALSE)
#'   
#'   # make an undirected graph
#'   undir <- make_undirected(regraph)
#'   
#'   # find cycles in undirected graph (proxy for braids)
#'   cycles <- find_cycles(
#'     graph     = undir,
#'     start     = start_node,
#'     return_as = "list",
#'     edge      = FALSE,
#'     wide      = TRUE,
#'     verbose   = verbose
#'   )
#'   
#'   # if(cycles$size() == 0 & is.null(single_braids)) {
#'   if(is.null(cycles)) {
#'     # message("No cycles found, returning NULL")
#'     return(NULL)
#'   }
#'   
#'   # remove added nodes from renode_circuits()
#'   braids <- lapply(cycles, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   braids <- lapply(1:length(braids), function(k) {
#'     # get COMIDs of each braid
#'     comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#'       dplyr::filter(tonode %in% braids[[k]]) %>%
#'       .$comid
#'   })
#'   
#'   # set braid_id names
#'   braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
#'   
#'   return(braids)
#' }
#' 
#' #' Detect whether a braid exists in a NHDPlus flowlines Network
#' #' Check if if a NHDPlus network dataset contains any braids. If multiple discontinuous networks are within the 'network' data, a 'terminal_id' can be be provided to uniquely check each network for braids. If no terminal_id is given, the function will try to infer the distinct networks and then check for braids in each component (using find_connected_components()).
#' #' @param network data.frame with comid, tonode, fromnode, divergence and (optionally)  terminalpa attributes.
#' #' @param terminal_id character, column name containing a unique identifier, delineating separate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
#' #' @param recycle logical, whether the return logical vector should be recycled to the length of the number of unique networks (disconnected networks/outlets/terminalpa). 
#' #' If FALSE (default), the function returns TRUE if ANY of the networks contain a braid. Otherwise, if TRUE, the function attempts to distinguish the different/separate network components and 
#' #' returns a logical vector the length of the number of connected components in the network.If a 'terminal_id' is given, the return logical vector will use the 'terminal_id' column to
#' #'  name the vector, if 'terminal_id' is NULL, then an arbitrary COMID is used to uniquely identify each distinct port of the network and these arbitrary COMIDs are used to name the logical vector elements.
#' #' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' #' 
#' #' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found. If multiple components are found OR a terminal_id column is given, each unique network is checked for braiding (recycles to length of unique "terminal_id")
#' #' @importFrom dplyr filter
#' #' @importFrom stats setNames
#' #' @export
#' is_braided <- function(
#'     network,
#'     terminal_id = NULL,
#'     recycle     = FALSE,
#'     verbose     = FALSE
#' ) {
#'   
#'   # # Test data for braids
#'   # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
#'   # net <-  net2 %>%
#'   #   dplyr::select(comid, fromnode, tonode, streamcalc, divergence, terminalpa) %>%
#'   #   dplyr::filter(!comid %in% c(1079041, 1078529, 1078505, 1078403, 1078391, 1078491, 1078485, 1078483))
#'   # network <- net %>%
#'   #   dplyr::select(-terminalpa)
#'   # terminal_id = NULL
#'   # recycle     = FALSE
#'   # network <- net
#'   # terminal_id = "terminalpa"
#'   # verbose     = FALSE
#'   
#'   # if no terminal_ids column name is given, create one by finding all connected componenets in network
#'   if(is.null(terminal_id)) {
#'     
#'     # determine which flowlines are part of which contiguous set of linestrings 
#'     # (creates a unique identifier for each seperated network of flowlines)
#'     network <- find_connected_components(network = network, add = TRUE, arbitrary = FALSE)
#'     
#'   } else { # if a terminal_id IS provided, the column name is checked and changed to "component_id"
#'     
#'     # check that 'terminal_id' is a column in 'network', if not throw an error
#'     if(!terminal_id %in% names(network)) {
#'       stop("Invalid terminal_id argument '", terminal_id, "', terminal_id must be a column in 'network'")
#'     }
#'     
#'     if(verbose) { message("User provided 'terminal_id': '", terminal_id, "'")}
#'     
#'     # name of the "tocomid" column in network that should be removed
#'     term_col <- names(network)[names(network) %in% terminal_id]
#'     
#'     # replace term_col column name with "terminalID"
#'     names(network) <- gsub(term_col, "component_id", names(network)) 
#'     
#'   }
#'   
#'   # unique(network$terminalpa) %>% length()
#'   # unique(network$component_id) %>% length()
#'   
#'   # list of the uniqueu component IDs 
#'   components <- unique(network$component_id)
#'   
#'   # loop through each of the unique 'component_id' and check whether each component has a braid or not
#'   braid_checks <- sapply(1:length(components), function(i) {
#'     
#'     # if(verbose) {
#'     #   message(i, "/", length(components))
#'     #   message("component[i]: ", components[i])
#'     #   message("# rows in component ", components[i], ": ", nrow(
#'     #     dplyr::filter(network, component_id %in% components[i])
#'     #   ))
#'     #   message('=====================')
#'     # }
#'     # dplyr::filter(network,
#'     #               component_id %in% components[i])$geometry %>% plot()
#'     
#'     # get list of braids for distinct network
#'     # filter network down to the iterations component (components[i])
#'     internal_is_braided(
#'       network = dplyr::filter(network,  
#'                               component_id %in% components[i]), 
#'       start   = NULL,
#'       verbose = verbose
#'     )
#'     
#'   })
#'   
#'   # if recycle == TRUE add the component_id as a name to the logical vector
#'   if (recycle) {
#'     # add names to braid_checks logical vector if recycle == TRUE
#'     braid_checks <- stats::setNames(braid_checks, components)
#'     
#'     return(braid_checks)
#'   }
#'   
#'   # if ANY are braids are found, return TRUE
#'   braid_checks <- any(braid_checks)
#'   
#'   return(braid_checks)
#' }
#' 
#' #' Internal function for detecting whether a braid exists in a NHDPlus flowlines Network
#' #' Function is used within is_braided() to determine if a network is braided (TRUE) or not (FALSE)
#' #' @param network data.frame with comid, tonode, fromnode, and (optionally) divergence and terminalpa attributes.
#' #' @param start integer COMID to start braid detection from. A start COMID should be provided in cases where network is made up of disconnected network components (i.e. a start COMID will detect all braids in each connected flowlines network) Default is NULL and braid checking will start from the flowline that has a tocomid value of "0", or a random flowline otherwise. 
#' #' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom fastmap fastmap
#' #' @importFrom stats setNames
#' internal_is_braided <- function(
#'     network,
#'     start   = NULL,
#'     verbose = FALSE
#' ) {
#'   
#'   # network = dplyr::filter(network,   component_id %in% components[i])
#'   # start   = NULL
#'   # verbose = verbose
#'   # network = dplyr::filter(network,
#'   #                         component_id %in% components[i])
#'   # start   = NULL
#'   # verbose = verbose
#'   
#'   # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
#'   if(nrow(network) <= 1) {
#'     
#'     # if(verbose) {
#'     #   message("No cycles found, returning NULL")
#'     # }
#'     
#'     return(FALSE)
#'   }
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # create directed acyclic graph
#'   graph <- create_dag(network)
#'   
#'   # get the fromnode associated with a given COMID 'start'
#'   start_node <- comid_to_node(graph, start)
#'   
#'   # drop graph geometry
#'   graph <- sf::st_drop_geometry(graph)
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identified as a cycle and thus a braid
#'   # Questionable at this point, other option is the code using single_cycles() function 
#'   graph <- renode_circuits(graph, verbose = FALSE)
#'   
#'   # make an undirected graph from DAG
#'   graph <- make_undirected(graph)  
#'   
#'   # make hashmap for tonode/fromnode topology for traversing graph
#'   topo_map <- make_topo_map(
#'     from_nodes = graph$fromnode,
#'     to_nodes   = graph$tonode,
#'     directed   = FALSE
#'   )
#'   
#'   # keep track of visited nodes
#'   visit <- fastmap::fastmap()
#' 
#'   # # set all marked values to FALSE
#'   visit$mset(.list = stats::setNames(
#'                                 lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i)
#'                                   { FALSE }
#'                                   ),
#'                                 unique(c(graph$fromnode, graph$tonode))
#'                                 )
#'              )
#'   
#'   # initialize is_cycle value
#'   is_cycle <- FALSE
#'   
#'   # Depth first search function for detecting cycles 
#'   detect_cycle <- function(node, prev) {
#' 
#'     # message("--------------")
#'     # message("node: ", node)
#'     # message("prev: ", prev)
#'     # message("is_cycle: ", is_cycle)
#'     # message("--------------")
#'     
#'     # if is_cycle is TRUE, return
#'     if(is_cycle) {
#'       # message("!!!!! is_cycle IS TRUE RETURNING !!!!!")
#'       return()
#'     }
#'     
#'     # visit node
#'     visit$set(node, TRUE)
#'     
#'     # get the neighbors of current node
#'     neighbors <- topo_map$get(node)$to_node
#' 
#'     # message("Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
#'     
#'     for (i in neighbors) {
#'       # message("NEIGHBOR: ", i)  
#'         if(
#'           # isTRUE(visit$get(i)) & i != prev
#'           visit$get(i) && i != prev
#'         ) {
#'           
#'           # message("!!!!!!!!! neighbor ", i, " HAS BEEN VISITED and i != prev (", i, " != ", prev, ")")
#'           # message("!!!!!!!!! SET is_cycle to TRUE")
#'           is_cycle <<- TRUE
#'           
#'           return()
#'           
#'         }
#'       # if neighbor "i" has NOT been visited yet, call DFS on neighbor "i" and node as "prev"
#'       if(!visit$get(i)) {
#'         # message("----> VISITING NEIGHBOR ", i)
#'         detect_cycle(node = i, prev = node)
#'         }
#'       }
#'   }
#'  
#'   # detect if any cycles in network
#'   detect_cycle(node = start_node,  prev = "none")
#' 
#'   return(is_cycle)
#' }
#' 
#' #' Collapse and rename braid IDs in a list of lists
#' #' Internal function. Ensures there are no duplicate braid_ids, making 
#' #' each braid ID unique. and collapsing the original list into a single list with list names being the braid_id
#' #' Collapse, deduplicate, and rename braid IDs in a list of lists
#' #' @description Internal function. Ensures there are no duplicate braid_ids, making each braid ID unique. and collapsing the original list into a single list with list names being the braid_id
#' #' @param x list, with each element either being NULL or a list with braid_id names (i.e. "braid_1", "braid_2", etc.)
#' #'  
#' #' @noRd
#' #' @keywords internal
#' #' @return list with unique braid IDs / COMIDs
#' reduce_braid_list <- function(x) {
#'   
#'   # x <- braid_list
#'   
#'   # Remove NULL values using indexing
#'   x <- x[!sapply(x, is.null)]
#'   # Filter(function(i) !is.null(i), x)
#'   
#'   # sort the comids in each braid
#'   # collapse the comids into an "_" underscore seperate string
#'   # collapse all underscore seperated comids into a single vector and remove the duplicates (i.e. unique())
#'   new_braids <- unique(
#'     unlist(
#'       lapply(1:length(x), function(i) {
#'         sapply(1:length(x[[i]]), function(k) {
#'           
#'           paste0(
#'             as.character(
#'               sort(
#'                 Reduce(c, x[[i]][k])
#'                 # x[[i]][k]
#'               )
#'             ),
#'             collapse = "_"
#'           )
#'         })
#'       })
#'     )
#'   )
#'   
#'   # split strings by the underscore
#'   new_braids <- strsplit(new_braids, "_")
#'   
#'   # convert to integer
#'   new_braids <- sapply(new_braids, as.integer)
#'   
#'   # assign new braid_ids as names to list
#'   names(new_braids) <- paste0("braid_", 1:length(new_braids))
#'   
#'   return(new_braids)
#' }
#' 
#' #' Determine the outlets/start COMIDS of a network
#' #' Determine comid of the terminal flowline leading out of a basin. Requires fromnode and tonode columns.
#' #' Used internally within 'find_braids()' function
#' #' @param network data.frame or sf object with comid, tonode, fromnode, and divergence attributes. If a "tocomid" column exists, it is recommended to remove this beforehand
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return numeric vector of COMIDs
#' #' @importFrom dplyr select
#' #' @importFrom nhdplusTools get_tocomid
#' get_start_ids <- function(
#'     network
#' ) {
#'   
#'   # network
#'   # add= F
#'   
#'   # get_terminal_ids(network, add = TRUE)
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # get to comids
#'   x <- 
#'     nhdplusTools::get_tocomid(
#'       dplyr::select(network, 
#'                     comid, fromnode, tonode, divergence),
#'       return_dendritic = TRUE,
#'       remove_coastal   = FALSE
#'     )
#'   
#'   # ID of comid that starts/ends the network
#'   starts <- x$comid[which(!x$tocomid %in% x$comid)]
#'   
#'   return(starts)
#'   
#' }
#' 
#' #' Add the terminal ID of for each flowline in NHDPlus dataframe
#' #' Determine comid of the terminal flowline leading out of a basin. Requires fromnode and tonode columns.
#' #' Used internally within 'find_braids()' function
#' #' @param network data.frame or sf object with comid, tonode, fromnode, and divergence attributes. If a "tocomid" column exists, it is recommended to remove this beforehand
#' #' @param add logical, whether to add the component_id to the original dataset. If TRUE (default) a terminalID column is added to original data, otherwise (FALSE) a vector of the unique terminalIDs is returned 
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return data.frame, sf data.frame or numeric vector
#' #' @importFrom dplyr select left_join
#' #' @importFrom nhdplusTools get_tocomid get_sorted
#' #' @importFrom sf st_drop_geometry
#' get_terminal_ids <- function(
#'     network, 
#'     add = TRUE
#' ) {
#'   
#'   # network
#'   # add= F
#'   
#'   # get_terminal_ids(network, add = TRUE)
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # get to comids
#'   x <- 
#'     nhdplusTools::get_tocomid(
#'       dplyr::select(network, 
#'                     comid, fromnode, tonode, divergence),
#'       return_dendritic = TRUE,
#'       remove_coastal   = FALSE
#'     )
#'   
#'   # sort network and get terminalID column
#'   x <- nhdplusTools::get_sorted(
#'     x,
#'     split = TRUE
#'   )
#'   
#'   # if add is TRUE, return the original dataframe with terminal ID added
#'   if (add) {
#'     
#'     # join x back with original 'network' data, adding the terminalID column
#'     x <- dplyr::left_join(
#'       network,
#'       dplyr::select(
#'         sf::st_drop_geometry(x), 
#'         comid, terminalID
#'       ),
#'       by = "comid"
#'     )
#'     
#'     return(x)
#'   } 
#'   
#'   # if add was NOT TRUE, then return the unique terminal IDs
#'   term_ids <- unique(x$terminalID)
#'   
#'   return(term_ids)
#'   
#' }
#' 
#' #' Find the connected components in a NHDPlus flowlines Network
#' #' 
#' #' Determine how many different, unconnected/seperate sets of flowlines are within a set of NHDPlus flowlines. 
#' #' The input 'network' dataset must contain a comid, tonode, fromnode, and then (optionally) divergence and terminalpa attributes.
#' #' Used internally within 'get_braid_list' and 'find_braids()' function to make sure each connected set of flowlines is addressed and braids are searched for in each seperated component.
#' #' @param network data.frame or sf object with comid, tonode, fromnode, and (optionally) divergence and terminalpa attributes. If a "tocomid" column exists, it is recommended to remove this beforehand
#' #' @param add logical, whether to add the component_id to the original dataset. If TRUE (default) the original dataset is returned with an additional component_id column, indicating the set of connected components each comid belongs too. If FALSE, a dataframe with the COMID and component_id is returned 
#' #' @param arbitrary logical, whether use arbitrary numbering of components or to use starting COMID as component_id. If TRUE (default) components are given a component ID 1 through the number of components, otherwise (FALSE) an arbitrary COMID is used to as the component_id flag for all connected components 
#' #' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' #' @return original dataframe with an added 'component_id' column (if add = TRUE) or a new dataframe with "comid" and "component_id" columns (if add = FALSE)
#' #' @importFrom dplyr tibble select left_join
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom fastmap fastmap
#' #' @importFrom stats setNames
#' #' @export
#' find_connected_components <- function(
#'     network,
#'     add       = TRUE,
#'     arbitrary = TRUE,
#'     verbose   = FALSE
#' ) {
#'   
#'   # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
#'   # net <-  net2 %>%
#'   #   dplyr::select(comid, fromnode, tonode, streamcalc, divergence, terminalpa) %>%
#'   #   dplyr::filter(!comid %in% c(1079041, 1078529, 1078505, 1078403, 1078391, 1078491, 1078485, 1078483))
#'   # network <- net %>%
#'   #   dplyr::select(-terminalpa)
#'   # plot(network$geometry)
#'   # add       = TRUE
#'   # arbitrary = FALSE
#'   # verbose   = FALSE
#'   
#'   # network <- nets
#'   # network <- dplyr::select(network, -tocomid)
#'   # add = FALSE
#'   
#'   # initialize NULL dropped_col value to store dropped to comid column if necessary
#'   dropped_col <- NULL
#'   
#'   # Store the original column order
#'   col_order <- c("component_id", names(network))
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # try and detect a "tocomid" like column, if its found in the network columns, remove that column
#'   if (any(names(network) %in% c("tocomid", "toCOMID", "to_comid", "TOCOMID", "to_COMID", "to_Comid", "TO_COMID"))) {  
#'     
#'     # name of the "tocomid" column in network that should be removed
#'     drop_tocomid <- names(network)[names(network) %in% c("tocomid", "toCOMID", "to_comid", "TOCOMID", "to_COMID", "to_Comid", "TO_COMID")]
#'     
#'     # store the dropped tocomid column to add back later if needed
#'     dropped_col <- sf::st_drop_geometry(network[, c("comid", drop_tocomid)])
#'     # network[, c("comid", drop_tocomid), drop = T]
#'     
#'     # remove to comid type column if found
#'     network <- network[, names(network) != drop_tocomid]
#'     
#'   }
#'   
#'   # create directed acyclic graph
#'   graph <- create_dag(network)
#'   
#'   # # # get the fromnode associated with a given COMID 'start'
#'   # start_nodes <- sapply(network_starts$comid, function(i) {comid_to_node(graph, i)})
#'   # start_node <- comid_to_node(graph, start_nodes)
#'   
#'   # drop graph geometry
#'   graph <- sf::st_drop_geometry(graph)
#'   
#'   # # stash comids and from IDs
#'   comid_map <- dplyr::select(
#'     graph,
#'     comid,
#'     fromnode,
#'     tonode
#'   )
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identified as a cycle and thus a braid
#'   # Questionable at this point, other option is the code using single_cycles() function 
#'   graph <- renode_circuits(graph, verbose = verbose)
#'   
#'   # make an undirected graph from DAG
#'   graph <- make_undirected(graph)  
#'   
#'   # make hashmap for tonode/fromnode topology for traversing graph
#'   topo_map <- make_topo_map(
#'     from_nodes = graph$fromnode,
#'     to_nodes   = graph$tonode
#'   )
#'   
#'   # Depth first search function to use to find/delineate connected components of network object
#'   dfs <- function(node, visit) {
#'     
#'     # message("VISITING node: ", node)
#'     
#'     # visit node
#'     visit$set(node, TRUE)
#'     
#'     # get the neighbors of current node
#'     neighbors <- topo_map$get(node)$to_node
#'     
#'     # message("---> Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
#'     
#'     # iterate through the neighbors of current node and run DFS on them if they have not been visited yet
#'     for (i in neighbors) {
#'       
#'       # message(" - NEIGHBOR: ", i)
#'       
#'       # if neighbor i has NOT been visited
#'       if (!visit$get(i)) {
#'         
#'         # message("!!!!!!!", i, " NOT VISISTED ---> RUN DFS !!!!!!!!!")
#'         dfs(i, visit)
#'         
#'       }
#'       
#'       # message("NEIGHBOR: ", i)          
#'       # message("===========================================")
#'       # message("===========================================")
#'     }
#'     
#'   }
#'   
#'   # keep track of visited nodes
#'   visit <- fastmap::fastmap()
#'   
#'   # # set all marked values to FALSE
#'   visit$mset(.list = stats::setNames(
#'     lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i){ FALSE }),
#'     unique(c(graph$fromnode, graph$tonode)))
#'   )
#'   
#'   # initialize count
#'   count = 0
#'   
#'   # hashmap for output components
#'   out <- fastmap::fastmap()
#'   
#'   # while there are still values left in the visit and topo_map hashmap
#'   while(visit$size() > 0 & topo_map$size() > 0) {
#'     # while(visit$size() > 0 & length(start_nodes) > 0) {
#'     
#'     # node to kickoff DFS from
#'     start_node <- names(
#'                       topo_map$as_list()[
#'                         length(topo_map$as_list())
#'                       ]
#'                     )
#'     
#'     # if a COMID should be used as the names of the component_id output list
#'     if(!arbitrary) {
#'       component_comid <- comid_map$comid[comid_map$tonode == start_node]
#'     }
#'     
#'     # message("===========================================")
#'     # message("========== ITERATION: ", count, " =============")
#'     # message("========== start_node: ", start_node, " =============")
#'     # # message("========== component_comid: ", component_comid, " =============")
#'     # message("===========================================")
#'     
#'     # run DFS from the 'start_node'
#'     dfs(start_node, visit)
#'     # dfs(start_nodes[[i]], visit)
#'     
#'     # extract the newly visited nodes
#'     new_visits <- visit$as_list()
#'     
#'     # remove newly visited nodes
#'     to_remove <- names(new_visits[new_visits == TRUE])
#'     
#'     # increment component count
#'     count = count + 1
#'     
#'     # remove set of connected components
#'     visit$remove(to_remove)
#'     
#'     topo_map$remove(to_remove)
#'     
#'     # store connectd component in out hashmap with the key being the connected component count number and 
#'     # the value being the nodes in the connected component
#'     
#'     # if arbitrary == TRUE, then use 'count' as 'component_id', otherwise use the "component_comid" 
#'     if(arbitrary) {
#'       
#'       # use "count" as "component_id
#'       out$set(as.character(count), to_remove)
#'       
#'     } else { 
#'       
#'       # use the "component_comid" as "component_id
#'       out$set(as.character(component_comid), to_remove)
#'       
#'     }
#'     
#'     # out$set(as.character(count), to_remove)
#' 
#'   }
#'   
#'   # resulting output
#'   out <- out$as_list()
#'   
#'   # ccreate dataframe of component number and node ID 
#'   out <- dplyr::tibble(
#'     component_id = rep(names(out), lengths(out)),
#'     fromnode     = as.integer(Reduce(c, out))
#'   )
#'   
#'   # join component ID back with original comid_map dataframe
#'   res <- 
#'     comid_map %>% 
#'     dplyr::left_join(
#'       out,
#'       by = "fromnode"
#'     )
#'   
#'   # out$fromnode %>% unique() %>% length()
#'   # res$fromnode %>% unique()
#'   
#'   # if add is TRUE, add the component ID to the original 'network' input
#'   if(add) {
#'     
#'     res <- dplyr::left_join(
#'       network,
#'       dplyr::select(res, 
#'                     comid, component_id
#'       ),
#'       by = "comid"
#'     )
#'     # names(res2)
#'     # if dropped_col is NOT NULL, then join the dropped call back with dataset
#'     if (!is.null(dropped_col)) {
#'       
#'       res <- dplyr::left_join(res, 
#'                               dropped_col,
#'                               by = "comid"
#'       )
#'       
#'     }
#'     
#'     # reorder columns to have original order with component_id as the first column
#'     res <- res[, col_order]
#'     
#'     # if add is FALSE, return dataframe with comid, fromnode, tnode, and component_id
#'   } else {
#'     
#'     # join just the comid, fromnode, tonode, data from 'network' with the comid and component_id from 'res'
#'     res <-  dplyr::left_join(
#'       dplyr::select(
#'         sf::st_drop_geometry(network), 
#'         comid, fromnode, tonode
#'       ),
#'       dplyr::select(res, 
#'                     comid, component_id
#'       ),
#'       by = "comid"
#'     )
#'   }
#'   
#'   return(res)
#'   
#'   # PLOT FOR EXAMPLES 
#'   # res %>% 
#'   #   ggplot2::ggplot() +
#'   #   ggplot2::geom_sf(ggplot2::aes(color = component_id))
#' 
#'   # count_components <- function(topo, visit) {
#'   #   count = 0
#'   #   for(i in 1:length(topo$as_list())) {
#'   #     node <- names(topo$as_list()[i])
#'   #     message("============ ITERATION i: ", i, " =============")
#'   #     message("============: ", node, " =============")
#'   #     if (!visit$get(node)) {
#'   #       message("INCREASING COUNT: ", count)
#'   #       count = count + 1
#'   #       dfs(node, visit)
#'   #     }
#'   #   }
#'   #   return(count)
#'   # }
#'   # count_components(topo = topo_map, visit = visit)
#' }
#' 
#' #' Find overlaping elements in a list of vectors
#' #' Given a list of vectors, return the indexes of the list elements that have duplicates in another list element. 
#' #' The value of each returned list element represents the index of the other list element that has duplicated vector values in the original list element
#' #' @param lst list
#' #' @param no_overlap_val numeric, character, or logical value to represent non overlapping values. Default is FALSE
#' #' @param rm_no_overlap logical, whether to remove list elements with no overlap. Default is FALSE, thus overlapping elements are not removed
#' #' @param verbose logical, whether to print messages or not. Default is FALSE
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return list with each value representing the index of the other list element that has duplicated vector values
#' find_overlaps <- function(
#'     lst, 
#'     no_overlap_val = FALSE,
#'     rm_no_overlap  = FALSE,
#'     verbose        = FALSE
#' ) {
#'   
#'   # lst <- braids
#'   overs <- lapply(seq_along(lst), function(i) {
#'     matches <- which(
#'       sapply(seq_along(lst), function(j) {
#'         i != j && all(lst[[i]] %in% lst[[j]])
#'       }))
#'     
#'     if (length(matches) > 0) {
#'       matches
#'     } else {
#'       no_overlap_val
#'     }
#'     
#'   })
#'   
#'   # whether to remove list elements with no overlap
#'   if(rm_no_overlap) {
#'     
#'     # if no overlap is NULL
#'     if(is.null(no_overlap_val)) {
#'       
#'       overs <- overs[!sapply(overs, is.null)]
#'       
#'       # no overlap is NOT NULL use  no_overlap_val to remove value
#'     } else {
#'       
#'       overs <- overs[!sapply(overs, function(j) {j == no_overlap_val})]
#'       
#'     }
#'   }
#'   
#'   # if no overlaps found return NULL
#'   if (length(overs) == 0) {
#'     
#'     if(verbose) { message("No overlaps found") }
#'     
#'     return(NULL)
#'     
#'   } 
#'   
#'   return(overs)
#'   
#'   # overs <- vector("list", length(lst))
#'   # for (i in 1:length(lst)) {
#'   #   matches <- c()
#'   #   for (j in 1:length(lst)) {
#'   #     if (i != j && all(lst[[i]] %in% lst[[j]])) {
#'   #       matches <- c(matches, j)
#'   #     }} if (length(matches) > 0) { overs[[i]] <- matches } else {  overs[[i]] <- FALSE}}
#'   # return(overs)
#' }
#' 
#' #' Create a list of all overlapping braid IDs for each element in a list of ID vectors
#' #' 
#' #' Given a list of braid_ids with each list element being a vector of IDs within the braid_id,
#' #' create a list of all the overlapping braid_ids such that a braid that is overlapping 
#' #' with other braids, will now contain the braid_id of its nearby/overlapping braid_ids
#' #'
#' #' @param x named list of vectors
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return list of the same length as input list with each list element containing a vector of all overlapping names in the rest of the list 'x' 
#' find_multibraids <- function(x) {
#'   
#'   # list to store the names of the list elements whose vectors have overlapping values
#'   overlaps <- list()
#'   # i = 1
#'   # go through original list of IDs and find overlaps
#'   for (i in seq_along(x)) {
#' 
#'     # sapply(x, function(vec) {
#'     #   vec %in% x[[i]]
#'     #   }
#'     # )
#'     
#'     overlap_names <- names(x)[
#'       sapply(x, function(vec) any(vec %in% x[[i]]))
#'     ]
#'     
#'     # remove self braid_id and then reinsert it at the front of vector
#'     overlap_names <- c(names(x)[i], 
#'                        overlap_names[names(x)[i] != overlap_names]
#'     )
#'     
#'     overlaps[[names(x)[i]]] <- overlap_names
#'   }
#'   
#'   return(overlaps)
#'   
#' }
#' 
#' #' Get the braid_ids of all neighboring braids from a specified braid_id
#' #' 
#' #' @param x dataframe, sf object network flowlines with braid_id column
#' #' @param ids character vector braid_id(s)
#' #' @param split_ids logical, if TRUE, then the comma seperated braid_ids are separated into individual braid_ids
#' #' @param only_unique logical, If TRUE, then only unique braid IDs are returned, otherwise (FALSE) all are returned. Default is FALSE. 
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return character vector of braid IDs neighboring one or multiple braid_ids
#' get_neighbor_braids <- function(x, ids, split_ids = FALSE, only_unique = FALSE) {
#'   
#'   
#'   # x = braids
#'   # ids = bids
#'   # only_unique = T
#'   # split_ids = FALSE
#'   
#'   # make groups for each braided section
#'   braid_groups <- lapply(1:length(ids), function(i) {
#'     
#'     # message("i: ", i, "/", length(ids))
#'     
#'     # braid IDs of interest
#'     bids <- strsplit(ids[i], ", ")[[1]]
#'     # bids <- strsplit(transects[i, ]$braid_id, ", ")[[1]]
#'     
#'     # get all linestrings that are apart of the braid_ids of interest
#'     
#'     bids_check <- sapply(1:length(x$braid_id), function(y) {
#'       any(
#'         strsplit(x$braid_id[y], ", ")[[1]] %in% bids
#'       )
#'     })
#'     
#'     # out <- unique(c(
#'     #             x[bids_check, ]$braid_id,
#'     #             unlist(strsplit(x[bids_check, ]$braid_id, ", "))
#'     #             )
#'     #           )
#'     out <- sort(
#'       unique(c(
#'         x[bids_check, ]$braid_id,
#'         unlist(strsplit(x[bids_check, ]$braid_id, ", "))
#'       )
#'       )
#'     )
#'     
#'     # split_ids is TRUE, then the coimma seperated braid_ids are seperated into individual braid_ids
#'     if(split_ids) {
#'       
#'       out <- sort(
#'         unique(unlist(strsplit(out, ", ")))
#'       )
#'       # out <- unique(unlist(strsplit(out, ", ")))
#'       
#'     }
#'     
#'     out
#'     
#'   })
#'   
#'   # assign names
#'   names(braid_groups) <- ids
#'   
#'   # remove uniques if desired
#'   if(only_unique) {
#'     braid_groups <- unique(unname(unlist(braid_groups)))
#'   }
#'   return(braid_groups)
#' }
#' 
#' #' Create a list of braid IDs containing COMIDs in each braid for a single continguous network
#' #'
#' #' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#' #' Internal function for use in 'get_braid_list()'
#' #' @param graph graph representation of hydrologic network. Output of get_node_topology2().
#' #' @param start Optional argument specifying the starting point for braid detection.
#' #' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#' #'
#' #' @noRd
#' #' @keywords internal
#' #' @return list of braid IDs with COMIDs within each braid
#' #' @importFrom dplyr select filter
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom stats setNames
#' internal_get_braid_list3  <- function(
#'     graph,
#'     crosswalk_id = NULL,
#'     start        = NULL,
#'     verbose      = FALSE
#' ) {
#'   
#'   # ---------------------------------------------------------------
#'   # network = dplyr::filter(network, 
#'   #                         component_id %in% components[i])
#'   # crosswalk_id = "id"
#'   # start   = NULL
#'   # verbose = verbose
#'   # net_init    <- sf::read_sf(testthat::test_path("testdata", "braided_flowlines.gpkg"))
#'   # crosswalk_id = "comid"
#'   # network %>% 
#'   #   dplyr::filter(hydroseq >= 1385417, hydroseq <= 1385423 | 
#'   #                   hydroseq %in% c(1390468, 1390467, 1390468, 1390469, 1390470, 1390471, 1390472, 1390463, 1390464)
#'   #   ) %>% .$geometry %>% plot()
#'   
#'   # network <-
#'   #   net_init %>%
#'   #   dplyr::filter(hydroseq >= 1385418, hydroseq <= 1385422 |
#'   #                   hydroseq %in% c(1390468, 1390467, 1390468,
#'   #                                   1390469,
#'   #                                   1390470, 1390471, 1390472
#'   #                                   # 1390463, 1390464
#'   #                   )) %>%
#'   #   dplyr::select(dplyr::any_of(crosswalk_id),
#'   #                 fromnode, tonode,
#'   #                 divergence,
#'   #                 geometry = geom
#'   #                 )
#'   # graph <- get_node_topology2(
#'   #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
#'   #   # network,
#'   #   crosswalk_id = crosswalk_id
#'   # )
#'   # ---------------------------------------------------------------
#'   
#'   # if 'network' has 1 or 0 rows, no braids are possible, return NULL 
#'   if(nrow(network) <= 1) {
#'     
#'     if(verbose) {
#'       message("No cycles found, returning NULL")
#'     }
#'     
#'     return(NULL)
#'   }
#'   
#'   # # lower case names
#'   # names(network) <- tolower(names(network))
#'   
#'   # get "to_<crosswalk_id>" string column name 
#'   to_crosswalk_id <- as_to_id(crosswalk_id)
#'   
#'   # # # turn network into a directed graph
#'   # graph <- get_node_topology2(
#'   #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
#'   #   # network,
#'   #   crosswalk_id = crosswalk_id
#'   # )
#'   # network$geometry %>% plot()
#'   
#'   # graph <- get_node_topology(
#'   #   dplyr::select(network, dplyr::any_of(crosswalk_id)),
#'   #   # network,
#'   #   crosswalk_id = crosswalk_id,
#'   #   deduplicate  = FALSE
#'   #   )
#'   
#'   # graph <- hydrofabric3D:::create_dag(network)
#'   
#'   # get the fromnode associated with a given COMID 'start'
#'   start_node <- id_to_node(graph, crosswalk_id, start)
#'   
#'   if(verbose) {
#'     message("Starting braid detection at ", 
#'             ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
#'   }
#'   
#'   # drop graph geometry
#'   graph <- sf::st_drop_geometry(graph)
#'   # graph <- sf::st_drop_geometry(graph)
#'   
#'   # stash comids and from IDs
#'   id_map <- dplyr::select(
#'     graph,
#'     dplyr::any_of(c(crosswalk_id, to_crosswalk_id)),
#'     # dplyr::any_of(c(crosswalk_id, to_crosswalk_id, "comid", "tocomid")),
#'     fromnode,
#'     tonode
#'   )
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identfied as a cycle and thus a braid
#'   # Questionable at this point, other option is the code below that uses 
#'   # single_cycles() function (COMMENTED OUT CODE BELOW)
#'   
#'   graph <- renode_circuits2(graph, crosswalk_id = crosswalk_id, verbose = FALSE)
#'   # graph <- hydrofabric3D:::renode_circuits(graph, verbose = FALSE)
#'   # graph <- hydrofabric3D:::renode_circuits(dplyr::rename(graph, tocomid = to_comid), verbose = FALSE)
#'   
#'   # make an undirected graph
#'   graph <- make_undirected2(graph)
#'   # undir <- make_undirected2(graph)
#'   # graph <- hydrofabric3D:::make_undirected(graph)
#'   
#'   # find cycles in undirected graph (proxy for braids)
#'   cycles <- hydrofabric3D:::find_cycles(
#'     graph     = graph,
#'     # graph = distinct(graph),
#'     # graph     = undir,
#'     start     = start_node,
#'     return_as = "list",
#'     edge      = FALSE,
#'     wide      = TRUE,
#'     verbose   = verbose
#'   )
#'   
#'   # if(cycles$size() == 0 & is.null(single_braids)) {
#'   if(is.null(cycles)) {
#'     # message("No cycles found, returning NULL")
#'     return(NULL)
#'   }
#'   
#'   # remove added nodes from renode_circuits()
#'   braids <- lapply(cycles, function(i) { 
#'     i[i %in% id_map$fromnode] 
#'   })
#'   
#'   # # match fromnodes from id_map to nodes identified in each cycle
#'   braids <- lapply(1:length(braids), function(k) {
#'     # get COMIDs of each braid
#'     id_map[id_map$fromnode %in% braids[[k]], ] %>%
#'       dplyr::filter(tonode %in% braids[[k]]) %>%
#'       .[[crosswalk_id]]
#'   })
#'   
#'   # set braid_id names
#'   braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
#'   
#'   return(braids)
#'   
#' }
#' 
#' # TODO: finalize and use this instead of get_node_topology() and create_dag()
#' get_node_topology2 <- function(
#'     x,
#'     crosswalk_id    = NULL
#' ) {
#'   
#'   ####  ####  ####  ####  ####  ####
#'   
#'   # to_crosswalk_id = "toid"
#'   # x <- dplyr::select(network, dplyr::any_of(crosswalk_id))
#'   # network,
#'   # crosswalk_id = crosswalk_id
#'   # x <- network
#'   # x = dplyr::select(network, dplyr::any_of(crosswalk_id))
#'   # crosswalk_id = "comid"
#'   # crosswalk_id = crosswalk_id
#'   # stash_nodes <- x %>% dplyr::select(comid, fromnode, tonode) %>% sf::st_drop_geometry()
#'   
#'   ####  ####  ####  ####  ####  ####
#'   
#'   # make a unique ID if one is not given (NULL 'id')
#'   if(is.null(crosswalk_id)) {
#'     # x             <- add_hydrofabric_id(x)
#'     crosswalk_id  <- 'hydrofabric_id'
#'   }
#'   
#'   REQUIRED_COLS <- c(crosswalk_id)
#'   
#'   if (!all(REQUIRED_COLS %in% names(x))) {
#'     
#'     missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(x))]
#'     
#'     stop("'x' is missing one or more of the required columns:\n > ", 
#'          paste0(missing_cols, collapse = "\n > "))
#'   }
#'   
#'   # temporary change crosswalk_id to a standard "id" for hydroloom
#'   names(x)[names(x) == crosswalk_id]    <- "id"
#'   
#'   topo <-
#'     x %>%
#'     dplyr::select(id) %>%
#'     # dplyr::select(network, dplyr::any_of(crosswalk_id)) %>% 
#'     # dplyr::select(id = comid) %>%
#'     hydroloom::hy() %>% 
#'     hydroloom::align_names() %>% 
#'     hydroloom::make_attribute_topology(min_distance = 5)
#'   
#'   topo <-
#'     topo %>%
#'     hydroloom::make_node_topology(add_div = TRUE) %>%
#'     hydroloom::add_toids(return_dendritic = FALSE) %>%
#'     dplyr::tibble()
#'   
#'   index_ids <- 
#'     topo %>% 
#'     hydroloom::make_index_ids() 
#'   
#'   adj_list <- 
#'     index_ids$to_list %>% 
#'     dplyr::mutate(toindid = sapply(toindid, function(x) paste(x, collapse = ","))) %>% 
#'     tidyr::separate_rows(toindid, sep = ",") %>%
#'     dplyr::mutate(toindid = as.integer(trimws(toindid)))  
#'   
#'   # reverse_edges <- 
#'   #   adj_list %>%
#'   #   dplyr::rename(fromid = indid, 
#'   #                 toid = toindid) %>%
#'   #   dplyr::mutate(
#'   #     temp_from = fromid, 
#'   #     fromid = toid, 
#'   #     toid = temp_from
#'   #   ) %>%
#'   #   dplyr::select(-temp_from) %>% 
#'   #   dplyr::rename(
#'   #     indid = fromid,
#'   #     toindid = toid
#'   #   )
#'   # 
#'   # # bind original and reverse edges, then drop remove duplicates
#'   # undir <-
#'   #   dplyr::bind_rows(adj_list_long, reverse_edges) %>%
#'   #   dplyr::distinct(indid, toindid, .keep_all = TRUE)
#'   
#'   adj <- 
#'     adj_list %>% 
#'     dplyr::select(id, fromnode = indid , tonode = toindid)
#'   
#'   adj <- 
#'     adj %>% 
#'     dplyr::left_join(
#'       dplyr::select(adj, toid = id, fromnode),
#'       by = c("tonode" = "fromnode")
#'     ) %>% 
#'     dplyr::distinct() %>% 
#'     dplyr::select(id, toid, fromnode, tonode) %>% 
#'     dplyr::mutate(
#'       toid = ifelse(is.na(toid), 0, toid)
#'     )
#'   
#'   # adj %>% 
#'   #   dplyr::arrange(id)
#'   # topo %>% 
#'   #   dplyr::arrange(id)
#'   # # remove repeated IDs
#'   # topo <- topo[!duplicated(topo$id), ]
#'   # 
#'   # # re generate node topology with duplicate IDs removed
#'   # topo <-
#'   #   topo %>% 
#'   #   dplyr::select(-fromnode, -tonode) %>% 
#'   #   hydroloom::make_node_topology(add = TRUE)
#'   
#'   # change "id" and "toid" back to "crosswalk_id" and "to_crosswalk_id"
#'   names(adj)[names(adj) == "id"]     <- crosswalk_id
#'   names(adj)[names(adj) == "toid"]   <- as_to_id(crosswalk_id)
#'   
#'   return(adj)
#'   
#' }
#' 
#' 
#' #' Create a node topology from edge network topology.
#' #'
#' #' This function creates a Directed Acyclic Graph (DAG) given an sf linestring network with a unique identifer.
#' #'
#' #' @param x A data frame or SF linestring dataframe representing a hydrologic network with crosswalk_id and linestring geometries (for SF dataframes) or a crosswalk_id with to_crosswalk_id columns 
#' #' @param crossswalk_id unique ID column name 
#' #' @noRd
#' #' @keywords internal
#' #' @return A data frame representing the Directed Acyclic Graph (DAG) of the network.
#' #' @importFrom dplyr select
#' #' @importFrom hydroloom hy align_names make_attribute_topology make_node_topology add_toids
#' get_node_topology <- function(
#'     x,
#'     crosswalk_id    = NULL
#' ) {
#'   
#'   #### CODE BELOW is probably DROPPABLE ----> ( if block that checks (!is.null(start)) ) ###
#'   #### is NULL check on "start" and if user DID give a start, then nhdplusTools::get_UT() is called to get more upstream COMIDs  ###
#'   #### we don't want data being pulled in, probably better to 
#'   #### leave it up to the user to provide the required data, instead of HOPING that we get the data they were expecting
#'   ####
#'   
#'   # crosswalk_id = "id"
#'   # to_crosswalk_id = "toid"
#'   # x <- network
#'   # make a unique ID if one is not given (NULL 'id')
#'   if(is.null(crosswalk_id)) {
#'     # x             <- add_hydrofabric_id(x)
#'     crosswalk_id  <- 'hydrofabric_id'
#'   }
#'   
#'   REQUIRED_COLS <- c(crosswalk_id)
#'   
#'   if (!all(REQUIRED_COLS %in% names(x))) {
#'     
#'     missing_cols <- REQUIRED_COLS[which(!REQUIRED_COLS %in% names(x))]
#'     
#'     stop("'x' is missing one or more of the required columns:\n > ", 
#'          paste0(missing_cols, collapse = "\n > "))
#'   }
#'   
#'   # temporary change crosswalk_id to a standard "id" for hydroloom
#'   names(x)[names(x) == crosswalk_id]    <- "id"
#'   # names(x)[names(x) == to_crosswalk_id] <- "toid"
#'   
#'   topo <-
#'     x %>%
#'     dplyr::select(id) %>%
#'     # dplyr::select(id, toid) %>% 
#'     # sf::st_drop_geometry() %>%
#'     hydroloom::hy() %>% 
#'     hydroloom::align_names() %>% 
#'     hydroloom::make_attribute_topology(min_distance = 5) %>%
#'     hydroloom::make_node_topology(add_div = TRUE) %>% 
#'     hydroloom::add_toids(return_dendritic = FALSE)
#'   
#'   # change "id" and "toid" back to "crosswalk_id" and "to_crosswalk_id"
#'   names(topo)[names(topo) == "id"]     <- crosswalk_id
#'   names(topo)[names(topo) == "toid"]   <- as_to_id(crosswalk_id)
#'   
#'   # TODO: For when NO linestrings are given, requires a TOID
#'   # x %>% 
#'   #     dplyr::select(id, toid) %>%
#'   #     sf::st_drop_geometry() %>%
#'   #     hydroloom::hy() %>% 
#'   #     hydroloom::align_names() %>% 
#'   #     hydroloom::make_node_topology(add_div = T)
#'   
#'   # x2_ind <- 
#'   #   topo %>% 
#'   #   hydroloom::make_index_ids()
#'   
#'   # outlets <- topo[!topo$tonode %in% topo$fromnode, ]$id
#'   # topo$name_attr          <- ""
#'   # topo$type_attr          <- ""
#'   # topo$major_types        <- ""
#'   # topo %>% 
#'   #   hydroloom::add_divergence(
#'   #     coastal_outlet_ids = outlets,
#'   #     inland_outlet_ids = c(),
#'   #     name_attr = "name_attr",
#'   #     type_attr = "type_attr",
#'   #     major_types = c("StreamRiver", "ArtificialPath", "Connector")
#'   #   )
#'   
#'   return(topo)
#'   
#' }
#' 
#' #' Create a Directed Acyclic Graph (DAG)
#' #'
#' #' This function creates a Directed Acyclic Graph (DAG) given a network with tonodes and fromnodes. The function allows for trimming the network based on a specific starting node and supports the option to reverse the graph.
#' #'
#' #' @param x A data frame representing the river network with columns 'comid', 'fromnode', 'tonode', and optionally 'divergence' for dendritic networks.
#' #' @param start A character string representing the starting node (COMID) of the network. If provided, the network will be trimmed to include only nodes upstream of the start node (including the start node itself).
#' #' @param add logical value indicating whether to add back the original columns to the resulting graph. If FALSE, only comid, tocomid, fromnode, tonode, and optionally divergence columns will be returned.
#' #' @param drop_geom logical value indicating whether to drop the geometry column from the resulting graph. If set to TRUE, the geometry column will be removed.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return A data frame representing the Directed Acyclic Graph (DAG) of the network.
#' #' @importFrom dplyr filter select
#' #' @importFrom nhdplusTools get_UT get_tocomid make_node_topology
#' #' @importFrom sf st_drop_geometry
#' #' @details It is highly recommended to include a 'divergence' column in the input 'x' data frame if the resulting graph will be used for locating cycles within the graph.
#' #' @seealso \code{\link{nhdplusTools::get_tocomid}}, \code{\link{nhdplusTools::make_node_topology}}, \code{\link{reverse_graph}}
#' create_dag <- function(
#'     x,
#'     start     = NULL,
#'     add       = TRUE,
#'     drop_geom = FALSE
#' ) {
#'   
#'   #### CODE BELOW is probably DROPPABLE ----> ( if block that checks (!is.null(start)) ) ###
#'   #### is NULL check on "start" and if user DID give a start, then nhdplusTools::get_UT() is called to get more upstream COMIDs  ###
#'   #### we don't want data being pulled in, probably better to 
#'   #### leave it up to the user to provide the required data, instead of HOPING that we get the data they were expecting
#'   ####
#'   
#'   # if a start ID is given, trim network down to
#'   # only nodes upstream of the ID (including the ID itself)
#'   if (!is.null(start)) {
#'   
#'     # message("Trimming network to start at COMID: ", start, " looking ", direction, "...")
#'     
#'     message("Trimming network to start at COMID: ", start)
#'     # if (reverse) {
#'     #   # Downstream COMIDs
#'     #   dd_comids <- nhdplusTools::get_DD(x, start)
#'     #   x         <- dplyr::filter(x, comid %in% dd_comids) 
#'     # } else {
#'     # upstream COMIDs
#'     ut_comids <- nhdplusTools::get_UT(x, start)
#'     x <- dplyr::filter(x, comid %in% ut_comids) 
#'     
#'     # }
#'     
#'   }
#'   ####
#'   #### We can drop this chunk above here ^^^^
#'   ####
#'   
#'   # make lower case names
#'   names(x) <- tolower(names(x))
#'   
#'   # if an "id" column exists, remove it
#'   if("id" %in% names(x)) {
#'     # if(any(grepl("id", names(x)))) {
#'     x <- dplyr::select(x, -id)
#'   }
#'   
#'   # if divergence column is given, make a dendritic network node topology
#'   if("divergence" %in% names(x)) {
#'     dendritic = TRUE
#'   } else {
#'     dendritic = FALSE
#'   }
#'   
#'   # get dendritic network
#'   network <-  dplyr::select(
#'     nhdplusTools::get_tocomid(
#'       x,
#'       # dplyr::select(x, comid, fromnode, tonode, hydroseq,
#'       # streamleve, streamorde, streamcalc, divergence),
#'       return_dendritic = dendritic,
#'       # return_dendritic = TRUE,
#'       add              = TRUE, 
#'       remove_coastal   = FALSE
#'     ),
#'     -fromnode, -tonode
#'   )
#'   # nhdplusTools::make_node_topology()
#'   # if(flag) {
#'   #   x[x$comid %in% network[duplicated(network$comid), ]$comid, ]$divergence = 2
#'   #   network[duplicated(network$comid), ]$divergence <- 2
#'   # }
#'   
#'   # if NOT dendritic, remove duplicates and return node topology
#'   if(!dendritic) {
#'     # message("NON DENDRITIC")
#'     # remove duplicates
#'     network <- network[!duplicated(network$comid), ]
#'     
#'     # make node topology
#'     network <- nhdplusTools::make_node_topology(
#'       network,
#'       add = TRUE
#'     )
#'     
#'   } else {
#'     
#'     # message("DENDRITIC")
#'     # get divergent tocomids
#'     div <- nhdplusTools::get_tocomid(
#'       x,
#'       # dplyr::select(x, comid, fromnode, tonode, hydroseq,
#'       # streamleve, streamorde, streamcalc, divergence),
#'       return_dendritic = FALSE,
#'       remove_coastal   = FALSE
#'     )
#'     
#'     # divergences w/ unique fromids in x but don't in div
#'     div <- div[div$tocomid %in% x$comid[x$divergence == 2], ]
#'     
#'     network <- nhdplusTools::make_node_topology(
#'       network,
#'       div,
#'       add = TRUE
#'     )
#'     
#'   }
#'   
#'   # whether to add back original columns or cut them out
#'   if(!add) {
#'     # message("NOT ADDING TO ORIGINAL DATA")
#'     # only return relevent columns, no divergence is dendritic = FALSE
#'     if(!dendritic) {
#'       # message("NO DIV COLUMN")
#'       network <- dplyr::select(network, comid, tocomid, fromnode, tonode)
#'     } else {
#'       # message("HAS DIV COLUMN")
#'       network <- dplyr::select(network, comid, tocomid, fromnode, tonode, divergence)
#'     }
#'   }
#'   
#'   # drop geometry
#'   if(drop_geom) {
#'     # message("DROPPING GEOM")
#'     # drop geometry
#'     network <- sf::st_drop_geometry(network)
#'   }
#'   
#'   # # reverse graph
#'   # if(reverse) {
#'   #   message("Reversing graph")
#'   #   network <- reverse_graph(network)
#'   # }
#'   
#'   return(network)
#' }
#' 
#' ### reverse_graph function that was used in "creae_dag()" 
#' ### DEPRECATED 
#' # # Reverse a graph (output of create_dag() function), not too sure how bulletproof this logic is,
#' # # Likely buggy and/or unneccassary
#' # reverse_graph <- function(df) {
#' #   
#' #   # data to reattach after reversal
#' #   keeps <- dplyr::select(df, -tocomid, -fromnode, -tonode)
#' #   
#' #   # preserve orgiinal names
#' #   orig_names <- names(df)
#' #   
#' #   # find most upstream node by hydroseq
#' #   most_upstream_node <- 
#' #     df %>% 
#' #     dplyr::filter(hydroseq == max(df$hydroseq)) %>% 
#' #     .$tonode
#' #   
#' #   # drop geometries
#' #   df <- 
#' #     df %>%
#' #     sf::st_drop_geometry() %>% 
#' #     dplyr::select(comid, tocomid, fromnode, tonode)
#' #   
#' #   # Create new columns for reversed node IDs and edge IDs
#' #   df$reversed_fromnode <- NA
#' #   df$reversed_tonode <- NA
#' #   df$reversed_comid <- NA
#' #   df$reversed_tocomid <- NA
#' #   
#' #   # Step 3: Reverse the direction of nodes
#' #   for (i in 1:nrow(df)) {
#' #     # i = 1
#' #     if (df[i, ]$fromnode  == 0) {
#' #       
#' #       df[i, ]$reversed_fromnode <- 0
#' #       df[i, ]$reversed_tonode <- 0
#' #       
#' #     } else {
#' #       new_comid    <- df[i, ]$comid
#' #       new_tocomid  <- df[i, ]$tocomid
#' #       
#' #       new_from     <- df[i, ]$fromnode
#' #       new_to       <- df[i, ]$tonode
#' #       
#' #       
#' #       df[i, ]$fromnode       <- new_to
#' #       df[i, ]$tonode         <- new_from   
#' #       
#' #       df[i, ]$comid          <- new_tocomid
#' #       df[i, ]$tocomid        <- new_comid   
#' #       
#' #       
#' #       df[i, ]$reversed_fromnode  <- new_from
#' #       df[i, ]$reversed_tonode    <- new_to
#' #       df[i, ]$reversed_comid     <- new_comid
#' #       df[i, ]$reversed_tocomid   <- new_tocomid
#' #       
#' #     }
#' #   }
#' #   
#' #   # set tonode of most upstream node to 0
#' #   df[df$reversed_fromnode == most_upstream_node, ]$tonode <- 0
#' #   df[df$reversed_fromnode == most_upstream_node, ]$tocomid <- 0
#' #   
#' #   df[df$reversed_fromnode != most_upstream_node & df$comid == 0, ]$comid <- df[df$reversed_fromnode != most_upstream_node & 
#' #                                                                                  df$comid == 0, ]$tocomid
#' #   
#' #   # rejoin data with other original columns in keeps
#' #   df <-
#' #     df %>% 
#' #     dplyr::left_join(
#' #       keeps, 
#' #       by = "comid"
#' #     ) %>% 
#' #     dplyr::select( all_of(orig_names)) %>% 
#' #     sf::st_as_sf()
#' #   
#' #   return(df)
#' # }
#' 
#' 
#' #' Convert a Directed Acyclic Graph (DAG) into an Undirected Graph
#' #'
#' #' This function takes a Directed Acyclic Graph (DAG), typically the output of the `create_dag()` function,
#' #' and converts it into an undirected graph. The purpose of this conversion is to prepare the graph for
#' #' cycle and braid detection functions, as they usually require an undirected graph.
#' #'
#' #' @param graph A data frame representing the Directed Acyclic Graph (DAG) with columns 'fromnode' and 'tonode'.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return An undirected graph represented as a data frame with columns 'fromnode' and 'tonode'.
#' #' @importFrom dplyr select tibble bind_rows
#' #' @importFrom sf st_drop_geometry
#' #' @examples
#' #'  \dontrun{
#' #' # direccted graph without cycles
#' #' graph <- data.frame(
#' #'  fromnode = c(1, 2, 3, 3),
#' #'  tonode   = c(2, 3, 4, 5)
#' #'  )
#' #'  # make undirected graph
#' #'  undirected <- make_undirected(graph)
#' #'  # directed graph containing a cycle
#' #'  graph <- data.frame(
#' #'  fromnode = c(1, 2, 3, 3, 4),
#' #'  tonode   = c(2, 3, 4, 5, 5)
#' #'  )
#' #'  # make undirected graph
#' #'  undirected <- make_undirected(graph)
#' #'  }
#' make_undirected <- function(graph) {
#'   
#'   # get to and from nodes
#'   adj <- 
#'     graph %>% 
#'     # dplyr::select(comid, tocomid, fromnode, tonode) %>%
#'     dplyr::select(fromnode, tonode) %>%
#'     sf::st_drop_geometry() %>% 
#'     dplyr::tibble()
#'   
#'   # opposite topology from directed graph
#'   rev_edges <- dplyr::select(
#'     adj, 
#'     fromnode = tonode,
#'     tonode   = fromnode
#'     # comid = tocomid,
#'     # tocomid = comid,
#'   )
#'   
#'   # add back edges
#'   adj <- dplyr::bind_rows(adj, rev_edges)
#'   
#'   return(adj)
#'   
#' }
#' 
#' #' Make a topology fastmap object from lists of to and from nodes representing a network
#' #' 
#' #' @param from_nodes numeric or character vector containing from node topology 
#' #' @param to_nodes numeric or character vector containing to node topology 
#' #' @param directed logical whether edge strings should be directed connection string ('->') or undirected connection string ('-'). If TRUE, edge strings will seperate nodes with a directed connection string ('->'). Default is TRUE.
#' #' @param ... Additional vectors (optional). Additional vectors should be mapped to the from node keys in the hashmap, must be the same length as from_nodes and to_nodes vectors.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return fastmap with from_nodes as keys and to_nodes as values in the hashmap
#' #' @importFrom fastmap fastmap
#' make_topo_map <- function(
#'     from_nodes,
#'     to_nodes,
#'     directed = TRUE,
#'     ...
#' ) {
#'   
#'   # Convert input arguments to character vectors
#'   from_nodes <- as.character(from_nodes)
#'   to_nodes <- as.character(to_nodes)
#'   
#'   # Create a list of additional arguments
#'   add_args <- list(...)
#'   
#'   # # initialize hashmap
#'   topo_map <- fastmap::fastmap()
#'   
#'   for (i in 1:length(from_nodes)) {
#'     #   # message("iteration ", i, "/", length(from_nodes))
#'     
#'     from_node <- from_nodes[i]
#'     to_node   <- to_nodes[i]
#'     
#'     # check if from_node has been added to hashmap already
#'     if (!topo_map$has(from_node)) {
#'       
#'       # # edge string
#'       # edge_str <- paste0(from_node, "->", to_node)
#'       
#'       # edge string
#'       if(directed) {
#'         edge_str <- paste0(from_node, "->", to_node)
#'       } else {
#'         edge_str <- paste0(from_node, "-", to_node)
#'       }
#'       
#'       map_entry <- list(
#'         to_node     = to_node,
#'         edge        = edge_str,
#'         count       = 1
#'       )
#'       
#'       if(length(add_args) > 0) {
#'         for (n in 1:length(names(add_args))) {
#'           key = names(add_args)[n]
#'           map_entry[[key]] <-add_args[[key]][i]
#'         }
#'       }
#'       
#'       # set new entry in topology map
#'       topo_map$set(from_node, map_entry)
#'       
#'     } else {
#'       
#'       # # edge string
#'       # edge_str <- paste0(from_node, "->", to_node)
#'       
#'       # edge string
#'       if(directed) {
#'         edge_str <- paste0(from_node, "->", to_node)
#'       } else {
#'         edge_str <- paste0(from_node, "-", to_node)
#'       }
#'       
#'       map_entry <- list(
#'         to_node     = c(topo_map$get(from_node)$to_node, to_node),
#'         edge        = c(topo_map$get(from_node)$edge, edge_str),
#'         count       = c(topo_map$get(from_node)$count + 1)
#'       )
#'       
#'       if(length(add_args) > 0) {
#'         
#'         for (n in 1:length(names(add_args))) {
#'           key = names(add_args)[n]
#'           map_entry[[key]] <- c(topo_map$get(from_node)[[key]], add_args[[key]][i])
#'         }
#'         
#'       }
#'       
#'       #  Update the existing entry in topo_map
#'       topo_map$set(from_node, map_entry)
#'     }
#'   }
#'   
#'   # return(topo_map)
#'   return(topo_map)
#' }
#' 
#' # network = network3
#' # # lower case names
#' # names(network) <- tolower(names(network))
#' # 
#' # # turn network into a directed graph
#' # dag <- create_dag(network)
#' # dag$comid
#' # dag2 <- sf::st_drop_geometry(dag)
#' # dag2 <- renode_circuits(dag2)
#' # 
#' # # make an undirected graph
#' # dag2 <- make_undirected(dag2)
#' # # dag$fromnode
#' # tm <- added_args(
#' #   from_nodes = dag2$fromnode, 
#' #   to_nodes   = dag2$tonode
#' #                  # divergence = dag$divergence,
#' #                  # streamcalc = dag$streamcalc,
#' #                  # so = dag$streamorde
#' #                  )
#' # 
#' # 
#' # tm2 <- make_topo_map(
#' #   from_nodes = dag$fromnode, 
#' #   to_nodes   = dag$tonode,
#' #   comid = dag$comid
#' #                  # divergence = dag$divergence,
#' #                  # streamcalc = dag$streamcalc,
#' #                  # so = dag$streamorde
#' # )
#' # plot(dag2$geometry)
#' # tmlist <- tm$as_list()
#' # tmlist2
#' # tmlist2 <- tm2$as_list()
#' # for (o in 1:length(tmlist2)) {
#' #   message("NEW FROM NODE = ", names(tmlist[o]))
#' #   message("ORIG FROM NODE = ", names(tmlist2[o]))
#' #   
#' #   message("names: ", names(tmlist[o]) == names(tmlist2[o]))
#' #   message("tonodes: ", paste0(c(tmlist[[o]]$to_node == tmlist2[[o]]$to_node), sep = ' - '))
#' #   message("edges: ", paste0(c(tmlist[[o]]$edge == tmlist2[[o]]$edge), sep = ' - '))
#' #   message("Count: ", paste0(c(tmlist[[o]]$count == tmlist2[[o]]$count), sep = ' - '))
#' #   message('=================')
#' #   
#' # }
#' 
#' 
#' # make_topo_map2 <- function(
#' #     from_nodes,
#' #     to_nodes,
#' #     from_ids = NULL,
#' #     to_ids   = NULL,
#' #     streamcalc = NULL,
#' #     streamorder = NULL,
#' #     divergence = NULL
#' # ) {
#' #   
#' #   
#' #   # make sure all inputs are characters for hashmap
#' #   from_nodes     <- as.character(from_nodes)
#' #   to_nodes       <- as.character(to_nodes)
#' #   from_ids       <- as.character(from_ids)
#' #   to_ids         <- as.character(to_ids)
#' #   streamcalc     <- as.character(streamcalc)
#' #   streamorder    <- as.character(streamorder)
#' #   divergence     <- as.character(divergence)
#' #   
#' #   # initialize hashmap
#' #   topo_map <- fastmap::fastmap()
#' #   
#' #   for (i in 1:length(from_nodes)) {
#' #     
#' #     # message("iteration ", i, "/", length(from_nodes))
#' #     
#' #     from_node <- from_nodes[i]
#' #     to_node   <- to_nodes[i]
#' #     from_id   <- from_ids[i]
#' #     to_id     <- to_ids[i]
#' #     sc        <- streamcalc[i]
#' #     so        <- streamorder[i]
#' #     div       <- divergence[i]
#' #     # uid       <- id[i]
#' #     
#' #     if (!topo_map$has(from_node)) {
#' #       # message(from_node, " NOT YET in map")
#' #       # message("Adding ", from_node, " to topo_map with value ", to_node)
#' #       
#' #       # edge string
#' #       edge_str <- paste0(from_node, "->", to_node)
#' #       
#' #       # Create a new entry in topo_map
#' #       if (!is.null(from_ids) & !is.null(to_ids)) {
#' #         # if (!is.null(id)) {
#' #         topo_map$set(from_node, list(
#' #           to_node     = to_node,
#' #           # edge       = edge_str,
#' #           # id     = uid
#' #           to_id       = to_id,
#' #           from_id     = from_id,
#' #           edge        = edge_str,
#' #           streamcalc  = sc,
#' #           streamorder = so,
#' #           div         = div,
#' #           count       = 1
#' #         )
#' #         )
#' #       } else {
#' #         topo_map$set(from_node, list(
#' #           to_node     = to_node,
#' #           edge        = edge_str,
#' #           streamcalc  = sc,
#' #           streamorder = so,
#' #           div         = div,
#' #           count       = 1
#' #         )
#' #         )
#' #       }
#' #       
#' #     } else {
#' #       # message("*****")
#' #       # message(from_node, " ALREADY IN MAP --> UPDATING")
#' #       # # message("UPDATING ", from_node," value from ", 
#' #       # #         topo_map$get(from_node), "to\n", 
#' #       # #         c(topo_map$get(from_node), to_node))
#' #       # message("*****")
#' #       
#' #       # new edge str to add to map
#' #       edge_str = paste0(from_node, "->", to_node)
#' #       
#' #       # Update the existing entry in topo_map
#' #       if (!is.null(from_ids) & !is.null(to_ids)) {
#' #         topo_map$set(from_node, list(
#' #           to_node     = c(topo_map$get(from_node)$to_node, to_node),
#' #           # edge       = c(topo_map$get(from_node)$edge, edge_str),
#' #           to_id       = c(topo_map$get(from_node)$to_id, to_id),
#' #           from_id     = c(topo_map$get(from_node)$from_id),
#' #           edge        = c(topo_map$get(from_node)$edge, edge_str),
#' #           streamcalc  = c(topo_map$get(from_node)$streamcalc, sc),
#' #           streamorder = c(topo_map$get(from_node)$streamorder, so),
#' #           div         = c(topo_map$get(from_node)$div, div),
#' #           count       = c(topo_map$get(from_node)$count + 1)
#' #         ))
#' #         
#' #       } else {
#' #         topo_map$set(from_node, list(
#' #           to_node     = c(topo_map$get(from_node)$to_node, to_node),
#' #           edge        = c(topo_map$get(from_node)$edge, edge_str),
#' #           streamcalc  = c(topo_map$get(from_node)$streamcalc, sc),
#' #           streamorder = c(topo_map$get(from_node)$streamorder, so),
#' #           div         = c(topo_map$get(from_node)$div, div),
#' #           count       = c(topo_map$get(from_node)$count + 1)
#' #         ))
#' #       }
#' #     }
#' #     # message("=============================")
#' #   }
#' #   
#' #   return(topo_map)
#' #   
#' # }
#' 
#' 
#' #' Convert a COMID to its corresponding fromnode in a Network
#' #'
#' #' This function takes a Network of NHDPlus flowlines represented as a data frame with columns 'comid',
#' #' 'tocomid', 'fromnode', and 'tonode'. Given a COMID character string, it returns the corresponding fromnode
#' #' in the network. This function is useful for identifying the starting node when performing cycle detection or braid detection.
#' #'
#' #' @param network A data frame representing the network of NHDPlus flowlines with columns 'comid', 'tocomid', 'fromnode', and 'tonode'.
#' #' @param comid The COMID character string to find its corresponding fromnode in the network. If NULL, the function selects a suitable fromnode.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return The fromnode corresponding to the given COMID in the network.
#' #' @examples
#' #'  \dontrun{
#' #' network <- data.frame(
#' #'   comid = c("A", "B", "C", "D"),
#' #'   tocomid = c("B", "C", "D", "E"),
#' #'   fromnode = c("X", "Y", "Z", "W"),
#' #'   tonode = c("Y", "Z", "W", "X")
#' #' )
#' #' fromnode <- comid_to_node(network, comid = "B")
#' #' }
#' #' @seealso \code{\link{make_undirected}}, \code{\link{find_cycles}}
#' comid_to_node <- function(
#'     network, 
#'     comid   = NULL
#' ) {
#'   
#'   # if a start COMID is given
#'   if(!is.null(comid)) {
#'     
#'     # if start COMID is NOT found in the network, throw error
#'     if(!comid %in% network$comid) { stop(comid, " COMID not found in network")}
#'     
#'     # if(verbose) {
#'     #   message("Starting braid detection at COMID: ", comid)
#'     # }
#'     
#'     # find fromnode of comid to start cycle detection at
#'     start_node <- as.character(network$fromnode[network$comid == comid])
#'     
#'   } else {
#'     
#'     # select the from node that has a tonode that is NOT in fromnodes
#'     start_node <- as.character(
#'       network$fromnode[!network$tonode %in% network$fromnode]
#'     )
#'     
#'     # # if no return, just use the first fromnode
#'     # if (length(start_node) == 0) {
#'     #   start_node <- as.character(graph$fromnode[1])
#'     # }
#'     # if ("0" %in% network$tocomid) {
#'     #   # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
#'     #   start_node <- as.character(network$fromnode[network$tocomid == "0"])[1]
#'     # } else {
#'     #   start_node <- as.character(network$fromnode)[1]
#'     # }
#'   }
#'   
#'   # if no return, just use the first fromnode
#'   if (length(start_node) == 0) {
#'     start_node <- as.character(network$fromnode[1])
#'   }
#'   
#'   # In case there is more than one start_node, select the FIRST element in start_node
#'   start_node <- start_node[1]
#'   
#'   return(start_node)
#'   
#' }
#' 
#' #' Artificially create cycles from portions of a network/graph that contain a circuit
#' #' (2 nodes and 2 unique edges between those nodes).
#' #'
#' #' This function is necessary because circuits do NOT get recognized by cycle detection
#' #' DFS implementation. By applying this function, we can artificially create cycles
#' #' from circuits that SHOULD represent a braided section of a river.
#' #'
#' #' @param graph A data frame representing the network/graph with columns 'fromnode', 'tonode',
#' #'              'comid', and 'tocomid'.
#' #' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return A modified data frame representing the network/graph with artificial cycles created from circuits.
#' #' @importFrom dplyr group_by add_count filter ungroup slice select bind_rows mutate
#' #' @examples
#' #'  \dontrun{
#' #' graph <- data.frame(
#' #'   fromnode = c(1, 2, 2, 3),
#' #'   tonode = c(2, 3, 3, 4),
#' #'   comid = c("A", "B", "C", "D"),
#' #'   tocomid = c("B", "C", "D", "E")
#' #' )
#' #' renode_circuits(graph)
#' #' }
#' renode_circuits <- function(graph, verbose = FALSE) {
#'   
#'   # unique nodes in graph
#'   unodes <- unique(c(graph$fromnode, graph$tonode)) 
#'   
#'   # unodes <- unique(c(unique(graph$fromnode), unique(graph$tonode)))
#'   single_cycles <-
#'     # network %>%
#'     graph %>%
#'     dplyr::group_by(fromnode, tonode) %>% 
#'     dplyr::add_count() %>% 
#'     dplyr::filter(n > 1) %>% 
#'     dplyr::ungroup()
#'   
#'   if(nrow(single_cycles) == 0) {
#'     if(verbose) { message("No circuits found") }
#'     return(graph)
#'     
#'   }
#'   
#'   # get one of the comids for single_cycles and reworked node topology
#'   renodes <- 
#'     single_cycles %>%
#'     dplyr::group_by(fromnode, tonode) %>% 
#'     dplyr::slice(1) %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::select(comid, tocomid, fromnode, tonode)
#'   
#'   # get the number of new nodes needed that don't overlap with current unique nodes in graph
#'   new_nodes <- seq(max(unodes)+1,  max(unodes) + nrow(renodes))
#'   
#'   # Inject a new fake node between each unique edge
#'   # to artificially create a cycle that can be detected in DFS function
#'   renodes <- dplyr::bind_rows(
#'     data.frame(
#'       comid = as.character(renodes$comid),
#'       # tocomid = tmp$comid,
#'       tocomid = paste0(renodes$comid, "_v2"),
#'       fromnode = renodes$fromnode,
#'       tonode = new_nodes
#'     ),
#'     data.frame(
#'       # comid = tmp$comid,
#'       comid = paste0(renodes$comid, "_v2"),
#'       tocomid = as.character(renodes$tocomid),
#'       fromnode = new_nodes,
#'       tonode = renodes$tonode
#'     ),
#'     dplyr::mutate(
#'       dplyr::select(
#'         dplyr::filter(
#'           single_cycles, !comid %in% renodes$comid
#'         ),
#'         comid, tocomid, fromnode, tonode),
#'       comid   = as.character(comid),
#'       tocomid = as.character(tocomid)
#'     )
#'   )
#'   
#'   # remove original versions of single cycle nodes and replace with reworked nodes
#'   regraph <- 
#'     dplyr::bind_rows(
#'       renodes, 
#'       dplyr::mutate(
#'         dplyr::filter(
#'           dplyr::select(graph, 
#'                         comid, tocomid, fromnode, tonode),
#'           !fromnode %in% single_cycles$fromnode
#'         ),
#'         comid   = as.character(comid),
#'         tocomid = as.character(tocomid)
#'       )
#'     )
#'   
#'   return(regraph)
#' }
#' 
#' #' Convert edge string seperating nodes by a "->" to just a "to_node" value
#' #'
#' #' @param edge character
#' #' @param to logical whether to return the tonode (TRUE) or fromnode(FALSE)
#' #' @param directed logical, whether the edge string is directed/TRUE ("->") or undirected/FALSE ("-")
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return character of "to" or "from" node
#' edge_to_node <- function(edge, 
#'                          to       = TRUE, 
#'                          directed = TRUE
#'                          ) {
#' 
#'   # set connection string if directed or undirected
#'   if(directed) {
#'     connect_str <- "->"
#'   } else {
#'     connect_str <- "-"
#'   }
#'   
#'   # if to node should be returned
#'   if(to) {
#'     # edge string
#'     edge_str <- sub(paste0(".*", connect_str), "", edge)
#'     # return(sub(".*->", "", edge))
#'     
#'   # if from node should be returned
#'   } else {
#'     # edge string
#'     edge_str <- sub(paste0(connect_str, ".*"), "", edge)
#'     # return(sub("->.*", "", edge))
#'   }
#'   
#'   return(edge_str)
#'   
#'   }
#' 
#' #' Find cycles in a graph/network starting from a specified node or the first available node
#' #'
#' #' This function performs a depth-first search (DFS) on a graph/network to find cycles. It starts
#' #' the search from a specified node or the first available node. If a start node is provided, it
#' #' checks if the node exists in the graph and throws an error if not found. The function returns
#' #' the cycles found as a list or a data frame with node and cycle IDs.
#' #' @param graph A data frame representing the graph/network with columns 'fromnode', 'tonode',
#' #'              'comid', and 'tocomid'. Graph must be converted to an undirected graph via make_undirected() 
#' #' @param start The starting node for the cycle detection. Default is NULL.
#' #' @param return_as The format to return the cycles. Possible values are "list" (default) or "dataframe".
#' #' @param edge logicl, whether to return edges (node pairings) or nodes. If TRUE, 'node-node' character string will be returned, otherwise a starting node is returned. Default is FALSE. 
#' #' @param wide logical, whether to return wide or long dataframe. Default is TRUE 
#' #' @param verbose Logical indicating whether to print verbose messages during the process. Default is FALSE.
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return A list of cycles found in the graph/network or a data frame with node and cycle IDs.
#' #' @importFrom dplyr relocate mutate bind_rows group_by summarise ungroup
#' #' @importFrom fastmap fastmap
#' #' @importFrom vctrs vec_c
#' #' @importFrom stats setNames
#' #' @examples
#' #'  \dontrun{
#' #' # directed graph with a directed cycle 3-4-5-3
#' #' graph <- data.frame(
#' #'   fromnode = c(1, 2, 3, 4, 5),
#' #'   tonode = c(2, 3, 4, 5, 3),
#' #'   comid = c("A", "B", "C", "D", "E"),
#' #'   tocomid = c("B", "C", "D", "E", "C")
#' #'   )
#' #'   
#' #' # start at node 1
#' #' find_cycles(graph, start   = 1, verbose = T)
#' #' 
#' #' # example directed graph with cycle 3-4-5-3 that needs to 
#' #' # be coerced into an undirected graph to detect cycles (via make_undirected() function)
#' #' graph = data.frame(
#' #'   fromnode = c(1, 2, 3, 3, 4),
#' #'   tonode   = c(2, 3, 4, 5, 5),
#' #'   comid    = c("A", "B", "C", "C", "D"),
#' #'   tocomid  = c("B", "C", "D", "E", "E")
#' #'   )
#' #'   
#' #' # make directed graph an undirected graph 
#' #' graph <- make_undirected(graph)
#' #' 
#' #' # start at node 1
#' #' find_cycles(graph, start = 1, verbose = T)
#' #' 
#' #' # no specified start node
#' #' find_cycles(graph, start = NULL, verbose = T)
#' #' }
#' find_cycles <- function(
#'     graph,
#'     start     = NULL,
#'     return_as = "list",
#'     edge      = FALSE,
#'     wide      = TRUE,
#'     verbose   = FALSE
#' ) {
#'   
#'   # graph     = undir
#'   # start     = start_node
#'   # return_as = "list"
#'   # # return_as = "dataframe",
#'   # # return_as = return_as,
#'   # verbose   = verbose
#'   # edge = TRUE
#'   
#'   # if a start COMID/Node is given
#'   if(!is.null(start)) {
#'     
#'     # if start COMID is NOT found in the network, throw error
#'     if(!start %in% graph$fromnode) { stop(start, " node not found in graph")}
#'     
#'     # if(verbose) { message("Starting braid detection at COMID: ", start)  }
#'     
#'     # find fromnode of comid to start cycle detection at
#'     # start_node <- as.character(graph$fromnode[graph$comid == start])
#'     start_node <- as.character(start)
#'     
#'   } else {
#'     # If no "start" is given, pick a node to start at
#'     if ("0" %in% graph$tonode) {
#'       
#'       # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
#'       start_node <- as.character(graph$fromnode[graph$tonode == "0"])[1]
#'       
#'       # if(verbose) {message("Starting braid detection at COMID: ", graph$comid[graph$tocomid == "0"][1])}
#'       
#'     } else {
#'       
#'       # if(verbose) { message("No 'tocomid' value equal to '0' found", 
#'       # "\nStarting braid detection at COMID: ", graph$comid[graph$fromnode == as.character(graph$fromnode)[1]] ) }
#'       start_node <- as.character(graph$fromnode)[1]
#'     }
#'   }
#'   
#'   # if(verbose) {
#'   #   message("Starting braid detection at node: ", start_node)
#'   # }
#'   
#'   # make a topology hashmap to use in DFS 
#'   topo_map <- make_topo_map(
#'     from_nodes = graph$fromnode,
#'     to_nodes   = graph$tonode,
#'     directed   = FALSE
#'     # from_nodes = graph$tonode,
#'     # to_nodes   = graph$fromnode
#'   )
#'   
#'   # topo_map$as_list()
#'   
#'   # keep track of visited nodes
#'   visits <- fastmap::fastmap()
#'   
#'   # # set all marked values to FALSE
#'   visits$mset(.list = stats::setNames(
#'     lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
#'            function(i){ 0 }),
#'     unique(c(graph$fromnode, graph$tonode))
#'     # lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), function(i){ 0 }),
#'     # unique(c(ungraph$fromnode, ungraph$tonode))
#'   )
#'   )
#'   # visits$as_list()
#'   # keep track of cycles nodes
#'   cycles <- fastmap::fastmap()
#'   
#'   # # # set all marked values to FALSE
#'   # cycles$mset(.list = stats::setNames(lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), function(i){ 0 }),
#'   #                         unique(c(ungraph$fromnode, ungraph$tonode))))
#'   
#'   # keep track of visited nodes
#'   prev_nodes <- fastmap::fastmap()
#'   
#'   # # set all marked values to FALSE
#'   prev_nodes$mset(.list = stats::setNames(
#'                               lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
#'                                      function(i){ 0 }),
#'                               unique(c(graph$fromnode, graph$tonode))
#'                               # lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), 
#'                               # function(i){ 0 }),
#'                               # unique(c(ungraph$fromnode, ungraph$tonode))
#'                                 )
#'                               )
#'   
#'   # cycle number count
#'   count <- 1
#'   
#'   # Depth first search function for marking cycles in undirected graph
#'   dfs <- function(n, p) {
#'     # message("node: ", n)
#'     # message("node ", n, " visit value: ", visits$get(n))
#'     
#'     # node has been fully visited
#'     if (visits$get(n) == 2) {
#'       return()
#'     }
#'     
#'     # node is in process of visitation
#'     if (visits$get(n) == 1) {
#'       
#'       # # map/list to store current level nodes
#'       v <- vctrs::vec_c()
#'       # v    <- list()
#'       
#'       curr <- p
#'       # v <- vctrs::vec_c(v, vctrs::vec_c(curr))
#'       
#'       # keep track of fromnodes
#'       if(!edge) {
#'         
#'         # fromnodes vector
#'         v <- vctrs::vec_c(v, vctrs::vec_c(curr))
#'         
#'       } else {
#'         # keep track of edges
#'         v <- vctrs::vec_c(v,
#'                           vctrs::vec_c(
#'                             topo_map$get(curr)$edge
#'                           )
#'         )
#'       }
#'       # v <- vctrs::vec_c(v,vctrs::vec_c( topo_map$get(curr)$edge ) )
#'       # v    <- list(v, list(curr))
#'       
#'       # while parent of n is not equal to itself
#'       while (curr != n) {
#'         
#'         # update curr value to previous values of curr
#'         
#'         # make edge string
#'         if(edge) {
#'           # v <- vctrs::vec_c(v, vctrs::vec_c(curr))
#'           edges <- paste0(prev_nodes$get(curr), "-", curr)
#'         } 
#'         # edges <- topo_map$get(curr)$edge
#'         # edges <- paste0(prev_nodes$get(curr), "-", curr)
#'         
#'         curr  <- prev_nodes$get(curr)
#'         
#'         # prev_nodes$as_list()
#'         # edges <- prev_nodes$get(curr)$edge
#'         
#'         # add cycle vertices to v
#'         # keep track of fromnodes
#'         if(!edge) {
#'           
#'           # fromnodes vector
#'           v <- vctrs::vec_c(v, vctrs::vec_c(curr))
#'           
#'         } else {
#'           # keep track of edges
#'           v <- vctrs::vec_c(v, vctrs::vec_c(edges))
#'           # v <- vctrs::vec_c(v, vctrs::vec_c(curr)
#'         }
#'         # # v <- vctrs::vec_c(v, vctrs::vec_c(curr))
#'         # v <- vctrs::vec_c(v,
#'         #                   # vctrs::vec_c(curr)
#'         #                   vctrs::vec_c(edges)
#'         #                   # vctrs::vec_c(topo_map$get(curr))
#'         #                   )
#'         # v = list(v, list(curr))
#'       }
#'       
#'       # add vertices for current cycle number to cycles hashmap 
#'       cycles$set(as.character(count), v)
#'       
#'       # incremenet count
#'       count <<-  count + 1
#'       
#'       return()
#'       
#'     }
#'     
#'     # set parents of node n 
#'     prev_nodes$set(n, p)
#'     
#'     # mark node as partially visited
#'     visits$set(n, 1)
#'     
#'     # get neighbors of node n
#'     neighbors <- topo_map$get(n)$to_node
#'     # neighbors <- edge_to_node(topo_map$get(n)$edge)
#'     
#'     # message("Neighbors of node: ",  n, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
#'     
#'     # iterate through neighbors of node n
#'     for (vert in neighbors) {
#'       
#'       if(vert == prev_nodes$get(n)) {
#'         # message("vert ", vert, " equals parent ", prev_nodes$get(n), " SKIPPING")
#'         next
#'       }
#'       
#'       # message("---- RUNNING DFS ON ----")
#'       # message("---> vert = ", vert)
#'       # message("---> n = ", n)
#'       
#'       dfs(vert, n)
#'       
#'       # message("***********")
#'     }
#'     # mark node as fully visited
#'     visits$set(n, 2)
#'     
#'     # message("=============================")
#'   }
#'   
#'   
#'   # Start DFS on a node
#'   dfs(start_node, "no_previous")
#'   
#'   # if(cycles$size() == 0 & is.null(single_braids)) {
#'   if(cycles$size() == 0) {
#'     
#'     if(verbose) {
#'       message("No cycles found, returning NULL")
#'     }
#'     
#'     return(NULL)
#'   }
#'   
#'   # extract cycles hashmap as a list
#'   result <- cycles$as_list()
#'   
#'   # if data should be returned as a 2 column dataframe with node and cycle_id
#'   if(return_as == "dataframe") {
#'     
#'     if(edge) {
#'       
#'       result <- lapply(1:length(result), function(k) {
#'           # get IDs of each node
#'             dplyr::relocate(
#'               dplyr::mutate(
#'                 data.frame(
#'                   edge     = result[[k]],
#'                   cycle_id = names(result[k])
#'                   # node     = cycles$as_list()[[k]],
#'                   # cycle_id = names(cycles$as_list()[k])
#'                   ),
#'                 fromnode = edge_to_node(edge, to = FALSE,  directed = FALSE),
#'                 tonode   = edge_to_node(edge, to = TRUE,  directed = FALSE)
#'                 ),
#'               edge, fromnode, tonode, cycle_id
#'               )
#'             })  %>% 
#'           dplyr::bind_rows() 
#'       
#'       if(wide) {
#'         
#'         result <- 
#'           result %>%
#'           dplyr::group_by(edge, fromnode, tonode) %>%
#'           # dplyr::group_by(edge) %>% 
#'           dplyr::summarise(
#'             cycle_id = paste0(cycle_id, collapse = ", ")
#'           ) %>%
#'           dplyr::ungroup()
#'       }
#'     } else {
#'       
#'       result <- lapply(1:length(result), function(k) {
#'         # get IDs of each node
#'         data.frame(
#'           node     = result[[k]],
#'           cycle_id = names(result[k])
#'           # node     = cycles$as_list()[[k]],
#'           # cycle_id = names(cycles$as_list()[k])
#'         )
#'       })  %>% 
#'         dplyr::bind_rows() 
#'       
#'       if(wide) {
#'         
#'         result <- 
#'           result %>%
#'           dplyr::group_by(node) %>%
#'           dplyr::summarise(
#'             cycle_id = paste0(cycle_id, collapse = ", ")
#'           ) %>%
#'           dplyr::ungroup()
#'       }
#'     }
#'   }
#'   
#'   return(result)
#'   
#' }
#' 
#' ### Example DAG network with fromnode and tonode topologies
#' # network <- data.frame(
#' #   comid = c("A", "B", "B", "C", "C", "D", "E", "F", "F", "F"),
#' #   fromnode = c("X", "Y", "Y", "Z", "Z", "W", "X", "Y", "Y", "Y"),
#' #   tonode = c("Y", "Y", "Z", "Z", "W", "X", "Y", "Z", "Z", "W")
#' # )
#' # single_cycles <- get_single_cycles(network)
#' # 
#' # network <- data.frame(
#' #   comid = c(100, 101, 102, 103),
#' #   fromnode = c(1, 2, 3, 5),
#' #   tonode = c(2, 3, 4, 3)
#' #   # comid = c("A", "B", "C", "D"),
#' #   # tocomid = c("B", "C", "D", "C")
#' #   # tocomid = c( 101, 102, 103, 104)
#' #   )
#' # create_dag(network)
#' 
#' 
#' #' Identify the indices of big multibraids 
#' #'
#' #' @param x list of vectors with each list element containing a vector COMIDs representing a braid
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return numeric indices of the multibraids
#' id_multibraids <- function(x) {
#'   
#'   # overlapped braids
#'   overlaps <- find_overlaps(
#'     lst            = x,
#'     no_overlap_val = FALSE, 
#'     rm_no_overlap  = FALSE, 
#'     verbose        = FALSE
#'   ) 
#'   
#'   big_cycles <- sort(
#'     unique(
#'       Reduce( 
#'         c, overlaps )
#'     )
#'   )
#'   
#'   # remove 0 values
#'   big_cycles <- big_cycles[big_cycles != 0]
#'   
#'   return(big_cycles)
#'   
#' }
#' 
#' #' Utility function that takes the output from 'find_braids(nested = TRUE)' and unpacks/unnests braid_id column
#' #' 
#' #' Unnests the comma seperated braid_id column into individual rows for each braid ID/comid pairing. 
#' #' This function will unnest the nested braid_ids column that is the output of calling 'find_braids()' with 'nested = TRUE', essentially giving the output of running 'nested = FALSE'.
#' #' 
#' #' @param braids dataframe or sf dataframe containing a "comid" and "braid_id" column
#' #' @param into_list logical, if TRUE, unpacked braid_ids and corresponding comids are returned as a list. Default is FALSE
#' #'
#' #' @return dataframe or list
#' #' @importFrom dplyr mutate rename relocate
#' #' @importFrom tidyr unnest
#' #' @examples
#' #'  \dontrun{
#' #' # get a NHDPlus flowlines network
#' #' net <- nhdplusTools::navigate_network(start = 101, mode = "UT", distance_km = 100)
#' #' 
#' #' # Drop some columns for clarity
#' #' net <- dplyr::select(net, comid, divergence, fromnode, tonode)
#' #' 
#' #' # locate braids in the network and specify nested braid_id return
#' #' braids <- find_braids(net, add = TRUE, nested = TRUE)
#' #' 
#' #' # unnest the nested braid_id column (explode the list column into individual rows)
#' #' unpacked <- unnpack_braids(braids)
#' #' }
#' #' @export
#' unnpack_braids <- function(braids, into_list = FALSE) {
#'   
#'   if(!"braid_id" %in% tolower(names(braids))) {
#'     stop("'braids' input does not contain 'braid_id' column ")
#'   }
#'   
#'   unnested_braids <- 
#'     braids %>% 
#'     # dplyr::mutate(split_braid_ids = stringr::str_split(braid_id, ", ")) %>%
#'     dplyr::mutate(split_braid_ids = strsplit(braid_id, ", ")) %>%
#'     tidyr::unnest(cols = c(split_braid_ids)) %>% 
#'     dplyr::rename(
#'       braid_members = braid_id, 
#'       braid_id      = split_braid_ids
#'     ) %>% 
#'     dplyr::relocate(comid, braid_id, braid_members)
#'   
#'   if(into_list) {
#'     unnested_braids <- split(unnested_braids$comid, unnested_braids$braid_id) 
#'   }
#'   
#'   return(unnested_braids)
#'   
#' }
#' 
#' 
#' # #Locate single braided sections of a river network
#' # #DEPRECATED
#' # #This function locates single braided sections of a river network. These sections are defined as sections of rivers that start and end at the same node but have different COMIDs. These braids are not detected by the depth-first search function used in `find_braids()`, so this function provides an alternative method to identify and extract the braided segments of the river network.
#' # #@param network A data frame representing the river network with columns 'comid', 'fromnode', and 'tonode'.
#' # #@param return_as A character string indicating the desired format of the output. Possible values are "list" (default) and "data.frame". If "list", the function returns a list of cycles where each list element contains a vector of COMIDs. If "data.frame", the function returns a data frame with columns 'cycle_id' and 'comid', where 'cycle_id' represents the ID of the cycle and 'comid' is a comma-separated string of the associated COMIDs.
#' # #@noRd
#' # #@keywords internal
#' # #@return A list or data frame (depending on the value of 'return_as') containing the single braided sections of the river network. Each cycle is identified by a unique cycle ID, and the associated COMIDs are provided for each cycle.
#' # #@seealso \code{\link{find_braids}}, \code{\link{comid_to_node}}
#' # #@importFrom dplyr group_by add_count filter mutate cur_group_id relocate ungroup select summarise
#' # #@importFrom stats setNames
#' # get_single_cycles <- function(
#' #     network, 
#' #     return_as = "list"
#' # ) {
#' #   
#' #   single_cycles <-
#' #     network %>%
#' #     # dag %>% 
#' #     dplyr::group_by(fromnode, tonode) %>% 
#' #     dplyr::add_count() %>% 
#' #     dplyr::filter(n > 1) %>% 
#' #     dplyr::mutate(
#' #       cycle_id = dplyr::cur_group_id()
#' #       # braid_id = paste0("braid_", dplyr::cur_group_id())
#' #       # braid_id = dplyr::cur_group_id()
#' #     ) %>% 
#' #     dplyr::relocate(cycle_id) %>% 
#' #     dplyr::ungroup() %>% 
#' #     dplyr::select(comid, cycle_id) 
#' #   # dplyr::select(comid, node = fromnode, cycle_id) 
#' #   
#' #   # if no single cycles are found, return NULL
#' #   if(nrow(single_cycles) == 0) {
#' #     
#' #     return(NULL)
#' #     
#' #   }
#' #   
#' #   if(return_as == "list") {
#' #     
#' #     single_cycles <- 
#' #       single_cycles %>%  
#' #       dplyr::group_by(cycle_id) %>% 
#' #       dplyr::summarise(
#' #         comid = paste0(comid, collapse = ", ")
#' #         # node = paste0(fromnode, collapse = ", ")
#' #       ) %>% 
#' #       dplyr::ungroup()
#' #     
#' #     # create a list of cycles with each list element containing a vector of COMIDs
#' #     single_cycles <- stats::setNames(
#' #       lapply(strsplit(single_cycles$comid, ", "), as.integer),
#' #       single_cycles$cycle_id
#' #       # as.integer(strsplit(single_braids$comid, ", ")), 
#' #       # single_braids$braid_id
#' #     )
#' #   }
#' #   return(single_cycles)
#' # }
#' 
#' # # seperate multibraided braids in a network object
#' # # network, sf linestring object with comid column
#' # # x is a list of vectors with each list element containing a vector COMIDs representing a braid
#' # seperate_braids <- function(
#' #     network, 
#' #     x
#' #     ) {
#' #   
#' #   # overlapped braids
#' #   overlaps <- find_overlaps(
#' #     lst            = x,
#' #     no_overlap_val = FALSE,
#' #     rm_no_overlap  = FALSE,
#' #     verbose        = FALSE
#' #   )
#' #   
#' #   # multibraids ID index
#' #   multis <- id_multibraids(x)
#' #   
#' #   for(j in 1:length(multis)) {
#' #     # j = 1
#' #     # big cycle iteration which is a cycle that contains one or more OTHER cycles
#' #     cyc = multis[j]
#' #     
#' #     # comids for multibraid
#' #     coms <- x[[cyc]]
#' #     
#' #     # small braids that overlap with current big multicycle
#' #     connects <- sapply(1:length(overlaps), function(k) {
#' #       cyc %in% overlaps[[k]]
#' #     })
#' #     
#' #     # small braids
#' #     smalls <- x[connects]
#' #     
#' #     # multibraided linestrings
#' #     multibraid <- dplyr::filter(network, comid %in% coms)
#' #     # plot(multibraid$geometry)
#' #     
#' #     # suppressWarnings({
#' #     suppressMessages({
#' #       
#' #     # convex hull points
#' #     chull_pts <- 
#' #       multibraid %>% 
#' #       sf::st_combine() %>%
#' #       smoothr::densify(n = 5) %>% 
#' #       sf::st_cast("POINT") %>% 
#' #       sf::st_as_sf()
#' #     
#' #     # plot(multibraid$geometry)
#' #     # plot(chull_pts, add = T)
#' #     
#' #     # convex hull
#' #     chull <- 
#' #       chull_pts %>% 
#' #       concaveman::concaveman(concavity = 1, length_threshold = 0) %>% 
#' #       sf::st_zm()       
#' #     
#' #     
#' #     # bounds <-    
#' #     #   multibraid %>% 
#' #     #   sf::st_union() %>%
#' #     #   smoothr::densify(n = 5)  %>%
#' #     #   sf::st_boundary() 
#' #     #   # sf::st_cast("MULTILINESTRING")
#' #     # plot(bounds, add =F)
#' #     # all(sf::st_is_empty(bounds))
#' #     # plot(multibraid$geometry, add = F)
#' #     # plot(bounds, add = T)
#' #     # plot(chull_pts$x, add = T)
#' #     
#' #      # points that interesect with convex hull
#' #      pt_mask <-  c(sf::st_intersects(chull_pts, chull, sparse = FALSE))
#' #      
#' #      # remove touching points
#' #      no_msk <- chull_pts[!pt_mask, ]
#' #      
#' #     # plot(no_msk$x, add = F)
#' #     # plot(chull$polygons, add = T)
#' #     # plot(multibraid$geometry, add = T)
#' #      
#' #     # keep only linestrings that touch points outside of convex hull
#' #     # res <- sf::st_filter(multibraid, no_msk)
#' #     # plot(res$geometry)
#' #      
#' #     # filter multibraid to just the linestrings that contain the outer points
#' #     multibraid <- sf::st_filter(multibraid, no_msk, .predicate = st_contains)
#' #    
#' #     # comids for multibraid
#' #     x[[cyc]] <- multibraid$comid
#' #     
#' #     })
#' #     
#' #     # nears <- multibraid[lengths(sf::st_intersects(multibraid, no_msk)) > 1, ]
#' #     # plot(nears$geometry)
#' #     
#' #     # mapview::mapview(multibraid, color = "red") + 
#' #     #   mapview::mapview(no_msk, col.Regions = "green") 
#' #     # updated <- lapply(1:length(smalls), function(z) {
#' #     # for (z in 1:length(smalls)) {
#' #     #   small_braids <- dplyr::filter(multibraid, comid %in% smalls[[z]])
#' #     #   # near <- sf::st_nearest_feature(chull, small_braids)
#' #     #   x[[cyc]] <- c(x[[cyc]][!x[[cyc]] %in% smalls[[z]]], small_braids[near, ]$comid)
#' #     # }
#' #   }
#' # 
#' #   return(x)
#' # }
#' 
#' ###
#' ### Braid GIF generator function ###
#' ### TODO: Add this to output package? 
#' ###
#' 
#' # #Generate and save an animation highlighting all of the braid IDs in a network
#' # #@param network sf dataframe with a braid_id column
#' # #@param save_path character, path to save gif to
#' # #@param title character, title of gif plot
#' # #@param height numeric, height in inches
#' # #@param width numeric, width in inches
#' # #@param gif_width numeric, width of gif output
#' # #@param gif_height numeric, height of gif output
#' # #@param delay numeric, time delay between animation in seconds
#' # #@param legend_pos character, position of legend. Either "bottom", "right", "left", "top", or "none"
#' # #@param theme_obj ggplot2 theme object. Default is NULL
#' # #@param verbose logical, whether to print messages. Default is TRUE
#' # #
#' # #@noRd
#' # #@keywords internal
#' # #@return NULL
#' # make_braids_gif <- function(network,
#' #                             save_path  = NULL,
#' #                             title = "",
#' #                             height     = 8,
#' #                             width      = 10,
#' #                             gif_width  = 1800,
#' #                             gif_height = 1500,
#' #                             delay      = 0.8,
#' #                             legend_pos = "bottom",
#' #                             theme_obj  = NULL,
#' #                             verbose    = FALSE
#' # ) {
#' # 
#' #   # if no save path is given
#' #   if(is.null(save_path)) {
#' #     # save path is current working directory
#' #     save_path <- getwd()
#' #   }
#' # 
#' # 
#' #   ubraids <- unique(network$braid_id)
#' # 
#' #   # this will create a temp folder, but tempdir() won't let you name it:
#' #   temp_dir <- tempdir()
#' # 
#' #   # this will create a folder within our temp folder, with a name of our choice:
#' #   new_dir <- paste0(temp_dir, "/braid_plots/")
#' # 
#' #   # check if new temp dir alrteady exists
#' #   if(dir.exists(new_dir)) {
#' # 
#' #     warning(new_dir, " already exists")
#' # 
#' #   }
#' # 
#' #   # create temporary directory
#' #   dir.create(path = new_dir)
#' # 
#' #   if(verbose) { message("Plotting braids... ")}
#' # 
#' #   # make names lowercase
#' #   names(network) <- tolower(names(network))
#' # 
#' #   # unique braid_ids
#' #   ubraids <- unique(network$braid_id)
#' # 
#' #   ubraids <- ubraids[!grepl("no_braid", ubraids)]
#' #   
#' #   # set geometry name of network to "geometry"
#' #   network <- nhdplusTools::rename_geometry(network, "geometry")
#' #   
#' #   for (i in 1:length(ubraids)) {
#' # 
#' #     if(verbose) {
#' #       message(ubraids[i], " - (", i, "/", length(ubraids), ")")
#' #     }
#' # 
#' #     braid_plot <-
#' #       # b %>%
#' #       network %>%
#' #       # dplyr::select(comid, braid_id, geometry) %>%
#' #       ggplot2::ggplot() +
#' #       # ggplot2::geom_sf(ggplot2::aes(fill = braid_id)) +
#' #       ggplot2::geom_sf(ggplot2::aes(color = braid_id)) +
#' #       gghighlight::gghighlight(braid_id == ubraids[i]) +
#' #       ggplot2::labs(
#' #         title = title,
#' #         caption = paste0(i, " / ", length(ubraids)),
#' #         color = "",
#' #         # caption = paste0("(", i, " / ", length(ubraids), ")")
#' #       ) 
#' #     
#' #     if(is.null(theme_obj)) {
#' #       braid_plot <- 
#' #         braid_plot +
#' #         ggplot2::theme_bw() +
#' #         ggplot2::theme(
#' #           plot.caption = ggplot2::element_text(size = 12, face = "bold"),
#' #           legend.position = legend_pos
#' #         )
#' #     } else {
#' #       braid_plot <- 
#' #         braid_plot + 
#' #         theme_obj +
#' #         ggplot2::theme(
#' #           plot.caption = ggplot2::element_text(size = 12, face = "bold"),
#' #           legend.position = legend_pos
#' #         )
#' #     }
#' # 
#' #     temp_file <- tempfile(pattern =  paste0(ifelse(i < 10, paste0("0", i), i), "_braid"),
#' #                           tmpdir = new_dir,
#' #                           fileext = ".png"
#' #     )
#' # 
#' #     # Generate a temporary file path
#' #     ggplot2::ggsave(
#' #       filename = temp_file,
#' #       plot = braid_plot,
#' #       height = height,
#' #       width = width,
#' #       scale = 1
#' #     )
#' # 
#' # 
#' #   }
#' # 
#' #   # save_path <- "D:/gif/braid_gif.gif"
#' #   png_files <- sort(list.files(new_dir, full.names = TRUE))
#' # 
#' #   gifski::gifski(png_files,
#' #                  gif_file = save_path,
#' #                  width    = gif_width,
#' #                  height   = gif_height,
#' #                  delay    = delay
#' #   )
#' # 
#' #   if(verbose) {
#' #     message("Saving braid gif:\n --- ", save_path)
#' #   }
#' # 
#' #   # unlink deletes temporary directory holding PNGs
#' #   unlink(new_dir, recursive = TRUE)
#' # 
#' # }
#' 
#' # ### 
#' # ### DEPRECATED ###
#' # ### 
#' # 
#' # # Given a direccted graph from create_dag(), make an UNDIRECTED adjcacency matrix
#' # make_adj_matrix <- function(graph) {
#' #   # network <- network3
#' #   # graph <- create_dag(network)
#' #   # graph
#' #   
#' #   # get to and from nodes
#' #   adj <- 
#' #     graph %>% 
#' #     # dplyr::select(comid, tocomid, fromnode, tonode) %>% 
#' #     dplyr::select(fromnode, tonode) %>% 
#' #     sf::st_drop_geometry() %>% 
#' #     dplyr::tibble()
#' #   
#' #   # opposite topology from directed graph
#' #   rev_edges <- dplyr::select(
#' #     adj, 
#' #     fromnode = tonode,
#' #     tonode   = fromnode
#' #   )
#' #   
#' #   # add back edges
#' #   adj <- dplyr::bind_rows(adj, rev_edges)
#' #   
#' #   # get unique nodes to build adjacency matrix
#' #   nodes <- unique(c(adj$fromnode, adj$tonode))
#' #   
#' #   node_mapping <- setNames(seq_along(nodes), as.character(nodes))
#' #   
#' #   # make empty matrix
#' #   adj_mat <- matrix(
#' #     0, 
#' #     nrow = length(nodes),
#' #     ncol = length(nodes),
#' #     byrow = T
#' #   )
#' #   
#' #   # adj
#' #   # topo_map <- make_topo_map(from_nodes = adj$fromnode, to_nodes = adj$tonode)
#' #   # topo_map$as_list()
#' #   # 
#' #   # plot(graph$geometry)
#' #   for (i in 1:nrow(adj)) {
#' #     # i = 1
#' #     
#' #     # message("i: ", i)
#' #     # message("adj fromnode: ", adj[i, ]$fromnode)
#' #     # message("adj tonode: ", adj[i, ]$tonode)
#' #     
#' #     from_node <-  node_mapping[[as.character(adj[i, ]$fromnode)]]
#' #     to_node <-  node_mapping[[as.character(adj[i, ]$tonode)]]
#' #     
#' #     # from_node <- node_mapping[adj[i, ]$fromnode]
#' #     # to_node <- node_mapping[adj[i, "tonode"]]
#' #     
#' #     adj_mat[from_node,  to_node] <- 1
#' #     
#' #     # message("==================")
#' #   }
#' #   
#' #   return(adj_mat)
#' #   
#' #   
#' # }
#' 
#' #' Traverse graph in a Breadth First Search (BFS) manner
#' #' @description Given a network with ID and toID columns, and a starting ID, traverse a river network using a Breadth First Search (BFS) algorithm to explore all nodes of directed acyclic graph (DAG)
#' #' @param network data.frame with ID, toID, hydroseq, startflag, terminalpa, and divergence attributes.
#' #' @param start COMID of node to start traversal and go upstream/downstream from
#' #' @param return_as character string of how to return DFS traversal output. Either "list", "dataframe", "vector". Default is "vector", which return a list of 2 vectors, one indicating the BFS order of nodes and the other the respective level of each node.
#' #' @param verbose logical print status updates?
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return data.frame containing the distance between pairs of network outlets.
#' #' @importFrom dplyr select left_join mutate
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom fastmap fastmap fastqueue
#' #' @importFrom vctrs vec_c
#' #' @importFrom stats setNames
#' bfs_traversal <- function(
#'     network, 
#'     start     = NULL,
#'     return_as = "vector",
#'     verbose   = FALSE
#' ) {
#'   # network = net3
#'   # start     = starts$comid[1]
#'   # return_as = "list"
#'   # verbose   = T
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # turn network into a directed graph
#'   dag <- create_dag(network)
#'   # graph <- create_dag(network)
#'   
#'   # get the fromnode associated with a given COMID 'start'
#'   start_node <- comid_to_node(dag, start)
#'   
#'   if(verbose) {
#'     message("Starting braid detection at ", 
#'             ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
#'   }
#'   
#'   # drop graph geometry
#'   dag <- sf::st_drop_geometry(dag)
#'   # graph <- sf::st_drop_geometry(graph)
#'   
#'   # stash comids and from IDs
#'   comid_map <- dplyr::select(
#'     # graph,
#'     dag,
#'     comid,
#'     fromnode,
#'     tonode
#'   )
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identfied as a cycle and thus a braid
#'   # Questionable at this point, other option is the code below that uses 
#'   # single_cycles() function (COMMENTED OUT CODE BELOW)
#'   dag <- renode_circuits(dag, verbose = FALSE)
#'   
#'   # make an undirected graph
#'   undir <- make_undirected(dag)
#'   # # if a start COMID/Node is given
#'   # if(!is.null(start)) {
#'   #   
#'   #   # if start COMID is NOT found in the network, throw error
#'   #   if(!start %in% dag$fromnode) { stop(start, " node not found in graph")}
#'   #   
#'   #   # if(verbose) { message("Starting braid detection at COMID: ", start)  }
#'   #   
#'   #   # find fromnode of comid to start cycle detection at
#'   #   # start_node <- as.character(graph$fromnode[graph$comid == start])
#'   #   start_node <- as.character(start)
#'   #   
#'   # } else {
#'   #   # If no "start" is given, pick a node to start at
#'   #   if ("0" %in% dag$tonode) {
#'   #     
#'   #     # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
#'   #     start_node <- as.character(dag$fromnode[graph$tonode == "0"])[1]
#'   #     
#'   #     # if(verbose) {message("Starting braid detection at COMID: ", graph$comid[graph$tocomid == "0"][1])}
#'   #     
#'   #   } else {
#'   #     
#'   #     # if(verbose) { message("No 'tocomid' value equal to '0' found", 
#'   #     # "\nStarting braid detection at COMID: ", graph$comid[graph$fromnode == as.character(graph$fromnode)[1]] ) }
#'   #     start_node <- as.character(graph$fromnode)[1]
#'   #   }
#'   # }
#'   
#'   # if(verbose) {
#'   #   message("Starting braid detection at node: ", start_node)
#'   # }
#'   
#'   # make a topology hashmap to use in DFS 
#'   topo_map <- make_topo_map(
#'     from_nodes = undir$fromnode,
#'     to_nodes   = undir$tonode
#'     # from_nodes = graph$tonode,
#'     # to_nodes   = graph$fromnode
#'   )
#'   
#'   # root <- as.character(undir[undir$tocomid == "0", ]$fromnode)
#'   root <- as.character(start_node)
#'   
#'   # keep track of visited nodes
#'   marked <- fastmap::fastmap()
#'   
#'   # set all marked values to FALSE
#'   marked$mset(.list = stats::setNames(
#'     lapply(1:length(unique(c(undir$fromnode, undir$tonode))), 
#'            function(i){ FALSE }), 
#'     unique(c(undir$fromnode, undir$tonode))
#'   )
#'   )
#'   
#'   # message("Starting ", direction, " traversal from node ", root)
#'   
#'   # # result list
#'   # res <- list()
#'   
#'   # output result object
#'   if (return_as %in% c("list", "dataframe")) {
#'     res <- list()
#'     
#'   } else {
#'     # vector
#'     res <- vctrs::vec_c()
#'     level_res <-  vctrs::vec_c()
#'     # res <- vctrs::vec_c(topo_map$get(root)$to_node)
#'   }
#'   
#'   # initiliaze queue
#'   q <- fastmap::fastqueue()
#'   
#'   # add root to queue
#'   q$add(root)
#'   
#'   # level counter
#'   count = 0
#'   
#'   # while (!rstackdeque::empty(q)) {
#'   while (q$size() != 0) {
#'     
#'     # incremenet count
#'     count <- count + 1 
#'     
#'     # current level 
#'     lvl   <- as.character(count)
#'     
#'     message("Current level: ", lvl)
#'     
#'     # length of queue
#'     qlen <- q$size()
#'     
#'     # map/list to store current level nodes
#'     level_map <- fastmap::fastmap()
#'     
#'     # loop through all the nodes in the current level of graph
#'     for (i in 1:qlen) {
#'       
#'       # message("INNER LOOP i = ", i)
#'       
#'       # get/pop the front element from the queue
#'       node <- as.character(q$mremove(1)[[1]])
#'       
#'       # message("Popping node ", 
#'       #         node, 
#'       #         " from front of queue"
#'       # )
#'       # q$as_list()
#'       # if(!marked$get(node)) {
#'       if(marked$get(node)) {
#'         
#'         # # set vertex to marked
#'         # marked$set(node, TRUE)
#'         
#'         # message("-----------------------------------")
#'         # message("---- NODE ", node, "ALREADY VISITED ----")
#'         # message("-----------------------------------")
#'         next
#'         
#'       } else {
#'         # message("-----------------------------------")
#'         # message("!!! MARKING ", node, " as VISITED !!!")
#'         # message("-----------------------------------")
#'         # set vertex to marked
#'         marked$set(node, TRUE)
#'       }
#'       
#'       
#'       # if(is.null(node)) {
#'       #   message("**********************")
#'       #   message("---- NODE IS NULL ----")
#'       #   message("**********************")
#'       #   next
#'       # }
#'       
#'       
#'       # check if any nodes have already been visited at current level
#'       if (!level_map$has(lvl)) {
#'         
#'         # add level to level_map
#'         level_map$set(
#'           lvl,
#'           list(node = node)
#'         )
#'         
#'       } else {
#'         # node = "14"
#'         # if current lvl is already in level_map, update the value with current node
#'         level_map$set(lvl, 
#'                       list(
#'                         node  = c(level_map$get(lvl)$node, node)
#'                       )
#'         )
#'       }
#'       
#'       # level <- c(level, node)
#'       
#'       # get neighbors of current node 
#'       neighbors <- topo_map$get(node)$to_node
#'       # message("Adding neighbors to queue:\n", paste0( " --> ", neighbors, sep = "\n"))
#'       
#'       # add neighbors off node to queue
#'       q$madd(.list = neighbors)
#'       
#'       # message("queue size: ", q$size())
#'       # message("* * * * * * * * * *")
#'     }
#'     
#'     # convert level map to a list
#'     level_map <- level_map$as_list()
#'     # level <- list(level, node)
#'     
#'     # message("!!! UPDATING RES --> LEVEL ", lvl, " IS COMPLETE !!! ")
#'     
#'     # output result object
#'     if (return_as %in% c("list", "dataframe")) {
#'       
#'       # append list of level nodes to result
#'       res <- c(res, stats::setNames(
#'         list(
#'           unname(unlist(level_map))
#'         ),
#'         names(level_map)
#'       )
#'       )
#'       
#'     } else {
#'       
#'       # vector
#'       # res <- vctrs::vec_c(res, vctrs::vec_c(level_map))
#'       res       <- vctrs::vec_c(res, vctrs::vec_c(level_map[[1]]$node))
#'       level_res <- vctrs::vec_c(level_res, vctrs::vec_c(rep(names(level_map),  
#'                                                             length(level_map[[1]]$node))
#'       ))
#'       
#'     }
#'     
#'     message("=============================")
#'   }
#'   
#'   # lengths(res)
#'   # lvls <- res
#'   # out_df <- data.frame(
#'   #   fromnode = Reduce(c, res),
#'   #   level    = rep(names(res), lengths(res))
#'   #   )
#'   # comid_map %>% 
#'   #   dplyr::bind_rows(
#'   #     lapply(1:length(res[-length(res)]), function(i) {
#'   #       data.frame(
#'   #         fromnode = res[[i]],
#'   #         level    = as.numeric(names(res)[[i]])
#'   #       )
#'   #     })
#'   #   )
#'   #   dplyr::filter(fromnode %in% unique(Reduce(c, res))) 
#'   
#'   if(return_as == "dataframe") {
#'     
#'     res <- data.frame(
#'       fromnode = as.character(Reduce(c, res)),
#'       level    = rep(names(res), lengths(res))
#'     ) %>% 
#'       dplyr::left_join(
#'         dplyr::mutate(
#'           comid_map,
#'           fromnode = as.character(fromnode)
#'           ),
#'         by = "fromnode"
#'         )
#'     
#'     # res <-  dplyr::bind_rows(
#'     #   lapply(1:length(res), function(i) {
#'     #     data.frame(
#'     #       fromnode = res[[i]],
#'     #       level    = as.numeric(names(res)[[i]])
#'     #     )
#'     #   })
#'     # )
#'     
#'   } else if (return_as == "vector") {
#'     
#'     res <- list(
#'       node  = res,
#'       level = level_res
#'     )
#'   }
#'   
#'   return(res)
#'   
#' }
#' 
#' #' Traverse graph in a Depth First Search (DFS) manner
#' #' @description Given a network with ID and toID columns, and a starting ID, traverse a river network using a Depth First Search (DFS) algorithm to explore all nodes of directed acyclic graph (DAG)
#' #' @param network data.frame with ID, toID, hydroseq, startflag, terminalpa, and divergence attributes.
#' #' @param start COMID of node to start traversal and go upstream/downstream from
#' #' @param return_as character string of how to return DFS traversal output. Either "list", "dataframe", "vector". Default is "list".
#' #' @param verbose logical print status updates?
#' #' 
#' #' @noRd
#' #' @keywords internal
#' #' @return data.frame containing the distance between pairs of network outlets.
#' #' @importFrom dplyr select left_join mutate bind_rows
#' #' @importFrom sf st_drop_geometry
#' #' @importFrom fastmap fastmap faststack
#' #' @importFrom vctrs vec_c
#' #' @importFrom stats setNames
#' dfs_traversal <- function(
#'     network,
#'     start   = NULL,
#'     return_as = "list",
#'     verbose = FALSE
#' ) {
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # turn network into a directed graph
#'   dag <- create_dag(network)
#'   # graph <- create_dag(network)
#'   
#'   # get the fromnode associated with a given COMID 'start'
#'   start_node <- comid_to_node(dag, start)
#'   
#'   if(verbose) {
#'     message("Starting braid detection at ", 
#'             ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
#'   }
#'   
#'   # drop graph geometry
#'   dag <- sf::st_drop_geometry(dag)
#'   # graph <- sf::st_drop_geometry(graph)
#'   
#'   # stash comids and from IDs
#'   comid_map <- dplyr::select(
#'     dag,
#'     comid,
#'     fromnode,
#'     tonode
#'   )
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identfied as a cycle and thus a braid
#'   # Questionable at this point, other option is the code below that uses 
#'   # single_cycles() function (COMMENTED OUT CODE BELOW)
#'   dag <- renode_circuits(dag, verbose = FALSE)
#'   
#'   # make an undirected graph
#'   undir <- make_undirected(dag)
#' 
#'   # make a topology hashmap to use in DFS 
#'   topo_map <- make_topo_map(
#'     from_nodes = undir$fromnode,
#'     to_nodes   = undir$tonode
#'   )
#'   
#'   # # create topology hashmap
#'   # topo_map <- make_topo_map(
#'   #   from_nodes  = graph$fromnode,
#'   #   to_nodes    = graph$tonode
#'   # )
#'   
#'   # keep track of visited nodes
#'   marked <- fastmap::fastmap()
#'   
#'   # set all marked values to FALSE
#'   marked$mset(.list = stats::setNames(
#'     lapply(1:length(unique(c(undir$fromnode, undir$tonode))), function(i){ FALSE }),
#'     unique(c(undir$fromnode, undir$tonode))
#'   )
#'   )
#'   
#'   # start DFS traversal from root node
#'   root <- as.character(start_node)
#'   # root <- as.character(undir[undir$tocomid == "0", ]$tonode)
#'   
#'   # output result object
#'   if (return_as %in% c("list", "dataframe")) {
#'     res <- list()
#'     
#'   } else {
#'     # vector
#'     res <- vctrs::vec_c()
#'     # res <- vctrs::vec_c(topo_map$get(root)$to_node)
#'     
#'   }
#'   
#'   # initialize stack
#'   stack <- fastmap::faststack()
#'   
#'   # push root to top of stack
#'   stack$push(root)
#'   
#'   # counter
#'   count = 0
#'   
#'   # while stack is non empty
#'   while (stack$size() > 0) {
#'     
#'     count = count + 1
#'     
#'     # message("stack size: ", stack$size())
#'     # message("count: ", count)
#'     # message("Stack top to bottom:\n--------------------\n",
#'     #         # rev(paste0(" - ", c(unlist(stack$as_list())), sep = "\n"))
#'     #         rev(paste0(" - ", c(unlist(stack$as_list())), sep = " "))
#'     #         )
#'     
#'     # pop from top of stack
#'     node <- stack$pop()
#'     
#'     # convert node to character for fastmap
#'     node <- as.character(node)
#'     
#'     # message("Popped ", node, " from top of stack")
#'     
#'     # if vertex hasn't been visited
#'     if(!marked$get(node)) {
#'       
#'       # message("!!!!!!! THIS IS THE VISITING PORTION OF THE DFS ALGO !!!!!!!")
#'       # Process the current node, add to result object
#'       if (return_as %in% c("list", "dataframe")) {
#'         
#'         # list
#'         res[[count]] <- topo_map$get(node)
#'         
#'       } else {
#'         # vector
#'         res <- vctrs::vec_c(res, vctrs::vec_c(node))
#'         # res <- vctrs::vec_c(res, vctrs::vec_c(topo_map$get(node)$to_node))
#'         # res <- vctrs::vec_c(res, vctrs::vec_c(topo_map$get(node)$edge))
#'         # data.frame(   order     = count, from_node = strsplit("112->111", "->")[[1]][1],
#'         #   to_node   = strsplit("112->111", "->")[[1]][2])
#'         
#'       }
#'       # res[[count]] <- topo_map$get(node)
#'       # res <- vctrs::vec_c(res, vctrs::vec_c(topo_map$get(node)$to_node))
#'       
#'       # set vertex to marked
#'       marked$set(node, TRUE)
#'       # marked$get(node)
#'       
#'       # neighbors of current vertex
#'       neighbors <- topo_map$get(node)$to_node
#'       
#'       # message("Neighbors of node ", node , " are:\n",  paste0(" --> ", c(neighbors), sep = "\n"))
#'       
#'       # iterate through neighbors and add to stack if NOT VISITED
#'       for (n in neighbors) {
#'         # message("neighbor n: ", n)
#'         if(!marked$get(n)) {
#'           # message("**** neighbor ", n, " HAS NOT BEEN VISITED ****")
#'           # message("Pushing ", n, " onto stack")
#'           
#'           stack$push(n)
#'           
#'         }
#'         # message("*************")
#'       }
#'     }
#'     # message("ONTO NEXT ITERATION OF WHILE LOOP")
#'     # message("====================================")
#'   }
#'   if(verbose) {
#'     message("Returning DFS traversal order")
#'   }
#'   
#'   # format return into dataframe if as_df is TRUE
#'   if (return_as == "dataframe") {
#'     res <- dplyr::bind_rows(res)
#'   }
#'   
#'   return(res)
#'   
#' }
#' 
#' 
#' # **************************
#' # ******** OLD CODE ********
#' # **************************
#' 
#' # find_braids2 <- function(
#' #     network,
#' #     terminal_id = NULL,
#' #     add         = FALSE,
#' #     nested      = TRUE,
#' #     verbose     = FALSE
#' #     ) {
#' #   # network     = net
#' #   # terminal_id = "terminalpa"
#' #   # add         = FALSE
#' #   # nested      = TRUE
#' #   # verbose     = FALSE
#' #   # # nhdplusTools::navigate_network(start = 101, mode = "UT")
#' # 
#' #   # get a list of braid IDs and comids within each braid
#' #   braids <- get_braid_list(
#' #     network     = network,
#' #     terminal_id = terminal_id,
#' #     # terminal_id = NULL,
#' #     verbose     = verbose
#' #   )
#' # 
#' #   # rename braids to numeric values
#' #   names(braids) <- 1:length(braids)
#' # 
#' #   # create a new list of braid_ids that has the braid_ids of
#' #   # all other braids connected to the given braid_id
#' #   # the first braid_id in any of the vectors in "multis" will be the reference/main braid_id and
#' #   # the following braid_ids will be the rest of the multibraided system
#' #   multis <- find_multibraids(braids)
#' # 
#' #   # # any vector with more than one braid_id is a multibraid
#' #   # is_multi <- unname(lengths(multis) > 1)
#' # 
#' #   # # add a logical value that says whether a COMID is part of a multibraided system,
#' #   # # or is a flowline in more than one braid (multiple braid_ids) --> column "is_multibraid"
#' # 
#' #   # format braids into a dataframe
#' #   braids <- lapply(1:length(braids), function(k) {
#' # 
#' #     # get COMIDs of each braid
#' #     data.frame(
#' #       comid         = braids[[k]],
#' #       braid_id      = names(braids[k]),
#' #       member_braids = paste0(multis[[names(braids[k])]], collapse = ", "),
#' #       is_multibraid = length(multis[[names(braids[k])]]) > 1
#' #     )
#' # 
#' #   }) %>%
#' #     dplyr::bind_rows() %>%
#' #     dplyr::tibble()
#' #    # if nested is NOT TRUE, then COMIDs (flowlines) may appear more than once in output dataset
#' #    # ----> (i.e. a COMID/flowline can be a part of more than one braid_id and thus will have a row for "braid_1" and "braid_2")
#' #    # if each comid should have a list of all the braids it is a part of
#' #        # if relationship == "one-to-one" then COMIDs may appear more than once
#' #        # in dataset as a COMID may be apart of more than one braid
#' #        # if(relationship == "one-to-many") {
#' #    if(!nested) {
#' # 
#' #      # replace numeric braid_id with "braid_<number>"
#' #      braids$braid_id <- paste0("braid_", braids$braid_id)
#' # 
#' #      # drop the member braids column
#' #      braids <- dplyr::select(braids, -member_braids)
#' # 
#' #    } else { # if nested is TRUE, then each COMID is unique and has a braid_id column with a comma seperated character string of all relevent braid_ids
#' # 
#' #      # if braid_ids should be nested so that each COMID has a braid_id column that reflects ALL braid_ids related/connected to it
#' #      braids <-
#' #        braids %>%
#' #        dplyr::group_by(comid)  %>%
#' #        dplyr::summarise(
#' #          braid_id = paste0(member_braids, collapse = ", ")
#' #        ) %>%
#' #        dplyr::ungroup() %>%
#' #        dplyr::mutate(
#' #          braid_id = lapply(strsplit(braid_id, ", "), function(x) {
#' #            paste0("braid_", sort(as.numeric(unique(x))))
#' #          })
#' #        )
#' # 
#' #      # if more than one braid_id in "braid_id" column, then it is a multibraid
#' #      braids$is_multibraid <- lengths(braids$braid_id) > 1
#' # 
#' #      # collapse braid_ids into comma seperated character string
#' #      braids$braid_id <- Reduce(c,
#' #                                lapply(braids$braid_id, function(k) {
#' #                                  paste0(k, collapse = ", ")
#' #                                  })
#' #                                )
#' # 
#' #      # braids <-
#' #      #   braids %>%
#' #      #   dplyr::group_by(comid) %>%
#' #      #   dplyr::summarise(
#' #      #     braid_id = paste0(braid_id, collapse = ", ")
#' #      #   ) %>%
#' #      #   dplyr::ungroup()
#' #    }
#' # 
#' #    # # add a logical value that says whether a COMID is part of a multibraided system,
#' #    # # or is a flowline in more than one braid (multiple braid_ids)
#' #    # braids$is_multibraid <- braids$comid %in% multibraids
#' # 
#' #    # if braid data should be added to original data
#' #    if(add) {
#' # 
#' #      # join back with original data
#' #      braids <- dplyr::mutate(
#' #                  dplyr::left_join(
#' #                    network,
#' #                    braids,
#' #                    by = "comid"
#' #                  ),
#' #                  braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#' #                )
#' #      # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#' #    }
#' # 
#' #    # set no_braid values to FALSE
#' #    braids$is_multibraid <- ifelse(is.na(braids$is_multibraid),
#' #                                   FALSE,
#' #                                   braids$is_multibraid
#' #                                   )
#' # 
#' #    # move braid_id and is_multibraided columns to front of dataframe
#' #    braids <- dplyr::relocate(braids, comid, braid_id, is_multibraid)
#' #    # braids <- dplyr::relocate(braids, braid_id, is_multibraid)
#' # 
#' #   return(braids)
#' #  }
#' 
#' #' #' Detect whether a braid exists in a NHDPlus flowlines Network (DEPRECATED)
#' #' #' DEPRECATED 
#' #' #' @param network data.frame with comid, tonode, fromnode, and (optionally) divergence and terminalpa attributes.
#' #' #' @param start integer COMID to start braid detection from. A start COMID should be provided in cases where network is made up of disconnected network components (i.e. a start COMID will detect all braids in each connected flowlines network) Default is NULL and braid checking will start from the flowline that has a tocomid value of "0", or a random flowline otherwise. 
#' #' #' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' #' #' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found
#' #' is_braided2 <- function(
#' #'     network,
#' #'     start   = NULL,
#' #'     verbose = FALSE
#' #' ) {
#' #'   
#' #'   
#' #'   # lower case names
#' #'   names(network) <- tolower(names(network))
#' #'   
#' #'   # create directed acyclic graph
#' #'   graph <- create_dag(network)
#' #'   
#' #'   # get the fromnode associated with a given COMID 'start'
#' #'   start_node <- comid_to_node(graph, start)
#' #'   
#' #'   # drop graph geometry
#' #'   graph <- sf::st_drop_geometry(graph)
#' #'   
#' #'   # create artificial cycles from circuits in graph, doing this allows for 
#' #'   # sections of river that form a circuit to be identified as a cycle and thus a braid
#' #'   # Questionable at this point, other option is the code using single_cycles() function 
#' #'   graph <- renode_circuits(graph, verbose = FALSE)
#' #'   
#' #'   # make an undirected graph from DAG
#' #'   graph <- make_undirected(graph)  
#' #'   
#' #'   # make hashmap for tonode/fromnode topology for traversing graph
#' #'   topo_map <- make_topo_map(
#' #'     from_nodes = graph$fromnode,
#' #'     to_nodes   = graph$tonode
#' #'   )
#' #'   
#' #'   # keep track of visited nodes
#' #'   visit <- fastmap::fastmap()
#' #'   
#' #'   # Depth first search function for detecting cycles 
#' #'   dfs <- function(node, prev) {
#' #'     
#' #'     if (visit$has(node)) {
#' #'       # if(visit$has(node)) {  
#' #'       return(FALSE)
#' #'       
#' #'     }
#' #'     
#' #'     # visit node
#' #'     visit$set(node, TRUE)
#' #'     
#' #'     # get the neighbors of current node
#' #'     neighbors <- topo_map$get(node)$to_node
#' #'     
#' #'     # message("Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
#' #'     
#' #'     for (i in neighbors) {
#' #'       
#' #'       # message("NEIGHBOR: ", i)          
#' #'       
#' #'       if (i == prev) {
#' #'         # message("!!! SKIPPING NEIGH == PREVIOUS --> ", i, " == ", prev, " !!!")
#' #'         next
#' #'       }
#' #'       
#' #'       if (!dfs(i, node)) {
#' #'         # message("!!! FOUND A CYCLE !!!")
#' #'         return(FALSE)
#' #'       }
#' #'       # message("=============================")
#' #'     }
#' #'     
#' #'     return(TRUE)
#' #'   }
#' #'   
#' #'   # # if a start node is given
#' #'   # if(!is.null(start)) {
#' #'   #   dfs(start, "-1")
#' #'   # } else {
#' #'   #   dfs("1", "-1")
#' #'   # }
#' #'   
#' #'   # # TODO need to account for situation where there are multiple "0' start nodes...
#' #'   # lapply(1:length(start_node), function(k) {
#' #'   #   dfs(start_node[k], "no_previous")
#' #'   # })
#' #'   
#' #'   # return cycle detection logical
#' #'   return(!dfs(start_node, "no_previous"))
#' #'   
#' #' }
#' 
#' #  make_divergences <- function(network,
#' #                               id_col = "comid",
#' #                               from_col = "fromnode",
#' #                               to_col = "tonode"
#' #                               ) {
#' # 
#' # 
#' #    net_test <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 40)
#' #    # net$geometry %>% plot()
#' #    # net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 300)
#' #    # net <-
#' #    network <- net_test
#' #    id_col = "comid"
#' #    from_col = "fromnode"
#' #    to_col = "tonode"
#' #    # names(network) <- tolower(names(network))
#' # 
#' #    network %>% names()
#' # 
#' #    net <- network[, c(id_col, from_col, to_col, "divergence", "totdasqkm")]
#' #    # net <- network[, c(id_col, from_col, to_col)]
#' # 
#' # 
#' #    # lower case names
#' #    names(network) <- tolower(names(network))
#' # 
#' #    # # create directed acyclic graph
#' #    # graph <- create_dag(net)
#' #    graph <- create_dag(dplyr::select(net,
#' #                                      comid, fromnode, tonode))
#' # 
#' #    # make hashmap for tonode/fromnode topology for traversing graph
#' #    topo_map <- make_topo_map(
#' #      from_nodes = graph$fromnode,
#' #      to_nodes   = graph$tonode,
#' #      directed   = TRUE
#' #    )
#' #    # keep track of visited nodes
#' #    in_degrees <- fastmap::fastmap()
#' #    out_degrees <- fastmap::fastmap()
#' # 
#' #    # # set all marked values to FALSE
#' #    in_degrees$mset(.list = stats::setNames(
#' #                  lapply(1:length(unique(c(graph$fromnode, graph$tonode)) ),
#' #                         function(i){ 0 }),
#' #                  unique(c(graph$fromnode, graph$tonode))
#' #                  ))
#' # 
#' #    # # set all marked values to FALSE
#' #    out_degrees$mset(.list = stats::setNames(
#' #                    lapply(1:length(unique(c(graph$fromnode, graph$tonode)) ),
#' #                           function(i){ 0 }),
#' #                    unique(c(graph$fromnode, graph$tonode))
#' #                  ))
#' #    topo <- topo_map$as_list()
#' # 
#' #    for(i in 1:length(topo)) {
#' # 
#' #      }
#' #      # make hashmap for tonode/fromnode topology for traversing graph
#' #      topo_map <- make_topo_map(
#' #        from_nodes = outs$fromnode,
#' #        to_nodes   = outs$tonode,
#' #        directed   = F
#' #      )
#' #      topo_map$as_list()
#' #      # make_undirected(no_dups)
#' # 
#' #      comid_map <-
#' #        graph %>%
#' #        dplyr::left_join(
#' #          sf::st_drop_geometry(
#' #            dplyr::select(network, comid, divergence)
#' #          ),
#' #          by = "comid"
#' #        )
#' # 
#' #      # make an undirected graph from DAG
#' #      undir <- make_undirected(graph)
#' #      # undir <- make_undirected(no_dups)
#' # 
#' #      # make hashmap for tonode/fromnode topology for traversing graph
#' #      topo_map <- make_topo_map(
#' #        from_nodes = undir$fromnode,
#' #        to_nodes   = undir$tonode,
#' #        directed   = TRUE
#' #      )
#' # 
#' # 
#' #      topo_map$as_list()
#' #      topo_map$size()
#' # 
#' #      divs <- topo_map$as_list()
#' #      #   # format braids into a dataframe
#' #      divs_df <- lapply(1:length(divs), function(k) {
#' #          # braids_df <- lapply(1:length(braids), function(k) {
#' #          # k = 1
#' #          # divs[[k]]$count
#' #          # names(divs[k])
#' #          # get COMIDs of each braid
#' #          data.frame(
#' #            fromnode = names(divs[k]),
#' #            tonode    = divs[[k]]$to_node,
#' #            count    = divs[[k]]$count,
#' #            edge    = divs[[k]]$edge
#' #          )
#' #        }) %>%
#' #          dplyr::bind_rows()
#' # 
#' #      out <-
#' #        divs_df %>%
#' #        dplyr::mutate(
#' #          divergences_v2 = dplyr::case_when(
#' #            count > 1 ~ 1,
#' #            TRUE      ~ 0
#' #          )
#' #        ) %>%
#' #        dplyr::left_join(
#' #          dplyr::mutate(
#' #            dplyr::select(
#' #            # sf::st_drop_geometry(graph),
#' #              graph,
#' #            comid, fromnode, divergence, geometry
#' #          ),
#' #          fromnode = as.character(fromnode)
#' #          ),
#' #          by = "fromnode"
#' #        ) %>%
#' #        dplyr::mutate(
#' #          div_check = dplyr::case_when(
#' #            divergences_v2 == 0 & divergence == 0 ~ TRUE,
#' #            TRUE      ~ FALSE
#' #          )
#' #        )
#' # 
#' #      make_topo_map()
#' # 
#' # 
#' #      divs$comid
#' #      cat(divs$comid)
#' #      cat(divs$tocomid)
#' # 
#' #      fromnode <- divs$comid
#' #      tonode <- divs$tocomid
#' # 
#' #      # Combine the two vectors into one
#' #      all_nodes <- unique(c(fromnode, tonode))
#' # 
#' #      # Create a mapping of original IDs to new IDs
#' #      node_mapping <- setNames(1:length(all_nodes), all_nodes)
#' # 
#' #      tmp <- data.frame(
#' #        uid = names(node_mapping),
#' #        node_id = unname(node_mapping)
#' #      )
#' #     1:length(all_nodes)
#' #     # Map the fromnode and tonode vectors to new IDs
#' #     fromnode_mapped <- node_mapping[fromnode]
#' #      tonode_mapped <- node_mapping[tonode]
#' #     fromnode <- divs$comid
#' #      tonode <- divs$tocomid
#' #     unique_nodes <- unique(c(fromnode, tonode))
#' # 
#' #      node_mapping <- setNames(
#' #        seq_along(unique_nodes),
#' #        unique_nodes     )   # node_mapping[as.character(fromnode)]
#' #      # node_mapping[as.character(tonode)]
#' #      fromnode_mapped <- node_mapping[as.character(fromnode)]
#' #      tonode_mapped <- node_mapping[as.character(tonode)]
#' #  adjacency_list <- vector("list", length(unique_nodes))
#' #  for (i in seq_along(fromnode_mapped)) {
#' #     # i = 1
#' #    from <- fromnode_mapped[i]
#' #       to <- tonode_mapped[i]
#' #        # c(adjacency_list[[from]], to)
#' #        # tonode_mapped
#' #        adjacency_list[[from]] <- c(adjacency_list[[from]], to)
#' #      }
#' #      # dup_comids <-
#' #      #   graph %>%
#' #      #   dplyr::count(comid) %>%
#' #      #   dplyr::arrange(-n) %>%
#' #      #   dplyr::filter(n != 1) %>%
#' #      #   .$comid
#' #      # drop_dups <-
#' #      #   graph %>%
#' #      #   dplyr::filter(
#' #      #     !comid %in% dup_comids
#' #      #   )
#' #      # dups <- graph[duplicated(graph$comid), ]
#' #    # )
#' #    nhdplusTools::make_node_topology(
#' #      graph,
#' #      add = TRUE
#' #    )
#' # 
#' #    # get the fromnode associated with a given COMID 'start'
#' #    start_node <- comid_to_node(graph, start)
#' # 
#' #    # drop graph geometry
#' #    graph <- sf::st_drop_geometry(graph)
#' # 
#' #    # create artificial cycles from circuits in graph, doing this allows for
#' #    # sections of river that form a circuit to be identified as a cycle and thus a braid
#' #    # Questionable at this point, other option is the code using single_cycles() function
#' #    graph <- renode_circuits(graph, verbose = FALSE)
#' # 
#' #    # make an undirected graph from DAG
#' #    graph <- make_undirected(graph)
#' # 
#' #    # make hashmap for tonode/fromnode topology for traversing graph
#' #    topo_map <- make_topo_map(
#' #      from_nodes = graph$fromnode,
#' #      to_nodes   = graph$tonode
#' #    )
#' #    # keep track of visited nodes
#' #    visit <- fastmap::fastmap()
#' # 
#' # }
