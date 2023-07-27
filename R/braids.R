#' Find braids in a network
#'
#' Find and uniquely identify braids in a network of flowlines, given a dataframe containing comid, fromnode, tonode and divergence as columns. 'find_braids()" identifies braids as cycles in the graph representation of the river network.
#'
#' @param network The network object representing the river network.
#' @param start Optional argument specifying the starting point for braid detection.
#' @param return_as Specifies the format of the output. Options are "list" (default) or "dataframe".
#' @param add Logical indicating whether to add braid information to the original network data.
#' @param nested Logical indicating whether the output dataframe should be nested, with each COMID having a list of all the braids it is a part of. If TRUE (Default), the braid_id column may contain multiple braid IDs for a given COMID. If FALSE, there may be duplicate COMIDs as a single COMID could be a part of multiple braids (braid_id)
#' @param verbose Logical indicating whether to display verbose messages during the braid detection process.
#'
#' @return Either a list of braids detected in the network. Each element of the list represents a braid and contains the COMIDs  that form the braid. Or a dataframe of COMIDs associated with there respective braid_id(s)
#'
#' @examples
#' net <- nhdplusTools::navigate_network(
#'  start       = 101,
#'  mode        = "UT", 
#'  distance_km = 100
#'  ) %>% 
#' dplyr::select( comid, divergence, totdasqkm, fromnode, tonode) 
#' 
#' # get a list of COMIDs in each braid of network
#' braid_list = find_braids(network = net, return_as = "list")
#' 
#' # add braid_id column to original dataset
#' braid_df = find_braids(network   = net, 
#'                        return_as = "dataframe",
#'                        nested    = TRUE,
#'                        add       = TRUE
#'                        )
#'                        
#' # returnal original data with each braid_id represented 
#' # by its individual COMIDs (may contain duplicate COMIDs)
#' braid_df = find_braids(network   = net, 
#'                        return_as = "dataframe",
#'                        nested    = FALSE,
#'                        add       = TRUE
#'                        )
#' @importFrom dplyr bind_rows tibble select filter group_by summarise ungroup mutate left_join relocate
#' @importFrom sf st_drop_geometry
#' @importFrom stats setNames
#' @importFrom magrittr %>% 
#'
#' @export
find_braids <- function(
    network,
    start        = NULL,
    return_as    = "list",
    add          = FALSE,
    nested       = TRUE,
    verbose      = FALSE
) {
  
  # check valid return_as input
  if(!return_as %in% c("list", "dataframe")) {
    stop("Invalid 'return_as' argument '",
         return_as, "'\n'return_as' must be: 'list' or 'dataframe'")
  }
  
  # # check relationship argument is valid
  # if(return_as == "dataframe") {
  #   if(!relationship %in% c("one-to-one", "one-to-many")) {
  #     stop("Invalid 'relationship' argument '",
  #          relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
  #   }
  # }
  
  # lower case names
  names(network) <- tolower(names(network))
  
  # turn network into a directed graph
  dag <- create_dag(network)
  
  # get the fromnode associated with a given COMID 'start'
  start_node <- comid_to_node(dag, start)
  
  if(verbose) {
    message("Starting braid detection at ", 
            ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
  }
  
  # drop graph geometry
  dag <- sf::st_drop_geometry(dag)
  # graph <- sf::st_drop_geometry(graph)
  
  # stash comids and from IDs
  comid_map <- dplyr::select(
    # graph,
    dag,
    comid,
    fromnode,
    tonode
  )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # Questionable at this point, other option is the code below that uses 
  # single_cycles() function (COMMENTED OUT CODE BELOW)
  regraph <- renode_circuits(dag, verbose = FALSE)
  
  # ##### - GET_SINGLE_CYCLES CODE -
  # # check for single cycles (2 nodes A,B that have unique edges)
  # # (i.e. 2 comids w/ exact same fromnodes/tonodes)
  # single_cycles <- get_single_cycles(dag)
  # # if cycles were found
  # if(!is.null(cycles)) {
  #   # match each fromnode with original COMIDs
  #   braids <- sapply(1:length(cycles), function(k) {
  #     # get COMIDs of each braid
  #     comid_map[comid_map$fromnode %in% cycles[[k]], ]$comid
  #   })
  #   # if single cycles are found in network
  #   if(!is.null(single_cycles)) {
  #     braids <- c(braids, unname(single_cycles))
  #   }
  #   # if NO cycles were found
  # } else {
  #   # if single cycles are found in network
  #   if(!is.null(single_cycles)) {
  #     braids <- c(unname(single_cycles))
  #   } else {
  #     message("No braids found, returning NULL")
  #     return(NULL)
  #   }
  # }
  
  # make an undirected graph
  undir <- make_undirected(regraph)
  
  # find cycles in undirected graph (proxy for braids)
  cycles <- find_cycles(
    graph     = undir,
    start     = start_node,
    return_as = "list",
    edge      = FALSE,
    wide      = TRUE,
    verbose   = verbose
  )
  
  # if(cycles$size() == 0 & is.null(single_braids)) {
  if(is.null(cycles)) {
    # message("No cycles found, returning NULL")
    return(NULL)
  }
  
  # remove added nodes from renode_circuits()
  braids <- lapply(cycles, function(i) { 
    i[i %in% comid_map$fromnode] 
  })
  
  # # match fromnodes from comid_map to nodes identified in each cycle
  braids <- lapply(1:length(braids), function(k) {
    # get COMIDs of each braid
    comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
      dplyr::filter(tonode %in% braids[[k]]) %>%
      .$comid
  })
  
  # set braid_id names
  braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
  
  # # if multibraids should be untangled
  # if(untangle) {
  #   if(verbose) { message("Attempting to seperate multibraids...") }
  #   braids <- seperate_braids(network, braids)
  # }
  
  # if data should be returned as a 2 column dataframe with comid and braid_ids
  if(return_as == "dataframe") {
    
    # get comids of multibraids
    multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
    
    # format braids into a dataframe
    braids <- lapply(1:length(braids), function(k) {
      # braids_df <- lapply(1:length(braids), function(k) {
      
      # get COMIDs of each braid
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
      braids <-
        dplyr::mutate(
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
  }
  
  return(braids)
}

#' Detect whether a braid exists in a NHDPlus flowlines Network
#'
#' @param network data.frame with comid, tonode, fromnode, and (optionally) divergence and terminalpa attributes.
#' @param start integer COMID to start braid detection from. A start COMID should be provided in cases where network is made up of disconnected network components (i.e. a start COMID will detect all braids in each connected flowlines network) Default is NULL and braid checking will start from the flowline that has a tocomid value of "0", or a random flowline otherwise. 
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found
#' @export
#'
#' @examples
is_braided <- function(
    network,
    start   = NULL,
    verbose = FALSE
) {
  # # lower case names
  # names(network) <- tolower(names(network))
  # 
  # # turn network into a directed graph
  # dag <- create_dag(network)
  # # graph <- create_dag(network)
  # 
  # # get the fromnode associated with a given COMID 'start'
  # start_node <- comid_to_node(dag, start)
  # 
  # if(verbose) {
  #   message("Starting braid detection at ", 
  #           ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
  # }
  # 
  # # drop graph geometry
  # dag <- sf::st_drop_geometry(dag)
  # # graph <- sf::st_drop_geometry(graph)
  # 
  # # stash comids and from IDs
  # comid_map <- dplyr::select(
  #   # graph,
  #   dag,
  #   comid,
  #   fromnode,
  #   tonode
  # )
  # 
  # # create artificial cycles from circuits in graph, doing this allows for 
  # # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # # Questionable at this point, other option is the code below that uses 
  # # single_cycles() function (COMMENTED OUT CODE BELOW)
  # dag <- renode_circuits(dag, verbose = verbose)
  # 
  # # make an undirected graph
  # undir <- make_undirected(dag)
  # network <- network3
  # start  <- "8898002"
  # start  <- "fgdegd"
  # start = NULL
  # plot(network$geometry)
  # # pick a node to start at
  # start_node <- as.character(network$fromnode[1])
  
  # lower case names
  names(network) <- tolower(names(network))
  
  # create directed acyclic graph
  graph <- create_dag(network)
  
  # get the fromnode associated with a given COMID 'start'
  start_node <- comid_to_node(graph, start)
  
  # drop graph geometry
  graph <- sf::st_drop_geometry(graph)
  
  # # if a start COMID is given
  # if(!is.null(start)) {
  #   # if start COMID is NOT found in the network, throw error
  #   if(!start %in% network$comid) { stop(start, " COMID not found in network")}
  #   # find fromnode of comid to start cycle detection at
  #   start_node <- as.character(graph$fromnode[ graph$comid == start])
  # } else {
  #   if ("0" %in% graph$tocomid) {
  #     # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
  #     start_node <- as.character(graph$fromnode[graph$tocomid == "0"])[1]
  #   } else {
  #     start_node <- as.character(graph$fromnode)[1]
  #   }
  # }
  # if(verbose) {
  #   message("Starting braid detection at ", 
  #           ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
  # }
  
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identified as a cycle and thus a braid
  # Questionable at this point, other option is the code using single_cycles() function 
  graph <- renode_circuits(graph, verbose = FALSE)
  
  # make an undirected graph from DAG
  graph <- make_undirected(graph)  
  
  # make hashmap for tonode/fromnode topology for traversing graph
  topo_map <- make_topo_map(
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode
    # from_nodes = graph$tonode,
    # to_nodes   = graph$fromnode
  )
  
  # keep track of visited nodes
  visit <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  # visit$mset(.list = stats::setNames(
  #                           lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i){ FALSE }), 
  #                           unique(c(graph$fromnode, graph$tonode))))
  
  # Depth first search function for detecting cycles 
  dfs <- function(node, prev) {
    
    if (visit$has(node)) {
      # if(visit$has(node)) {  
      return(FALSE)
      
    }
    
    # visit node
    visit$set(node, TRUE)
    
    # get the neighbors of current node
    neighbors <- topo_map$get(node)$to_node
    
    # message("Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    for (i in neighbors) {
      
      # message("NEIGHBOR: ", i)          
      
      if (i == prev) {
        # message("!!! SKIPPING NEIGH == PREVIOUS --> ", i, " == ", prev, " !!!")
        next
      }
      
      if (!dfs(i, node)) {
        # message("!!! FOUND A CYCLE !!!")
        return(FALSE)
      }
      # message("=============================")
    }
    
    return(TRUE)
  }
  
  # # if a start node is given
  # if(!is.null(start)) {
  #   dfs(start, "-1")
  # } else {
  #   dfs("1", "-1")
  # }
  
  # # TODO need to account for situation where there are multiple "0' start nodes...
  # lapply(1:length(start_node), function(k) {
  #   dfs(start_node[k], "no_previous")
  # })
  
  # return cycle detection logical
  return(!dfs(start_node, "no_previous"))
  
}

# given a list of vectors, return the indexes of the list elements that have duplicates in another list element. The value of each returned list element represents the index of the other list element that has duplicated vector values in the original list element
find_overlaps <- function(
    lst, 
    no_overlap_val = FALSE,
    rm_no_overlap  = FALSE,
    verbose        = FALSE
) {
  
  # lst <- braids
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
# 
# # find overlapping COMIDs in list of COMID vectors and
# # remove the overlapping (double counting) of COMIDs in different braids
# untangle_braids <- function(x) {
#   # x = braids
#   
#   # overlapped braids
#   overlaps <- find_overlaps(
#     lst            = x,
#     no_overlap_val = FALSE, 
#     rm_no_overlap  = FALSE, 
#     verbose        = FALSE
#   ) 
#   
#   big_cycles <- 
#     overlaps %>% 
#     Reduce(c, . ) %>% 
#     unique() %>% 
#     sort()
#   
#   # remove 0 values
#   big_cycles <- big_cycles[big_cycles != 0]
#   
#   for(j in 1:length(big_cycles)) {
#   
#     
#     # big cycle iteration which is a cycle that contains one or more OTHER cycles
#     cyc = big_cycles[j]
#     
#     # main big COMID
#     big <- x[[cyc]]
#     
#     message("cyc: ", cyc)
#     
#     connects <- sapply(1:length(overlaps), function(k) {
#       # k = 4
#       cyc %in% overlaps[[k]]
#     })
#     
#     
#     smalls <- x[connects]
#     
#     # drop small COMIDs from big COMID
#     dropped <- big[!big %in% Reduce(c, smalls)]
#     
#     graph <- 
#       net %>% 
#       dplyr::filter(comid %in% big)
#     
#     # get COMID for big cycles and the fromnode/tonodes
#     big_geom <- 
#       net %>% 
#       dplyr::filter(comid %in% dropped)
#     
#     # get COMID for small cycles and the fromnode/tonodes
#     small_geom <- 
#       net %>% 
#       dplyr::filter(comid %in% Reduce(c, smalls))
#     
#     # mapview::mapview(graph, color = "dodgerblue")  + 
#     #   mapview::mapview(small_geom, color = "red") + 
#     #   mapview::mapview(big_geom, color = "green") 
#     # 
#     # graph$geometry %>% plot()
#     # graph$geometry %>% plot()
#     # 
#   }
#   
# }

# # find overlapping COMIDs in list of COMID vectors and
# # remove the overlapping (double counting) of COMIDs in different braids
# seperate_braids3 <- function(x, net) {
#   
#   # x = braids
#   # net = network
#   # cmap <- comid_map

#   # overlapped braids
#   overlaps <- find_overlaps(
#     lst            = x,
#     no_overlap_val = FALSE, 
#     rm_no_overlap  = FALSE, 
#     verbose        = FALSE
#   ) 
#   
#   big_cycles <- 
#     overlaps %>% 
#     Reduce(c, . ) %>% 
#     unique() %>% 
#     sort()
#   
#   # remove 0 values
#   big_cycles <- big_cycles[big_cycles != 0]
#   
#   # overlaps[Reduce(c, overlaps) != 0]
#   
#   # while(!is.null(overlaps)) {
#   
#   # i = 1
#   # 4 %in% overlaps[[i]]
#   
#   for(j in 1:length(big_cycles)) {
#     # j = 6
#     
#     # big cycle iteration which is a cycle that contains one or more OTHER cycles
#     cyc = big_cycles[j]
#     
#     # main big COMID
#     big <- x[[cyc]]
#     
#     message("cyc: ", cyc)
#     
#     connects <- sapply(1:length(overlaps), function(k) {
#       # k = 4
#       cyc %in% overlaps[[k]]
#     })
#     
#     # overlaps
#     
#     smalls <- x[connects]
#     
#     # connects
#     
#     # drop small COMIDs from big COMID
#     dropped <- big[!big %in% Reduce(c, smalls)]
#     
#     # big
#     
#     # x
#     # dropped
#     
#     # get COMID for big cycles and the fromnode/tonodes
#     big_geom <- 
#       net %>% 
#       dplyr::filter(comid %in% dropped)
#     
#     # get COMID for small cycles and the fromnode/tonodes
#     small_geom <- 
#       net %>% 
#       dplyr::filter(comid %in% Reduce(c, smalls))
#     
#     # tonode in big geom: 550015139
#     # fromnode in big geom 550015136
#     #  need to find the linestring in (fromnode/tonode) in small_geom that has:
#     #  --> tonode == big geom fromnode
#     #  --> fromnode == big geom tonode
#     #  --> fromnodes == big geom fromnodes
#     #  --> tonodes == big geom fromnodes
#     # small_geom %>% 
#     #   dplyr::filter(
#     #     # tonode %in% big_geom$fromnode,
#     #     # fromnode %in% big_geom$tonode
#     #     (tonode %in% big_geom$fromnode & fromnode %in% big_geom$tonode) |
#     #       fromnode %in% big_geom$fromnode
#     #   )
#     
#     # from nodes from big equal to from nodes from small from nodes in big == tonodes in small
#     # get the connecting linestrings for missing connections after removing small cycle COMIDs
#     fromtos <- 
#       small_geom %>% 
#       dplyr::filter(
#         # tonode %in% big_geom$fromnode,
#         # fromnode %in% big_geom$tonode
#         (tonode %in% big_geom$fromnode & fromnode %in% big_geom$tonode) |
#           fromnode %in% big_geom$fromnode
#       ) %>% 
#       dplyr::group_by(fromnode, tonode) %>% 
#       dplyr::mutate(tmp_id = 1:n()) %>% 
#       dplyr::add_count() %>% 
#       dplyr::ungroup()
#     
#     # singleton linestrings to add back
#     singles <- 
#       fromtos %>% 
#       dplyr::filter(n == 1) 
#     
#     # cycles with multiple linestrings with the same fromto/tonodes
#     multis <- 
#       fromtos %>% 
#       dplyr::filter(n > 1) 
#     
#     # get the linestring from each fromnode/tonode combination that has the lowest divergence value without ties
#     # bind the single line strings we picked out from 'multi' with the original singletons
#     keeps <- 
#       multis %>% 
#       dplyr::group_by(fromnode, tonode) %>% 
#       # dplyr::filter(divergence == max(divergence))
#       dplyr::slice_min(divergence, with_ties = FALSE) %>% 
#       dplyr::ungroup() %>% 
#       dplyr::bind_rows(singles)
#     
#     # updated big COMIDs
#     big_update <- c(dropped, keeps$comid)
#     # x
#     
#     # replace COMIDs
#     x[[cyc]] <- big_update 
#     # drop_big <- sapply(1:length(overlaps), function(k) {
#     
#     for (k in 1:length(overlaps)) {
#       
#       # k = 1
#       # overlaps[[k]]
#       
#       # message("k: ", k)
#       # message("overlaps[[k]]: ", overlaps[[k]])
#       
#       if(cyc %in% overlaps[[k]]) {
#         
#         # message("FOUND ", cyc, " IN ", overlaps[[k]], " REMOVING ", cyc)
#         if(length(overlaps[[k]]) == 1) {
#           
#           overlaps[[k]] <- FALSE
#           
#         } else {
#           
#           overlaps[[k]] <- overlaps[[k]][overlaps[[k]] != cyc]
#           
#         }
#       }
#     }
#   }
#   return(x)
# }

# find overlapping COMIDs in list of COMID vectors and
# remove the overlapping (double counting) of COMIDs in different braids
seperate_braids2 <- function(x) {
  
  # overlapped braids
  overlaps <- find_overlaps(
    lst            = x,
    no_overlap_val = FALSE, 
    rm_no_overlap  = FALSE, 
    verbose        = FALSE
  )
  
  while(!is.null(overlaps)) {
    # # number of multibraids/overlaps
    # multibraids <- sum(lengths(overlaps[sapply(overlaps, function(i) { !isFALSE(i)}) ]))
    
    # # number of multibraids/overlaps
    # multibraids <- length(overlaps[sapply(overlaps, function(i) { !isFALSE(i)})])
    # multis <- overlaps[sapply(overlaps, function(i) { !isFALSE(i)})]
    
    for (i in 1:length(overlaps)) {
      
      multis <- overlaps[[i]]  
      
      if(isFALSE(multis)) {
        next
      }
      # message("i: ", i)
      # message("multis: ", multis)
      
      for (k in 1:length(multis)) {
        # message("k: ", k)
        # message("removing COMIDs from ", multis[k], " that are in ", i)
        # message("multis[k]: ",      multis[k])
        
        # remove braids containing multiple braids
        x[[multis[k]]] <- x[[multis[k]]][
          !x[[multis[k]]] %in% x[[i]]
        ]
        # message("--------------")
      }
      # message("=========================") 
    }
    
    overlaps <- find_overlaps(
      lst            = x,
      no_overlap_val = FALSE,
      rm_no_overlap  = TRUE,
      verbose        = FALSE
    )
  }
  return(x)
}

# find overlapping COMIDs in list of COMID vectors and remove the overlapping (double counting) of COMIDs in different braids
seperate_braids4 <- function(x) {
  
  # overlapped braids
  overlaps <- find_overlaps(
    lst            = x,
    no_overlap_val = FALSE, 
    rm_no_overlap  = FALSE, 
    verbose        = FALSE
  )
  
  while(!is.null(overlaps)) {

    # number of multibraids/overlaps
    multibraids <- length(overlaps[
      sapply(overlaps, function(i) { !isFALSE(i)})
    ])
    
    overlaps[
      sapply(overlaps, function(i) { !isFALSE(i)})
    ]
    # multibraids <-  length(overlaps[overlaps != FALSE])
    # message("multibraids: ", multibraids)
    
    # smaller braid
    small <- which(overlaps == TRUE)
    
    # larger braid
    big   <- overlaps[small]
    
    for (n in 1:length(small)) {
      # message("n: ", n)
      # message("small[[n]]: ", small[[n]])
      # message("big[[n]]: ", big[[n]])
      
      for (k in 1:length(big[[n]])) {
        # message("k: ", k)
        # message("[big[[n]][k]]: ", big[[n]][k])
        
        # remove braids containing multiple braids
        x[[big[[n]][k]]] <- x[[big[[n]][k]]][ 
          !x[[big[[n]]]] %in% x[[small[n]]] 
        ]
      }
      # message('-----------')
    }
    
    
    overlaps <- find_overlaps(
      lst            = x,
      no_overlap_val = FALSE,
      rm_no_overlap  = TRUE,
      verbose        = FALSE
    )
    # message('======================') 
  }
  
  return(x)
  
}




#' Create a Directed Acyclic Graph (DAG)
#'
#' This function creates a Directed Acyclic Graph (DAG) given a network with tonodes and fromnodes. The function allows for trimming the network based on a specific starting node and supports the option to reverse the graph.
#'
#' @param x A data frame representing the river network with columns 'comid', 'fromnode', 'tonode', and optionally 'divergence' for dendritic networks.
#' @param start A character string representing the starting node (COMID) of the network. If provided, the network will be trimmed to include only nodes upstream of the start node (including the start node itself).
#' @param add logical value indicating whether to add back the original columns to the resulting graph. If FALSE, only comid, tocomid, fromnode, tonode, and optionally divergence columns will be returned.
#' @param drop_geom logical value indicating whether to drop the geometry column from the resulting graph. If set to TRUE, the geometry column will be removed.
#' @param reverse A logical value indicating whether to reverse the graph after creating the DAG. If set to TRUE, the graph will be reversed.
#'
#' @return A data frame representing the Directed Acyclic Graph (DAG) of the network.
#'
#' @details It is highly recommended to include a 'divergence' column in the input 'x' data frame if the resulting graph will be used for locating cycles within the graph.
#'
#' @examples
#' @seealso \code{\link{nhdplusTools::get_tocomid}}, \code{\link{nhdplusTools::make_node_topology}}, \code{\link{reverse_graph}}
create_dag <- function(
    x,
    start     = NULL,
    add       = TRUE,
    drop_geom = FALSE,
    reverse   = FALSE
) {
  
  # if a start ID is given, trim network down to
  # only nodes upstream of the ID (including the ID itself)
  if (!is.null(start)) {
    # 
    # message("Trimming network to start at COMID: ", start, " looking ", direction, "...")
    
    message("Trimming network to start at COMID: ", start)
    # if (reverse) {
    #   # Downstream COMIDs
    #   dd_comids <- nhdplusTools::get_DD(x, start)
    #   x         <- dplyr::filter(x, comid %in% dd_comids) 
    # } else {
    # upstream COMIDs
    ut_comids <- nhdplusTools::get_UT(x, start)
    x <- dplyr::filter(x, comid %in% ut_comids) 
    
    # }
    
  }
  
  # make lower case names
  names(x) <- tolower(names(x))
  
  # if an "id" column exists, remove it
  if("id" %in% names(x)) {
    # if(any(grepl("id", names(x)))) {
    x <- dplyr::select(x, -id)
  }
  
  # if divergence column is given, make a dendritic network node topology
  if("divergence" %in% names(x)) {
    dendritic = TRUE
  } else {
    dendritic = FALSE
  }
  # x$divergxencec = 0
  
  # get dendritic network
  network <-  dplyr::select(
    nhdplusTools::get_tocomid(
      x,
      # dplyr::select(x, comid, fromnode, tonode, hydroseq,
      # streamleve, streamorde, streamcalc, divergence),
      return_dendritic = dendritic,
      # return_dendritic = TRUE,
      add              = TRUE, 
      remove_coastal   = FALSE
    ),
    -fromnode, -tonode
  )
  
  # if(flag) {
  #   x[x$comid %in% network[duplicated(network$comid), ]$comid, ]$divergence = 2
  #   network[duplicated(network$comid), ]$divergence <- 2
  # }
  
  # if NOT dendritic, remove duplicates and return node topology
  if(!dendritic) {
    # message("NON DENDRITIC")
    # remove duplicates
    network <- network[!duplicated(network$comid), ]
    
    # make node topology
    network <- nhdplusTools::make_node_topology(
      network,
      add = TRUE
    )
    
  } else {
    
    # message("DENDRITIC")
    # get divergent tocomids
    div <- nhdplusTools::get_tocomid(
      x,
      # dplyr::select(x, comid, fromnode, tonode, hydroseq,
      # streamleve, streamorde, streamcalc, divergence),
      return_dendritic = FALSE,
      remove_coastal   = FALSE
    )
    
    # divergences w/ unique fromids in x but don't in div
    div <- div[div$tocomid %in% x$comid[x$divergence == 2], ]
    
    network <- nhdplusTools::make_node_topology(
      network,
      div,
      add = TRUE
    )
    
  }
  
  # whether to add back original columns or cut them out
  if(!add) {
    # message("NOT ADDING TO ORIGINAL DATA")
    # only return relevent columns, no divergence is dendritic = FALSE
    if(!dendritic) {
      # message("NO DIV COLUMN")
      network <- dplyr::select(network, comid, tocomid, fromnode, tonode)
    } else {
      # message("HAS DIV COLUMN")
      network <- dplyr::select(network, comid, tocomid, fromnode, tonode, divergence)
    }
  }
  
  # drop geometry
  if(drop_geom) {
    # message("DROPPING GEOM")
    # drop geometry
    network <- sf::st_drop_geometry(network)
  }
  
  # # reverse graph
  # if(reverse) {
  #   message("Reversing graph")
  #   network <- reverse_graph(network)
  # }
  
  return(network)
}


# Reverse a graph (output of create_dag() function), not too sure how bulletproof this logic is,
# Likely buggy and/or unneccassary
reverse_graph <- function(df) {
  
  # data to reattach after reversal
  keeps <- dplyr::select(df, -tocomid, -fromnode, -tonode)
  
  # preserve orgiinal names
  orig_names <- names(df)
  
  # find most upstream node by hydroseq
  most_upstream_node <- 
    df %>% 
    dplyr::filter(hydroseq == max(df$hydroseq)) %>% 
    .$tonode
  
  # drop geometries
  df <- 
    df %>%
    sf::st_drop_geometry() %>% 
    dplyr::select(comid, tocomid, fromnode, tonode)
  
  # Create new columns for reversed node IDs and edge IDs
  df$reversed_fromnode <- NA
  df$reversed_tonode <- NA
  df$reversed_comid <- NA
  df$reversed_tocomid <- NA
  
  # Step 3: Reverse the direction of nodes
  for (i in 1:nrow(df)) {
    # i = 1
    if (df[i, ]$fromnode  == 0) {
      
      df[i, ]$reversed_fromnode <- 0
      df[i, ]$reversed_tonode <- 0
      
    } else {
      new_comid    <- df[i, ]$comid
      new_tocomid  <- df[i, ]$tocomid
      
      new_from     <- df[i, ]$fromnode
      new_to       <- df[i, ]$tonode
      
      
      df[i, ]$fromnode       <- new_to
      df[i, ]$tonode         <- new_from   
      
      df[i, ]$comid          <- new_tocomid
      df[i, ]$tocomid        <- new_comid   
      
      
      df[i, ]$reversed_fromnode  <- new_from
      df[i, ]$reversed_tonode    <- new_to
      df[i, ]$reversed_comid     <- new_comid
      df[i, ]$reversed_tocomid   <- new_tocomid
      
    }
  }
  
  # set tonode of most upstream node to 0
  df[df$reversed_fromnode == most_upstream_node, ]$tonode <- 0
  df[df$reversed_fromnode == most_upstream_node, ]$tocomid <- 0
  
  df[df$reversed_fromnode != most_upstream_node & df$comid == 0, ]$comid <- df[df$reversed_fromnode != most_upstream_node & 
                                                                                 df$comid == 0, ]$tocomid
  
  # rejoin data with other original columns in keeps
  df <-
    df %>% 
    dplyr::left_join(
      keeps, 
      by = "comid"
    ) %>% 
    dplyr::select( all_of(orig_names)) %>% 
    sf::st_as_sf()
  
  return(df)
}

#' Reverse the topology of a Directed Acyclic Graph
#' Given a DAG dataframe with comid, tocomid, fromnode, and tonode, attributes, this function will reverese the topology and start the graph at the most upstream/downstream node. Internal function to be used with create_dag().
#' @param graph dataframe or sf dataframe with comid, tocomid, fromnode, and tonode attributes
#'
#' @return dataframe with originally node topology in reverse
#' @export
#'
#' @examples
reverse_dag <- function(graph, start = NULL) {
  
  start = "8898002"
  
  ds_graph <- dplyr::select(
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(graph,
                      comid,
                      tocomid,
                      fromnode,
                      tonode
        )
      ),
      sf::st_drop_geometry(
        dplyr::select(
          graph, 
          new_comid = tocomid,
          new_tocomid = comid,
          new_fromnode = tonode,
          new_tonode = fromnode
        )
      ),
      by = c("fromnode" = "new_fromnode", "comid" = "new_comid")
    ),
    comid = tocomid,
    tocomid = comid,
    fromnode, tonode, new_tocomid, new_tonode
    # -tonode, -tocomid
  )
  
  ds_graph[ds_graph$tocomid == start, ]$tocomid <- 0
  ds_graph[ds_graph$tocomid == start, ]$new_tonode  <- 0
  
  # set start of graph tcomid and tonode to 0
  ds_graph[is.na(ds_graph$tonode),]$tocomid <- 0
  ds_graph[is.na(ds_graph$tonode),]$tonode  <- 0
  
  # reverse comid/tocomid fromnode/tonode topology
  ds_graph <- dplyr::select(
    dplyr::select(
      dplyr::left_join(
        sf::st_drop_geometry(
          dplyr::select(graph,
                        comid,
                        tocomid,
                        fromnode,
                        tonode
          )
        ),
        sf::st_drop_geometry(
          dplyr::select(
            graph, 
            new_comid = tocomid,
            new_tocomid = comid,
            new_fromnode = tonode,
            new_tonode = fromnode
          )
        ),
        by = c("fromnode" = "new_fromnode", "comid" = "new_comid")
      ),
      -tonode, -tocomid
    ),
    comid,
    tocomid = new_tocomid,
    fromnode, 
    tonode = new_tonode
  )
  
  # set start of graph tcomid and tonode to 0
  ds_graph[is.na(ds_graph$tonode),]$tocomid <- 0
  ds_graph[is.na(ds_graph$tonode),]$tonode  <- 0
  
  # rejoin reversed graph with original dataset
  ds_graph <- dplyr::relocate(
    dplyr::left_join(
      dplyr::select(
        graph, 
        -tocomid, 
        -fromnode,
        -tonode
      ),
      ds_graph,
      by = "comid"
    ),
    comid, tocomid, fromnode, tonode
  )
  
  return(ds_graph)
  
}

#' Convert a Directed Acyclic Graph (DAG) into an Undirected Graph
#'
#' This function takes a Directed Acyclic Graph (DAG), typically the output of the `create_dag()` function,
#' and converts it into an undirected graph. The purpose of this conversion is to prepare the graph for
#' cycle and braid detection functions, as they usually require an undirected graph.
#'
#' @param graph A data frame representing the Directed Acyclic Graph (DAG) with columns 'fromnode' and 'tonode'.
#'
#' @return An undirected graph represented as a data frame with columns 'fromnode' and 'tonode'.
#'
#' @examples
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
make_undirected <- function(graph) {
  
  # get to and from nodes
  adj <- 
    graph %>% 
    # dplyr::select(comid, tocomid, fromnode, tonode) %>%
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
#' @param from_nodes 
#' @param to_nodes 
#' @param directed logical whether edge strings should be directed connection string ('->') or undirected connection string ('-'). If TRUE, edge strings will seperate nodes with a directed connection string ('->'). Default is TRUE.
#' @param ... Additional vectors (optional). Additional vectors should be mapped to the from node keys in the hashmap, must be the same length as from_nodes and to_nodes vectors.
#'
#' @return fastmap with from_nodes as keys and to_nodes as values in the hashmap
#' @export
#'
#' @examples
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
# network = network3
# # lower case names
# names(network) <- tolower(names(network))
# 
# # turn network into a directed graph
# dag <- create_dag(network)
# dag$comid
# dag2 <- sf::st_drop_geometry(dag)
# dag2 <- renode_circuits(dag2)
# 
# # make an undirected graph
# dag2 <- make_undirected(dag2)
# # dag$fromnode
# tm <- added_args(
#   from_nodes = dag2$fromnode, 
#   to_nodes   = dag2$tonode
#                  # divergence = dag$divergence,
#                  # streamcalc = dag$streamcalc,
#                  # so = dag$streamorde
#                  )
# 
# 
# tm2 <- make_topo_map(
#   from_nodes = dag$fromnode, 
#   to_nodes   = dag$tonode,
#   comid = dag$comid
#                  # divergence = dag$divergence,
#                  # streamcalc = dag$streamcalc,
#                  # so = dag$streamorde
# )
# plot(dag2$geometry)
# tmlist <- tm$as_list()
# tmlist2
# tmlist2 <- tm2$as_list()
# for (o in 1:length(tmlist2)) {
#   message("NEW FROM NODE = ", names(tmlist[o]))
#   message("ORIG FROM NODE = ", names(tmlist2[o]))
#   
#   message("names: ", names(tmlist[o]) == names(tmlist2[o]))
#   message("tonodes: ", paste0(c(tmlist[[o]]$to_node == tmlist2[[o]]$to_node), sep = ' - '))
#   message("edges: ", paste0(c(tmlist[[o]]$edge == tmlist2[[o]]$edge), sep = ' - '))
#   message("Count: ", paste0(c(tmlist[[o]]$count == tmlist2[[o]]$count), sep = ' - '))
#   message('=================')
#   
# }


# make_topo_map2 <- function(
#     from_nodes,
#     to_nodes,
#     from_ids = NULL,
#     to_ids   = NULL,
#     streamcalc = NULL,
#     streamorder = NULL,
#     divergence = NULL
# ) {
#   
#   
#   # make sure all inputs are characters for hashmap
#   from_nodes     <- as.character(from_nodes)
#   to_nodes       <- as.character(to_nodes)
#   from_ids       <- as.character(from_ids)
#   to_ids         <- as.character(to_ids)
#   streamcalc     <- as.character(streamcalc)
#   streamorder    <- as.character(streamorder)
#   divergence     <- as.character(divergence)
#   
#   # initialize hashmap
#   topo_map <- fastmap::fastmap()
#   
#   for (i in 1:length(from_nodes)) {
#     
#     # message("iteration ", i, "/", length(from_nodes))
#     
#     from_node <- from_nodes[i]
#     to_node   <- to_nodes[i]
#     from_id   <- from_ids[i]
#     to_id     <- to_ids[i]
#     sc        <- streamcalc[i]
#     so        <- streamorder[i]
#     div       <- divergence[i]
#     # uid       <- id[i]
#     
#     if (!topo_map$has(from_node)) {
#       # message(from_node, " NOT YET in map")
#       # message("Adding ", from_node, " to topo_map with value ", to_node)
#       
#       # edge string
#       edge_str <- paste0(from_node, "->", to_node)
#       
#       # Create a new entry in topo_map
#       if (!is.null(from_ids) & !is.null(to_ids)) {
#         # if (!is.null(id)) {
#         topo_map$set(from_node, list(
#           to_node     = to_node,
#           # edge       = edge_str,
#           # id     = uid
#           to_id       = to_id,
#           from_id     = from_id,
#           edge        = edge_str,
#           streamcalc  = sc,
#           streamorder = so,
#           div         = div,
#           count       = 1
#         )
#         )
#       } else {
#         topo_map$set(from_node, list(
#           to_node     = to_node,
#           edge        = edge_str,
#           streamcalc  = sc,
#           streamorder = so,
#           div         = div,
#           count       = 1
#         )
#         )
#       }
#       
#     } else {
#       # message("*****")
#       # message(from_node, " ALREADY IN MAP --> UPDATING")
#       # # message("UPDATING ", from_node," value from ", 
#       # #         topo_map$get(from_node), "to\n", 
#       # #         c(topo_map$get(from_node), to_node))
#       # message("*****")
#       
#       # new edge str to add to map
#       edge_str = paste0(from_node, "->", to_node)
#       
#       # Update the existing entry in topo_map
#       if (!is.null(from_ids) & !is.null(to_ids)) {
#         topo_map$set(from_node, list(
#           to_node     = c(topo_map$get(from_node)$to_node, to_node),
#           # edge       = c(topo_map$get(from_node)$edge, edge_str),
#           to_id       = c(topo_map$get(from_node)$to_id, to_id),
#           from_id     = c(topo_map$get(from_node)$from_id),
#           edge        = c(topo_map$get(from_node)$edge, edge_str),
#           streamcalc  = c(topo_map$get(from_node)$streamcalc, sc),
#           streamorder = c(topo_map$get(from_node)$streamorder, so),
#           div         = c(topo_map$get(from_node)$div, div),
#           count       = c(topo_map$get(from_node)$count + 1)
#         ))
#         
#       } else {
#         topo_map$set(from_node, list(
#           to_node     = c(topo_map$get(from_node)$to_node, to_node),
#           edge        = c(topo_map$get(from_node)$edge, edge_str),
#           streamcalc  = c(topo_map$get(from_node)$streamcalc, sc),
#           streamorder = c(topo_map$get(from_node)$streamorder, so),
#           div         = c(topo_map$get(from_node)$div, div),
#           count       = c(topo_map$get(from_node)$count + 1)
#         ))
#       }
#     }
#     # message("=============================")
#   }
#   
#   return(topo_map)
#   
# }


#' Convert a COMID to its corresponding fromnode in a Network
#'
#' This function takes a Network of NHDPlus flowlines represented as a data frame with columns 'comid',
#' 'tocomid', 'fromnode', and 'tonode'. Given a COMID character string, it returns the corresponding fromnode
#' in the network. This function is useful for identifying the starting node when performing cycle detection or braid detection.
#'
#' @param network A data frame representing the network of NHDPlus flowlines with columns 'comid', 'tocomid', 'fromnode', and 'tonode'.
#' @param comid The COMID character string to find its corresponding fromnode in the network. If NULL, the function selects a suitable fromnode.
#' @return The fromnode corresponding to the given COMID in the network.
#'
#' @examples
#' network <- data.frame(
#'   comid = c("A", "B", "C", "D"),
#'   tocomid = c("B", "C", "D", "E"),
#'   fromnode = c("X", "Y", "Z", "W"),
#'   tonode = c("Y", "Z", "W", "X")
#' )
#' fromnode <- comid_to_node(network, comid = "B")
#' 
#' @seealso \code{\link{make_undirected}}, \code{\link{find_cycles}}
comid_to_node <- function(
    network, 
    comid   = NULL
) {
  
  # if a start COMID is given
  if(!is.null(comid)) {
    
    # if start COMID is NOT found in the network, throw error
    if(!comid %in% network$comid) { stop(comid, " COMID not found in network")}
    
    # if(verbose) {
    #   message("Starting braid detection at COMID: ", comid)
    # }
    
    # find fromnode of comid to start cycle detection at
    start_node <- as.character(network$fromnode[network$comid == comid])
    
  } else {
    
    if ("0" %in% network$tocomid) {
      
      # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
      start_node <- as.character(network$fromnode[network$tocomid == "0"])[1]
      
      # if(verbose) {
      #   message("Starting braid detection at COMID: ", network$comid[network$tocomid == "0"][1])
      # }
      
    } else {
      
      # if(verbose) {
      #   message("No 'tocomid' value equal to '0' found",
      #           "\nStarting braid detection at COMID: ",
      #           network$comid[network$fromnode == as.character(network$fromnode)[1]]
      #   )
      # }
      start_node <- as.character(network$fromnode)[1]
    }
  }
  
  return(start_node)
}

#' Artificially create cycles from portions of a network/graph that contain a circuit
#' (2 nodes and 2 unique edges between those nodes).
#'
#' This function is necessary because circuits do NOT get recognized by cycle detection
#' DFS implementation. By applying this function, we can artificially create cycles
#' from circuits that SHOULD represent a braided section of a river.
#'
#' @param graph A data frame representing the network/graph with columns 'fromnode', 'tonode',
#'              'comid', and 'tocomid'.
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' @return A modified data frame representing the network/graph with artificial cycles created
#'         from circuits.
#'
#' @examples
#' graph <- data.frame(
#'   fromnode = c(1, 2, 2, 3),
#'   tonode = c(2, 3, 3, 4),
#'   comid = c("A", "B", "C", "D"),
#'   tocomid = c("B", "C", "D", "E")
#' )
#' renode_circuits(graph)
renode_circuits <- function(graph, verbose = FALSE) {
  
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
  
  # get one of the comids for single_cycles and reworked node topology
  renodes <- 
    single_cycles %>%
    dplyr::group_by(fromnode, tonode) %>% 
    dplyr::slice(1) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(comid, tocomid, fromnode, tonode)
  
  # get the number of new nodes needed that don't overlap with current unique nodes in graph
  new_nodes <- seq(max(unodes)+1,  max(unodes) + nrow(renodes))
  
  # Inject a new fake node between each unique edge
  # to artificially create a cycle that can be detected in DFS function
  renodes <- dplyr::bind_rows(
    data.frame(
      comid = as.character(renodes$comid),
      # tocomid = tmp$comid,
      tocomid = paste0(renodes$comid, "_v2"),
      fromnode = renodes$fromnode,
      tonode = new_nodes
    ),
    data.frame(
      # comid = tmp$comid,
      comid = paste0(renodes$comid, "_v2"),
      tocomid = as.character(renodes$tocomid),
      fromnode = new_nodes,
      tonode = renodes$tonode
    ),
    dplyr::mutate(
      dplyr::select(
        dplyr::filter(
          single_cycles, !comid %in% renodes$comid
        ),
        comid, tocomid, fromnode, tonode),
      comid   = as.character(comid),
      tocomid = as.character(tocomid)
    )
  )
  
  # remove original versions of single cycle nodes and replace with reworked nodes
  regraph <- 
    dplyr::bind_rows(
      renodes, 
      dplyr::mutate(
        dplyr::filter(
          dplyr::select(graph, 
                        comid, tocomid, fromnode, tonode),
          !fromnode %in% single_cycles$fromnode
        ),
        comid   = as.character(comid),
        tocomid = as.character(tocomid)
      )
    )
  
  return(regraph)
}

# convert edge string "->" to just a "to_node" value
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

#' Find cycles in a graph/network starting from a specified node or the first available node
#'
#' This function performs a depth-first search (DFS) on a graph/network to find cycles. It starts
#' the search from a specified node or the first available node. If a start node is provided, it
#' checks if the node exists in the graph and throws an error if not found. The function returns
#' the cycles found as a list or a data frame with node and cycle IDs.
#'
#' @param graph A data frame representing the graph/network with columns 'fromnode', 'tonode',
#'              'comid', and 'tocomid'. Graph must be converted to an undirected graph via make_undirected() 
#' @param start The starting node for the cycle detection. Default is NULL.
#' @param return_as The format to return the cycles. Possible values are "list" (default) or "dataframe".
#' @param edge logicl, whether to return edges (node pairings) or nodes. If TRUE, 'node-node' character string will be returned, otherwise a starting node is returned. Default is FALSE. 
#' @param wide logical, whether to return wide or long dataframe. Default is TRUE 
#' @param verbose Logical indicating whether to print verbose messages during the process. Default is FALSE.
#'
#' @return A list of cycles found in the graph/network or a data frame with node and cycle IDs.
#'
#' @examples
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
find_cycles <- function(
    graph,
    start     = NULL,
    return_as = "list",
    edge      = FALSE,
    wide      = TRUE,
    verbose   = FALSE
) {
  # graph     = undir
  # start     = start_node
  # return_as = "list"
  # # return_as = "dataframe",
  # # return_as = return_as,
  # verbose   = verbose
  # edge = TRUE
  
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
  
  # topo_map$as_list()
  
  # keep track of visited nodes
  visits <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  visits$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
           function(i){ 0 }),
    unique(c(graph$fromnode, graph$tonode))
    # lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), function(i){ 0 }),
    # unique(c(ungraph$fromnode, ungraph$tonode))
  )
  )
  # visits$as_list()
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
                              # lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), 
                              # function(i){ 0 }),
                              # unique(c(ungraph$fromnode, ungraph$tonode))
                                )
                              )
  
  # cycle number count
  count <- 1
  
  # # Depth first search function for marking cycles in undirected graph
  # dfs <- function(n, p) {
  #   # message("node: ", n)
  #   # message("node ", n, " visit value: ", visits$get(n))
  #   
  #   # node has been fully visited
  #   if (visits$get(n) == 2) {
  #     return()
  #   }
  #   
  #   # node is in process of visitation
  #   if (visits$get(n) == 1) {
  #     
  #     # # map/list to store current level nodes
  #     v <- vctrs::vec_c()
  #     # v    <- list()
  #     
  #     curr <- p
  #     v <- vctrs::vec_c(v, vctrs::vec_c(curr))
  #     # v    <- list(v, list(curr))
  #     
  #     # while parent of n is not equal to itself
  #     while (curr != n) {
  #       
  #       # update curr value to previous values of curr
  #       curr <- prev_nodes$get(curr)
  #       
  #       # add cycle vertices to v
  #       v <- vctrs::vec_c(v, vctrs::vec_c(curr))
  #       # v = list(v, list(curr))
  #     }
  #     
  #     # add vertices for current cycle number to cycles hashmap 
  #     cycles$set(as.character(count), v)
  #     
  #     # incremenet count
  #     count <<-  count + 1
  #     
  #     return()
  #     
  #   }
  #   
  #   # set parents of node n 
  #   prev_nodes$set(n, p)
  #   
  #   # mark node as partially visited
  #   visits$set(n, 1)
  #   
  #   # get neighbors of node n
  #   neighbors <- topo_map$get(n)$to_node
  #   
  #   # message("Neighbors of node: ",  n, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
  #   
  #   # iterate through neighbors of node n
  #   for (vert in neighbors) {
  #     
  #     if(vert == prev_nodes$get(n)) {
  #       # message("vert ", vert, " equals parent ", prev_nodes$get(n), " SKIPPING")
  #       next
  #     }
  #     
  #     # message("---- RUNNING DFS ON ----")
  #     # message("---> vert = ", vert)
  #     # message("---> n = ", n)
  #     
  #     dfs(vert, n)
  #     
  #     # message("***********")
  #   }
  #   # mark node as fully visited
  #   visits$set(n, 2)
  #   
  #   # message("=============================")
  # }
  
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
      
      # # map/list to store current level nodes
      v <- vctrs::vec_c()
      # v    <- list()
      
      curr <- p
      # v <- vctrs::vec_c(v, vctrs::vec_c(curr))
      
      # keep track of fromnodes
      if(!edge) {
        
        # fromnodes vector
        v <- vctrs::vec_c(v, vctrs::vec_c(curr))
        
      } else {
        # keep track of edges
        v <- vctrs::vec_c(v,
                          vctrs::vec_c(
                            topo_map$get(curr)$edge
                          )
        )
      }
      # v <- vctrs::vec_c(v,vctrs::vec_c( topo_map$get(curr)$edge ) )
      # v    <- list(v, list(curr))
      
      # while parent of n is not equal to itself
      while (curr != n) {
        
        # update curr value to previous values of curr
        
        # make edge string
        if(edge) {
          # v <- vctrs::vec_c(v, vctrs::vec_c(curr))
          edges <- paste0(prev_nodes$get(curr), "-", curr)
        } 
        # edges <- topo_map$get(curr)$edge
        # edges <- paste0(prev_nodes$get(curr), "-", curr)
        
        curr  <- prev_nodes$get(curr)
        
        # prev_nodes$as_list()
        # edges <- prev_nodes$get(curr)$edge
        
        # add cycle vertices to v
        # keep track of fromnodes
        if(!edge) {
          
          # fromnodes vector
          v <- vctrs::vec_c(v, vctrs::vec_c(curr))
          
        } else {
          # keep track of edges
          v <- vctrs::vec_c(v, vctrs::vec_c(edges))
          # v <- vctrs::vec_c(v, vctrs::vec_c(curr)
        }
        # # v <- vctrs::vec_c(v, vctrs::vec_c(curr))
        # v <- vctrs::vec_c(v,
        #                   # vctrs::vec_c(curr)
        #                   vctrs::vec_c(edges)
        #                   # vctrs::vec_c(topo_map$get(curr))
        #                   )
        # v = list(v, list(curr))
      }
      
      # add vertices for current cycle number to cycles hashmap 
      cycles$set(as.character(count), v)
      
      # incremenet count
      count <<-  count + 1
      
      return()
      
    }
    
    # set parents of node n 
    prev_nodes$set(n, p)
    
    # mark node as partially visited
    visits$set(n, 1)
    
    # get neighbors of node n
    neighbors <- topo_map$get(n)$to_node
    # neighbors <- edge_to_node(topo_map$get(n)$edge)
    
    # edge_to_node(topo_map$get(n)$edge)
    # topo_map$get("85")
    # topo_map$get("85")$to_node
    # topo_map$get("85")$edge
    # edge_to_node(topo_map$get("85")$edge)
    # topo_map$as_list()
    
    # message("Neighbors of node: ",  n, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    # iterate through neighbors of node n
    for (vert in neighbors) {
      
      if(vert == prev_nodes$get(n)) {
        # message("vert ", vert, " equals parent ", prev_nodes$get(n), " SKIPPING")
        next
      }
      
      # message("---- RUNNING DFS ON ----")
      # message("---> vert = ", vert)
      # message("---> n = ", n)
      
      dfs(vert, n)
      
      # message("***********")
    }
    # mark node as fully visited
    visits$set(n, 2)
    
    # message("=============================")
  }
  
  # n = start_node
  # p ="no_previous"
  
  # Start DFS on a node
  dfs(start_node, "no_previous")
  
  # cycles$as_list()
  # es <- cycles$get("22")
  # es
  # edge_tos <- edge_to_node(es, to = TRUE, directed = FALSE)
  # edge_tos
  # edge_froms <- edge_to_node(es, to = FALSE,  directed = FALSE)
  # edge_froms
  # 
  # edge_or <- dag_sf %>% 
  #   dplyr::filter(fromnode %in% edge_froms |tonode %in% edge_tos )
  # 
  # edge_and <-  dag_sf %>% 
  #   dplyr::filter( fromnode %in% edge_froms, tonode %in% edge_tos)
  # 
  # mapview::mapview(dag_sf) +
  #   mapview::mapview(edge_or, color = "green") +
  #   mapview::mapview(edge_and, color = "red") 
  # edge_tos %in% dag_sf$fromnode 

  
  # if(cycles$size() == 0 & is.null(single_braids)) {
  if(cycles$size() == 0) {
    message("No cycles found, returning NULL")
    return(NULL)
  }
  
  # extract cycles hashmap as a list
  result <- cycles$as_list()
  
  # if data should be returned as a 2 column dataframe with node and cycle_id
  if(return_as == "dataframe") {
    
    if(edge) {
      
      result <- lapply(1:length(result), function(k) {
          # get IDs of each node
            dplyr::relocate(
              dplyr::mutate(
                data.frame(
                  edge     = result[[k]],
                  cycle_id = names(result[k])
                  # node     = cycles$as_list()[[k]],
                  # cycle_id = names(cycles$as_list()[k])
                  ),
                fromnode = edge_to_node(edge, to = FALSE,  directed = FALSE),
                tonode   = edge_to_node(edge, to = TRUE,  directed = FALSE)
                ),
              edge, fromnode, tonode, cycle_id
              )
            })  %>% 
          dplyr::bind_rows() 
      
      if(wide) {
        
        result <- 
          result %>%
          dplyr::group_by(edge, fromnode, tonode) %>%
          # dplyr::group_by(edge) %>% 
          dplyr::summarise(
            cycle_id = paste0(cycle_id, collapse = ", ")
          ) %>%
          dplyr::ungroup()
      }
    } else {
      
      result <- lapply(1:length(result), function(k) {
        # get IDs of each node
        data.frame(
          node     = result[[k]],
          cycle_id = names(result[k])
          # node     = cycles$as_list()[[k]],
          # cycle_id = names(cycles$as_list()[k])
        )
      })  %>% 
        dplyr::bind_rows() 
      
      if(wide) {
        
        result <- 
          result %>%
          dplyr::group_by(node) %>%
          dplyr::summarise(
            cycle_id = paste0(cycle_id, collapse = ", ")
          ) %>%
          dplyr::ungroup()
      }
    }
  }
  
  return(result)
  
}


# network <- data.frame(
#   comid = c("A", "B", "B", "C", "C", "D", "E", "F", "F", "F"),
#   fromnode = c("X", "Y", "Y", "Z", "Z", "W", "X", "Y", "Y", "Y"),
#   tonode = c("Y", "Y", "Z", "Z", "W", "X", "Y", "Z", "Z", "W")
# )
# single_cycles <- get_single_cycles(network)
# 
# network <- data.frame(
#   comid = c(100, 101, 102, 103),
#   fromnode = c(1, 2, 3, 5),
#   tonode = c(2, 3, 4, 3)
#   # comid = c("A", "B", "C", "D"),
#   # tocomid = c("B", "C", "D", "C")
#   # tocomid = c( 101, 102, 103, 104)
#   )
# 
# create_dag(network)
# 
# single_cycles <- get_single_cycles(create_dag(network))

#' Locate single braided sections of a river network
#'
#' This function locates single braided sections of a river network. These sections are defined as sections of rivers that start and end at the same node but have different COMIDs. These braids are not detected by the depth-first search function used in `find_braids()`, so this function provides an alternative method to identify and extract the braided segments of the river network.
#'
#' @param network A data frame representing the river network with columns 'comid', 'fromnode', and 'tonode'.
#' @param return_as A character string indicating the desired format of the output. Possible values are "list" (default) and "data.frame". If "list", the function returns a list of cycles where each list element contains a vector of COMIDs. If "data.frame", the function returns a data frame with columns 'cycle_id' and 'comid', where 'cycle_id' represents the ID of the cycle and 'comid' is a comma-separated string of the associated COMIDs.
#'
#' @return A list or data frame (depending on the value of 'return_as') containing the single braided sections of the river network. Each cycle is identified by a unique cycle ID, and the associated COMIDs are provided for each cycle.
#'
#' @examples

#' @seealso \code{\link{find_braids}}, \code{\link{comid_to_node}}
get_single_cycles <- function(
    network, 
    return_as = "list"
) {
  
  single_cycles <-
    network %>%
    # dag %>% 
    dplyr::group_by(fromnode, tonode) %>% 
    dplyr::add_count() %>% 
    dplyr::filter(n > 1) %>% 
    dplyr::mutate(
      cycle_id = dplyr::cur_group_id()
      # braid_id = paste0("braid_", dplyr::cur_group_id())
      # braid_id = dplyr::cur_group_id()
    ) %>% 
    dplyr::relocate(cycle_id) %>% 
    dplyr::ungroup() %>% 
    dplyr::select(comid, cycle_id) 
  # dplyr::select(comid, node = fromnode, cycle_id) 
  
  # if no single cycles are found, return NULL
  if(nrow(single_cycles) == 0) {
    
    return(NULL)
    
  }
  
  if(return_as == "list") {
    
    single_cycles <- 
      single_cycles %>%  
      dplyr::group_by(cycle_id) %>% 
      dplyr::summarise(
        comid = paste0(comid, collapse = ", ")
        # node = paste0(fromnode, collapse = ", ")
      ) %>% 
      dplyr::ungroup()
    
    # create a list of cycles with each list element containing a vector of COMIDs
    single_cycles <- stats::setNames(
      lapply(strsplit(single_cycles$comid, ", "), as.integer),
      single_cycles$cycle_id
      # as.integer(strsplit(single_braids$comid, ", ")), 
      # single_braids$braid_id
    )
  }
  return(single_cycles)
}

# Identify the index of big multibraids 
# x is a list of vectors with each list element containing a vector COMIDs representing a braid
id_multibraids <- function(x) {
  
  # overlapped braids
  overlaps <- find_overlaps(
    lst            = x,
    no_overlap_val = FALSE, 
    rm_no_overlap  = FALSE, 
    verbose        = FALSE
  ) 
  
  big_cycles <- sort(
    unique(
      Reduce( 
        c, overlaps )
    )
  )
  
  # remove 0 values
  big_cycles <- big_cycles[big_cycles != 0]
  
  return(big_cycles)
  
}

# seperate multibraided braids in a network object
# network, sf linestring object with comid column
# x is a list of vectors with each list element containing a vector COMIDs representing a braid
seperate_braids <- function(
    network, 
    x
    ) {
  
  # overlapped braids
  overlaps <- find_overlaps(
    lst            = x,
    no_overlap_val = FALSE,
    rm_no_overlap  = FALSE,
    verbose        = FALSE
  )
  
  # multibraids ID index
  multis <- id_multibraids(x)
  
  for(j in 1:length(multis)) {
    # j = 1
    # big cycle iteration which is a cycle that contains one or more OTHER cycles
    cyc = multis[j]
    
    # comids for multibraid
    coms <- x[[cyc]]
    
    # small braids that overlap with current big multicycle
    connects <- sapply(1:length(overlaps), function(k) {
      cyc %in% overlaps[[k]]
    })
    
    # small braids
    smalls <- x[connects]
    
    # multibraided linestrings
    multibraid <- dplyr::filter(network, comid %in% coms)
    # plot(multibraid$geometry)
    
    # suppressWarnings({
    suppressMessages({
      
    # convex hull points
    chull_pts <- 
      multibraid %>% 
      sf::st_combine() %>%
      smoothr::densify(n = 5) %>% 
      sf::st_cast("POINT") %>% 
      sf::st_as_sf()
    
    # plot(multibraid$geometry)
    # plot(chull_pts, add = T)
    
    # convex hull
    chull <- 
      chull_pts %>% 
      concaveman::concaveman(concavity = 1, length_threshold = 0) %>% 
      sf::st_zm()       
    
    
    # bounds <-    
    #   multibraid %>% 
    #   sf::st_union() %>%
    #   smoothr::densify(n = 5)  %>%
    #   sf::st_boundary() 
    #   # sf::st_cast("MULTILINESTRING")
    # plot(bounds, add =F)
    # all(sf::st_is_empty(bounds))
    # plot(multibraid$geometry, add = F)
    # plot(bounds, add = T)
    # plot(chull_pts$x, add = T)
    
     # points that interesect with convex hull
     pt_mask <-  c(sf::st_intersects(chull_pts, chull, sparse = FALSE))
     
     # remove touching points
     no_msk <- chull_pts[!pt_mask, ]
     
    # plot(no_msk$x, add = F)
    # plot(chull$polygons, add = T)
    # plot(multibraid$geometry, add = T)
     
    # keep only linestrings that touch points outside of convex hull
    # res <- sf::st_filter(multibraid, no_msk)
    # plot(res$geometry)
     
    # filter multibraid to just the linestrings that contain the outer points
    multibraid <- sf::st_filter(multibraid, no_msk, .predicate = st_contains)
   
    # comids for multibraid
    x[[cyc]] <- multibraid$comid
    
    })
    
    # nears <- multibraid[lengths(sf::st_intersects(multibraid, no_msk)) > 1, ]
    # plot(nears$geometry)
    
    # mapview::mapview(multibraid, color = "red") + 
    #   mapview::mapview(no_msk, col.Regions = "green") 
    # updated <- lapply(1:length(smalls), function(z) {
    # for (z in 1:length(smalls)) {
    #   small_braids <- dplyr::filter(multibraid, comid %in% smalls[[z]])
    #   # near <- sf::st_nearest_feature(chull, small_braids)
    #   x[[cyc]] <- c(x[[cyc]][!x[[cyc]] %in% smalls[[z]]], small_braids[near, ]$comid)
    # }
  }

  return(x)
}

# 
# find_braids <- function(
#     network,
#     start        = NULL,
#     return_as    = "list",
#     add          = FALSE,
#     nested       = TRUE,
#     verbose      = FALSE
# ) {
#   
#   # network = network_div
#   # start     = NULL
#   # return_as = "dataframe"
#   # add       = T
#   # relationship = "one-to-one"
#   # nested = TRUE
#   # unbraid = F
#   # verbose   = T
#   
#   # network   = net
#   # start     = NULL
#   # return_as = "dataframe"
#   # add       = T
#   # nested    = T
#   # # relationship = "one-to-one",
#   # # relationship = "one-to-many",
#   # untangle     = FALSE
#   # verbose      = FALSE
#   
#   # relationship = "one-to-many",
#   
#   # # check relationship argument is valid
#   # if(return_as == "dataframe") {
#   #   if(!relationship %in% c("one-to-one", "one-to-many")) {
#   #     stop("Invalid 'relationship' argument '", 
#   #          relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
#   #   }
#   # }
#   
#   # lower case names
#   names(network) <- tolower(names(network))
#   
#   # turn network into a directed graph
#   dag <- create_dag(network)
#   # dag_sf <- create_dag(network)
#   # graph <- create_dag(network)
#   
#   # get the fromnode associated with a given COMID 'start'
#   start_node <- comid_to_node(dag, start)
#   
#   if(verbose) {
#     message("Starting braid detection at ", 
#             ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
#   }
#   
#   # drop graph geometry
#   dag <- sf::st_drop_geometry(dag)
#   # graph <- sf::st_drop_geometry(graph)
#   
#   # stash comids and from IDs
#   comid_map <- dplyr::select(
#     # graph,
#     dag,
#     comid,
#     fromnode,
#     tonode
#   )
#   
#   # create artificial cycles from circuits in graph, doing this allows for 
#   # sections of river that form a circuit to be identfied as a cycle and thus a braid
#   # Questionable at this point, other option is the code below that uses 
#   # single_cycles() function (COMMENTED OUT CODE BELOW)
#   regraph <- renode_circuits(dag, verbose = FALSE)
#   
#   # ##### - GET_SINGLE_CYCLES CODE -
#   # # check for single cycles (2 nodes A,B that have unique edges)
#   # # (i.e. 2 comids w/ exact same fromnodes/tonodes)
#   # single_cycles <- get_single_cycles(dag)
#   # 
#   # # if cycles were found
#   # if(!is.null(cycles)) {
#   #   # match each fromnode with original COMIDs
#   #   braids <- sapply(1:length(cycles), function(k) {
#   #     # get COMIDs of each braid
#   #     comid_map[comid_map$fromnode %in% cycles[[k]], ]$comid
#   #   })
#   #   # if single cycles are found in network
#   #   if(!is.null(single_cycles)) {
#   #     braids <- c(braids, unname(single_cycles))
#   #   }
#   #   # if NO cycles were found
#   # } else {
#   #   # if single cycles are found in network
#   #   if(!is.null(single_cycles)) {
#   #     braids <- c(unname(single_cycles))
#   #   } else {
#   #     message("No braids found, returning NULL")
#   #     return(NULL)
#   #   }
#   # }
#   
#   # make an undirected graph
#   undir <- make_undirected(regraph)
#   
#   # find cycles in undirected graph (proxy for braids)
#   cycles <- find_cycles(
#                 graph     = undir,
#                 start     = start_node,
#                 return_as = "list",
#                 edge      = FALSE,
#                 wide      = TRUE,
#                 verbose   = verbose
#                 )
#   
#   # if(cycles$size() == 0 & is.null(single_braids)) {
#   if(is.null(cycles)) {
#     # message("No cycles found, returning NULL")
#     return(NULL)
#   }
# 
#   # remove added nodes from renode_circuits()
#   braids <- lapply(cycles, function(i) { 
#     i[i %in% comid_map$fromnode] 
#   })
#   
#   # # match fromnodes from comid_map to nodes identified in each cycle
#   braids <- lapply(1:length(braids), function(k) {
#     # get COMIDs of each braid
#     comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#       dplyr::filter(tonode %in% braids[[k]]) %>%
#       .$comid
#   })
#   
#   # set braid_id names
#   braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
#   
#   # # if multibraids should be untangled
#   # if(untangle) {
#   #   if(verbose) { message("Attempting to seperate multibraids...") }
#   #   braids <- seperate_braids(network, braids)
#   # }
#   
#   # if data should be returned as a 2 column dataframe with comid and braid_ids
#   if(return_as == "dataframe") {
#     
#     # get comids of multibraids
#     multibraids <- unique(Reduce(c, braids[id_multibraids(braids)]))
#     
#     # # get comids of multibraids
#     # multibraids <- id_multibraids(braids)
#     # mb_ids      <- unique(Reduce(c, braids[multibraids]))
#     
#     # format braids into a dataframe
#     braids <- lapply(1:length(braids), function(k) {
#     # braids_df <- lapply(1:length(braids), function(k) {
#       
#       # get COMIDs of each braid
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
#     # (braids$comid %in% multibraids) | braids$braid_id == "no_braid"
#     
#     # # set no_braid values to FALSE
#     # braids$is_multibraid <- ifelse(is.na(braids$is_multibraid), 
#     #                                      FALSE, braids$is_multibraid)
#     
#     # braids$is_multibraided <- braids$comid %in% mb_ids
#     
#     
#     # if braid data should be added to original data
#     if(add) {
#       
#       # join back with original data
#       braids <-
#         dplyr::mutate(
#           dplyr::left_join(
#             network,
#             braids,
#             by = "comid"
#           ),
#           braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#         )
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
#     # else {
#     #   if(relationship == "one-to-many") {
#     #     braids <- dplyr::group_by(braids, comid) %>% 
#     #       dplyr::summarise(dplyr::group_by(braids, comid),
#     #       braid_id = paste0(braid_id, collapse = ", ") ) %>% 
#     #       dplyr::ungroup() } }
#   }
#   
#   return(braids)
# }

#' find_braids3 <- function(
#'     network,
#'     start        = NULL,
#'     return_as    = "list",
#'     add          = FALSE,
#'     relationship = "one-to-many",
#'     unbraid      = TRUE,
#'     verbose      = FALSE
#' ) {
#'   
#'   # # # ---- BRAIDED SYSTEM TEST DATA ----
#'   # network = network_div2
#'   # start     = NULL
#'   # return_as = "list"
#'   # add       = FALSE
#'   # verbose   = T
#'   # add = FALSE
#'   # verbose = TRUE
#'   # return_as = "list"
#'   # return_as = "dataframe"
#'   # mapview::mapview(graph)
#'   # graph <- create_dag(network)
#'   # network = network3
#'   # plot(network$geometry)
#'   # network = network3
#'   
#'   network = network_div
#'   start     = NULL
#'   return_as = "dataframe"
#'   add       = T
#'   relationship = "one-to-one"
#'   unbraid = F
#'   verbose   = T
#'   
#' 
#'   
#'   # check relationship argument is valid
#'   if(return_as == "dataframe") {
#'     if(!relationship %in% c("one-to-one", "one-to-many")) {
#'       stop("Invalid 'relationship' argument '", relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
#'     }
#'     
#'   }
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # turn network into a directed graph
#'   dag <- create_dag(network)
#'   # dag_sf <- create_dag(network)
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
#'   regraph <- renode_circuits(dag, verbose = FALSE)
#'   
#'   ##### - GET_SINGLE_CYCLES CODE -
#'   # # check for single cycles (2 nodes A,B that have unique edges)
#'   # # (i.e. 2 comids w/ exact same fromnodes/tonodes)
#'   # single_cycles <- get_single_cycles(add_args, return_as)
#'   # 
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
#'     # return_as = "dataframe",
#'     # return_as = return_as,
#'     verbose   = verbose 
#'   )
#'   
#'   # mapview::mapview(dag_sf)
#'   
#'   # cycles_df <- find_cycles(
#'   #   graph     = undir,
#'   #   start     = start_node,
#'   #   return_as = "dataframe",
#'   #   verbose   = verbose
#'   # )
#'   
#'   # if(cycles$size() == 0 & is.null(single_braids)) {
#'   if(is.null(cycles)) {
#'     # message("No cycles found, returning NULL")
#'     return(NULL)
#'     # return(network)
#'   }
#'   
#'   # cycles <- dplyr::filter(b, braid_id == ubraids[i]) %>% 
#'   #   .$comid
#'   
#'   # remove added nodes from renode_circuits()
#'   braids <- lapply(cycles, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # # # remove added nodes from renode_circuits()
#'   # braid_nodes <- lapply(cycles, function(i) {
#'   #   i[i %in% comid_map$fromnode]
#'   # })
#' 
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   # braids2 <- lapply(1:length(braids), function(k) {
#'   #   # get COMIDs of each braid
#'   #   comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'   #     dplyr::filter(tonode %in% braids[[k]])})
#'   # braid_index <- 1:length(braids)
#'   # # for (i in 1:length(braids)) {
#'   # dup_cycles <- lapply(1:length(braids), function(i) {
#'   #           subs <- sapply(braid_index[braid_index != i], function(k) {
#'   #             if(all(braids[[i]] %in% braids[[k]])) {
#'   #               k
#'   #               } 
#'   #             }) %>% 
#'   #             Reduce(c, .)
#'   #   comid_map[comid_map$fromnode %in% unname(unlist(braids[subs])), ] %>% 
#'   #     dplyr::filter(tonode %in%   unname(unlist(braids[subs])))  %>% 
#'   #     make_undirected()
#'   #   }) 
#'   
#'   # ORIGINAL WORKING
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   braids <- lapply(1:length(braids), function(k) {
#'     # get COMIDs of each braid
#'     comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#'       dplyr::filter(tonode %in% braids[[k]]) %>%
#'       .$comid
#'     
#'     # comid_map[comid_map$fromnode %in% braids[[k]], ]$comid
#'     # dplyr::filter(comid_map, fromnode %in% braids[[k]])
#'   })
#'   
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   # braids <- lapply(1:length(braids), function(k) {
#'   #   # get COMIDs of each braid
#'   #   # tmp <- comid_map[comid_map$fromnode %in% braids[[k]], ]
#'   #   # k = 22
#'   #   
#'   #   tmp <- 
#'   #     comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'   #     dplyr::group_by(fromnode) %>% 
#'   #     dplyr::add_count() %>% 
#'   #     dplyr::rename(fromnode_n = n) %>% 
#'   #     dplyr::ungroup() %>% 
#'   #     dplyr::group_by(tonode) %>% 
#'   #     dplyr::add_count() %>% 
#'   #     dplyr::rename(tonode_n = n) %>% 
#'   #     dplyr::ungroup() %>% 
#'   #     dplyr::group_by(fromnode, tonode) %>% 
#'   #     dplyr::add_count() %>% 
#'   #     dplyr::rename(combo_n = n) %>% 
#'   #     dplyr::ungroup()
#'   #   dups <- 
#'   #     tmp %>% 
#'   #     dplyr::filter(combo_n != 1) %>% 
#'   #     .$comid
#'   #   picks <- 
#'   #     tmp %>%
#'   #     dplyr::filter(fromnode_n != 1) %>% 
#'   #     dplyr::group_by(fromnode) %>% 
#'   #     dplyr::slice_min(tonode_n, with_ties = FALSE) %>% 
#'   #     dplyr::ungroup() %>% 
#'   #     dplyr::select(-fromnode_n, -tonode_n) %>% 
#'   #     dplyr::bind_rows(      
#'   #       dplyr::select(
#'   #         dplyr::filter(tmp, 
#'   #                       fromnode_n == 1), 
#'   #         -fromnode_n, -tonode_n
#'   #       )
#'   #     )
#'   #   unique(c(picks$comid, dups))
#'   #   # comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'   #   #   dplyr::filter(tonode %in% braids[[k]]) %>% 
#'   #   #   .$comid
#'   # })
#'   
#'   
#' 
#'   # #### TEST ####
#'   # # braid_sf1 <- dag_sf %>% dplyr::filter(fromnode %in% c("2", "56", "58"))
#'   # braid_nodes
#'   # braids
#'   # braid_sf_small <- dplyr::filter(dag_sf, fromnode %in% c("5", "7"))
#'   # braid_sf_big <- dplyr::filter(dag_sf, fromnode %in% c("5", "7", "53", "8", "54"))
#'   # braid_sf_cycbig <- dplyr::filter(dag_sf, comid %in%   braids[[21]])
#'   # 
#'   # # 3558470 3558466 3558484 3558480 3558482 3558472 3558464 3558486 3558468
#'   # # 3558466 3558484 3558480 3558482 3558472 3558486
#'   # 
#'   # braid_sf_big$comid
#'   # braids[[21]]
#'   # mapview::mapview(dag_sf) + 
#'   #   mapview::mapview(braid_sf_big, color = "red") + 
#'   #   mapview::mapview(braid_sf_small, color = "green") +
#'   #   mapview::mapview(braid_sf_cycbig, color = "green")
#'   # #### TEST ####
#'   # # find_overlaps(braids, rm_no_overlap = T)
#'   # braids2 <- seperate_braids3(braids, network)
#'   # 
#'   # if(unbraid) {
#'   #   # seperate braids with duplicates into individual parts
#'   #   braids <- seperate_braids2(braids)
#'   # }
#'   
#'   # find_overlaps(braids)
#'   # find_overlaps(braids, rm_no_overlap = T)
#'   
#'   # set braid_id names
#'   braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
#'   # braids <- stats::setNames(braids, paste0("braid_",  1:length(cycles)))
#'   # braids <- stats::setNames(braids, paste0("braid_", names(cycles)))
#'   # braids2 <- stats::setNames(braids2, paste0("braid_", names(cycles)))
#'   
#'   braids[[5]]
#'   parse_braids <- function(x, net) {
#'     # sf::sf_use_s2(FALSE)
#'     # cycle_list <- braids
#'     # net <- network
#'     # x = braids
#'     # net = network
#'     # cmap <- comid_map
#'     
#'     find_overlaps(
#'       cycle_list
#'     )
#'    
#'     # overlapped braids
#'     overlaps <- find_overlaps(
#'       lst            = x,
#'       no_overlap_val = FALSE, 
#'       rm_no_overlap  = FALSE, 
#'       verbose        = FALSE
#'     ) 
#'     
#'     big_cycles <- 
#'       overlaps %>% 
#'       Reduce(c, . ) %>% 
#'       unique() %>% 
#'       sort()
#'     
#'     # remove 0 values
#'     big_cycles <- big_cycles[big_cycles != 0]
#'     
#'     # overlaps[Reduce(c, overlaps) != 0]
#'     
#'     # while(!is.null(overlaps)) {
#'     
#'     for(j in 1:length(big_cycles)) {
#'   
#'       
#'       # big cycle iteration which is a cycle that contains one or more OTHER cycles
#'       cyc = big_cycles[j]
#'       
#'       # comids for multibraid
#'       coms <- x[[cyc]]
#'       
#'       connects <- sapply(1:length(overlaps), function(k) {
#'         # k = 4
#'         cyc %in% overlaps[[k]]
#'       })
#'       
#'       overlaps
#'       smalls <- x[connects]
#'       # smalls <- x[connects]
#'       
#'       
#'             t = st_combine(multibraid) %>%
          # st_cast("POINT") %>%
          #   st_as_sf() %>%
          #   concaveman::concaveman() %>%
          #   st_zm()
#'       # multibraided COMIDs
#'       multibraid <- dplyr::filter(net, comid %in% coms)
#'       
#'       chull <- 
#'         multibraid %>% 
#'         sf::st_union() %>%
#'         sf::st_convex_hull() %>% 
#'         sf::st_cast("MULTILINESTRING")
#'       
#'       # updated <- lapply(1:length(smalls), function(z) {
#'       for (z in 1:length(smalls)) {
#'         small_braids <- dplyr::filter(multibraid, comid %in% smalls[[z]])
#'         
#'         near <- sf::st_nearest_feature(chull, small_braids)
#'         # small_braids[near, ]$comid
#'         # smalls[[z]][smalls[[z]] %in%  small_braids[near, ]$comid]
#'         # 
#'         x[[cyc]] <- c(x[[cyc]][!x[[cyc]] %in% smalls[[z]]], small_braids[near, ]$comid)
#'         
#'         # sf::st_touches(chull, small_braids)
#'         
#'       }
#'       # x[[cyc]]
#'       # nets <- net %>% 
#'       #   dplyr::filter(comid %in% x[[cyc]])
#'       # plot(nets$geometry)
#'       # small_braids <- 
#'       #   multibraid %>% 
#'       #   dplyr::filter(comid %in% unlist(smalls))
#'       # 
#'       # sf::st_nearest_feature(chull, small_braids)
#'       # sf::st_touches(chull, small_braids)
#'       # 
#'       # mapview::mapview(multibraid) + chull + nets
#'       # # %>% 
#'       # #   sf::st_transform(4269)
#'       # 
      # t = st_combine(multibraid) %>%
      #   st_cast("POINT") %>%
      #   st_as_sf() %>%
      #   concaveman::concaveman() %>%
      #   st_zm()
#'       # 
#'     }
#'     
#'     for(j in 1:length(big_cycles)) {
#'       # big cycle iteration which is a cycle that contains one or more OTHER cycles
#'       cyc = big_cycles[j]
#'       
#'       # main big COMID
#'       big <- x[[cyc]]
#'       
#'       message("cyc: ", cyc)
#'       
#'       connects <- sapply(1:length(overlaps), function(k) {
#'         cyc %in% overlaps[[k]]
#'       })
#'       
#'       smalls <- x[connects]
#' 
#'       # drop small COMIDs from big COMID
#'       dropped <- big[!big %in% Reduce(c, smalls)]
#' 
#'       
#'       graph_big <-
#'         net %>%
#'         dplyr::filter(comid %in% big)
#'       
#'       # get COMID for big cycles and the fromnode/tonodes
#'       big_geom <- dplyr::filter(net, comid %in% dropped)
#'       
#'       # get COMID for small cycles and the fromnode/tonodes
#'       small_geom <- dplyr::filter(net, comid %in% Reduce(c, smalls))
#'       
#'       mapview::mapview(graph_big, color = "dodgerblue")  + 
#'         mapview::mapview(small_geom, color = "red") + 
#'         mapview::mapview(big_geom, color = "green") 
#'       mapview::mapview(graph_big, color = "dodgerblue")  + 
#'         mapview::mapview(small_geom, color = "red") + 
#'         mapview::mapview(big_geom, color = "green") +
#'         mapview::mapview(ograph, color = "green") 
#'       graph$geometry %>% plot()
#'       graph$geometry %>% plot()
#'       
#'       ograph <-
#'         network %>%
#'         # dplyr::filter(comid %in% dropped) %>%
#'         # dplyr::filter(comid %in% dropped) %>%
#'         dplyr::filter(comid %in% big) %>% 
#'         create_dag()
#'     
#'         start_nodes <- ograph[ograph$tocomid == 0, ]$fromnode
#'       
#'       # stash comids and from IDs
#'       cmap <- dplyr::select(
#'         ograph,
#'         comid,
#'         fromnode,
#'         tonode
#'       )
#'       
#'       ugraph <-
#'         ograph %>%
#'         renode_circuits() %>%
#'         make_undirected()
#'       
#'       # for (n in 1:length(start_nodes)) {
#'       recycle <- lapply(1:length(start_nodes), function(n) {
#'         
#'         s <- start_nodes[n]
#'         
#'         message("Start node: ", s)
#'         
#'         inner_cyc <- find_cycles(ugraph, start = s)
#'         
#'         # remove added nodes from renode_circuits()
#'         inner_cyc <- lapply(inner_cyc, function(i) { 
#'                         i[i %in% cmap$fromnode] 
#'                       })
#'         # ORIGINAL WORKING
#'         # # match fromnodes from comid_map to nodes identified in each cycle
#'         inner_coms <- lapply(1:length(inner_cyc), function(k) {
#'           # get COMIDs of each braid
#'           cmap[cmap$fromnode %in% inner_cyc[[k]], ] %>%
#'             dplyr::filter(tonode %in% inner_cyc[[k]]) %>%
#'             .$comid
#'           # comid_map[comid_map$fromnode %in% braids[[k]], ]$comid
#'           # dplyr::filter(comid_map, fromnode %in% braids[[k]])
#'         })
#'         inner_coms
#'         # dfs_traversal2()
#'         })
#'       recycle %>% Reduce(c, .) %>% unique()
#'     }
#'   }
#'   
#'   # if data should be returned as a 2 column dataframe with comid and braid_ids
#'   if(return_as == "dataframe") {
#'     
#'     # # format braids into a dataframe
#'     # braids_df <- lapply(1:length(braids2), function(k) {
#'     #   # get COMIDs of each braid
#'     #   data.frame(
#'     #     comid    = braids2[[k]],
#'     #     braid_id = names(braids2[k])
#'     #   )
#'     # }) %>% 
#'     #   dplyr::bind_rows() %>% 
#'     #   dplyr::tibble()
#'     
#'     # format braids into a dataframe
#'     # braids <- lapply(1:length(braids), function(k) {
#'     braids <- lapply(1:length(braids), function(k) {
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
#'     if(relationship == "one-to-many") {
#'       
#'       braids <-
#'         braids %>% 
#'         dplyr::group_by(comid) %>% 
#'         dplyr::summarise(
#'           braid_id = paste0(braid_id, collapse = ", ")
#'         ) %>% 
#'         dplyr::ungroup()
#'     }
#'     
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
#'       
#'       
#'       # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#'     } 
#'     # else {
#'     #   if(relationship == "one-to-many") {
#'     #     braids <-
#'     #       braids %>% 
#'     #       dplyr::group_by(comid) %>% 
#'     #       dplyr::summarise(
#'     #         braid_id = paste0(braid_id, collapse = ", ")
#'     #       ) %>% 
#'     #       dplyr::ungroup()
#'     #   }
#'     # }
#'   }
#'   
#'   return(braids)
#' }
#' 
#' #' Traverse graph in a Depth First Search (DFS) manner
#' #' @description Given a network with ID and toID columns, and a starting ID, traverse a river network using a Depth First Search (DFS) algorithm to explore all nodes of directed acyclic graph (DAG)
#' #' @param graph data.frame with ID, toID, hydroseq, startflag, terminalpa, and divergence attributes.
#' #' @param as_df logical, whether to return DFS traversal as a dataframe or as a list. If TRUE (default) DFS traversal will be returned as a dataframe.
#' #' @param return_as character string of how to return DFS traversal output. Either "list", "dataframe", "vector". Default is "list".
#' #' @param start ID of node to start traversal and go upstream from
#' #' @param dendritic logical, whether to make network dendritic or not. If FALSE, divergent nodes (divergence value of 2), will be connected to the upstream flowline and the network will contain cycles. Default is FALSE
#' #' @param verbose logical print status updates?
#' #' @return data.frame containing the distance between pairs of network outlets.
#' dfs_traversal2 <- function(
#'     graph,
#'     # as_df   = TRUE,
#'     return_as = "list",
#'     # start   = NULL,
#'     # dendritic = TRUE,
#'     verbose = FALSE
#' ) {
#'   # dropped
#'   # y = big_geom
#'   # cmap %>% 
#'   #   dplyr::filter(comid %in% y$comid) %>% 
#'   #   create_dag() %>% 
#'   #   dplyr::select(-tocomid) %>% 
#'   #   is_braided()
#'   # undir
#'   # y %>% nhdplusTools::make_standalone()
#'   # cmap
#'   # # # create Directed Acyclic Graph
#'   # ng <- dplyr::bind_rows(big_geom, small_geom)
#'   # graph <- create_dag(ng)
#'   # make_undirected(graph) %>% 
#'   #   is_braided()
#'   # graph %>% 
#'   #   dplyr::select(-tocomid) %>% 
#'   #   find_cycles()
#'   # st <- graph %>% 
#'   #   dplyr::filter(tocomid == 0)
#'   # mapview::mapview(graph, color = "dodgerblue")  + 
#'   #   mapview::mapview(small_geom, color = "red") + 
#'   #   mapview::mapview(st, color = "red") + 
#'   #   mapview::mapview(big_geom, color = "green")  + 
#'   #   mapview::mapview(y, color = "blue") 
#'   #
#'   # # create topology hashmap
#'   # topo_map <- make_topo_map(
#'   #   from_nodes  = graph$tonode,
#'   #   to_nodes    = graph$fromnode,
#'   #   from_ids    = graph$tocomid,
#'   #   to_ids      = graph$comid,
#'   #   streamcalc  = graph$streamcalc,
#'   #   streamorder = graph$streamorde,
#'   #   divergence  = graph$divergence
#'   # )
#'   # dag %>%
#'   # network %>% 
#'   # dplyr::filter(comid %in% braids[[5]]) %>% 
#'   # create_dag() %>% 
#'   # dplyr::select(-tocomid) %>% 
#'   # make_undirected() %>% 
#'   # dplyr::select(-tocomid) %>%
#'   # renode_circuits() %>% 
#'   # make_undirected() %>% 
#'   # is_braided()
#'   # dag %>%
#'   # mapview::mapview(ograph)
#'   # 
#'   # 
#'   # ograph <-
#'   #   network %>%
#'   #   dplyr::filter(comid %in% braids[[5]]) %>%
#'   #   create_dag()
#'   
#'   # fline_nodes <- 
#'   #   ograph %>% 
#'   #   dplyr::filter(tocomid == 0)
#'   # end <- nhdplusTools::get_node(fline_nodes, position = "end")
#'   # start <- nhdplusTools::get_node(fline_nodes, position = "start")
#'   # dists <- c(max(sf::st_distance(end)), 
#'   #            max(sf::st_distance(start)))
#'   # which.min(dists)
#'   
#'   # if (dists == 1) {
#'   #   
#'   #   pt <- 
#'   # }
#'   # 
#'   # # stash comids and from IDs
#'   # cmap <- dplyr::select(
#'   #   ograph,
#'   #     comid,
#'   #     fromnode,
#'   #     tonode
#'   #   )
#'   # 
#'   # graph <-
#'   #   ograph %>%
#'   #   renode_circuits() %>%
#'   #   make_undirected()
#'   
#'   # 
#'   # tm <- make_topo_map(
#'   #   from_nodes = ungraph$fromnode,
#'   #   to_nodes = ungraph$tonode
#'   # )
#'   # tm$as_list()
#'   # find_overlaps()
#'   # create topology hashmap
#'   topo_map <- make_topo_map(
#'     from_nodes  = graph$fromnode,
#'     to_nodes    = graph$tonode
#'     # from_nodes  = graph$tonode,
#'     # to_nodes    = graph$fromnode,
#'   )
#'   # topo_map$as_list()
#'   # keep track of visited nodes
#'   marked <- fastmap::fastmap()
#'   
#'   # set all marked values to FALSE
#'   marked$mset(.list = stats::setNames(
#'     lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i){ FALSE }),
#'     unique(c(graph$fromnode, graph$tonode))
#'   )
#'   )
#'   # start DFS traversal from root node
#'   # root <- as.character(graph[graph$tocomid == "0", ]$tonode)
#'   root <- graph$fromnode[1]
#'   # return_as = "vector"
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
#' # find_cycles(
#' #   graph,
#' #   start = 3
#' # )
#' 
#' find_braids2 <- function(
#'     network,
#'     start        = NULL,
#'     return_as    = "list",
#'     add          = FALSE,
#'     relationship = "one-to-many",
#'     unbraid      = TRUE,
#'     verbose      = FALSE
#' ) {
#'   
#'   # # # ---- Version 2 ----
#'   # network = network_div2
#'   # start     = NULL
#'   # return_as = "list"
#'   # add       = FALSE
#'   # verbose   = T
#'   # add = FALSE
#'   # verbose = TRUE
#'   # return_as = "list"
#'   # return_as = "dataframe"
#'   # mapview::mapview(graph)
#'   # graph <- create_dag(network)
#'   # network = network3
#'   # plot(network$geometry)
#'   # network = network3
#'   
#'   network = network_div
#'   start     = NULL
#'   return_as = "dataframe"
#'   add       = T
#'   relationship = "one-to-one"
#'   unbraid = F
#'   verbose   = T
#'   
#'   # check relationship argument is valid
#'   if(return_as == "dataframe") {
#'     if(!relationship %in% c("one-to-one", "one-to-many")) {
#'       stop("Invalid 'relationship' argument '", relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
#'     }
#'     
#'   }
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # turn network into a directed graph
#'   dag <- create_dag(network)
#'   # dag_sf <- create_dag(network)
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
#'   regraph <- renode_circuits(dag, verbose = FALSE)
#'   
#'   ##### - GET_SINGLE_CYCLES CODE -
#'   # # check for single cycles (2 nodes A,B that have unique edges)
#'   # # (i.e. 2 comids w/ exact same fromnodes/tonodes)
#'   # single_cycles <- get_single_cycles(add_args, return_as)
#'   # 
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
#'   # # fix from and tonodes after renoding to include circuits
#'   # fix_froms    <- dplyr::filter(regraph, fromnode > max(comid_map$fromnode))
#'   # fix_tos  <- dplyr::filter(regraph, tonode > max(comid_map$tonode))
#'   # dplyr::left_join(
#'   #   dplyr::select(
#'   #     fix_tos, 
#'   #     comid,
#'   #     temp_tocomid = tocomid, 
#'   #     fromnode
#'   #     # tonode
#'   #   ),
#'   #   dplyr::select(
#'   #     fix_froms,
#'   #     temp_tocomid = comid, 
#'   #     tocomid,
#'   #     tonode
#'   #   ),
#'   #   by = "temp_tocomid"
#'   # ) %>% 
#'   #   dplyr::select(comid, tocomid, fromnode, tonode)
#'   
#'   # make an undirected graph
#'   # undir <- make_undirected(dag)
#'   undir <- make_undirected(regraph)
#'   
#'   # find cycles in undirected graph (proxy for braids)
#'   cycles_lst <- find_cycles(
#'   # cycles <- find_cycles(
#'     graph     = undir,
#'     start     = start_node,
#'     return_as = "list",
#'     edge      = FALSE,
#'     # return_as = "dataframe",
#'     # return_as = return_as,
#'     verbose   = verbose 
#'   )
#'   
#'   cycle_edges <- find_cycles(
#'     graph     = undir,
#'     start     = start_node,
#'     return_as = "list",
#'     edge      = TRUE,
#'     # return_as = "dataframe",
#'     # return_as = return_as,
#'     verbose   = verbose 
#'   )
#'   
#'   cyc_edges_df <- find_cycles(
#'     graph     = undir,
#'     start     = start_node,
#'     return_as = "dataframe",
#'     edge      = T,
#'     wide      = FALSE,
#'     # return_as = "dataframe",
#'     # return_as = return_as,
#'     verbose   = verbose 
#'   )
#'   cyc_df <- find_cycles(
#'     graph     = undir,
#'     start     = start_node,
#'     return_as = "dataframe",
#'     edge      = F,
#'     wide      = FALSE,
#'     # return_as = "dataframe",
#'     # return_as = return_as,
#'     verbose   = verbose 
#'   )
#'   # mapview::mapview(dag_sf)
#'   
#'   # cycles_df <- find_cycles(
#'   #   graph     = undir,
#'   #   start     = start_node,
#'   #   return_as = "dataframe",
#'   #   verbose   = verbose
#'   # )
#'   
#'   # if(cycles$size() == 0 & is.null(single_braids)) {
#'   if(is.null(cycles)) {
#'     # message("No cycles found, returning NULL")
#'     return(NULL)
#'     # return(network)
#'   }
#'   
#'   # cycles <- dplyr::filter(b, braid_id == ubraids[i]) %>% 
#'   #   .$comid
#'   
#'   # remove added nodes from renode_circuits()
#'   braids <- lapply(cycles, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # remove added nodes from renode_circuits()
#'   branode_lst <- lapply(cycles_lst, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # remove added nodes from renode_circuits()
#'   braid_nodes <- lapply(cycles, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # cycle_edges
#'   # edges <- cycle_edges[["14"]]
#'   
#'   # remove added nodes from renode_circuits()
#'   braids <- lapply(cycle_edges, function(i) { 
#'     # i[i %in% comid_map$fromnode] 
#'     # ll <- cycle_edges[[23]]
#'     # edge_to_node(ll, to = FALSE, directed = FALSE)
#'     # i[i %in% paste0(comid_map$fromnode, "-", comid_map$tonode)]
#'     # 
#'     tos <- edge_to_node(i, to = TRUE, directed = FALSE)
#'     froms <- edge_to_node(i, to = FALSE, directed = FALSE)
#'  
#'     i[tos %in% comid_map$fromnode & froms %in% comid_map$fromnode]
#'     # tos %in% comid_map$fromnode & froms %in% comid_map$fromnode
#'     
#'     # i[edge_to_node(i, to = FALSE, directed = FALSE) %in% comid_map$fromnode]
#'     # i[i %in% paste0(comid_map$fromnode, "-", comid_map$tonode)]
#'     
#'   })
#'   
#'   braids2 <- lapply(1:length(braids), function(k) {
#'     # k = 22
#'     froms <- edge_to_node(braids[[k]], to = FALSE, directed = FALSE)
#'     
#'     # get COMIDs of each braid
#'     comid_map[comid_map$fromnode %in% froms, ] %>%
#'       # dplyr::filter(tonode %in% froms) %>% 
#'       .$comid
#'     # # get COMIDs of each braid
#'     # comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#'     #   dplyr::filter(tonode %in% braids[[k]])
#'     
#'     }) 
#'   # %>% 
#'   #   dplyr::bind_rows()
#'   
#'   # tmp_dag <-
#'     dag_sf %>% 
#'     dplyr::mutate(
#'       edge = paste0(fromnode, "-", tonode)
#'     ) %>% 
#'     dplyr::filter(
#'       fromnode %in% froms
#'       # tonode %in% tos
#'       # edge %in% braids[[j]]
#'     )
#'     # dplyr::filter(
#'       # edge %in% braids[[j]]
#'     # )
#'   mapview::mapview(dag_sf) +  mapview::mapview(tmp_dag, color = "red")
#'   plot(tmp_dag$geometry)
#'   # remove added nodes from renode_circuits()
#'   braid_map <- lapply(1:length(cycle_edges), function(i) { 
#'     data.frame(
#'       edge     = cycle_edges[[i]],
#'       fromnode = edge_to_node(cycle_edges[[i]], to = FALSE, directed = FALSE),
#'       tonode   = edge_to_node(cycle_edges[[i]], to = TRUE, directed = FALSE)
#'     ) %>% 
#'       dplyr::mutate(
#'         cycle_id = names(cycle_edges[i])
#'       )
#'     # i[i %in% comid_map$fromnode] 
#'     # i[i %in% paste0(comid_map$fromnode, "-", comid_map$tonode)]
#'   }) %>% 
#'     dplyr::bind_rows()
#'   
#'   
#'   # remove added nodes from renode_circuits()
#'   braid_nodes <- lapply(cycle_edges, function(i) { 
#'     # i[i %in% comid_map$fromnode] 
#'     i[i %in% paste0(comid_map$fromnode, "-", comid_map$tonode)]
#'     
#'   })
#'   # cycle_edges
#'   # edges <- cycle_edges[["14"]]
#'   # from_nodes <- edge_to_node(edges, to = FALSE, directed = FALSE)
#'   # to_nodes <- edge_to_node(edges, to = TRUE, directed = FALSE)
#'   # cmap <- dplyr::mutate(comid_map, edge = paste0(fromnode, "-", tonode))
#'   # edges %in% paste0(comid_map$fromnode, "-", comid_map$tonode)
#'   # comid_map[paste0(comid_map$fromnode, "-", comid_map$tonode) %in% edges, ]
#'   # cmap$edge %in% edges 
#'   # cmap[cmap$edge %in% edges, ]
#'   # from_nodes %in% comid_map$fromnode
#'   # to_nodes %in% comid_map$
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   # braids2 <- lapply(1:length(braids), function(k) {
#'   #   # get COMIDs of each braid
#'   #   comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#'   #     dplyr::filter(tonode %in% braids[[k]])})
#'   # braid_index <- 1:length(braids)
#'   # # for (i in 1:length(braids)) {
#'   # dup_cycles <- lapply(1:length(braids), function(i) {
#'   #           subs <- sapply(braid_index[braid_index != i], function(k) {
#'   #             if(all(braids[[i]] %in% braids[[k]])) {
#'   #               k
#'   #               } 
#'   #             }) %>% 
#'   #             Reduce(c, .)
#'   #   comid_map[comid_map$fromnode %in% unname(unlist(braids[subs])), ] %>% 
#'   #     dplyr::filter(tonode %in%   unname(unlist(braids[subs])))  %>% 
#'   #     make_undirected()
#'   #   }) 
#'   
#'   braid_edges
#'   tmp <- 
#'     braid_map %>% 
#'     dplyr::left_join(
#'       dplyr::select(
#'         dplyr::mutate(
#'           comid_map,
#'           edge     =  paste0(fromnode, "-", tonode)
#'         ), 
#'         comid, edge
#'       ),
#'       by = "edge"
#'     )
#'   
#' 
#'    tmp <-  dplyr::select(
#'       dplyr::mutate(
#'         comid_map,
#'         edge     =  paste0(fromnode, "-", tonode)
#'       ), 
#'       comid, edge
#'     ) %>% 
#'     dplyr::left_join(
#'       braid_map, 
#'       by = "edge"
#'     )
#'   comid_map
#'   
#'   dplyr::select(
#'     dplyr::mutate(
#'     comid_map,
#'     edge     =  paste0(fromnode, "-", tonode)
#'     ), 
#'     comid, edge
#'     )
#'   # blen1 <- unname(lengths(braid_edges))
#'   # lengths(braid_nodes)
#'   
#'   branode_lst2 <- lapply(1:length(branode_lst), function(k) {
#'     # get COMIDs of each braid
#'     comid_map[comid_map$fromnode %in% branode_lst[[k]], ] %>%
#'       dplyr::filter(tonode %in% branode_lst[[k]]) %>%
#'       .$comid
#'     
#'     #   # comid_map[comid_map$fromnode %in% braids[[k]], ]$comid
#'     #   # dplyr::filter(comid_map, fromnode %in% braids[[k]])
#'   })
#'   
#'   # match edges in comid_map to edge (pair of nodes) identified in each cycle
#'   # braids <- 
#'   # braids <- 
#'     lapply(1:length(braids), function(k) {
#'     
#'       k = 18
#'       braids[[k]]
#'       
#'       froms <- edge_to_node(braids[[k]], to = FALSE, directed = FALSE)
#'       tos   <- edge_to_node(braids[[k]], to = TRUE, directed = FALSE)
#'      
#'       # get COMIDs of each braid
#'       rev_map <- 
#'         comid_map %>% 
#'         dplyr::mutate(
#'           edge     =  paste0(fromnode, "-", tonode),
#'           rev_edge =  paste0(tonode, "-", fromnode)
#'         )
#'       rev_map
#'       
#'       rev_map %>% 
#'         dplyr::filter(
#'           fromnode %in% froms
#'           # edge %in% braids[[k]]
#'         )
#'         # dplyr::filter(fromnode == 43)
#'       rev_map %>% 
#'         dplyr::filter(
#'           edge %in% braids[[k]]
#'         )
#'       comid_map[
#'         paste0(comid_map$fromnode, "-", comid_map$tonode) %in% braids[[k]], 
#'       ]
#'       # comid_map[
#'       #   paste0(comid_map$fromnode, "-", comid_map$tonode) %in% braids[[k]], 
#'       #   ]$comid
#'     
#'     
#'       })
#'   
#'   # braid_edges[[12]]
#'   # braid_edges[[22]]
#'   # blen12 <- unname(lengths(braid_edges))
#'   # blen1 == blen12
#'   # length(braid_edges)
#'   # lengths(braid_nodes)
#'   # # ORIGINAL WORKING
#'   # # # match fromnodes from comid_map to nodes identified in each cycle
#'   # braids <- lapply(1:length(braids), function(k) {
#'   #   # get COMIDs of each braid
#'   #   comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#'   #     dplyr::filter(tonode %in% braids[[k]]) %>%
#'   #     .$comid
#'   # 
#'   # #   # comid_map[comid_map$fromnode %in% braids[[k]], ]$comid
#'   # #   # dplyr::filter(comid_map, fromnode %in% braids[[k]])
#'   # })
#'   
#'   # match fromnodes from comid_map to nodes identified in each cycle
#'   braids <- lapply(1:length(braids), function(k) {
#'     # get COMIDs of each braid
#'     # tmp <- comid_map[comid_map$fromnode %in% braids[[k]], ]
#'     # k = 22
#'     
#'     tmp <- 
#'       comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'       dplyr::group_by(fromnode) %>% 
#'       dplyr::add_count() %>% 
#'       dplyr::rename(fromnode_n = n) %>% 
#'       dplyr::ungroup() %>% 
#'       dplyr::group_by(tonode) %>% 
#'       dplyr::add_count() %>% 
#'       dplyr::rename(tonode_n = n) %>% 
#'       dplyr::ungroup() %>% 
#'       dplyr::group_by(fromnode, tonode) %>% 
#'       dplyr::add_count() %>% 
#'       dplyr::rename(combo_n = n) %>% 
#'       dplyr::ungroup()
#'     
#'     dups <- 
#'       tmp %>% 
#'       dplyr::filter(combo_n != 1) %>% 
#'       .$comid
#'     
#'     picks <- 
#'       tmp %>%
#'       dplyr::filter(fromnode_n != 1) %>% 
#'       dplyr::group_by(fromnode) %>% 
#'       dplyr::slice_min(tonode_n, with_ties = FALSE) %>% 
#'       dplyr::ungroup() %>% 
#'       dplyr::select(-fromnode_n, -tonode_n) %>% 
#'       dplyr::bind_rows(      
#'         dplyr::select(
#'           dplyr::filter(tmp, 
#'                         fromnode_n == 1), 
#'           -fromnode_n, -tonode_n
#'         )
#'       )
#'     
#'     unique(c(picks$comid, dups))
#'     # comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'     #   dplyr::filter(tonode %in% braids[[k]]) %>% 
#'     #   .$comid
#'   })
#'   
#'   # 56: 3558454 
#'   # 2: 3558458 
#'   # 58: 3558448 
#'   # braid_sf1 <- dag_sf %>% dplyr::filter(fromnode %in% c("2", "56", "58"))
#'   braid_nodes
#'   braids
#'   braid_sf_small <- dplyr::filter(dag_sf, fromnode %in% c("5", "7"))
#'   braid_sf_big <- dplyr::filter(dag_sf, fromnode %in% c("5", "7", "53", "8", "54"))
#'   braid_sf_cycbig <- dplyr::filter(dag_sf, comid %in%   braids[[21]])
#'   
#'   # 3558470 3558466 3558484 3558480 3558482 3558472 3558464 3558486 3558468
#'   # 3558466 3558484 3558480 3558482 3558472 3558486
#'   
#'   braid_sf_big$comid
#'   braids[[21]]
#'   mapview::mapview(dag_sf) + 
#'     mapview::mapview(braid_sf_big, color = "red") + 
#'     mapview::mapview(braid_sf_small, color = "green") +
#'     mapview::mapview(braid_sf_cycbig, color = "green")
#'   # find_overlaps(braids, rm_no_overlap = T)
#'   braids2 <- seperate_braids3(braids, network)
#'   
#'   if(unbraid) {
#'     # seperate braids with duplicates into individual parts
#'     braids <- seperate_braids2(braids)
#'   }
#'   
#'   # find_overlaps(braids)
#'   # find_overlaps(braids, rm_no_overlap = T)
#'   
#'   # set braid_id names
#'   braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
#'   # braids <- stats::setNames(braids, paste0("braid_",  1:length(cycles)))
#'   # braids <- stats::setNames(braids, paste0("braid_", names(cycles)))
#'   # braids2 <- stats::setNames(braids2, paste0("braid_", names(cycles)))
#'   
#'   # if data should be returned as a 2 column dataframe with comid and braid_ids
#'   if(return_as == "dataframe") {
#'     
#'     # # format braids into a dataframe
#'     # braids_df <- lapply(1:length(braids2), function(k) {
#'     #   # get COMIDs of each braid
#'     #   data.frame(
#'     #     comid    = braids2[[k]],
#'     #     braid_id = names(braids2[k])
#'     #   )
#'     # }) %>% 
#'     #   dplyr::bind_rows() %>% 
#'     #   dplyr::tibble()
#'     
#'     # format braids into a dataframe
#'     braids_df <-
#'     # braids <-
#'       lapply(1:length(braids2), function(k) {
#'       # get COMIDs of each braid
#'       data.frame(
#'         comid    = braids2[[k]],
#'         braid_id = names(braids2[k])
#'       )
#'     }) %>% 
#'       dplyr::bind_rows() %>% 
#'       dplyr::tibble()
#'     
#'     # if each comid should have a list of all the braids it is a part of
#'     # if relationship == "one-to-one" then COMIDs may appear more than once
#'     # in dataset as a COMID may be apart of more than one braid
#'     if(relationship == "one-to-many") {
#'       # braids_df <-
#'         # braids_df %>% 
#'       braids <-
#'         braids %>%
#'         dplyr::group_by(comid) %>% 
#'         dplyr::summarise(
#'           braid_id = paste0(braid_id, collapse = ", ")
#'         ) %>% 
#'         dplyr::ungroup()
#'     }
#'     
#'     
#'     # if braid data should be added to original data
#'     if(add) {
#'       
#'       # join back with original data
#'       # braids_df <-
#'       braids <-
#'         dplyr::mutate(
#'           dplyr::left_join(
#'             network,
#'             braids,
#'             # braids_df,
#'             by = "comid"
#'           ),
#'           braid_id = ifelse(is.na(braid_id), "no_braid", braid_id)
#'         )
#'       
#'       
#'       # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#'     } 
#'     # else {
#'     #   if(relationship == "one-to-many") {
#'     #     braids <-
#'     #       braids %>% 
#'     #       dplyr::group_by(comid) %>% 
#'     #       dplyr::summarise(
#'     #         braid_id = paste0(braid_id, collapse = ", ")
#'     #       ) %>% 
#'     #       dplyr::ungroup()
#'     #   }
#'     # }
#'   }
#'   
#'   return(braids)
#' }
#' find_braids4 <- function(
#'     network,
#'     start        = NULL,
#'     return_as    = "list",
#'     add          = FALSE,
#'     relationship = "one-to-many",
#'     unbraid      = TRUE,
#'     verbose      = FALSE
#' ) {
#'   
#'   # # # ---- BRAIDED SYSTEM TEST DATA ----
#'   # network = network_div2
#'   # start     = NULL
#'   # return_as = "list"
#'   # add       = FALSE
#'   # verbose   = T
#'   # add = FALSE
#'   # verbose = TRUE
#'   # return_as = "list"
#'   # return_as = "dataframe"
#'   # mapview::mapview(graph)
#'   # graph <- create_dag(network)
#'   # network = network3
#'   # plot(network$geometry)
#'   # network = network3
#'   
#'   network = network_div
#'   start     = NULL
#'   return_as = "dataframe"
#'   add       = T
#'   relationship = "one-to-one"
#'   unbraid = F
#'   verbose   = T
#'   
#'   # check relationship argument is valid
#'   if(return_as == "dataframe") {
#'     if(!relationship %in% c("one-to-one", "one-to-many")) {
#'       stop("Invalid 'relationship' argument '", relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
#'     }
#'     
#'   }
#'   
#'   # lower case names
#'   names(network) <- tolower(names(network))
#'   
#'   # turn network into a directed graph
#'   dag <- create_dag(network)
#'   # dag_sf <- create_dag(network)
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
#'   regraph <- renode_circuits(dag, verbose = FALSE)
#'   
#'   ##### - GET_SINGLE_CYCLES CODE -
#'   # # check for single cycles (2 nodes A,B that have unique edges)
#'   # # (i.e. 2 comids w/ exact same fromnodes/tonodes)
#'   # single_cycles <- get_single_cycles(add_args, return_as)
#'   # 
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
#'     # return_as = "dataframe",
#'     # return_as = return_as,
#'     verbose   = verbose 
#'   )
#'   
#'   # mapview::mapview(dag_sf)
#'   
#'   # cycles_df <- find_cycles(
#'   #   graph     = undir,
#'   #   start     = start_node,
#'   #   return_as = "dataframe",
#'   #   verbose   = verbose
#'   # )
#' 
#'   # if(cycles$size() == 0 & is.null(single_braids)) {
#'   if(is.null(cycles)) {
#'     # message("No cycles found, returning NULL")
#'     return(NULL)
#'     # return(network)
#'   }
#'   
#'   # cycles <- dplyr::filter(b, braid_id == ubraids[i]) %>% 
#'   #   .$comid
#'   
#'   # remove added nodes from renode_circuits()
#'   braids <- lapply(cycles, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # remove added nodes from renode_circuits()
#'   braid_nodes <- lapply(cycles, function(i) { 
#'     i[i %in% comid_map$fromnode] 
#'   })
#'   
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   # braids2 <- lapply(1:length(braids), function(k) {
#'   #   # get COMIDs of each braid
#'   #   comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'   #     dplyr::filter(tonode %in% braids[[k]])})
#'   # braid_index <- 1:length(braids)
#'   # # for (i in 1:length(braids)) {
#'   # dup_cycles <- lapply(1:length(braids), function(i) {
#'   #           subs <- sapply(braid_index[braid_index != i], function(k) {
#'   #             if(all(braids[[i]] %in% braids[[k]])) {
#'   #               k
#'   #               } 
#'   #             }) %>% 
#'   #             Reduce(c, .)
#'   #   comid_map[comid_map$fromnode %in% unname(unlist(braids[subs])), ] %>% 
#'   #     dplyr::filter(tonode %in%   unname(unlist(braids[subs])))  %>% 
#'   #     make_undirected()
#'   #   }) 
#'   
#'   # ORIGINAL WORKING
#'   # # match fromnodes from comid_map to nodes identified in each cycle
#'   braids <- lapply(1:length(braids), function(k) {
#'     # get COMIDs of each braid
#'     comid_map[comid_map$fromnode %in% braids[[k]], ] %>%
#'       dplyr::filter(tonode %in% braids[[k]]) %>%
#'       .$comid
#' 
#'     # comid_map[comid_map$fromnode %in% braids[[k]], ]$comid
#'     # dplyr::filter(comid_map, fromnode %in% braids[[k]])
#'   })
#'   
#'   # match fromnodes from comid_map to nodes identified in each cycle
#'   braids <- lapply(1:length(braids), function(k) {
#'     # get COMIDs of each braid
#'     # tmp <- comid_map[comid_map$fromnode %in% braids[[k]], ]
#'     # k = 22
#'     
#'     tmp <- 
#'       comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'       dplyr::group_by(fromnode) %>% 
#'       dplyr::add_count() %>% 
#'       dplyr::rename(fromnode_n = n) %>% 
#'       dplyr::ungroup() %>% 
#'       dplyr::group_by(tonode) %>% 
#'       dplyr::add_count() %>% 
#'       dplyr::rename(tonode_n = n) %>% 
#'       dplyr::ungroup() %>% 
#'       dplyr::group_by(fromnode, tonode) %>% 
#'       dplyr::add_count() %>% 
#'       dplyr::rename(combo_n = n) %>% 
#'       dplyr::ungroup()
#'     
#'     dups <- 
#'       tmp %>% 
#'       dplyr::filter(combo_n != 1) %>% 
#'       .$comid
#'     
#'     picks <- 
#'       tmp %>%
#'       dplyr::filter(fromnode_n != 1) %>% 
#'       dplyr::group_by(fromnode) %>% 
#'       dplyr::slice_min(tonode_n, with_ties = FALSE) %>% 
#'       dplyr::ungroup() %>% 
#'       dplyr::select(-fromnode_n, -tonode_n) %>% 
#'       dplyr::bind_rows(      
#'         dplyr::select(
#'           dplyr::filter(tmp, 
#'                         fromnode_n == 1), 
#'           -fromnode_n, -tonode_n
#'         )
#'       )
#'     
#'     unique(c(picks$comid, dups))
#'     # comid_map[comid_map$fromnode %in% braids[[k]], ] %>% 
#'     #   dplyr::filter(tonode %in% braids[[k]]) %>% 
#'     #   .$comid
#'   })
#'   
#'   # 56: 3558454 
#'   # 2: 3558458 
#'   # 58: 3558448 
#'   # braid_sf1 <- dag_sf %>% dplyr::filter(fromnode %in% c("2", "56", "58"))
#'   braid_nodes
#'   braids
#'   braid_sf_small <- dplyr::filter(dag_sf, fromnode %in% c("5", "7"))
#'   braid_sf_big <- dplyr::filter(dag_sf, fromnode %in% c("5", "7", "53", "8", "54"))
#'   braid_sf_cycbig <- dplyr::filter(dag_sf, comid %in%   braids[[21]])
#'   
#'   # 3558470 3558466 3558484 3558480 3558482 3558472 3558464 3558486 3558468
#'   # 3558466 3558484 3558480 3558482 3558472 3558486
#'   
#'   braid_sf_big$comid
#'   braids[[21]]
#'   mapview::mapview(dag_sf) + 
#'     mapview::mapview(braid_sf_big, color = "red") + 
#'     mapview::mapview(braid_sf_small, color = "green") +
#'     mapview::mapview(braid_sf_cycbig, color = "green")
#'   # find_overlaps(braids, rm_no_overlap = T)
#'   braids2 <- seperate_braids3(braids, network)
#'   
#'   if(unbraid) {
#'     # seperate braids with duplicates into individual parts
#'     braids <- seperate_braids2(braids)
#'   }
#'   
#'   # find_overlaps(braids)
#'   # find_overlaps(braids, rm_no_overlap = T)
#'   
#'   # set braid_id names
#'   braids <- stats::setNames(braids, paste0("braid_",  1:length(braids)))
#'   # braids <- stats::setNames(braids, paste0("braid_",  1:length(cycles)))
#'   # braids <- stats::setNames(braids, paste0("braid_", names(cycles)))
#'   # braids2 <- stats::setNames(braids2, paste0("braid_", names(cycles)))
#'   
#'   # if data should be returned as a 2 column dataframe with comid and braid_ids
#'   if(return_as == "dataframe") {
#'     
#'     # # format braids into a dataframe
#'     # braids_df <- lapply(1:length(braids2), function(k) {
#'     #   # get COMIDs of each braid
#'     #   data.frame(
#'     #     comid    = braids2[[k]],
#'     #     braid_id = names(braids2[k])
#'     #   )
#'     # }) %>% 
#'     #   dplyr::bind_rows() %>% 
#'     #   dplyr::tibble()
#'     
#'     # format braids into a dataframe
#'     braids <- lapply(1:length(braids), function(k) {
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
#'     if(relationship == "one-to-many") {
#'       
#'       braids <-
#'         braids %>% 
#'         dplyr::group_by(comid) %>% 
#'         dplyr::summarise(
#'           braid_id = paste0(braid_id, collapse = ", ")
#'         ) %>% 
#'         dplyr::ungroup()
#'     }
#'     
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
#'       
#'       
#'       # braids[is.na(braids$braid_id), ]$braid_id <- "no_braid"
#'     } 
#'     # else {
#'     #   if(relationship == "one-to-many") {
#'     #     braids <-
#'     #       braids %>% 
#'     #       dplyr::group_by(comid) %>% 
#'     #       dplyr::summarise(
#'     #         braid_id = paste0(braid_id, collapse = ", ")
#'     #       ) %>% 
#'     #       dplyr::ungroup()
#'     #   }
#'     # }
#'   }
#'   
#'   return(braids)
#' }

make_braids_gif <- function(network, 
                            save_path  = NULL,       
                            height     = 8,
                            width      = 10,
                            gif_width  = 1800, 
                            gif_height = 1500,
                            delay      = 0.8,
                            legend_pos = "bottom",
                            verbose    = FALSE
) {
  
  # if no save path is given
  if(is.null(save_path)) {
    # save path is current working directory
    save_path <- getwd()
  }
  
  
  ubraids <- unique(network$braid_id)
  
  # this will create a temp folder, but tempdir() won't let you name it:
  temp_dir <- tempdir() 
  
  # this will create a folder within our temp folder, with a name of our choice:
  new_dir <- paste0(temp_dir, "/braid_plots/")
  
  # check if new temp dir alrteady exists
  if(dir.exists(new_dir)) {
    
    warning(new_dir, " already exists")
    
  }
  
  # create temporary directory
  dir.create(path = new_dir)
  
  if(verbose) { message("Plotting braids... ")}
  
  # make names lowercase
  names(network) <- tolower(names(network))
  
  # unique braid_ids
  ubraids <- unique(network$braid_id)
  
  ubraids <- ubraids[!grepl("no_braid", ubraids)]
  
  for (i in 1:length(ubraids)) {
    
    if(verbose) {
      message(ubraids[i], " - (", i, "/", length(ubraids), ")")
    }
    
    braid_plot <- 
      # b %>%
      network %>%
      # dplyr::select(comid, braid_id, geometry) %>%
      ggplot2::ggplot() +
      # ggplot2::geom_sf(ggplot2::aes(fill = braid_id)) +
      ggplot2::geom_sf(ggplot2::aes(color = braid_id)) +
      gghighlight::gghighlight(braid_id ==  ubraids[i]) + 
      ggplot2::labs(
        caption = paste0(i, " / ", length(ubraids)),
        color = "",
        # caption = paste0("(", i, " / ", length(ubraids), ")")
      ) + 
      ggplot2::theme_bw() + 
      ggplot2::theme(
        plot.caption = ggplot2::element_text(size = 12, face = "bold"),
        legend.position = legend_pos
      ) 
    
    temp_file <- tempfile(pattern =  paste0(ifelse(i < 10, paste0("0", i), i), "_braid"), 
                          tmpdir = new_dir,
                          fileext = ".png"
    ) 
    
    # Generate a temporary file path
    ggplot2::ggsave(
      filename = temp_file,
      plot = braid_plot,
      height = height,
      width = width,
      scale = 1
    )
    
    
  }
  
  # save_path <- "D:/gif/braid_gif.gif"
  png_files <- sort(list.files(new_dir, full.names = TRUE))
  
  gifski::gifski(png_files, 
                 gif_file = save_path,
                 width    = gif_width, 
                 height   = gif_height,
                 delay    = delay
  )
  
  if(verbose) {
    message("Saving braid gif:\n --- ", save_path)
  }
  
  # unlink deletes temporary directory holding PNGs
  unlink(new_dir, recursive = TRUE) 
  
}

# Utility function that takes the output from 'find_braids(nested = TRUE)' and unpacks/unnests braid_id column
# unnests the comma seperated braid_id column into individual rows for each braid ID/comid pairing.
# (there can thus be duplicate COMIDs as some COMIDs are part of more than 1 braid )
# (i.e. COMID 1: braid_id: "braid_1", "braid_2", "braid_4", 
#  ---- > these 3 braid_ids for COMID 1 would become 3 separate rows after unpacking)
unnpack_braids <- function(braids, into_list = FALSE) {
  
  if(!"braid_id" %in% tolower(names(braids))) {
    stop("'braids' input does not contain 'braid_id' column ")
  }
  
  unnested_braids <- 
    braids %>% 
    dplyr::mutate(split_braid_ids = stringr::str_split(braid_id, ", ")) %>%
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

# Given a direccted graph from create_dag(), make an UNDIRECTED adjcacency matrix
make_adj_matrix <- function(graph) {
  # network <- network3
  # graph <- create_dag(network)
  # graph
  
  # get to and from nodes
  adj <- 
    graph %>% 
    # dplyr::select(comid, tocomid, fromnode, tonode) %>% 
    dplyr::select(fromnode, tonode) %>% 
    sf::st_drop_geometry() %>% 
    dplyr::tibble()
  
  # opposite topology from directed graph
  rev_edges <- dplyr::select(
    adj, 
    fromnode = tonode,
    tonode   = fromnode
  )
  
  # add back edges
  adj <- dplyr::bind_rows(adj, rev_edges)
  
  # get unique nodes to build adjacency matrix
  nodes <- unique(c(adj$fromnode, adj$tonode))
  
  node_mapping <- setNames(seq_along(nodes), as.character(nodes))
  
  # make empty matrix
  adj_mat <- matrix(
    0, 
    nrow = length(nodes),
    ncol = length(nodes),
    byrow = T
  )
  
  # adj
  # topo_map <- make_topo_map(from_nodes = adj$fromnode, to_nodes = adj$tonode)
  # topo_map$as_list()
  # 
  # plot(graph$geometry)
  for (i in 1:nrow(adj)) {
    # i = 1
    
    # message("i: ", i)
    # message("adj fromnode: ", adj[i, ]$fromnode)
    # message("adj tonode: ", adj[i, ]$tonode)
    
    from_node <-  node_mapping[[as.character(adj[i, ]$fromnode)]]
    to_node <-  node_mapping[[as.character(adj[i, ]$tonode)]]
    
    # from_node <- node_mapping[adj[i, ]$fromnode]
    # to_node <- node_mapping[adj[i, "tonode"]]
    
    adj_mat[from_node,  to_node] <- 1
    
    # message("==================")
  }
  
  return(adj_mat)
  
  
}

#' Traverse graph in a Breadth First Search (BFS) manner
#' @description Given a network with ID and toID columns, and a starting ID, traverse a river network using a Breadth First Search (BFS) algorithm to explore all nodes of directed acyclic graph (DAG)
#' @param network data.frame with ID, toID, hydroseq, startflag, terminalpa, and divergence attributes.
#' @param start COMID of node to start traversal and go upstream/downstream from
#' @param return_as character string of how to return DFS traversal output. Either "list", "dataframe", "vector". Default is "vector", which return a list of 2 vectors, one indicating the BFS order of nodes and the other the respective level of each node.
#' @param reverse Logical whether to reverse the graph to start from upstream and traverse downstream. If FALSE (default) traversal will start at furthest downstream outlet or at start comid
#' @param direction character direction to traverse graph, upstream or downstream. Default is "upstream"
#' @param verbose logical print status updates?
#' @return data.frame containing the distance between pairs of network outlets.
bfs_traversal2 <- function(
    graph, 
    start     = NULL,
    reverse = FALSE,
    return_as = "vector",
    verbose   = FALSE
) {
  
  # lower case names
  names(network) <- tolower(names(network))
  
  # turn network into a directed graph
  dag <- create_dag(network)
  # graph <- create_dag(network)
  
  # get the fromnode associated with a given COMID 'start'
  start_node <- comid_to_node(dag, start)
  
  if(verbose) {
    message("Starting braid detection at ", 
            ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
  }
  
  # drop graph geometry
  dag <- sf::st_drop_geometry(dag)
  # graph <- sf::st_drop_geometry(graph)
  
  # stash comids and from IDs
  comid_map <- dplyr::select(
    # graph,
    dag,
    comid,
    fromnode,
    tonode
  )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identfied as a cycle and thus a braid
  # Questionable at this point, other option is the code below that uses 
  # single_cycles() function (COMMENTED OUT CODE BELOW)
  dag <- renode_circuits(dag, verbose = FALSE)
  
  # make an undirected graph
  undir <- make_undirected(dag)
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
  
  # if(verbose) {
  #   message("Starting braid detection at node: ", start_node)
  # }
  
  # make a topology hashmap to use in DFS 
  topo_map <- make_topo_map(
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode
    # from_nodes = graph$tonode,
    # to_nodes   = graph$fromnode
  )
  
  root <- as.character(graph[graph$tocomid == "0", ]$fromnode)

  # keep track of visited nodes
  marked <- fastmap::fastmap()
  
  # set all marked values to FALSE
  marked$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))), 
           function(i){ FALSE }), 
    unique(c(graph$fromnode, graph$tonode))
  )
  )
  
  # message("Starting ", direction, " traversal from node ", root)
  
  # # result list
  # res <- list()
  
  # output result object
  if (return_as %in% c("list", "dataframe")) {
    res <- list()
    
  } else {
    # vector
    res <- vctrs::vec_c()
    level_res <-  vctrs::vec_c()
    # res <- vctrs::vec_c(topo_map$get(root)$to_node)
  }
  
  # initiliaze queue
  q <- fastmap::fastqueue()
  
  # add root to queue
  q$add(root)
  
  # level counter
  count = 0
  
  # while (!rstackdeque::empty(q)) {
  while (q$size() != 0) {
    
    # incremenet count
    count <- count + 1 
    
    # current level 
    lvl   <- as.character(count)
    
    message("Current level: ", lvl)
    
    # length of queue
    qlen <- q$size()
    
    # map/list to store current level nodes
    level_map <- fastmap::fastmap()
    
    # loop through all the nodes in the current level of graph
    for (i in 1:qlen) {
      
      # message("INNER LOOP i = ", i)
      
      # get/pop the front element from the queue
      node <- as.character(q$mremove(1)[[1]])
      
      # message("Popping node ", 
      #         node, 
      #         " from front of queue"
      # )
      # q$as_list()
      # if(!marked$get(node)) {
      if(marked$get(node)) {
        
        # # set vertex to marked
        # marked$set(node, TRUE)
        
        # message("-----------------------------------")
        # message("---- NODE ", node, "ALREADY VISITED ----")
        # message("-----------------------------------")
        next
        
      } else {
        # message("-----------------------------------")
        # message("!!! MARKING ", node, " as VISITED !!!")
        # message("-----------------------------------")
        # set vertex to marked
        marked$set(node, TRUE)
      }
      
      
      # if(is.null(node)) {
      #   message("**********************")
      #   message("---- NODE IS NULL ----")
      #   message("**********************")
      #   next
      # }
      
      
      # check if any nodes have already been visited at current level
      if (!level_map$has(lvl)) {
        
        # add level to level_map
        level_map$set(
          lvl,
          list(node = node)
        )
        
      } else {
        # node = "14"
        # if current lvl is already in level_map, update the value with current node
        level_map$set(lvl, 
                      list(
                        node  = c(level_map$get(lvl)$node, node)
                      )
        )
      }
      
      # level <- c(level, node)
      
      # get neighbors of current node 
      neighbors <- topo_map$get(node)$to_node
      # message("Adding neighbors to queue:\n", paste0( " --> ", neighbors, sep = "\n"))
      
      # add neighbors off node to queue
      q$madd(.list = neighbors)
      
      # message("queue size: ", q$size())
      # message("* * * * * * * * * *")
    }
    
    # convert level map to a list
    level_map <- level_map$as_list()
    # level <- list(level, node)
    
    # message("!!! UPDATING RES --> LEVEL ", lvl, " IS COMPLETE !!! ")
    
    # output result object
    if (return_as %in% c("list", "dataframe")) {
      
      # append list of level nodes to result
      res <- c(res, stats::setNames(
        list(
          unname(unlist(level_map))
        ),
        names(level_map)
      )
      )
      
    } else {
      
      # vector
      # res <- vctrs::vec_c(res, vctrs::vec_c(level_map))
      res       <- vctrs::vec_c(res, vctrs::vec_c(level_map[[1]]$node))
      level_res <- vctrs::vec_c(level_res, vctrs::vec_c(rep(names(level_map),  
                                                            length(level_map[[1]]$node))
      ))
      
    }
    
    message("=============================")
  }
  
  
  if(return_as == "dataframe") {
    
    res <-  dplyr::bind_rows(
      lapply(1:length(res), function(i) {
        data.frame(
          fromnode = res[[i]],
          level    = as.numeric(names(res)[[i]])
        )
      })
    )
    
  } else if (return_as == "vector") {
    
    res <- list(
      node  = res,
      level = level_res
    )
  }
  
  return(res)
  
}

#' Traverse graph in a Depth First Search (DFS) manner
#' @description Given a network with ID and toID columns, and a starting ID, traverse a river network using a Depth First Search (DFS) algorithm to explore all nodes of directed acyclic graph (DAG)
#' @param graph data.frame with ID, toID, hydroseq, startflag, terminalpa, and divergence attributes.
#' @param as_df logical, whether to return DFS traversal as a dataframe or as a list. If TRUE (default) DFS traversal will be returned as a dataframe.
#' @param return_as character string of how to return DFS traversal output. Either "list", "dataframe", "vector". Default is "list".
#' @param start ID of node to start traversal and go upstream from
#' @param dendritic logical, whether to make network dendritic or not. If FALSE, divergent nodes (divergence value of 2), will be connected to the upstream flowline and the network will contain cycles. Default is FALSE
#' @param verbose logical print status updates?
#' @return data.frame containing the distance between pairs of network outlets.
dfs_traversal2 <- function(
    graph,
    # as_df   = TRUE,
    return_as = "list",
    # start   = NULL,
    # dendritic = TRUE,
    verbose = FALSE
) {
  
  #
  # # create topology hashmap
  # topo_map <- make_topo_map(
  #   from_nodes  = graph$tonode,
  #   to_nodes    = graph$fromnode,
  #   from_ids    = graph$tocomid,
  #   to_ids      = graph$comid,
  #   streamcalc  = graph$streamcalc,
  #   streamorder = graph$streamorde,
  #   divergence  = graph$divergence
  # )
  
  # create topology hashmap
  topo_map <- make_topo_map(
    from_nodes  = graph$fromnode,
    to_nodes    = graph$tonode
    # from_nodes  = graph$tonode,
    # to_nodes    = graph$fromnode,
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
  root <- as.character(graph[graph$tocomid == "0", ]$tonode)
  
  # output result object
  if (return_as %in% c("list", "dataframe")) {
    res <- list()
    
  } else {
    # vector
    res <- vctrs::vec_c()
    # res <- vctrs::vec_c(topo_map$get(root)$to_node)
    
  }
  
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
    if(!marked$get(node)) {
      
      # message("!!!!!!! THIS IS THE VISITING PORTION OF THE DFS ALGO !!!!!!!")
      # Process the current node, add to result object
      if (return_as %in% c("list", "dataframe")) {
        
        # list
        res[[count]] <- topo_map$get(node)
        
      } else {
        # vector
        res <- vctrs::vec_c(res, vctrs::vec_c(node))
        # res <- vctrs::vec_c(res, vctrs::vec_c(topo_map$get(node)$to_node))
        # res <- vctrs::vec_c(res, vctrs::vec_c(topo_map$get(node)$edge))
        # data.frame(   order     = count, from_node = strsplit("112->111", "->")[[1]][1],
        #   to_node   = strsplit("112->111", "->")[[1]][2])
        
      }
      # res[[count]] <- topo_map$get(node)
      # res <- vctrs::vec_c(res, vctrs::vec_c(topo_map$get(node)$to_node))
      
      # set vertex to marked
      marked$set(node, TRUE)
      # marked$get(node)
      
      # neighbors of current vertex
      neighbors <- topo_map$get(node)$to_node
      
      # message("Neighbors of node ", node , " are:\n",  paste0(" --> ", c(neighbors), sep = "\n"))
      
      # iterate through neighbors and add to stack if NOT VISITED
      for (n in neighbors) {
        # message("neighbor n: ", n)
        if(!marked$get(n)) {
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
  if(verbose) {
    message("Returning DFS traversal order")
  }
  
  # format return into dataframe if as_df is TRUE
  if (return_as == "dataframe") {
    res <- dplyr::bind_rows(res)
  }
  
  return(res)
  
}

#' Find next cycle in a NHDPlus flowlines Network
#'
#' @param network data.frame with comid, tonode, fromnode, and (optionally) divergence and terminalpa attributes.
#' @param start integer COMID to start braid detection from. A start COMID should be provided in cases where network is made up of disconnected network components (i.e. a start COMID will detect all braids in each connected flowlines network) Default is NULL and braid checking will start from the flowline that has a tocomid value of "0", or a random flowline otherwise. 
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found
#' @export
#'
#' @examples
find_next_cycle <- function(
    network,
    start   = NULL,
    verbose = FALSE
) {
  
  # start  <- "8898002"
  # start  <- "fgdegd"
  # start = NULL
  # plot(network$geometry)
  # # pick a node to start at
  # start_node <- as.character(network$fromnode[1])
  
  # lower case names
  names(network) <- tolower(names(network))
  
  # create directed acyclic graph
  dag <- create_dag(network)
  # graph <- create_dag(network)
  
  # if a start COMID is given
  if(!is.null(start)) {
    
    # if start COMID is NOT found in the network, throw error
    if(!start %in% network$comid) { stop(start, " COMID not found in network")}
    
    if(verbose) {
      message("Starting braid detection at COMID: ", start)
    }
    
    # find fromnode of comid to start cycle detection at
    start_node <- as.character(dag$fromnode[ dag$comid == start])
    # start_node <- as.character(graph$fromnode[ graph$comid == start])
    
  } else {
    
    if ("0" %in% dag$tocomid) {
      
      # if no start COMID is given, use nodes where tocomid == 0, (i.e. start of graph)
      # start_node <- as.character(graph$fromnode[graph$tocomid == "0"])[1]
      start_node <- as.character(dag$fromnode[dag$tocomid == "0"])[1]
      
      if(verbose) {
        message("Starting braid detection at COMID: ", dag$comid[dag$tocomid == "0"][1])
        # message("Starting braid detection at COMID: ", graph$comid[graph$tocomid == "0"][1])
      }
      
    } else {
      
      if(verbose) {
        message("No 'tocomid' value equal to '0' found", 
                "\nStarting braid detection at COMID: ",  
                dag$comid[dag$fromnode == as.character(dag$fromnode)[1]]
                # graph$comid[graph$fromnode == as.character(graph$fromnode)[1]]
        )
      }
      
      start_node <- as.character(dag$fromnode)[1]
      # start_node <- as.character(graph$fromnode)[1]
    }
  }
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identified as a cycle and thus a braid
  # Questionable at this point, other option is the code using single_cycles() function 
  graph <- renode_circuits(dag)
  # graph <- renode_circuits(graph)
  
  # make an undirected graph from DAG
  graph <- make_undirected(graph)  
  
  # make hashmap for tonode/fromnode topology for traversing graph
  topo_map <- make_topo_map(
    from_nodes = graph$tonode,
    to_nodes   = graph$fromnode
  )
  
  # keep track of visited nodes
  visit <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  # visit$mset(.list = stats::setNames(
  #                           lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i){ FALSE }), 
  #                           unique(c(graph$fromnode, graph$tonode))))
  
  last_node <- "nope"
  
  # Depth first search function for detecting cycles 
  dfs <- function(node, prev) {
    
    if (visit$has(node)) {
      # if(visit$has(node)) {  
      return(FALSE)
      
    }
    
    # visit node
    visit$set(node, TRUE)
    
    # get the neighbors of current node
    neighbors <- topo_map$get(node)$to_node
    
    message("Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    last_node <<- node
    message("LAST NODE OF CYCLE: ", last_node)
    
    for (i in neighbors) {
      
      message("NEIGHBOR: ", i)
      
      if (i == prev) {
        message("!!! SKIPPING NEIGH == PREVIOUS --> ", i, " == ", prev, " !!!")
        next
      }
      
      if (!dfs(i, node)) {
        message("IN CYCLE: NEIGHBOR: ", i)
        message("IN CYCLE: NODE: ", node)
        message("!!! FOUND A CYCLE !!!")
        return(FALSE)
      }
      message("=============================")
    }
    
    return(TRUE)
  }
  
  # return cycle detection logical
  return(!dfs(start_node, "no_previous"))
  
}

find_first_cycles <- function(
    graph,
    start     = NULL,
    return_as = "list",
    verbose   = FALSE
) {
  
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
  
  if(verbose) {
    message("Starting braid detection at node: ", start_node)
    # message("Starting braid detection at COMID: ", graph$comid[graph$tocomid == "0"][1])
  }
  
  # make a topology hashmap to use in DFS 
  topo_map <- make_topo_map(
    # from_nodes = graph$tonode,
    # to_nodes   = graph$fromnode
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode
  )
  # topo_map$as_list()
  # keep track of visited nodes
  visits <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  visits$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
           function(i){ 0 }),
    unique(c(graph$fromnode, graph$tonode))
    # lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), function(i){ 0 }),
    # unique(c(ungraph$fromnode, ungraph$tonode))
  )
  )
  # visits$as_list()
  
  # keep track of cycles nodes
  cycles <- fastmap::fastmap()
  
  # # # set all marked values to FALSE
  # cycles$mset(.list = stats::setNames(
  #                         lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), function(i){ 0 }),
  #                         unique(c(ungraph$fromnode, ungraph$tonode))))
  
  # keep track of visited nodes
  prev_nodes <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  prev_nodes$mset(.list = stats::setNames(
    lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
           function(i){ 0 }),
    unique(c(graph$fromnode, graph$tonode))
    # lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), 
    # function(i){ 0 }),
    # unique(c(ungraph$fromnode, ungraph$tonode))
  )
  )
  # prev_nodes$as_list()
  
  # cycle number count
  count <- 1
  
  dfs <- function(n, p) {
    # dfs <- function(n, p, visits, prev_nodes) {
    
    # message("node: ", n)
    # message("node ", n, " visit value: ", visits$get(n))
    
    # node has been fully visited
    if (visits$get(n) == 2) {
      return()
    }
    
    # node is in process of visitation
    if (visits$get(n) == 1) {
      
      # # map/list to store current level nodes
      v <- vctrs::vec_c()
      # v    <- list()
      # v <- fastmap::fastmap()
      
      curr <- p
      v <- vctrs::vec_c(v, vctrs::vec_c(curr))
      # v    <- list(v, list(curr))
      
      # message("v BEFORE WHILE LOOP: ", paste0(c(v), sep = "\n"))
      
      # while parent of n is not equal to itself
      while (curr != n) {
        
        # update curr value to previous values of curr
        curr <- prev_nodes$get(curr)
        
        # add cycle vertices to v
        v <- vctrs::vec_c(v, vctrs::vec_c(curr))
        # v = list(v, list(curr))
      }
      
      # message("v AFTER WHILE LOOP: ", paste0(c(v), sep = "\n"))
      # message("!!!! MARKING CYCLE NODE = ", n, " !!!!!")
      # 
      # add vertices for current cycle number to cycles hashmap 
      cycles$set(as.character(count), v)
      
      # incremenet count
      count <<-  count + 1
      
      return()
      
    }
    
    # set parents of node n 
    prev_nodes$set(n, p)
    # prev_nodes$as_list()
    
    # mark node as partially visited
    visits$set(n, 1)
    
    # get neighbors of node n
    neighbors <- topo_map$get(n)$to_node
    
    # message("Neighbors of node: ",  n, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    # iterate through neighbors of node n
    for (vert in neighbors) {
      
      message("vert: ", vert)
      
      if(vert == prev_nodes$get(n)) {
        # message("vert ", vert, " equals parent ", prev_nodes$get(n))
        # message("!!!! SKIPPING ITERTATION !!!! ")
        next
      }
      
      # message("---- RUNNING DFS ON ----")
      # message("---> vert = ", vert)
      # message("---> n = ", n)
      # message("------------------")
      
      dfs(vert, n)
      
      # message("$$$ COUNT = ", count, " $$$")
      
      if(count > 1) {
        # message("COUNT > 1, RETURNING!!!")
        return()
        
      }
      # message("***********")
    }
    # message("!!! MARKING NODE ", n, " to 2 !!! ")
    
    # mark node as fully visited
    visits$set(n, 2)
    
    # message("=============================")
  }
  
  # Start DFS on a node
  dfs(start_node, "no_previous")
  
  # cycles$as_list()
  # cycles$size()
  
  # if(cycles$size() == 0 & is.null(single_braids)) {
  if(cycles$size() == 0) {
    message("No cycles found, returning NULL")
    return(NULL)
  }
  
  # extract cycles hashmap as a list
  result <- cycles$as_list()
  
  # if data should be returned as a 2 column dataframe with node and cycle_id
  if(return_as == "dataframe") {
    
    result <- lapply(1:length(result), function(k) {
      # result <- lapply(1:length(cycles$as_list()), function(k) {
      # get IDs of each node
      data.frame(
        node     = result[[k]],
        cycle_id = names(result[k])
        # node     = cycles$as_list()[[k]],
        # cycle_id = names(cycles$as_list()[k])
      )
    })  %>% 
      dplyr::bind_rows() %>%
      dplyr::group_by(node) %>% 
      dplyr::summarise(
        cycle_id = paste0(cycle_id, collapse = ", ")
      ) %>%
      dplyr::ungroup()
    
  }
  
  return(result)
  
}

# ------- SNAP NETWORK LINES TO MAKE CORRECT TO AND FROM NODES FOR UNDIRECTED GRAPH ----

#   # Function to find the closest node ID for each linestring
# assign_nodes <- function(sf_linestrings) {
#   # Convert linestrings to points representing start and end points
#   points <- st_cast(sf_linestrings, "POINT")
#   
#   # Compute pairwise distances between points
#   dist_matrix <- as.matrix(st_distance(points, points))
#   plot(points$geometry)
#   # Find the closest point for each point (excluding points from the same linestring)
#   closest_points <- apply(dist_matrix, 1, function(row) {
#     idx <- order(row)[-1]  # Exclude the first (diagonal) entry
#     closest_point_idx <- idx[1]
#     return(closest_point_idx)
#   })
#   
#   # Extract the IDs of the closest points
#   closest_point_ids <- sf_linestrings$comid[closest_points]
#   
#   # Create a new sf data frame with the "fromnode" and "tonode" columns
#   nodes_sf <- st_sf(ID = sf_linestrings$comid,
#                     fromnode2 = closest_point_ids,
#                     tonode2 = closest_point_ids)
#   
#   return(nodes_sf)
# }
# sf_linestrings <- roads_sf
# # Get the nodes_sf with "fromnode" and "tonode" columns
# nodes_sf <- assign_nodes(roads_sf)
# 
# # Combine nodes_sf with the original linestrings
# final_sf <- st_set_attributes(roads_sf, nodes_sf[, c("fromnode", "tonode")])
# 
# print(final_sf)

#' # ---- TEST NEW ALGO ----
#' # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 50 )
#' net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 20 )
#' plot(net2$geometry)
#' 
# get the braid_ids of all neighboring braids from a specified braid_id
# provide a network with braid_ids and a vector of braid_ids that you want to get all neighboring/memboring braid IDs for the specified braid
# x: sf object network flowlines with braid_id column
# ids: character vector braid_id(s)
# Only unique: if TRUE, then only unique braid IDs are returned, otherwise all are returned
get_neighbor_braids <- function(x, ids, split_ids = FALSE, only_unique = FALSE) {


  # x = braids
  # ids = bids
  # only_unique = T
  # split_ids = FALSE
  
  # make groups for each braided section
  braid_groups <- lapply(1:length(ids), function(i) {

    # message("i: ", i, "/", length(ids))

    # braid IDs of interest
    bids <- strsplit(ids[i], ", ")[[1]]
    # bids <- strsplit(transects[i, ]$braid_id, ", ")[[1]]

    # get all linestrings that are apart of the braid_ids of interest

    bids_check <- sapply(1:length(x$braid_id), function(y) {
      any(
        strsplit(x$braid_id[y], ", ")[[1]] %in% bids
      )
    })
    
    # out <- unique(c(
    #             x[bids_check, ]$braid_id,
    #             unlist(strsplit(x[bids_check, ]$braid_id, ", "))
    #             )
    #           )
    out <- sort(
              unique(c(
                x[bids_check, ]$braid_id,
                unlist(strsplit(x[bids_check, ]$braid_id, ", "))
              )
              )
            )

    # split_ids is TRUE, then the coimma seperated braid_ids are seperated into individual braid_ids
    if(split_ids) {

      out <- sort(
        unique(unlist(strsplit(out, ", ")))
        )
      # out <- unique(unlist(strsplit(out, ", ")))
    
    }

    out

  })

  # assign names
  names(braid_groups) <- ids
  
  # remove uniques if desired
  if(only_unique) {
    braid_groups <- unique(unname(unlist(braid_groups)))
  }
  return(braid_groups)
}

#' # Convert Network --> Adjaceny list/topology map (undirected graph)
#' # takes network object and creates a fastmap() topology map/adjacency list (undirected graph)
#' network_to_adj <- function(network, start = NULL, verbose = TRUE) {
#'   # # here we need to make 
#'   # network <- dplyr::filter(network, comid %in% big_braids)
#'   
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
#'   # mapview::mapview(sub_dag)
#'   
#'   # drop graph geometry
#'   dag <- sf::st_drop_geometry(dag)
#'   
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
#'   # make topology map (adjaceny list/relationships)
#'   topo <- make_topo_map(
#'     from_nodes = undir$fromnode,
#'     to_nodes   = undir$tonode
#'   )
#'   
#'   topo <- list(
#'             adjacency = topo,
#'             comids    = comid_map,
#'             graph     = undir
#'             )
#'   
#'   return(topo)
#'   
#' }
#' 
#' # paths is a list of lists where each outter list element represents a node on an undirected graph, 
#' # and inner list represents the path sequences (each inner list element is a character vector (i.e. c("1", "3", "5"))) between nodes, 
#' # starting from the original 'from_node' (outer list element)
#' make_path_map <- function(paths) {
#' 
#'   # add from_node names if they don't already exist
#'   if(is.null(names(paths))) {
#'     names(paths) <- paste0(1:length(paths))
#'   }
#'   
#'   path_map <- fastmap::fastmap()
#'   
#'   for (i in 1:length(paths)) {
#'     # message("iteration ", i, "/", length(paths))
#'     
#'     from_node <- names(paths[i])
#'     node_paths <- paths[[i]]
#'     
#'     # check if from_node has been added to hashmap already
#'     if (!path_map$has(from_node)) {
#'       edge_str <- sapply(1:length(node_paths), function(k) {
#'         paste0(node_paths[[k]], collapse = "-") 
#'       })
#'       
#'       names(node_paths) <- paste0(1:length(node_paths))
#'       
#'       map_entry <- list(
#'         paths       = node_paths,
#'         edge        = edge_str,
#'         count       = length(edge_str)
#'       )
#'       
#'       # set new entry in topology map
#'       path_map$set(from_node, map_entry)
#'       
#'     } else {
#'       # edge string
#'       edge_str <- sapply(1:length(node_paths), function(k) {
#'         paste0(node_paths[[k]], collapse = "-") 
#'       })
#'       
#'       names(node_paths) <- paste0(1:length(node_paths))
#'       
#'       map_entry <- list(
#'         paths       = c(path_map$get(from_node)$paths, node_paths),
#'         edge        = c(path_map$get(from_node)$edge, edge_str),
#'         count       = c(path_map$get(from_node)$count + 1)
#'       )
#'       
#'       # Update the existing entry in topo_map
#'       path_map$set(from_node, map_entry)
#'     }
#'   }
#'   
#'   return(path_map)
#' }
#' 
#' 
#' # graph is a fastmap::fastmap() representing a from_node/to_node topology of an undirected graph
#' # format, boolean, whether return sohuld be well formatted list from fastmap()$as_list() or, 
#' # if FALSE, just a list of lists with the outer list being the starting node and the inner lists containing the cycle paths
#' extract_cycles <- function(graph, format = TRUE) {
#' 
#'   # graph = topo
#'   # format = FALSE
#'   
#'   # define DFS function for generating all cycles
#'   dfs <- function(graph, start, end) {
#'     stack <- fastmap::faststack()
#'     
#'     stack$push(list(start, list()))
#'     
#'     paths <- list()
#'     count = 0
#'     
#'     while (stack$size() > 0) {
#'       node <- stack$pop()
#'       state <- node[[1]]
#'       path <- node[[2]]
#'       
#'       # if(!is.null(state)) { message("state: ", state) } 
#'       # if(!is.null(path)) { message("path: ", path) }
#'       if (length(path) > 0 && state == end) {
#'         paths <- c(paths, list(path))
#'         next
#'       }
#'       
#'       # ---- NEW WAY USING HASHMAP ADJ LIST ----
#'       # curr_state <- graph$get(as.character(state))$to_node
#'       
#'       for (next_state in graph$get(as.character(state))$to_node) {
#'       # for (next_state in curr_state) {
#'         # message("next_state: ", next_state)
#'         if (next_state %in% path) {
#'           next
#'         }
#'         
#'         new_node <- list(next_state, c(path, next_state))
#'         stack$push(new_node)
#'         count = count + 1
#'         
#'       }
#'       # # ---- ORIGINAL WAY USING LIST ADJ LIST ----
#'       # # message("ITERATING THRU --> graph[[state]]: ",   paste0(graph[[state]], collapse = "-"))
#'       # for (next_state in graph[[state]]) {
#'       #   message("next_state: ", next_state)
#'       #   if (next_state %in% path) {
#'       #     next
#'       #   }
#'       #   new_node <- list(next_state, c(path, next_state))
#'       #   stack$push(new_node)
#'       #   count = count + 1
#'       # message("=====================")
#'       # message("======== END ========")
#'       # message("======== count: ", count , " ========")
#'       # message("=====================")
#'       # }
#'     }
#'     
#'     return(paths)
#'   }
#' 
#'   # topo_map <- make_topo_map(
#'   #   from_nodes = undir$fromnode,
#'   #   to_nodes = undir$tonode,
#'   #   directed = FALSE)
#'   # # topo_map$as_list()
#'   # graph  <- topo_map
#'   # graph$as_list()
#'   # # # map/list to store current level nodes
#'   # # v <- vctrs::vec_c()
#'   
#'   # generate all cycles
#'   cycles <- lapply(graph$keys(), function(node) {
#'     
#'     # run DFS on each start node
#'     lapply(dfs(graph, node, node), function(path) {
#'       unlist(c(node, path))
#'     })
#'     
#'   })
#'   
#'   # if data should be returned in a formatted fastmap list (hashmap)
#'   if(format) {
#'     cycles <- make_path_map(cycles)$as_list()
#'     
#'     return(cycles)
#'     
#'   }
#'   
#'   # add names
#'   names(cycles) <- paste0(1:length(cycles))
#'   # names(cycles) <- paste0("cycle_", 1:length(cycles))
#'   
#'   return(cycles)
#'   
#'   # OLD DFS
#'   # dfs <- function(graph, start, end) {
#'   #   stack <- fastmap::faststack()
#'   #   stack$push(list(start, list()))
#'   #   paths <- list()
#'   #   while (stack$size() > 0) {
#'   #     node <- stack$pop()
#'   #     state <- node[[1]]
#'   #     path <- node[[2]]
#'   #     if (length(path) > 0 && state == end) {
#'   #       paths <- c(paths, list(path))
#'   #       next
#'   #     }
#'   #     for (next_state in graph[[state]]) {
#'   #       if (next_state %in% path) { next }
#'   #       new_node <- list(next_state, c(path, next_state))
#'   #       stack$push(new_node)
#'   #     }}
#'   #   return(paths)
#'   # }
#'   # graph <- list(
#'   #   "1" = c(2), "2" = c(1, 3, 4),
#'   #   "3" = c(2, 5, 6), "4" = c(2, 5),
#'   #   "5" = c(3, 4, 7, 8),
#'   #   "6" = c(3, 7), 
#'   # "7" = c(5, 6),
#'   # "8" = c(5))
#'   
#' }
#' 
#' # Clean the outputs of extract_cycles (internal)
#' # cycles is a list of lists each outter list element is the starting node and 
#' # the inner list elements are the node IDs of any cycles found from the starting node
#' # edge_minimum: numeric, minimum number of edges to be considered a cycle. Default is 3, all cycles with less than 3 edges are removed
#' clean_cycles <- function(cycles, edge_minimum = 3) {
#'   # cycles = sub_cycles
#'   # edge_minimum = 5
#'   # filter out the single circuit/simple braids, and keep the unique list elements
#'   trim_cycles <- lapply(1:length(cycles), function(k) {
#'     # k = 16
#'     # cycles
#'     # filter to cycles that have more than 3 members
#'     cycles[[k]] <- cycles[[k]][
#'       lengths(cycles[[k]]) > edge_minimum
#'     ]
#'     
#'     if(length(cycles[[k]]) == 0) {
#'       NULL
#'     } else {
#'       cycles[[k]]
#'     }
#'   }) %>% 
#'     unique()
#'   
#'   # go through each of the trimmed down cycles and further remove duplicates
#'   # keep only the unique and then unlist the output retrim list
#'   trim_cycles <- lapply(1:length(trim_cycles), function(k) {
#'     
#'     # just return NULL if no cycle nodes
#'     if(is.null(trim_cycles[[k]])) {
#'       NULL
#'     } else {
#'       
#'       # for each cycle path for a given node, grab the unique node values, sort the vector, and make a string from path (sep = "-")
#'       lapply(1:length(trim_cycles[[k]]), function(z) {
#'         path <- trim_cycles[[k]][[z]]
#'         paste0(sort(unique(path)), collapse = "-")
#'       }) %>% 
#'         unique()
#'     }
#'     
#'   }) %>% 
#'     unique() %>% 
#'     unlist()
#'   
#'   # split each braid into individual braids
#'   trim_cycles <- strsplit(trim_cycles, "-")
#'   
#'   # assign names to braid
#'   names(trim_cycles) <- paste0("new_braid_", 1:length(trim_cycles))
#'   
#'   return(trim_cycles)
#'   
#' }
#' tmpfunc <- function() {
#'   
#' 
#'   # define DFS function for generating all cycles
#'   dfs <- function(graph, start, end) {
#'     stack <- fastmap::faststack()
#'     
#'     stack$push(list(start, list()))
#'     
#'     paths <- list()
#'     count = 0
#'     
#'     while (stack$size() > 0) {
#'       node <- stack$pop()
#'       state <- node[[1]]
#'       path <- node[[2]]
#'       
#'       # if(!is.null(state)) { message("state: ", state) } 
#'       # if(!is.null(path)) { message("path: ", path) }
#'       if (length(path) > 0 && state == end) {
#'         paths <- c(paths, list(path))
#'         next
#'       }
#'       
#'       # ---- NEW WAY USING HASHMAP ADJ LIST ----
#'       # curr_state <- graph$get(as.character(state))$to_node
#'       
#'       for (next_state in graph$get(as.character(state))$to_node) {
#'         # for (next_state in curr_state) {
#'         # message("next_state: ", next_state)
#'         if (next_state %in% path) {
#'           next
#'         }
#'         
#'         new_node <- list(next_state, c(path, next_state))
#'         stack$push(new_node)
#'         count = count + 1
#'         
#'       }
#'       # # ---- ORIGINAL WAY USING LIST ADJ LIST ----
#'       # # message("ITERATING THRU --> graph[[state]]: ",   paste0(graph[[state]], collapse = "-"))
#'       # for (next_state in graph[[state]]) {
#'       #   message("next_state: ", next_state)
#'       #   if (next_state %in% path) {
#'       #     next
#'       #   }
#'       #   new_node <- list(next_state, c(path, next_state))
#'       #   stack$push(new_node)
#'       #   count = count + 1
#'       # message("=====================")
#'       # message("======== END ========")
#'       # message("======== count: ", count , " ========")
#'       # message("=====================")
#'       # }
#'     }
#'     
#'     return(paths)
#'   }
#'   library(fastmap)
#'   df
#'   graph
#'   # # Sample dataframe representing the graph with "fromnode" and "tonode" columns
#'   # df <- data.frame(
#'   #   fromnode = c(1, 1, 1, 2, 3, 2, 4, 8, 8, 9),
#'   #   tonode = c(2, 3, 4, 3, 4, 6, 6, 7, 9, 7)
#'   # )
#'   df <- graph
#'   library(fastmap)
#'   
#'   # Sample dataframe representing the graph with "fromnode" and "tonode" columns
#'   df <- data.frame(
#'     fromnode = c(1, 1, 1, 2, 3, 2, 4, 8, 8, 9),
#'     tonode = c(2, 3, 4, 3, 4, 6, 6, 7, 9, 7)
#'   )
#'   
#'   cycles <- list()
#'   
#'   main <- function() {
#'     for (i in 1:nrow(df)) {
#'       visited_nodes <- c(df$fromnode[i])
#'       findNewCycles(list(df$fromnode[i]), visited_nodes, visited_edges = character(0))
#'     }
#'     
#'     for (cy in cycles) {
#'       path <- sapply(cy, function(node) { as.character(node) })
#'       s <- paste(path, collapse = ",")
#'       print(s)
#'     }
#'   }
#'   
#'   findNewCycles <- function(path, visited_nodes, visited_edges) {
#'     start_node <- path[[1]]
#'     next_node <- NULL
#'     sub <- list()
#'     
#'     for (i in 1:nrow(df)) {
#'       node1 <- df$fromnode[i]
#'       node2 <- df$tonode[i]
#'       edge <- paste(node1, node2, sep = "->")
#'       if (start_node == node1 || start_node == node2) {
#'         if (node1 == start_node) {
#'           next_node <- node2
#'         } else {
#'           next_node <- node1
#'         }
#'         
#'         if (!(next_node %in% visited_nodes)) {
#'           sub <- list(next_node)
#'           sub <- c(sub, path)
#'           visited_nodes <- c(visited_nodes, next_node)
#'           visited_edges <- c(visited_edges, edge)
#'           findNewCycles(sub, visited_nodes, visited_edges)
#'         } else if (length(path) > 2 && next_node == path[[length(path)]]) {
#'           p <- rotate_to_smallest(path)
#'           inv <- invert(p)
#'           if (isNew(p) && isNew(inv) && all(duplicated(visited_edges) == FALSE)) {
#'             cycles <<- c(cycles, list(p))
#'           }
#'         }
#'       }
#'     }
#'   }
#'   
#'   invert <- function(path) {
#'     return(rotate_to_smallest(rev(path)))
#'   }
#'   
#'   rotate_to_smallest <- function(path) {
#'     n <- which.min(path)
#'     return(c(path[n:length(path)], path[1:(n - 1)]))
#'   }
#'   
#'   isNew <- function(path) {
#'     sorted_path <- sort(sapply(path, as.character))
#'     for (cy in cycles) {
#'       sorted_cy <- sort(sapply(cy, as.character))
#'       if (identical(sorted_cy, sorted_path)) {
#'         return(FALSE)
#'       }
#'     }
#'     return(TRUE)
#'   }
#'   
#'   main()
#'   
#'   
#'   cycles <- list()
#'   
#'   main <- function() {
#'     res <- vctrs::vec_c()
#'     
#'     for (i in 1:nrow(df)) {
#'       findNewCycles(list(df$fromnode[i]), vector())
#'     }
#'     
#'     for (cy in cycles) {
#'       path <- sapply(cy, function(node) { as.character(node) })
#'       s <- paste(path, collapse = "-")
#'       print(s)
#'       
#'       res <- vctrs::vec_c(res, s)
#'       
#'     }
#'     return(res)
#'   }
#'   
#'   findNewCycles <- function(path, visited_nodes) {
#'     start_node <- path[[1]]
#'     next_node <- NULL
#'     sub <- list()
#'     
#'     for (i in 1:nrow(df)) {
#'       node1 <- df$fromnode[i]
#'       node2 <- df$tonode[i]
#'       if (start_node == node1 || start_node == node2) {
#'         if (node1 == start_node) {
#'           next_node <- node2
#'         } else {
#'           next_node <- node1
#'         }
#'         
#'         if (!(next_node %in% visited_nodes)) {
#'           sub <- list(next_node)
#'           sub <- c(sub, path)
#'           visited_nodes <- c(visited_nodes, next_node)
#'           findNewCycles(sub, visited_nodes)
#'         } else if (length(path) > 2 && next_node == path[[length(path)]]) {
#'           p <- rotate_to_smallest(path)
#'           inv <- invert(p)
#'           if (isNew(p) && isNew(inv)) {
#'             cycles <<- c(cycles, list(p))
#'           }
#'         }
#'       }
#'     }
#'   }
#'   
#'   invert <- function(path) {
#'     return(rotate_to_smallest(rev(path)))
#'   }
#'   
#'   rotate_to_smallest <- function(path) {
#'     n <- which.min(path)
#'     return(c(path[n:length(path)], path[1:(n - 1)]))
#'   }
#'   
#'   isNew <- function(path) {
#'     sorted_path <- sort(sapply(path, as.character))
#'     for (cy in cycles) {
#'       sorted_cy <- sort(sapply(cy, as.character))
#'       if (identical(sorted_cy, sorted_path)) {
#'         return(FALSE)
#'       }
#'     }
#'     return(TRUE)
#'   }
#'   
#'   out <- main()
#'   graph <- list(
#'     c(1, 2),
#'     c(1, 3), 
#'     c(1, 4), 
#'     c(2, 3), 
#'     c(3, 4),
#'     c(2, 6),
#'     c(4, 6), 
#'     c(8, 7), 
#'     c(8, 9), 
#'     c(9, 7)
#'   )
#'   
#'   cycles <- list()
#'   
#'   main <- function() {
#'     for (edge in graph) {
#'       for (node in edge) {
#'         findNewCycles(list(node))
#'       }
#'     }
#'     
#'     for (cy in cycles) {
#'       path <- sapply(cy, function(node) { as.character(node) })
#'       s <- paste(path, collapse = ",")
#'       print(s)
#'     }
#'   }
#'   
#'   findNewCycles <- function(path) {
#'     start_node <- path[[1]]
#'     next_node <- NULL
#'     sub <- list()
#'     
#'     for (edge in graph) {
#'       node1 <- edge[[1]]
#'       node2 <- edge[[2]]
#'       if (start_node %in% edge) {
#'         if (node1 == start_node) {
#'           next_node <- node2
#'         } else {
#'           next_node <- node1
#'         }
#'         
#'         if (!visited(next_node, path)) {
#'           sub <- list(next_node)
#'           sub <- c(sub, path)
#'           findNewCycles(sub)
#'         } else if (length(path) > 2 && next_node == path[[length(path)]]) {
#'           p <- rotate_to_smallest(path)
#'           inv <- invert(p)
#'           if (isNew(p) && isNew(inv)) {
#'             cycles <<- c(cycles, list(p))
#'           }
#'         }
#'       }
#'     }
#'   }
#'   
#'   invert <- function(path) {
#'     return(rotate_to_smallest(rev(path)))
#'   }
#'   
#'   rotate_to_smallest <- function(path) {
#'     n <- which.min(path)
#'     return(c(path[n:length(path)], path[1:(n - 1)]))
#'   }
#'   
#'   isNew <- function(path) {
#'     sorted_path <- sort(sapply(path, as.character))
#'     for (cy in cycles) {
#'       sorted_cy <- sort(sapply(cy, as.character))
#'       if (identical(sorted_cy, sorted_path)) {
#'         return(FALSE)
#'       }
#'     }
#'     return(TRUE)
#'   }
#'   
#'   visited <- function(node, path) {
#'     return(node %in% path)
#'   }
#'   
#'   main()
#'   
#' }
#' find_all_cycles <- function(
#'     network,
#'     start        = NULL,
#'     return_as    = "list",
#'     add          = FALSE,
#'     nested       = TRUE,
#'     verbose      = FALSE
#' ) {
#'   
#'   # check valid return_as input
#'   if(!return_as %in% c("list", "dataframe")) {
#'     stop("Invalid 'return_as' argument '",
#'          return_as, "'\n'return_as' must be: 'list' or 'dataframe'")
#'   }
#'   
#'   # # check relationship argument is valid
#'   # if(return_as == "dataframe") {
#'   #   if(!relationship %in% c("one-to-one", "one-to-many")) {
#'   #     stop("Invalid 'relationship' argument '",
#'   #          relationship, "'\n'relationship' must be: 'one-to-one' or 'one-to-many'")
#'   #   }
#'   # }
#'   # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 50 )
#'   net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 20)
#'   plot(net2$geometry)
#'   network <- net2
#'   start        = NULL
#'   return_as    = "list"
#'   add          = FALSE
#'   nested       = TRUE
#'   verbose      = TRUE
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
#'   # make a topology hashmap to use in DFS 
#'   topo_map <- make_topo_map(
#'     from_nodes = undir$fromnode,
#'     to_nodes   = undir$tonode,
#'     directed   = FALSE
#'   )
#'   
#'   # cycles <- extract_cycles(graph = topo_map, format = TRUE)
#'   # cycles2 <- extract_cycles(graph = topo_map, format = F)
#'   # 
#'   # comid_map$braid_count <- lengths(cycles2)
#'   # trim_cycles <- lapply(1:length(cycles2), function(k) {
#'   #   
#'   #   cycles2[[k]] <- cycles2[[k]][
#'   #     lengths(cycles2[[k]]) > 3
#'   #   ]
#'   #   
#'   #   if(length(cycles2[[k]]) == 0) {
#'   #     NULL
#'   #   } else {
#'   #     cycles2[[k]]
#'   #   }
#'   # })
#'   
#'   braids <- find_braids(network = network, return_as = "dataframe", add = T, nested = TRUE, verbose = T)
#'   braids_un <- find_braids(network = network, return_as = "dataframe", add = T, nested = F, verbose = T)
#'   braids_un <-  dplyr::arrange(braids_un, hydroseq)
#'   
#'   braids_lst <- find_braids(network = network, 
#'                             return_as = "list", 
#'                             add = T, 
#'                             nested = T, 
#'                             verbose = T
#'                             )
#'   
#'   # unpack nested braids into list format
#'   unpacked <- 
#'     braids %>% 
#'     sf::st_drop_geometry() %>% 
#'     dplyr::filter(braid_id != "no_braid") %>% 
#'     dplyr::select(comid, braid_id) %>% 
#'     unnpack_braids(into_list = T)
#'   
#'   # braids$braid_id
#'   # 
#'   # 
#'   # braids %>% 
#'   #   dplyr::mutate(
#'   #     braid_id
#'   #   )
#'   # 
#'   # unpacked <- unnpack_braids(braids)
#'   # 
#'   # 
#'   # make_braids_gif(braids_un,
#'   #                 save_path  = "/Users/anguswatters/Desktop/braid_check.gif",
#'   #                 height     = 8,
#'   #                 width      = 10,
#'   #                 gif_width  = 1800,
#'   #                 gif_height = 1500,
#'   #                 delay      = 0.8,
#'   #                 legend_pos = "bottom",
#'   #                 verbose    = TRUE
#'   # )
#'   # 
#'   # lst <- braids_lst
#'   # no_overlap_val = FALSE
#'   
#'   overlaps <- find_overlaps(unpacked,   
#'                             no_overlap_val = TRUE,
#'                             rm_no_overlap  = FALSE
#'                             )
#'   
#'   # # Find the indices of elements that are not TRUE (i.e., numbers)
#'   total_overlaps <- 1:length(overlaps)
#' 
#'   # get indicies of braids that have multiple overlaps and also the main large braid
#'   big_idx <- c(
#'                 total_overlaps[which(!sapply(overlaps, isTRUE))],
#'                 unique(unlist(overlaps[which(!sapply(overlaps, isTRUE))]))
#'               )
#'   
#'   # get indicies of main large braids
#'   big_idx <- unique(
#'               unlist(
#'                 overlaps[which(!sapply(overlaps, isTRUE))]
#'                 )
#'               )
#'   
#'   # big_idx <- unique(unlist(overlaps))
#'   
#'   # big_braids <- braids_lst[big_idx]
#'   big_names <- names(unpacked[big_idx])
#'   
#'   # get the comids of the big braids
#'   big_braids <- unique(unlist(unname(unpacked[big_idx])))
#'   
#'   # locate neighboring braids to our big_names braid_ids of interest
#'   bids <- get_neighbor_braids(
#'     x   = braids, 
#'     ids = big_names,
#'     split_ids = TRUE
#'     # only_unique = T
#'     )
#'   
#'   # boi[[1]]
#'   
#'   lapply(1:length(bids), function(k) {
#'     k = 1
#'     
#'     # main braid name
#'     bname <- names(bids)[[k]]
#'     
#'     # braids of interest
#'     bois <- bids[[k]]
#'     
#'     big_bids <- unique(
#'                   unlist(
#'                     unname(
#'                       unpacked[names(unpacked) %in% bois]
#'                       )
#'                     )
#'                   )
#'     # here we need to make 
#'     sub_net <- dplyr::filter(braids, comid %in% big_bids)
#'     
#'     
#'     topo <- network_to_adj(network = sub_net,
#'                            start = NULL, 
#'                            verbose = TRUE
#'                            )
#'     
#'     # comid map and topology hashmap
#'     comid_map <- topo$comids
#'     topology  <- topo$adjacency
#'     graph     <- topo$graph
#'     
#'     # find_cycles(graph = graph)
#'     # trav <- dfs_traversal2(graph = graph, return_as = "dataframe", graph_as_map = F, verbose = T)
#'     # 
#'     # trav$from_node <-  sub("(.*)->.*", "\\1",   trav$edge)
#'     # 
#'     # unique(sort(graph$tonode)) %in% unique(sort(as.numeric(trav$from_node)))
#'     # 
#'     # trav$from_node %>% unique()
#'     # 
#'     # 
#'     # dfs_traversal2(graph = topology, return_as = "list", graph_as_map = T, verbose = T)
#'     
#'     # extract sub cycles
#'     sub_cycles <- extract_cycles(graph = topology, format = F)
#'     
#'     # remove non cycles and duplicates and give braid_ids
#'     sub_cycles <- clean_cycles(sub_cycles, edge_minimum = 3)
#'     
#'     base_cycle <- 
#'       sub_net %>% 
#'       dplyr::select(comid, braid_id, fromnode, tonode) %>% 
#'       dplyr::left_join(
#'         dplyr::select(
#'           comid_map, 
#'           comid, 
#'           from_node = fromnode,
#'           to_node = tonode
#'         ), 
#'         by = "comid"
#'       )
#'     
#'     sub_cycles
#'     res = c()
#'     i = 1
#'     for (i in 1:length(sub_cycles)) {
#'       sub_cycles[[i]]
#'       
#'       tmp <- 
#'         base_cycle %>% 
#'         dplyr::filter(!from_node %in% sub_cycles[[i]])
#'       
#'       # tmp_map <- 
#'       #   comid_map %>% 
#'       #   dplyr::filter(!fromnode %in% sub_cycles[[i]])
#'       
#'       mapview::mapview(base_cycle, color = "dodgerblue") + 
#'         mapview::mapview(updated_braids, color = "green") + 
#'         mapview::mapview(tmp, color = "red") 
#'       
#'       add_ids <- sub_cycles[[i]]
#'       add_comids <- comid_map[comid_map$fromnode %in% sub_cycles[[i]], ]$comid
#'       
#'       for (z in 1:length(add_comids)) {
#'         z = 1
#'         add_ids[z]
#'         add_comids[z]
#'         
#'         # map_add <- comid_map[comid_map$comid == add_comids[z], ]
#'         to_add <- base_cycle[base_cycle$comid == add_comids[z], ]
#'         
#'         before_add <- is_braided(dplyr::select(
#'           tmp,
#'           -from_node, -to_node)
#'           )
#'         
#'         # temporarily add new edge
#'         # tmp_map <- dplyr::bind_rows(tmp_map, map_add)  
#'         tmp <- dplyr::bind_rows(tmp, to_add)  
#'         
#'        is_braided(dplyr::select(
#'           base_cycle,
#'           -from_node, -to_node)
#'         )
#'         
#'         
#'       }
#'       
#'       
#'       i = 1
#'       
#'     }
#'     
#'     
#'     new_braids <- lapply(1:length(sub_cycles), function(k) {
#'       
#'       res <- dplyr::filter(comid_map, fromnode %in% sub_cycles[[k]]) 
#'       
#'       res$braid_id <- names(sub_cycles)[[k]]
#'       
#'       res
#'       
#'     }) %>% 
#'       dplyr::bind_rows()
#'     
#'     orig_braids <- 
#'       braids %>% 
#'       dplyr::select(comid, old_braid_id = braid_id, fromnode, tonode, geometry)
#'     
#'     updated_braids <- 
#'       orig_braids %>% 
#'       dplyr::filter(comid %in% new_braids$comid) %>% 
#'       dplyr::left_join(
#'         dplyr::select(new_braids, comid, from_node = fromnode, to_node = tonode, braid_id),
#'         by = "comid"
#'       )
#'     mapview::mapview(orig_braids, color = "dodgerblue") + 
#'       mapview::mapview(updated_braids, color = "red") +
#'       mapview::mapview(new_biggy, color = "green") +
#'       # mapview::mapview(the_rest, color = "gold") +
#'       mapview::mapview(braids, color = "gold") 
#'     mapview::mapview(biggy, color = "dodgerblue") + 
#'       mapview::mapview(smalls, color = "red") +
#'       mapview::mapview(smalls_all, color = "green") +
#'       mapview::mapview(the_rest, color = "gold") +
#'       mapview::mapview(braids, color = "gold") 
#'     
#'     new_big_map <- 
#'       comid_map %>% 
#'       dplyr::filter(!comid %in% new_braids$comid)
#'     new_biggy <- 
#'       orig_braids %>% 
#'       # dplyr::filter(comid %in% big_bids, !comid %in% new_braids$comid) %>% 
#'       dplyr::filter(comid %in% big_bids) %>% 
#'       dplyr::left_join(
#'         dplyr::select(new_big_map, comid, from_node = fromnode, to_node = tonode),
#'         by = "comid"
#'       )
#'     
#'     mapview::mapview(orig_braids, color = "dodgerblue") + 
#'       mapview::mapview(updated_braids, color = "red") +
#'       mapview::mapview(new_biggy, color = "green") +
#'       # mapview::mapview(the_rest, color = "gold") +
#'       mapview::mapview(braids, color = "gold") 
#'     biggy <- 
#'       braids %>% 
#'       dplyr::filter(comid %in% big_bids) %>% 
#'       dplyr::select(comid, fromnode, tonode)
#'     
#'     smalls <- 
#'       braids %>% 
#'       dplyr::filter(comid %in% unname(unlist(sub_cycles)))
#'     sub_cycles
#'     unname(unlist(sub_cycles))
#'     
#'     smalls_coms <- dplyr::filter(comid_map, 
#'         fromnode %in% unname(unlist(sub_cycles))
#'         # fromnode %in% unname(unlist(sub_cycles)) |  tonode %in% unname(unlist(sub_cycles))
#'       )$comid  
#'     
#'     smalls_coms_all <-  dplyr::filter(comid_map,
#'                       # fromnode %in% unname(unlist(sub_cycles))
#'                       fromnode %in% unname(unlist(sub_cycles)) |  tonode %in% unname(unlist(sub_cycles))
#'                     )$comid
#'     smalls <- 
#'       braids %>% 
#'       dplyr::filter(comid %in% smalls_coms)
#'     
#'     smalls_all <- 
#'       braids %>% 
#'       dplyr::filter(comid %in% smalls_coms_all)
#'     the_rest <- 
#'       biggy %>% 
#'       dplyr::filter(!comid %in% smalls_coms)
#'     
#'     mapview::mapview(biggy, color = "dodgerblue") + 
#'       mapview::mapview(smalls, color = "red") +
#'       mapview::mapview(smalls_all, color = "green") +
#'       mapview::mapview(the_rest, color = "gold") +
#'     mapview::mapview(braids, color = "gold") 
#'       # mapview::mapview(biggy, color = "dodgerblue")
#'     
#'     })
#'   
#'   
#'   unpacked[names(unpacked) %in% bids[[1]]]
#'   
#'   overlaps
#'   
#'   # tt <-  c(tt, list(braid_5 = c(tt$braid_6, "braid_3")))
#'   # tt <-  c(tt, list(braid_5 = c(tt$braid_6)))
#'   # tt %>% unique()
#'   
#'   # tmp <- 
#'   #   braids %>% 
#'   #   dplyr::filter(braid_id != "no_braid") %>% 
#'   #   dplyr::filter(braid_id %in% big_names | grepl(big_names, braid_id)) %>% 
#'   #   dplyr::select(braid_id, comid, geometry)
#'   # 
#'   # tmp2 <- 
#'   #   braids %>% 
#'   #   dplyr::filter(braid_id != "no_braid") %>% 
#'   #   dplyr::filter(braid_id %in% boi[[1]] | grepl(big_names, braid_id)) %>%
#'   #   dplyr::select(braid_id, comid, geometry)
#'   
#'   # mapview::mapview(tmp, color = "red") +
#'   #   mapview::mapview(tmp2, color = "green") +
#'   #   dplyr::select(braids, braid_id, comid)
#'   
#'   # here we need to make 
#'   sub_net <- network %>% dplyr::filter(comid %in% big_braids)
#'   
#'   names(sub_net) <- tolower(names(sub_net))
#'   
#'   # turn network into a directed graph
#'   sub_dag <- create_dag(sub_net)
#'   
#'   # get the fromnode associated with a given COMID 'start'
#'   sub_start <- comid_to_node(sub_dag, start)
#'   
#'   if(verbose) {
#'     message("Starting braid detection at ", 
#'             ifelse(is.null(start), paste0("node: ", start_node), paste0("COMID: ", start)))
#'   }
#'   # mapview::mapview(sub_dag)
#'   # drop graph geometry
#'   sub_dag <- sf::st_drop_geometry(sub_dag)
#'   # graph <- sf::st_drop_geometry(graph)
#'   
#'   # stash comids and from IDs
#'   sub_comid_map <- dplyr::select(
#'     # graph,
#'     sub_dag,
#'     comid,
#'     fromnode,
#'     tonode
#'   )
#'   
#'   # create artificial cycles from circuits in graph, doing this allows for 
#'   # sections of river that form a circuit to be identfied as a cycle and thus a braid
#'   # Questionable at this point, other option is the code below that uses 
#'   # single_cycles() function (COMMENTED OUT CODE BELOW)
#'   sub_regraph <- renode_circuits(sub_dag, verbose = FALSE)
#'   
#'   # make an undirected graph
#'   sub_undir <- make_undirected(sub_regraph)
#'   
#'   sub_topo <- make_topo_map(
#'     from_nodes = sub_undir$fromnode,
#'     to_nodes = sub_undir$tonode
#'   )
#'   
#'   sub_cycles <- extract_cycles(graph = sub_topo, format = F)
#'   # sub_cycles <- extract_cycles(graph = topo, format = TRUE)
#'   # sub_cycles2 <- extract_cycles(graph = topo, format = F)
#'   # length(sub_cycles2)
#'   # unname(lengths(sub_cycles2))
#'   # comid_map$braid_count <- lengths(sub_cycles)
#'   clean_cycles <- function(cycles, edge_minimum = 3) {
#'     
#'     # filter out the single circuit/simple braids, and keep the unique list elements
#'     trim_cycles <- lapply(1:length(cycles), function(k) {
#'       
#'       # filter to cycles that have more than 3 members
#'       cycles[[k]] <- cycles[[k]][
#'         lengths(cycles[[k]]) > edge_minimum
#'       ]
#'       
#'       if(length(cycles[[k]]) == 0) {
#'         NULL
#'       } else {
#'         cycles[[k]]
#'       }
#'     }) %>% 
#'       unique()
#'     
#'     # go through each of the trimmed down cycles and further remove duplicates
#'     # keep only the unique and then unlist the output retrim list
#'     trim_cycles <- lapply(1:length(trim_cycles), function(k) {
#'       
#'       # just return NULL if no cycle nodes
#'       if(is.null(trim_cycles[[k]])) {
#'         NULL
#'       } else {
#'         
#'         # for each cycle path for a given node, grab the unique node values, sort the vector, and make a string from path (sep = "-")
#'         lapply(1:length(trim_cycles[[k]]), function(z) {
#'           path <- trim_cycles[[k]][[z]]
#'           paste0(sort(unique(path)), collapse = "-")
#'         }) %>% 
#'           unique()
#'       }
#'       
#'     }) %>% 
#'       unique() %>% 
#'       unlist()
#'     
#'     # split each braid into individual braids
#'     trim_cycles <- strsplit(trim_cycles, "-")
#'     
#'     # assign names to braid
#'     names(trim_cycles) <- paste0("new_braid_", 1:length(trim_cycles))
#'     
#'     return(trim_cycles)
#'     
#'   }
#'   # # filter out the single circuit/simple braids, and keep the unique list elements
#'   # trim_cycles <- lapply(1:length(sub_cycles), function(k) {
#'   #   
#'   #   # filter to cycles that have more than 3 members
#'   #   sub_cycles[[k]] <- sub_cycles[[k]][
#'   #                               lengths(sub_cycles[[k]]) > 3
#'   #                             ]
#'   #   
#'   #   if(length(sub_cycles[[k]]) == 0) {
#'   #     NULL
#'   #   } else {
#'   #     sub_cycles[[k]]
#'   #   }
#'   # }) %>% 
#'   #   unique()
#'   # 
#'   # # go through each of the trimmed down cycles and further remove duplicates
#'   # # keep only the unique and then unlist the output retrim list
#'   # trim_cycles <- lapply(1:length(trim_cycles), function(k) {
#'   #   
#'   #   # just return NULL if no cycle nodes
#'   #   if(is.null(trim_cycles[[k]])) {
#'   #     NULL
#'   #   } else {
#'   #     
#'   #     # for each cycle path for a given node, grab the unique node values, sort the vector, and make a string from path (sep = "-")
#'   #     lapply(1:length(trim_cycles[[k]]), function(z) {
#'   #       path <- trim_cycles[[k]][[z]]
#'   #       paste0(sort(unique(path)), collapse = "-")
#'   #     }) %>% 
#'   #       unique()
#'   #   }
#'   #   
#'   # }) %>% 
#'   #   unique() %>% 
#'   #   unlist()
#'   # 
#'   # # split each braid into individual braids
#'   # trim_cycles <- strsplit(trim_cycles, "-")
#'   # 
#'   # # assign names to braid
#'   # names(trim_cycles) <- paste0("new_braid_", 1:length(trim_cycles))
#'   
#'   # retrim[[1]][[1]]
#'   # sub_comid_map$fromnode %in% retrim[[1]]
#' 
#'   new_braids <- lapply(1:length(retrim), function(k) {
#' 
#'     res <- dplyr::filter(sub_comid_map, fromnode %in% retrim[[k]]) 
#'     
#'     res$braid_id <- names(retrim)[[k]]
#'     
#'     res
#'     
#'     }) %>% 
#'     dplyr::bind_rows()
#'   
#'   orig_braids <- 
#'     braids %>% 
#'     dplyr::select(comid, old_braid_id = braid_id, fromnode, tonode, geometry)
#'   
#'   updated_braids <- 
#'     orig_braids %>% 
#'     dplyr::filter(comid %in% new_braids$comid) %>% 
#'     dplyr::left_join(
#'       dplyr::select(new_braids, comid, from_node = fromnode, to_node = tonode, braid_id),
#'       by = "comid"
#'     )
#'   updated_braids
#'   mapview::mapview(orig_braids, color = "dodgerblue") +
#'     mapview::mapview(updated_braids, color = "red")
#'   
#'   trim_cycles[[4]][[1]] == trim_cycles[[4]][[2]]
#'   unique(unlist(trim_cycles[[4]]))
#'   
#'   length(unique(trim_cycles))
#'   topo <- make_topo_map(
#'     from_nodes = braid_nodes$fromnode,
#'     to_nodes = braid_nodes$tonode
#'   )
#'   
#'   topo$as_list()
#'   cyc1 <- extract_cycles(graph = topo, format = TRUE)
#'   cyc2 <- extract_cycles(graph = topo, format = FALSE)
#'   
#'   # cycles <- extract_cycles(graph = topo_map, format = TRUE)
#'   # cycles2 <- extract_cycles(graph = topo_map, format = FALSE)
#'   overlaps[overlaps == FALSE] = TRUE
#'   
#'   braids_lst[overlaps]
#'   braids
#'   only_braids <- dplyr::filter(braids, braid_id != "no_braid")
#'   # only_braids$braid_id
#'   only_braids$nbraids <- lengths(strsplit(only_braids$braid_id, ", ")) 
#'   only_braids <- only_braids %>% 
#'     dplyr::relocate(comid, braid_id, nbraids)
#'   only_braids$fromnode
#'   braid6 <- braids_un %>% dplyr::filter(braid_id == "braid_6")
#'   braid6_nested <- braids %>% dplyr::filter(braid_id == "braid_6" | grepl("braid_6", braid_id))
#'   
#'   mapview::mapview(only_braids, color = "gold") +
#'     mapview::mapview(braids, color = "dodgerblue") + 
#'     mapview::mapview(braid6, color = "red") + 
#'     mapview::mapview(braid6_nested, color = "green") 
#'   
#'   only_braids$braid_id %>% unique()
#'   tmp_net <- 
#'     network %>% 
#'     dplyr::select(comid, fromnode, tonode, divergence, hydroseq) %>% 
#'     dplyr::left_join(
#'       dplyr::select(comid_map,
#'                     comid, from_node = fromnode, to_node = tonode),
#'       by = "comid"
#'     )
#'   mapview::mapview(tmp_net)
#'   cycles <- extract_cycles(graph = topo_map, format = TRUE)
#'   cycles2 <- extract_cycles(graph = topo_map, format = FALSE)
#'   comid_map$braid_count <- lengths(cycles2)
#' lengths(unname(cycles2))
#'   
#'   trim_cycles <- lapply(1:length(cycles2), function(k) {
#' 
#'     cycles2[[k]] <- cycles2[[k]][
#'                       lengths(cycles2[[k]]) > 3
#'                       ]
#'                     
#'     if(length(cycles2[[k]]) == 0) {
#'       NULL
#'     } else {
#'       cycles2[[k]]
#'     }
#'   })
#'   cycles2[[10]][!cycles2[[10]] %in% trim_cycles[[10]]]
#'   lengths(trim_cycles)
#'   unique(trim_cycles)
#'   lengths(unique(trim_cycles))
#'   length(unique(trim_cycles))
#'   comid_map
#'   comid_map
#'   cycles
#'   length(undir$fromnode %>% unique())
#'   length(undir$tonode %>% unique())
#'   # --------------------------------------
#'   # --------------------------------------
#'   # --------------------------------------
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
#'   )
#'   )
#'   # visits$as_list()
#'   # keep track of cycles nodes
#'   cycles <- fastmap::fastmap()
#'   
#'   # keep track of visited nodes
#'   prev_nodes <- fastmap::fastmap()
#'   
#'   # # set all marked values to FALSE
#'   prev_nodes$mset(.list = stats::setNames(
#'     lapply(1:length(unique(c(graph$fromnode, graph$tonode))),
#'            function(i){ 0 }),
#'     unique(c(graph$fromnode, graph$tonode))
#'     # lapply(1:length(unique(c(ungraph$fromnode, ungraph$tonode))), 
#'     # function(i){ 0 }),
#'     # unique(c(ungraph$fromnode, ungraph$tonode))
#'   )
#'   )
#'   start_node
#'   topo_map$as_list()
#'   fringe <- fastmap::fastmap()
#'   f <- fastmap::fastmap()
#'   
#'   stack <- fastmap::faststack()
#'   
#'   f$set(start_node, list(start = start_node, start_list = list()))
#'   
#'   flist <- list(
#'     start_node, 
#'     list(start = start_node, start_list = list())
#'     )
#'   
#'   # f$as_list()
#'   stack$push(flist)
#'   stack$as_list()
#'   fringe <- list(list(start_node, list()))
#'   start_node = start_node
#'   
#'   fringe[-length(fringe)]
#'   ll = list(1, 4, 6, 4)
#'   
#'   ll[-length(ll)]
#'   ll[length(ll)]
#'   sp <- stack$pop()
#'   state <- sp[[1]]
#'   path <- sp[[2]]
#'   
#'   stack <- fastmap::faststack()
#'   
#'   # push element to the top of the list
#'   stack$push()
#'   
#'   # pops top element from stack
#'   stack$pop()
#'   
#'   # cycle number count
#'   dfs <- function(graph, start, end) {
#'     fringe <- list(list(start, list()))
#'     paths <- list()
#'     
#'     while (length(fringe) > 0) {
#'       node <- fringe[[length(fringe)]]
#'       fringe <- fringe[-length(fringe)]
#'       state <- node[[1]]
#'       path <- node[[2]]
#'       
#'       if (length(path) > 0 && state == end) {
#'         paths <- c(paths, list(path))
#'         next
#'       }
#'       
#'       for (next_state in graph[[state]]) {
#'         if (next_state %in% path) {
#'           next
#'         }
#'         
#'         new_node <- list(next_state, c(path, next_state))
#'         fringe <- c(fringe, list(new_node))
#'       }
#'     }
#'     
#'     return(paths)
#'   }
#'   
#'   graph <- list(
#'     "1" = c(2),
#'     "2" = c(1, 3, 4),
#'     "3" = c(2, 5, 6),
#'     "4" = c(2, 5),
#'     "5" = c(3, 4, 7, 8),
#'     "6" = c(3, 7),
#'     "7" = c(5, 6),
#'     "8" = c(5)
#'   )
#'   
#'   cycles <- lapply(names(graph), function(node) {
#'     lapply(dfs(graph, node, node), function(path) {
#'       c(node, path)
#'     })
#'   })
#'   library(coro)
#'   
#'   dfs <- function(graph, start, end) {
#'     fringe <- list(list(start, list()))
#'     paths <- list()
#'     
#'     while (length(fringe) > 0) {
#'       node <- fringe[[length(fringe)]]
#'       fringe <- fringe[-length(fringe)]
#'       state <- node[[1]]
#'       path <- node[[2]]
#'       
#'       if (length(path) > 0 && state == end) {
#'         paths <- c(paths, list(path))
#'         next
#'       }
#'       
#'       for (next_state in graph[[state]]) {
#'         if (next_state %in% path) {
#'           next
#'         }
#'         
#'         new_node <- list(next_state, c(path, next_state))
#'         fringe <- c(fringe, list(new_node))
#'       }
#'     }
#'     
#'     return(paths)
#'   }
#' stack$size()
#' 
#' 
#'   dfs <- function(graph, start, end) {
#'     stack <- fastmap::faststack()
#'     # out <- fastmap::fastmap()
#'     stack$push(list(start, list()))
#'     
#'     paths <- list()
#'     
#'     while (stack$size() > 0) {
#'       node <- stack$pop()
#'       state <- node[[1]]
#'       path <- node[[2]]
#'       
#'       if (length(path) > 0 && state == end) {
#'         paths <- c(paths, list(path))
#'         next
#'       }
#'       
#'       for (next_state in graph[[state]]) {
#'         if (next_state %in% path) {
#'           next
#'         }
#'         
#'         new_node <- list(next_state, c(path, next_state))
#'         stack$push(new_node)
#'       }
#'     }
#'     
#'     return(paths)
#'   }
#'   
#'   graph <- list(
#'     "1" = c(2),
#'     "2" = c(1, 3, 4),
#'     "3" = c(2, 5, 6),
#'     "4" = c(2, 5),
#'     "5" = c(3, 4, 7, 8),
#'     "6" = c(3, 7),
#'     "7" = c(5, 6),
#'     "8" = c(5)
#'   )
#'   
#'   path_map <- fastmap::fastmap()
#'   
#'   cycles <- lapply(names(graph), function(node) {
#'      lapply(dfs(graph, node, node), function(path) {
#'         unlist(c(node, path))
#'         # paste0(c(node, path), collapse = "-")
#'       # if (!path_map$has(node)) {
#'       #   path_map$set(node ,list(braid = paste0(c(node, path), collapse = "-")))
#'       # }
#'       # list( nodes = c(node, path),  p = paste0(c(node, path), collapse = "-"))
#'     })
#'   })
#'   
#'   
#'   
#'   # names(cycles) <- paste0("node_", 1:length(cycles))
#'   
#'   names(cycles) <- paste0(1:length(cycles))
#'   
#'   path_map <- fastmap::fastmap()
#'   
#'   for (i in 1:length(cycles)) {
#'     
#'     message("iteration ", i, "/", length(cycles))
#'     
#'     from_node <- names(cycles[i])
#'     cyc <-   cycles[[i]]
#'     
#'     # check if from_node has been added to hashmap already
#'     if (!path_map$has(from_node)) {
#' 
#'       edge_str <- sapply(1:length(cyc), function(k) {
#'         paste0(cyc[[k]], collapse = "-") 
#'       })
#'       
#'       # add cycle names
#'       names(cyc) <- paste0(1:length(cyc))
#'       # names(cyc) <- paste0("cycle_", 1:length(cyc))
#'       
#'       map_entry <- list(
#'         cycle       = cyc,
#'         edge        = edge_str,
#'         count       = length(edge_str)
#'       )
#' 
#'       # set new entry in topology map
#'       path_map$set(from_node, map_entry)
#'       
#'       # path_map$as_list()
#'       # path_map$get("2")
#'       
#'     } else {
#'       
#'       # edge string
#'       edge_str <- sapply(1:length(cyc), function(k) {
#'         paste0(cyc[[k]], collapse = "-") 
#'       })
#'       
#'       names(cyc) <- paste0(1:length(cyc))
#'       # names(cyc) <- paste0("cycle_", 1:length(cyc))
#'       
#'       map_entry <- list(
#'         to_node     = c(topo_map$get(from_node)$cycle, cyc),
#'         edge        = c(topo_map$get(from_node)$edge, edge_str),
#'         count       = c(topo_map$get(from_node)$count + 1)
#'       )
#'       
#'       # if(length(add_args) > 0) {
#'       #   for (n in 1:length(names(add_args))) {
#'       #     key = names(add_args)[n]
#'       #     map_entry[[key]] <- c(topo_map$get(from_node)[[key]], add_args[[key]][i])
#'       #   }
#'       # }
#'       #  Update the existing entry in topo_map
#'       path_map$set(from_node, map_entry)
#'     }
#'   }
#'   path_map$as_list()
#'   path_map$keys()
#'   path_map$get("3")
#'   names(cycles) <- paste0("node_", 1:length(cycles))
#'   path_map <- fastmap::fastmap()
#'   path_map$mset(node_2 = unlist(cycles[[2]]), nothing = c(31))
#'   path_map$mset(cycles = cycle)
#'   cycles$node_1
#'   path_map$as_list()
#'   cycles
#'   names(cycles)[1]
#'   cycles[[2]]
#'   unlist(cycles[[2]])
#'   c(10, 20, 30)
#'   # Create the fastmap object
#'   m <- fastmap::fastmap()
#'   m$mset(numbers = c(10, 20, 30), nothing = c(31))
#'   m$as_list()
#'   m$keys()
#'   path_map$set(key =   names(cycles)[1])
#'   cycles[[2]]
#'   path_map$as_list()
#'   cycles[[1]]
#'   cycles[[2]][[2]]
#'   
#'   names(cycles) <- paste0("node_", 1:length(cycles))
#'   c(node, path)
#'   
#'   cycles$node_1[[1]]
#'   n =   cycles$node_1[[1]][[1]]
#'   p =   cycles$node_1[[1]][-1]
#'   
#'   paste0(c(n, p), collapse = "-")
#'   
#'   
#'   for (i in 1:length(names(cycles))) {
#'     
#'     
#'     
#'     
#'   }
#'   
#'   
#'   class(cycles[[1]])
#'   class(cycles[[2]])
#'   cycles[[2]]$sub_braid_2
#'   length(cycles[[2]])
#'   
#'   tt <- lapply(names(graph), function(node) {
#'     message(node)
#'     x = node
#'     node
#'     lapply(dfs(graph, node, node), function(path) {
#'       c(node, path)
#'     })
#'     x
#'   })
#'   
#'   node = tt[[2]]
#'   
#'   class(cycles)
#'   names(cycles) <- paste0("node_", 1:length(cycles))
#'   names(cycles)
#'   
#'   length(cycles)
#'   length(cycles[[7]])
#'   
#'   # Function to convert a cycle to a unique string representation
#'   cycle_to_string <- function(cycle) {
#'     cycle_str <- paste(sort(as.character(cycle)), collapse = '-')
#'     return(cycle_str)
#'   }
#'   
#'   # Applying DFS and removing duplicates
#'   unique_cycles <- list()
#'   all_cycles <- lapply(names(graph), function(node) {
#'     lapply(dfs(graph, node, node), function(path) {
#'       cycle <- c(node, path)
#'       cycle_str <- cycle_to_string(cycle)
#'       if (!(cycle_str %in% unique_cycles)) {
#'         unique_cycles <- c(unique_cycles, cycle_str)
#'         cycle
#'       } else {
#'         NULL
#'       }
#'     })
#'   })
#'   
#'   # Filter out NULL elements and count the number of unique cycles
#'   unique_cycles <- Filter(Negate(is.null), unlist(all_cycles, recursive = FALSE))
#'   num_unique_cycles <- length(unique_cycles)
#'   
#'   dfs <- function(graph, start, end, unique_cycles = list()) {
#'     stack <- fastmap::faststack()
#'     stack$push(list(start, list()))
#'     
#'     paths <- list()
#'     
#'     while (stack$size() > 0) {
#'       node <- stack$pop()
#'       state <- node[[1]]
#'       path <- node[[2]]
#'       
#'       if (length(path) > 0 && state == end) {
#'         cycle <- c(start, path)
#'         cycle_str <- paste(sort(as.character(cycle)), collapse = '-')
#'         
#'         if (!(cycle_str %in% unique_cycles)) {
#'           unique_cycles <- c(unique_cycles, cycle_str)
#'           paths <- c(paths, list(path))
#'         }
#'         
#'         next
#'       }
#'       
#'       for (next_state in graph[[state]]) {
#'         if (next_state %in% path) {
#'           next
#'         }
#'         
#'         new_node <- list(next_state, c(path, next_state))
#'         stack$push(new_node)
#'       }
#'     }
#'     
#'     return(list(paths, unique_cycles))
#'   }
#'   
#'   graph <- list(
#'     "1" = c(2),
#'     "2" = c(1, 3, 4),
#'     "3" = c(2, 5, 6),
#'     "4" = c(2, 5),
#'     "5" = c(3, 4, 7, 8),
#'     "6" = c(3, 7),
#'     "7" = c(5, 6),
#'     "8" = c(5)
#'   )
#'   
#'   # Applying DFS and getting the unique cycles
#'   start_node <- names(graph)[1]
#'   cycles_result <- dfs(graph, start_node, start_node)
#'   all_cycles <- cycles_result[[1]]
#'   unique_cycles <- cycles_result[[2]]
#' }
#' #' Traverse graph in a Depth First Search (DFS) manner
#' #' @description Given a network with ID and toID columns, and a starting ID, traverse a river network using a Depth First Search (DFS) algorithm to explore all nodes of directed acyclic graph (DAG)
#' #' @param graph data.frame with ID, toID, hydroseq, startflag, terminalpa, and divergence attributes.
#' #' @param as_df logical, whether to return DFS traversal as a dataframe or as a list. If TRUE (default) DFS traversal will be returned as a dataframe.
#' #' @param return_as character string of how to return DFS traversal output. Either "list", "dataframe", "vector". Default is "list".
#' #' @param start ID of node to start traversal and go upstream from
#' #' @param dendritic logical, whether to make network dendritic or not. If FALSE, divergent nodes (divergence value of 2), will be connected to the upstream flowline and the network will contain cycles. Default is FALSE
#' #' @param verbose logical print status updates?
#' #' @return data.frame containing the distance between pairs of network outlets.
#' dfs_traversal2 <- function(
#'     graph,
#'     # as_df   = TRUE,
#'     return_as = "list",
#'     # start   = NULL,
#'     # dendritic = TRUE,
#'     graph_as_map = FALSE,
#'     verbose = FALSE
#' ) {
#'   
#'   #
#'   # # create topology hashmap
#'   # topo_map <- make_topo_map(
#'   #   from_nodes  = graph$tonode,
#'   #   to_nodes    = graph$fromnode,
#'   #   from_ids    = graph$tocomid,
#'   #   to_ids      = graph$comid,
#'   #   streamcalc  = graph$streamcalc,
#'   #   streamorder = graph$streamorde,
#'   #   divergence  = graph$divergence
#'   # )
#'   # graph = comid_map
#'   # if(!graph_as_map) {
#'     # create topology hashmap
#'     topo_map <- make_topo_map(
#'       from_nodes  = graph$fromnode,
#'       to_nodes    = graph$tonode
#'       # from_nodes  = graph$tonode,
#'       # to_nodes    = graph$fromnode,
#'     )
#'   #   
#'   # } else {
#'     
#'   #   topo_map <- graph
#'   #   
#'   # }
#'   
#'   # topology  <- topo$adjacency
#'   # topo_map <- topology
#'   # graph = comid_map
#'   # verbose = T
#'   # return_as = "list"
#'   # # create topology hashmap
#'   # topo_map <- make_topo_map(
#'   #   from_nodes  = graph$fromnode,
#'   #   to_nodes    = graph$tonode
#'   #   # from_nodes  = graph$tonode,
#'   #   # to_nodes    = graph$fromnode,
#'   # )
#'   
#'   # keep track of visited nodes
#'   marked <- fastmap::fastmap()
#'   
#'   # set all marked values to FALSE
#'   marked$mset(.list = stats::setNames(
#'     lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i){ FALSE }),
#'     unique(c(graph$fromnode, graph$tonode))
#'   )
#'   )
#'   # marked$as_list()
#'   # start DFS traversal from root node
#'   # graph
#'   
#'   # graph$tonode == 0
#'   
#'   root = as.character(graph$fromnode[1])
#'   # root <- as.character(graph[graph$tocomid == "0", ]$tonode)
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
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
#' 
