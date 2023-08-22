# library(pbapply)
library(terra)
library(sf)
library(dplyr)
# library(terrainSliceR)
library(mapview)
library(smoothr)
library(nhdplusTools)
library(wk)
library(geos)
library(vctrs)
library(AOI)
library(ggplot2)
library(tidyr)

source("R/transects.R")
source("R/braids.R")
source("R/fix_transects.R")

# # *******************************
# # ---- Test data for braids  ----
# # *******************************

# net <- 
net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 30)

net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)

net2 <- 
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 50)
system.time({
  transect_lines2 = cut_cross_sections3(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = TRUE,
    # terminal_id = NULL,
    terminal_id = "terminalpa",
    add       = TRUE,
    use_original = T
  )
})
mapview::mapview(transect_lines2) + mapview::mapview(braids, color = "red")
braids <- find_braids(
  network = net2,
  add = TRUE, 
  nested = FALSE,
  verbose = TRUE
) %>%
  dplyr::filter(braid_id != "no_braid")
net2$geometry %>% plot()

# braids

braids <- find_braids_df(
  network = net2,
  add = TRUE, 
  nested = FALSE,
  verbose = TRUE
  )
braids$braid_id %>% unique()
braids$braid_id %>% unique() %>% length()

braids2 <- find_braids(  network = net2,
                         return_as = "dataframe",
              add = TRUE, 
              nested = FALSE,
              verbose = TRUE
              )

braids2$braid_id %>% unique()
braids2$braid_id %>% unique() %>% length()

make_braids_gif(braids2,
                save_path  = "/Users/anguswatters/Desktop/updated_braid_test3.gif",
                height     = 8,
                width      = 10,
                gif_width  = 1800,
                gif_height = 1500,
                delay      = 0.9,
                legend_pos = "bottom",
                verbose    = TRUE
)
make_braids_gif()

# # *********************************************
# # ---- Test data for fix_braid_transects() ----
# # *********************************************

ref_net <- sf::read_sf("/Users/anguswatters/Downloads/01_reference_features.gpkg", layer = "flowlines") 
names(ref_net) <- tolower(names(ref_net))
# net <-   
#   ref_net %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm))) %>%
#   dplyr::slice(1:20000)
ref_net <-
  ref_net %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

net <-
  ref_net %>%
  dplyr::filter(terminalpa %in% unique(ref_net$terminalpa)[1:20])
#   # dplyr::slice(1:5000)

system.time({
  transect_lines = cut_cross_sections3(
    net       = ref_net,
    id        = "comid",
    cs_widths = pmax(50, ref_net$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = TRUE,
    # terminal_id = NULL,
    terminal_id = "terminalpa",
    add       = TRUE,
    use_original = T
  )
})
system.time({
  transect_lines_unfixed = cut_cross_sections3(
    net       = ref_net,
    id        = "comid",
    cs_widths = pmax(50, ref_net$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = FALSE,
    # terminal_id = NULL,
    terminal_id = "terminalpa",
    add       = TRUE,
    use_original = T
  )
})


sf::write_sf(transect_lines, "/Users/anguswatters/Desktop/01_vpu_fixed_transects.gpkg")
sf::write_sf(transect_lines_unfixed, "/Users/anguswatters/Desktop/01_vpu_unfixed_transects.gpkg")
net
system.time({
  blist <- get_braid_list_latest(
    network = net
  )
})

system.time({
  blist <- get_braid_list(
    network = net
  )
})

system.time({
  new_braids <- find_braids_df(
    network = net, 
    terminal_id = "terminalpa", 
    add = T, 
    nested  = T
  )
})

system.time({
  old_braids <- find_braids_df(
    network = net, 
    add = T, 
    nested  = T,
    new = FALSE
  )
})


# ref_net$

# terminal


ref_net <- 
  ref_net %>% 
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
starts <- c(2010288, 1962269, 1853299, 1852198, 1852502, 1852542, 1852774, 1974485, 1974615)

ends <- unique(ref_net$terminalpa )
# count up number of linestrings in each terminalpa set of lienstrings
counts <- lapply(1:length(ends), function(i) {
  # message(i, "/", length(ends))
  data.frame(
    terminalpa = ends[i],
    nrows = nrow(dplyr::filter(ref_net, terminalpa == ends[i]))
  )
}) %>% 
  dplyr::bind_rows()
starts <- unique(counts[1:50,]$terminalpa)
# starts <- c(1853299)
counts %>%
  dplyr::filter(terminalpa %in% starts)
# dplyr::summarise(tot = sum(nrows))

net <- 
  ref_net %>% 
  dplyr::filter(terminalpa %in% starts)
system.time({
  transect_lines = cut_cross_sections3(
    net       = ref_net,
    id        = "comid",
    cs_widths = pmax(50, ref_net$bf_width * 7),
    num       = 6,
    densify = NULL,
    fix_braids = TRUE,
    add       = TRUE,
    use_original = T
  )
})
braids <- find_braids_df(
  network = net, 
  add = T, 
  nested  = T
) %>% 
  dplyr::filter(braid_id != "no_braid")
mapview::mapview(transect_lines, color = "green") + mapview::mapview(braids, color = "red")  + net
# find

# ********************************************************
# ---- TEST NUMBER OF CONNECTED COMPONENTS IN A GRAPH ----
# ********************************************************
ends <- unique(ref_net$terminalpa )
# count up number of linestrings in each terminalpa set of lienstrings
counts <- lapply(1:length(ends), function(i) {
  # message(i, "/", length(ends))
  data.frame(
    terminalpa = ends[i],
    nrows = nrow(dplyr::filter(ref_net, terminalpa == ends[i]))
  )
}) %>% 
  dplyr::bind_rows()

starts <- c(2010288, 1962269, 1853299, 1852198, 1852502, 1852542, 1852774)
starts <- c(2010288, 1962269, 1853299, 1852198, 1852502, 1852542, 1852774, 1974485, 1974615, 2040883)
# starts <- c(1853299)
counts %>% 
  dplyr::filter(terminalpa %in% starts)
  # dplyr::summarise(tot = sum(nrows))

nets <- 
  ref_net %>% 
  dplyr::filter(terminalpa %in% starts)

bs <- find_braids_df(nets, add = T, nested = F)
bs$braid_id %>% unique()

bbbbs <- bs %>% 
  dplyr::filter(braid_id != "no_braid")
nobraids <- bs %>% 
  dplyr::filter(braid_id == "no_braid")

mapview::mapview(nobraids) + mapview::mapview(bbbbs, color= "red")
nets
seperate_nets <- find_connected_components(ref_net, add = TRUE)

seperate_nets$component_id %>% unique() %>% length()

ggplot2::ggplot() +
  ggplot2::geom_sf(data = bs, ggplot2::aes(color = braid_id))
find_braids(
  network = dplyr::filter(seperate_nets, component_id == "1"), 
  return_as = "list"
  )

find_braids(
  network = dplyr::filter(seperate_nets, component_id == "2"), 
  return_as = "list"
)

find_braids(
  network = dplyr::filter(seperate_nets, component_id == "3"), 
  return_as = "list"
)
mapview::mapview(nets)


#' Count the number of distinct networks in NHDPlus dataframe
#' Determine the number of distinct (unconnected) networks there are in a dataframe or sf object of flowlines. Requires fromnode and tonode columns.
#' Used internally within 'find_braids()' function
#' @param network data.frame or sf object with comid, tonode, fromnode, and divergence attributes. If a "tocomid" column exists, it is recommended to remove this beforehand
#' @return logical, number of distinct contiguious networks in 'network'
count_networks <- function(
    network
) {
  
  # lower case names
  names(network) <- tolower(names(network))
  
  # get to comids
  network <- 
    nhdplusTools::get_tocomid(
      dplyr::select(network, 
                    comid, fromnode, tonode, divergence),
      return_dendritic = TRUE,
      remove_coastal   = FALSE
      )
  
  # sort network and get terminalID column
  network <- nhdplusTools::get_sorted(
                              network,
                              split = TRUE
                              )
  
  # count of unique terminal IDs
  count <- length(unique(network$terminalID)) 
  
  
  # # number of rows that have a tonode that is NOT found in the fromnode column (i.e. they lead out of network)
  # count <-  nrow(
  #             dplyr::filter(network,
  #                           !tonode %in% fromnode
  #                           )
  #             )
  return(count)

}

#' Add the terminal ID of for each flowline in NHDPlus dataframe
#' Determine comid of the terminal flowline leading out of a basin. Requires fromnode and tonode columns.
#' Used internally within 'find_braids()' function
#' @param network data.frame or sf object with comid, tonode, fromnode, and divergence attributes. If a "tocomid" column exists, it is recommended to remove this beforehand
#' @param add logical, whether to add the component_id to the original dataset. If TRUE (default) a terminalID column is added to original data, otherwise (FALSE) a vector of the unique terminalIDs is returned 
#' @return data.frame, sf data.frame or numeric vector
get_terminal_ids <- function(
    network, 
    add = TRUE
) {
  
  # x <- network
  # rm(x)
  # network <- nets
  
  # lower case names
  names(network) <- tolower(names(network))
  
  # get to comids
  x <- 
    nhdplusTools::get_tocomid(
      dplyr::select(network, 
                    comid, fromnode, tonode, divergence),
      return_dendritic = TRUE,
      remove_coastal   = FALSE
    )
  
  # sort network and get terminalID column
  x <- nhdplusTools::get_sorted(
    x,
    split = TRUE
  )
  
  # if add is TRUE, return the original dataframe with terminal ID added
  if (add) {
    
    # join x back with original 'network' data, adding the terminalID column
    x <- dplyr::left_join(
            network,
            dplyr::select(
              sf::st_drop_geometry(x), 
              comid, terminalID
              ),
            by = "comid"
            )
    
    return(x)
  } 
  
  # if add was NOT TRUE, then return the unique terminal IDs
  term_ids <- unique(x$terminalID)

  return(term_ids)
  
}

#' Find the connected components in a NHDPlus flowlines Network
#' Determine how many different, unconnected/seperate sets of flowlines are within a set of NHDPlus flowlines. 
#' The input 'network' dataset must contain a comid, tonode, fromnode, and then (optionally) divergence and terminalpa attributes.
#' Used internally within 'find_braids()' function to make sure each connected set of flowlines is addressed and braids are searched for in each seperated component.
#' @param network data.frame or sf object with comid, tonode, fromnode, and (optionally) divergence and terminalpa attributes. If a "tocomid" column exists, it is recommended to remove this beforehand
#' @param add logical, whether to add the component_id to the original dataset. If TRUE (default) the original dataset is returned with an additional component_id column, indicating the set of connected components each comid belongs too. If FALSE, a dataframe with the 
#' @param verbose logical print status updates, if TRUE, messages will print. Default is FALSE.
#' @return logical, If TRUE, atleast one braid was detected in network, FALSE if no braids were found
#' @export
#'
#' @examples
find_connected_components <- function(
    network,
    add     = TRUE,
    verbose = FALSE
) {
  
  # network = nets
  # add = T
  
  # nets %>% 
  #   dplyr::filter(!tonode %in% fromnode)
  # network <- nets
  # network <- dplyr::select(network, -tocomid)
  # add = FALSE
  
  # initialize NULL dropped_col value to store dropped to comid column if necessary
  dropped_col <- NULL
  
  # Store the original column order
  col_order <- c("component_id", names(network))

  # lower case names
  names(network) <- tolower(names(network))
  
  # try and detect a "tocomid" like column, if its found in the network columns, remove that column
  if (any(names(network) %in% c("tocomid", "toCOMID", "to_comid", "TOCOMID", "to_COMID", "to_Comid", "TO_COMID"))) {  

    # name of the "tocomid" column in network that should be removed
    drop_tocomid <- names(network)[names(network) %in% c("tocomid", "toCOMID", "to_comid", "TOCOMID", "to_COMID", "to_Comid", "TO_COMID")]
    
    # store the dropped tocomid column to add back later if needed
    dropped_col <- sf::st_drop_geometry(network[, c("comid", drop_tocomid)])
    # network[, c("comid", drop_tocomid), drop = T]
    
    # remove to comid type column if found
    network <- network[, names(network) != drop_tocomid]
    
  }
  
  # create directed acyclic graph
  graph <- create_dag(network)
  
  # # # get the fromnode associated with a given COMID 'start'
  # start_nodes <- sapply(network_starts$comid, function(i) {comid_to_node(graph, i)})
  # start_node <- comid_to_node(graph, start_nodes)
  
  # drop graph geometry
  graph <- sf::st_drop_geometry(graph)
  
  # # stash comids and from IDs
  comid_map <- dplyr::select(
                      graph,
                      comid,
                      fromnode,
                      tonode
                    )
  
  # create artificial cycles from circuits in graph, doing this allows for 
  # sections of river that form a circuit to be identified as a cycle and thus a braid
  # Questionable at this point, other option is the code using single_cycles() function 
  graph <- renode_circuits(graph, verbose = verbose)
  
  # make an undirected graph from DAG
  graph <- make_undirected(graph)  
  
  # make hashmap for tonode/fromnode topology for traversing graph
  topo_map <- make_topo_map(
    from_nodes = graph$fromnode,
    to_nodes   = graph$tonode
  )
  
  # Depth first search function to use to find/delinate connected components of network object
  dfs <- function(node, visit) {
    
    # message("VISITING node: ", node)
    
    # visit node
    visit$set(node, TRUE)
    
    # get the neighbors of current node
    neighbors <- topo_map$get(node)$to_node
    
    # message("---> Neighbors of node: ",  node, ":\n", paste0(" - ", c(neighbors), sep = "\n"))
    
    # iterate through the neighbors of current node and run DFS on them if they have not been visited yet
    for (i in neighbors) {
      
      # message(" - NEIGHBOR: ", i)
      
      # if neighbor i has NOT been visited
      if (!visit$get(i)) {
        
        # message("!!!!!!!", i, " NOT VISISTED ---> RUN DFS !!!!!!!!!")
        dfs(i, visit)
        
      }
      
      # message("NEIGHBOR: ", i)          
      # message("===========================================")
      # message("===========================================")
    }
  
  }
  
  # keep track of visited nodes
  visit <- fastmap::fastmap()
  
  # # set all marked values to FALSE
  visit$mset(.list = stats::setNames(
                        lapply(1:length(unique(c(graph$fromnode, graph$tonode))), function(i){ FALSE }),
                        unique(c(graph$fromnode, graph$tonode)))
                        )
  
  # initialize i pointer
  i = 1
  
  # initialize count
  count = 0
  
  # hashmap for output components
  out <- fastmap::fastmap()
  
  # topo_map$as_list()[3]
  # length(topo_map$as_list())
  topo_map$get("929")
  # while there are still values left in the visit and topo_map hashmap
  while(visit$size() > 0 & topo_map$size() > 0) {
  # while(visit$size() > 0 & length(start_nodes) > 0) {
    
    # node to kickoff DFS from, get the node at the end of the list
    start_node <- names(
                      topo_map$as_list()[
                        length(topo_map$as_list())
                        ]
                      )
    # start_node <- names(topo_map$as_list()[i])
    
    message("===========================================")
    message("========== ITERATION: ", i, " =============")
    message("========== start_node: ", start_node, " =============")
    message("===========================================")

    # run DFS from the 'start_node'
    dfs(start_node, visit)
    # dfs(start_nodes[[i]], visit)
    
    # extract the newly visited nodes
    new_visits <- visit$as_list()
    
    # remove newly visited nodes
    to_remove <- names(new_visits[new_visits == TRUE])
    
    # increment component count
    count = count + 1
    
    # remove set of connected components
    visit$remove(to_remove)
    
    topo_map$remove(to_remove)
    
    # store connectd component in out hashmap with the key being the connected component count number and 
    # the value being the nodes in the connected component
    out$set(as.character(count), to_remove)
    
    # increment i pointer
    i = i + 1

  }
  
  # resulting output
  out <- out$as_list()

  # ccreate dataframe of component number and node ID 
  out <- dplyr::tibble(
            component_id = rep(names(out), lengths(out)),
            fromnode     = as.integer(Reduce(c, out))
          )
  
  # join component ID back with original comid_map dataframe
  res <- 
    comid_map %>% 
    dplyr::left_join(
      out,
      by = "fromnode"
    )
  
  # out$fromnode %>% unique() %>% length()
  # res$fromnode %>% unique()
  
  # if add is TRUE, add the component ID to the original 'network' input
  if(add) {

    res <- dplyr::left_join(
              network,
              dplyr::select(res, 
                            comid, component_id
                            ),
              by = "comid"
              )
    # names(res2)
    # if dropped_col is NOT NULL, then join the dropped call back with dataset
    if (!is.null(dropped_col)) {
  
      res <- dplyr::left_join(res, 
                               dropped_col,
                               by = "comid"
                               )
      
    }
    
    # reorder columns to have original order with component_id as the first column
    res <- res[, col_order]
    
  # if add is FALSE, return dataframe with comid, fromnode, tnode, and component_id
  } else {
    
    # join just the comid, fromnode, tonode, data from 'network' with the comid and component_id from 'res'
    res <-  dplyr::left_join(
                dplyr::select(
                    sf::st_drop_geometry(network), 
                    comid, fromnode, tonode
                    ),
                dplyr::select(res, 
                              comid, component_id
                              ),
                by = "comid"
                )
    }
  
  return(res)
  
  # count_components <- function(topo, visit) {
  #   count = 0
  #   for(i in 1:length(topo$as_list())) {
  #     node <- names(topo$as_list()[i])
  #     message("============ ITERATION i: ", i, " =============")
  #     message("============: ", node, " =============")
  #     if (!visit$get(node)) {
  #       message("INCREASING COUNT: ", count)
  #       count = count + 1
  #       dfs(node, visit)
  #     }
  #   }
  #   return(count)
  # }
  # count_components(topo = topo_map, visit = visit)
}


nets %>% 
  dplyr::mutate(terminalpa = as.character(terminalpa)) %>% 
  ggplot2::ggplot() +
  ggplot2::geom_sf(ggplot2::aes(color = terminalpa))

# turn network into a directed graph
dag <- create_dag(nets)
x
x = nets
start     = NULL
add       = TRUE
drop_geom = FALSE
reverse   = FALSE

topo <-
  x %>% 
  dplyr::select(comid, fromnode, tonode, divergence) %>% 
  get_tocomid(
    return_dendritic = TRUE,
    remove_coastal = FALSE, 
    add = TRUE
    )

topo
topo %>% make_node_topology()

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
# x
# x$divergxencec = 0
# x <- nhdplusTools::make_standalone(x)

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
mapview::mapview(nets)
# ********************************************************
# ********************************************************
# ********************************************************
system.time({
tnet = cut_cross_sections3(
  net       = ref_net,
  id        = "comid",
  cs_widths = pmax(50, ref_net$bf_width * 7),
  num       = 5,
  fix_braids = TRUE,
  add       = TRUE,
  use_original = T
)
})
# net <- 
#   ref_net %>% 
#   dplyr::filter(terminalpa == 1921053) 
  # dplyr::filter(terminalpa == ends[i])

# transects_nofix = cut_cross_sections3(
#   net       = net,
#   id        = "comid",
#   cs_widths = pmax(50, net$bf_width * 7),
#   num       = 10,
#   fix_braids = F,
#   add       = TRUE,
#   use_original = T
# )
# net$terminalfl %>% unique()
ends <- unique(ref_net$terminalpa )
ends
# i = 4

counts <- lapply(1:length(ends), function(i) {

  
  
  
  # i = 1
  message(i, "/", length(ends))

  # net <-
    data.frame(
      terminalpa = ends[i],
      nrows = ref_net %>%
                dplyr::filter(terminalpa == ends[i]) %>% 
                nrow()
    )
  
  }) %>% 
  dplyr::bind_rows()
tlist = list()
system.time({
  # for (i in 1:length(ends)) {
  # transects <- lapply(1:length(ends), function(i) {
for (i in 1:length(ends)) {
  
  

    # i = 1
    message(i, "/", length(ends))
    # net <- nhdplusTools::navigate_network(start = 719018, mode = "UT",  distance_km = 6)
    # net <- 
    #   net %>% 
    #   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
    #   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
    # transect_lines = cut_cross_sections3(
    #                     net          = net,
    #                     id           = "comid",
    #                     cs_widths    = pmax(50, net$bf_width * 7),
    #                     num          = 5,
    #                     fix_braids   = F,
    #                     add          = TRUE,
    #                     use_original = TRUE
    #                   )
    # plot(net$geometry)0
     net <-
       ref_net %>%
       dplyr::filter(terminalpa == ends[i])
     
     if(nrow(net) <= 1) {
       message("==== ONLY 1 row in net, SKIPPING ====")
       next
     }
      # dplyr::filter(terminalpa == 1921053)
    
     # plot(net$geom)
     # # mapview::mapview(tmp_net) +
     # #   mapview::mapview(transects_fix_new, color = "red") +
     # #   mapview::mapview(transects_nofix, color = "green")
     # 
     # system.time({
       tnet = cut_cross_sections3(
         net       = net,
         id        = "comid",
         cs_widths = pmax(50, net$bf_width * 7),
         num       = 5,
         fix_braids = TRUE,
         add       = TRUE,
         use_original = T
       )
     # })
     # 
     # transects_fix_new$geometry
     # plot(transects_fix_new$geometry)
     # system.time({
       # transect_lines = cut_cross_sections3(
       #   net       = net,
       #   id        = "comid",
       #   cs_widths = pmax(50, net$bf_width * 7),
       #   num       = 5,
       #   fix_braids = T,
       #   add       = TRUE,
       #   use_original = T
       # )
  
       # })
     # plot(net$geometry)
     # plot(transect_lines$geometry, col = "red", add = T)
     # mapview::mapview(net) +
     #   mapview::mapview(transects_fix_new, color = "red") +
     #   mapview::mapview(transect_lines, color = "green")
       
       tlist[[i]] <- tnet
    # })
    }
  })

mapview::mapview(net) + mapview::mapview(starts, color  = "red")
counts %>% unlist() %>% sum()
nrow(net)
starts 

starts <- 
  net %>% 
  dplyr::filter(terminalfl == 1)
mapview::mapview(net) + mapview::mapview(starts, color  = "red")
net %>% names()
net %>% 
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
sf::st_layers("/Users/anguswatters/Downloads/01_reference_features.gpkg")
net_tbl <- sf::read_sf("/Users/anguswatters/Downloads/uniform_01.gpkg", layer = "network") %>% 
  dplyr::filter(type == "network")

net <- sf::read_sf("/Users/anguswatters/Downloads/uniform_01.gpkg", layer = "flowpaths") 

net <- 
  net %>%
  dplyr::mutate(comid = stringr::str_extract(member_comid, "^[^,]+")) %>% 
  dplyr::relocate(comid) %>% 
  nhdplusTools::align_nhdplus_names() %>% 
  dplyr::rename(tot_drainage_areasqkm = totdasqkm)

net$comid %>% unique() %>% length()
mapview::mapview(net[1, ])

test_gpkg$type %>% unique()
test_gpkg
sf::st_layers("/Users/anguswatters/Downloads/uniform_01.gpkg")

# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 500)
net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 50)
net3 <- 
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
system.time({
  transects_fix = cut_cross_sections2(
    net       = net3,
    id        = "comid",
    cs_widths = pmax(50, net3$bf_width * 7),
    num       = 5,
    fix_braids = TRUE,
    add       = TRUE
  )
  
})

system.time({
  transects_fix_new = cut_cross_sections3(
    net       = net3,
    id        = "comid",
    cs_widths = pmax(50, net3$bf_width * 7),
    num       = 10,
    fix_braids = TRUE,
    add       = TRUE,
    use_original = FALSE
  )
})

# big_net <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 500)
big_net <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 400)
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 50)
big_net2 <- 
  big_net %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))


system.time({
  transects_fix_new2 = cut_cross_sections3(
    net       = big_net2,
    id        = "comid",
    cs_widths = pmax(50, big_net2$bf_width * 7),
    num       = 10,
    fix_braids = TRUE,
    add       = TRUE,
    use_original = F
  )
})

# # ggplot theme 
# thm <- 
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
#     plot.subtitle =  ggplot2::element_text(size = 14, hjust = 0.5)
#   )
# # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
# # net2 <- nhdplusTools::navigate_network(start = 1079041, mode = "UT",  distance_km = 40)
# # net2 <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 100)
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 30)
# mapview::mapview(net2)
# 
# braids <- find_braids(
#   network = net2,
#   return_as = "dataframe", 
#   add = T, 
#   nested = FALSE
# )%>% 
#   dplyr::arrange(hydroseq)
# 
# make_braids_gif(braids, 
#                 save_path  = "/Users/anguswatters/Desktop/small_braid_animation2.gif",       
#                 height     = 8,
#                 width      = 10,
#                 gif_width  = 1800, 
#                 gif_height = 1500,
#                 delay      = 0.8,
#                 legend_pos = "bottom",
#                 verbose    = TRUE
# )
# 
# # # add bf_width column to network
# net3 <-
#   net2 %>%
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# transects_nofix = cut_cross_sections2(
#   net       = net3,
#   id        = "comid",
#   cs_widths = pmax(50, net3$bf_width * 7),
#   num       = 5,
#   fix_braids = FALSE,
#   add       = TRUE
# )
# orig_trans_plot <- 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = net3) +
#   ggplot2::geom_sf(data = transects_nofix, color = "red", lwd = 1) + 
#   ggplot2::labs(title = "Original braid transects") +
#   thm
# 
# ggplot2::ggsave(orig_trans_plot, filename = "/Users/anguswatters/Desktop/original_transects.png",
#                 height = 8, width = 12, scale = 1)
# transects_fixed = cut_cross_sections2(
#   net       = net3,
#   id        = "comid",
#   cs_widths = pmax(50, net3$bf_width * 7),
#   num       = 5,
#   fix_braids = TRUE,
#   add       = TRUE
# )
# 
# fixed_trans_plot <- 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = net3) +
#   ggplot2::geom_sf(data = transects_fixed, color = "forestgreen", lwd = 1) +
#   ggplot2::labs(title = "Updated braid transects") +
#   thm
# library(patchwork)
# trans_plot <- orig_trans_plot + fixed_trans_plot 
# fixed_trans_plot
# ggplot2::ggsave(fixed_trans_plot, filename = "/Users/anguswatters/Desktop/fixed_transects.png",
#                 height = 8, width = 12, scale = 1)
# ggplot2::ggsave(trans_plot, filename = "/Users/anguswatters/Desktop/updating_transects_plots.png",
#                 height = 8, width = 12, scale = 1)
# 
# 
# 
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 60)
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = net2)
# braids <- find_braids(
#   network = net2,
#   return_as = "dataframe", 
#   add = T, 
#   nested = FALSE
# )%>% 
#   dplyr::arrange(hydroseq)
# 
# make_braids_gif(braids, 
#                 save_path  = "/Users/anguswatters/Desktop/small_braid_animation2.gif",       
#                 height     = 8,
#                 width      = 10,
#                 gif_width  = 1800, 
#                 gif_height = 1500,
#                 delay      = 0.8,
#                 verbose    = TRUE
# )
# net3 <- nhdplusTools::navigate_network(start = 1079041, mode = "UT",  distance_km = 40)
# # net3 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100 )
# # net2 <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 100)
# # net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 200)
# mapview::mapview(net3)
# net3 <-
#   net3 %>%
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# transects_fixed2 = cut_cross_sections2(
#   net       = net3,
#   id        = "comid",
#   cs_widths = pmax(50, net3$bf_width * 7),
#   num       = 5,
#   fix_braids = FALSE,
#   add       = TRUE
# )
# 
# unfixed_trans_plot2 <-
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = net3) +
#   ggplot2::geom_sf(data = transects_fixed2, color = "red", lwd = 1) +
#   # ggplot2::labs(title = "Updated braid transects") +
#   thm
# 
# ggplot2::ggsave(unfixed_trans_plot2, filename = "/Users/anguswatters/Desktop/old_transects.png",
#                 height = 8, width = 12, scale = 1)
# 
# braids <- find_braids(
#   network = net3,
#   return_as = "dataframe", 
#   add = T, 
#   nested = FALSE
# ) %>% 
#   dplyr::arrange(hydroseq)
# 
# braids %>% 
#   nhdplusTools::get_sorted(split = T)
# 
# make_braids_gif(braids, 
#                 save_path  = "/Users/anguswatters/Desktop/large_braid_animation.gif",       
#                 height     = 8,
#                 width      = 10,
#                 gif_width  = 1800, 
#                 gif_height = 1500,
#                 delay      = 0.8,
#                 verbose    = TRUE
# )
# ********************************
# -------- Plot generator --------
# ********************************
# 
# # AREA 2
# test_net <- nhdplusTools::navigate_network(start = 101, mode = "UT", distance_km = 50) %>%
#   # dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# plot(test_net$geometry)
# 
# whole_net <- nhdplusTools::navigate_network(start = 101, mode = "UT", distance_km = 100) %>%
#   # dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# plot(whole_net$geometry)
# 
# transects <- cut_cross_sections2(  net       = whole_net,
#                                    id        = "comid",
#                                    cs_widths = pmax(50, whole_net$bf_width * 7),
#                                    num       = 5,
#                                    fix_braids = FALSE,
#                                    add       = TRUE)
# whole_net$gnis_name
# mapview::mapview(whole_net)
# ggplot2::ggplot() +
#   ggplot2::geom_sf(data = whole_net) + 
#   ggplot2::geom_sf(data = transects,color = "red", lwd = 2) + 
#   thm
# plot(transects$geometry)
# # AREA 1
# test_net <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 30) %>%
#   # dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# plot(test_net$geometry)
# whole_net <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 350) %>%
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# plot(whole_net$geometry)
# Network = whole_net
# Subset = test_net
# mapview::mapview(Network, color = "dodgerblue") + mapview::mapview(Subset, color = "red")
# test_net$gnis_name %>% unique()
# test_braids <- 
#   test_net %>% 
#   find_braids(return_as = "dataframe", add = T, nested = F)
# make_braids_gif(test_braids, 
#                 save_path  = "/Users/anguswatters/Desktop/updated_braid_gif.gif", 
#                             title = "South Platte River, Eastern Colorado",
#                             height     = 8,
#                             width      = 10,
#                             gif_width  = 1800, 
#                             gif_height = 1500,
#                             delay      = 0.8,
#                             legend_pos = "bottom",
#                             verbose    = FALSE
# )
# # make transects
# 
# transects <- cut_cross_sections2(  net       = test_net,
#                                    id        = "comid",
#                                    cs_widths = pmax(50, test_net$bf_width * 7),
#                                    num       = 5,
#                                    fix_braids = FALSE,
#                                    add       = TRUE)
# ntransects_plot <- 
#   ggplot2::ggplot() +
#   ggplot2::geom_sf(data = test_net) + 
#   ggplot2::geom_sf(data = transects,color = "red", lwd = 1) + 
#   ggplot2::labs(
#     title = "South Platte River, Eastern Colorado",
#     color = ""
#   ) +
#   thm
# 
# ggplot2::ggsave(
#   ntransects_plot,
#   filename = "/Users/anguswatters/Desktop/ntransects.png", 
#   height     = 8,
#   width      = 10,
#   scale   = 1
# )
# thm <- 
#   ggplot2::theme_bw() +
#   ggplot2::theme(
#     plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
#     plot.subtitle =  ggplot2::element_text(size = 14, hjust = 0.5),
#     legend.position = "none"
#   )
# 
# test_braids %>% 
#   sf::st_union() %>% 
#   sf::st_centroid()
# 
# # inset map point
# lng = c(-103.2289)
# lat = c(40.58117)
# 
# 
# pt <-   data.frame(lng = lng, 
#                    lat = lat
# ) %>% 
#   sf::st_as_sf( coords = c("lng", "lat"), crs = 4326) %>% 
#   sf::st_geometry() %>% 
#   sf::st_centroid()
# install.packages("ggmapinset")
# library(ggmapinset)
# 
# whole_net <- 
#   dplyr::mutate(
#   whole_net,
#   comid = as.character(comid)
# ) %>% 
#   dplyr::mutate(
#     map_id = dplyr::case_when(
#       comid %in% test_net$comid ~ "subset",
#       TRUE ~ "Whole network"
#     )
#   )
# clean <-
#   ggplot2::ggplot() +
#   ggmapinset::geom_sf_inset(data = sf::st_transform(
#     whole_net,
#     4326), ggplot2::aes(color = map_id)) +
#   ggmapinset::geom_inset_frame() +
#   gghighlight::gghighlight(map_id == "subset") +
#   ggplot2::labs(
#     title = "South Platte River, Eastern Colorado",
#     color = ""
#   ) +
#   ggmapinset::coord_sf_inset(inset = ggmapinset::configure_inset(centre = pt,
#                                                                  scale = 10,
#                                            translation = c(250, -110),
#                                          radius = 9)) +
# 
#     thm
# clean



# *******************************
# -------- TEST DATASETS --------
# *******************************

# 
# net8 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 50) %>%
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net2 <-nhdplusTools::navigate_network(start = 101, mode = "UT", distance_km = 250)
net8 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 200) 
net2 <-nhdplusTools::navigate_network(start = 101, mode = "UT", distance_km = 250)
net_sy <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 150)
net3 <- nhdplusTools::navigate_network(start = 1079041, mode = "UT",  distance_km = 150)
plot(net_sy$geometry)

sf::write_sf(
  net8,
  "/Users/anguswatters/Desktop/terrain_slicer_test_data/network1.gpkg", 
)
sf::write_sf(
  net2,
  "/Users/anguswatters/Desktop/terrain_slicer_test_data/network2.gpkg", 
)
sf::write_sf(
  net_sy,
  "/Users/anguswatters/Desktop/terrain_slicer_test_data/network3.gpkg", 
)

# sf::write_sf(
#   net8,
#   "/Users/anguswatters/Desktop/ntransects.png", 
# )
# 
# sf::write_sf(
#   net8,
#   "/Users/anguswatters/Desktop/ntransects.png", 
# )
# net2 <- sf::read_sf("/Users/anguswatters/Desktop/terrain_slicer_test_data/network3.gpkg")
# net2 <- sf::read_sf("/Users/anguswatters/Desktop/terrain_slicer_test_data/network1.gpkg")
net2 <- sf::read_sf("/Users/anguswatters/Desktop/terrain_slicer_test_data/01_reference_features.gpkg", 
                    layer = "flowlines")
layers <- sf::st_layers("/Users/anguswatters/Desktop/terrain_slicer_test_data/01_reference_features.gpkg")

names(net2) <- tolower(names(net2))

net3 <- 
  net2 %>% 
  # nhdplusTools::align_nhdplus_names() %>% 
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

starts <- 
  net3 %>% 
  dplyr::filter(totdasqkm == 0)
trav = bfs_traversal2(network = net3, start = starts$comid[1],     return_as = "dataframe")
ucomids <- unique(Reduce(c, trav)) 

trav_net <- 
  net3 %>% 
  dplyr::filter(comid %in% trav$comid)
min(net3$totdasqkm)

system.time({
  transects_fix_new = cut_cross_sections3(
    net       = trav_net,
    id        = "comid",
    cs_widths = pmax(50, trav_net$bf_width * 7),
    num       = 5,
    fix_braids = T,
    add       = TRUE,
    use_original = FALSE
  )
})

starts <- 
  net3 %>% 
  dplyr::filter(totdasqkm == 0)

trav = bfs_traversal2(network = net3, start = starts$comid[1],     return_as = "dataframe")
ucomids <- unique(Reduce(c, trav)) 

trav_net <- 
  net3 %>% 
  dplyr::filter(comid %in% trav$comid)
unlist(trav)
mapview::mapview(starts, color = "red")  + bs

bs <- find_braids(trav_net, return_as = "dataframe", add = T, nested = T)

bs <- bs %>% dplyr::filter(braid_id != "no_braid") 
net3 %>% names()
plot(net3$geom)
# 
# data_info <-
#   data_info %>% 
#   dplyr::left_join(
#     res,
#     by = c("dataset" = "expr")
#   )
# 
# data_info %>% 
#   ggplot2::ggplot() +
#   # ggplot2::geom_line(ggplot2::aes(x = nrows, y = time, color = fixed))
#   # ggplot2::geom_line(ggplot2::aes(x = npoints, y = time, color = fixed))
#   # ggplot2::geom_line(ggplot2::aes(x = braid_count, y = time, color = fixed))
#   ggplot2::geom_line(ggplot2::aes(x = braid_comids, y = time, color = fixed))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = npoints)) 
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = braid_comids))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = time, color = fixed))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = time, color = fixed))

# net3 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100 )

system.time({
transects_nofix_orig = cut_cross_sections3(
  net       = net3,
  id        = "comid",
  cs_widths = pmax(50, net3$bf_width * 7),
  num       = 10,
  fix_braids = FALSE,
  add       = TRUE,
  use_original = TRUE
)
})
system.time({
  transects_nofix_new = cut_cross_sections3(
    net       = net3,
    id        = "comid",
    cs_widths = pmax(50, net3$bf_width * 7),
    num       = 10,
    fix_braids = FALSE,
    add       = TRUE,
    use_original = FALSE
  )
})

system.time({
  transects_fix_orig = cut_cross_sections3(
    net       = net3,
    id        = "comid",
    cs_widths = pmax(50, net3$bf_width * 7),
    num       = 5,
    fix_braids = T,
    add       = TRUE,
    use_original = TRUE
  )
})
system.time({
  transects_fix_new = cut_cross_sections3(
    net       = net3,
    id        = "comid",
    cs_widths = pmax(50, net3$bf_width * 7),
    num       = 5,
    fix_braids = T,
    add       = TRUE,
    use_original = FALSE
  )
})
transects_nofix_orig
transects_nofix_new
mapview::mapview(transects_nofix_orig, color = "red") + 
  mapview::mapview(transects_fix_orig, color = "red") +
mapview::mapview(transects_nofix_new, color = "red") + 
    mapview::mapview(transects_fix_new, color = "dodgerblue") + net3


transects_nofix_new %>% 
  dplyr::filter(!hy_id %in% transects_nofix_orig$hy_id)

net_diff   <- sf::st_difference(transects_nofix_new, transects_nofix_orig)
system.time({
  transects_fixed = cut_cross_sections2(
    net       = net3,
    id        = "comid",
    cs_widths = pmax(50, net3$bf_width * 7),
    num       = 5,
    fix_braids = TRUE,
    add       = TRUE
  )
  
}

)
# transects_fixed <- find_middle_flowlines(net3, transects_nofix)
# transects_fixed <- fix_braid_transects5000(net3, transects_nofix)
# transects_nofix = cut_cross_sections2(
#   net       = net3,
#   id        = "comid",
#   cs_widths = pmax(50, net3$bf_width * 7),
#   num       = 5,
#   fix_braids = FALSE,
#   add       = TRUE
# )
# 
mapview::mapview(transects_fixed, color = "red") +
  # mapview::mapview(new_trans, color = "red") +
  mapview::mapview(transects_nofix, color = "green") +
  mapview::mapview(net3, color = "dodgerblue")

# *********************************************************
# *********************************************************

# ****************************************
# ------------- LATEST (geos) ------------
# ****************************************

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects_latest2 <- function(
    net, 
    transect_lines,
    braid_threshold = NULL
) {
  
  # set geometry name of network to "geometry"
  net <- nhdplusTools::rename_geometry(net, "geometry")
  
  # transect_lines <-  transects_nofix
  # net <- net3
  # braid_threshold = NULL
  # braid_threshold = 25000
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  if(all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  message("Fixing braid transects...")
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <-
  braids <-  
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    # dplyr::group_by(comid) %>% 
    # dplyr::mutate(ncomid = n()) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # view data on map
  # mapview::mapview(not_braids, color = "dodgerblue") +
  # mapview::mapview(only_braids, color = "red") 
  
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x         = braids, 
      originals = not_braids, 
      threshold = braid_threshold,
      verbose   = TRUE
    )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
  }
  
  # # unique braid_ids/COMIDs
  # ubraids <- unique(only_braids$braid_id)
  # ucoms <- unique(only_braids$comid)
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) %>% 
    # dplyr::filter(divergence == 0)
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(-totdasqkm)
  
  # keep track of all original crossections
  all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
  
  # column to store the relative position within the braid of the flowline we're on 
  xs$relative_position <- NA
  
  # flag determining whether transect should/has been replaced
  xs$changed <- FALSE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # empty columns to store number of head/tail intersections
  xs$head_cuts     <- NA
  xs$tail_cuts     <- NA
  
  # empty columns to store distance needed to extend from head/tail of line
  xs$head_distance <- NA
  xs$tail_distance <- NA
  
  # data.table::data.table(xs)[1, ]
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    
    message("===== NO 'xs' transect lines =====")
    message("===== returning original data =====")
    
    return(transect_lines)
    
  } else {
    message("===== ", nrow(xs) , " 'xs' transect lines =====")
    # message("===== returning original data =====")
  }
  
  # braids %>% 
  #   geos_make_collection() %>% 
  #   geos_unary_union() %>% 
  #   st_as_sfc()
  # braids %>% 
  #   dplyr::mutate(
  #     geometry  =  geos::geos_geometry(.)
  #   ) %>% 
  #   dplyr::relocate(geometry2)
  # geos::as_geos_geometry(braids )
  
  # braids$geometry <-  geos::geos_geometry(braids$geometry)
  # mapview::mapview(braids, color = "dodgerblue") +
  #   mapview::mapview(xs, color = "red") +
  # mapview::mapview(xs[i, ], color = "green")
  
  # Loop through every single cross section and determine:
  # 1. its relative position
  # 2. how far to extend the line
  # 3. in what order should transects be extended, 
  # 4. in what direction to extend the transect
  for(i in 1:nrow(xs)) {
    # for(i in 1:17) {
    # message("i: ", i, "/", nrow(xs))
    # i = 18 
    # # transect line
    # tline <- xs[i, ]$geometry
    # i = 18
    # curr <- xs[i, ]
    
    # comid of transect line
    com <- xs$hy_id[i]
    
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    
    # get neighboring braid ID for our current braid
    neighbor_braids <- get_neighbor_braids(x = braids, ids = bids, only_unique = T)
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      braids,
      braid_id %in% neighbor_braids,
      comid != com
    )
    
    # # convert "others" geometry to geos_geometry
    # others$geometry <- geos::as_geos_geometry(others$geometry)
    # 
    # geos::as_geos_geometry(others$geometry) %>% sf::st_geometry()
    # 
    # others$geometry <- geos::as_geos_geometry(others$geometry)
    # 
    # sf::st_geometry(others$geometry)
    
    # Error in st_geometry.sf(x) : 
    
    # attr(obj, "sf_column") does not point to a geometry column.
    # Did you rename it, without setting st_geometry(obj) <- "newname"?
    
    # INPUTS INTO NEW AUGMENT TRANSECTS DF FUNCTION
    # cross_section = xs[i, ]
    # curr = xs[i, ]
    # geoms_to_cut <- others
    # max_distance = NULL
    # by = 1
    
    # tree <- geos::geos_strtree(braids[-81, ])
    # gg <- geos::as_geos_geometry(xs[1, ])
    # # tree[1]
    # 
    # geos::geos_strtree_query(tree, gg)
    # ttmp <- braids[81, ]
    # mapview::mapview(ttmp) + xs[1, ]
    # tree
    
    # other_meta <- sf::st_drop_geometry(others)
    # geoms_to_cut <- geos::as_geos_geometry(others$geometry)
    
    # geoms_to_cut  = others
    extend_maps <- geos_augment_transect(
      cross_section = xs[i, ],
      geoms_to_cut  = geos::as_geos_geometry(others$geometry),
      geom_ids      = others$comid,
      max_distance  = NULL, 
      by            = 1, 
      as_df         = FALSE,
      carry_geom    = FALSE
    )
    
    # extend_maps$head$as_list()
    position <- extend_maps$head$get("position")
    
    # message("----> position: ", position)
    
    # if(is.na(position)) {
    #   message("!!!!!! !!!!!!!!!!!!!!!!! !!!!!!!!!")
    #   message("!!!!!! FOUND AN NA POSITION VALUE !!!!!!!!!")
    #   message("!!!!!! !! iter: ", i ," !!!!!!!!!")
    # }
    
    # if a flowline on the inner portion of a braid, make extension and insert
    if(position == "inner") {
      # message("Extending ", i, " and checking if valid replacement...")
      # extend line out by total distance key values in head and tail maps
      res_geom <- geos_extend_transects(
        starter_line   = geos::as_geos_geometry(xs$geometry[i]),
        head_distance  = extend_maps$head$get("total_distance"),
        tail_distance  = extend_maps$tail$get("total_distance"),
        extra_distance = xs$cs_widths[i]/2
      )
      
      # mapview::mapview(others) + braids + res_geom + not_braids
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
      if(
        # !any(
        #   lengths(
        #     sf::st_intersects(sf::st_as_sf(res_geom),
        #                     dplyr::filter(xs[-i,], changed)
        #                     )
        #   ) > 0)
        !geos::geos_intersects_any(
          res_geom,
          geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        )
      ) {
        
        # !geos::geos_intersects_any(
        #   res_geom,
        #   geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        # )
        # geos::geos_intersects(
        #   res_geom,
        #   geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        # )
        # geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        # if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
        # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # updatem geometry with new, extended cross section
        xs$geometry[i] <- sf::st_geometry(
          sf::st_as_sf(res_geom)
        )
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        # xs[i, ]$changed <- TRUE
      }
      
      # update relative position column
      xs$relative_position[i] <- extend_maps$head$get("position")
      
      # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
      xs$pending[i] <- extend_maps$head$get("pending")
      
      # update head/tail distances values in dataframe w/ values from head/tail hashmaps
      xs$head_distance[i] <- extend_maps$head$get("total_distance")
      xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
      
      # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
      xs$head_cuts[i] <- extend_maps$head$get("count")
      xs$tail_cuts[i] <- extend_maps$tail$get("count")
      
      
    } else {
      # message("Postpone processing: ", i)
      
      # update relative position column
      xs$relative_position[i] <- extend_maps$head$get("position")
      
      # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
      xs$pending[i] <- extend_maps$head$get("pending")
      
      # update head/tail distances values in dataframe w/ values from head/tail hashmaps
      xs$head_distance[i] <- extend_maps$head$get("total_distance")
      xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
      
      # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
      xs$head_cuts[i] <- extend_maps$head$get("count")
      xs$tail_cuts[i] <- extend_maps$tail$get("count")
      
    }
    
    # message("=================")
  }
  
  # tmp <- xs %>% dplyr::filter(is.na(relative_position))
  # mapview::mapview(xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") + other_xs
  #   mapview::mapview(tmp, color = "green")
  # net_intersects <- sf::st_intersects(not_braids, xs)
  # lengths(net_intersects)
  
  
  # # keep only the transects that were changed/extended
  # to_keep <- dplyr::filter(xs, changed)
  
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  # mapview::mapview(xs, color = "red") + braids + not_braids
  
  # check intersection of keeps and NOT BRAID
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- geos::geos_intersects_any(
    geos::as_geos_geometry(xs),
    geos::as_geos_geometry(not_braids)
  )
  # net_intersects <- sf::st_intersects(not_braids, xs)
  
  # remove updated cross sections that intersect with the NOT BRAIDED flowlines
  if(any(net_intersects)) {
    
    message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
    xs <- xs[!net_intersects, ]
    
  }
  
  # mapview::mapview(xs2, color = "green") +
  #   mapview::mapview(tmpy, color = "gold") +
  #   mapview::mapview(not_braids, color = "dodgerblue") + 
  #   mapview::mapview(braids, color = "red") +
  # mapview::mapview(xs, color = "green")
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs = dplyr::filter(xs, 
                           !changed, 
                           relative_position != "inner"
  )
  # other_xs = dplyr::filter(xs, !changed)
  # tt <- dplyr::filter(xs, !changed, relative_position != "inner")
  # tt <- dplyr::filter(xs, !changed)
  
  # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  # dplyr::filter(xs, changed)
  # dplyr::filter(xs, !changed, relative_position == "inner")
  
  # inner transects that haven't been changed
  unchanged_inners <- dplyr::filter(xs, 
                                    !changed,
                                    relative_position == "inner")
  
  # keep only changed flowlines
  xs <- dplyr::filter(xs, changed) 
  
  # intersections between updated inner cross sections ("xs") and the remaining inner cross sections that were NOT changed ("unchanged_inners")
  inner_intersects <- geos::geos_intersects_any(
    geos::as_geos_geometry(unchanged_inners$geometry),
    geos::as_geos_geometry(xs$geometry)
  )
  
  # add back into "xs" the unchanged inner transects that do NOT intersect with our updated/extended inner transect lines
  xs <- dplyr::bind_rows(
    xs,
    unchanged_inners[!inner_intersects, ]
  )
  
  
  # # # # keep ALL "inner" transects, both the ones that were extended ("changed" == TRUE) and not changed inners
  # xs <- dplyr::filter(xs, changed | relative_position == "inner")
  
  # check intersection of keeps xs with other_xs
  
  # indices of other_xs transects that now intersect with the updated/extended 'xs' transects. 
  # All the cross section lines in "xs" are now "inner" lines that were extended
  other_intersects <- geos::geos_intersects_any(
    geos::as_geos_geometry(other_xs$geometry),
    geos::as_geos_geometry(xs$geometry)
  )
  # other_intersects <- sf::st_intersects(xs, other_xs)
  # unlist(sf::st_intersects(xs, other_xs))
  
  # net_intersects <- sf::st_intersects(not_braids, xs)
  # lengths(other_intersects)
  
  # if there ARE some intersections, remove those intersecting lines from 'div_xs'
  if(any(other_intersects)) {
    message("Removing ", table((unlist(other_intersects)))["TRUE"], " transect lines from 'other_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[!other_intersects, ]
  }
  
  # # flag determining whether transect should be replaced
  # other_xs$changed <- FALSE
  
  # if there are still other (non "inner") transects, do extension processing
  if (nrow(other_xs) > 0) {
    
    # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
    # loop through the remaining transects that were NOT "inner" lines, and do extensions
    for (i in 1:nrow(other_xs)) {
      
      # message("i: ", i, "/", nrow(other_xs))
      
      # other_xs$relative_position[i]
      # other_xs$head_distance[i]
      # other_xs$tail_distance[i]
      # other_xs$head_cuts[i]
      # other_xs$tail_cuts[i]
      # other_xs$cs_widths[i]
      # i = 1
      # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
      if (other_xs$relative_position[i] == "no_intersects") {
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
        next
      }
      
      # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
      res_geom <- geos_extend_transects(
        starter_line   = geos::as_geos_geometry(other_xs$geometry[i]),
        head_distance  = other_xs$head_distance[i],
        tail_distance  = other_xs$tail_distance[i],
        extra_distance = xs$cs_widths[i]/2
      )
      # mapview::mapview(res_geom, color = "green") +
      #   mapview::mapview(braids, color = "dodgerblue") +
      #   mapview::mapview(xs, color = "red") +
      #   mapview::mapview(other_xs$geometry[i], color = "cyan")
      #   braids + res_geom + not_braids
      
      # sf::st_intersects(res_geom, xs)
      # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
      # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom, 
      #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
      
      if(
        !any(
          geos::geos_intersects_any(
            geos::as_geos_geometry(xs),
            geos::as_geos_geometry(res_geom)
          )) &
        !any(geos::geos_intersects_any(
          geos::as_geos_geometry(other_xs[-i, ]),
          geos::as_geos_geometry(res_geom)
        ))
      ) {
        
        # # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # replace geometry with extended line
        other_xs$geometry[i] <- sf::st_geometry(sf::st_as_sf(res_geom))
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
      }
      
      # message("=================")
    }
    
    # # # keep only the transects that were changed/extended
    # other_drop <- dplyr::filter(other_xs, !changed)
    # 
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)
    # mapview::mapview(res_geom, color = "green") +
    #   mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(xs, color = "red") +
    #   # mapview::mapview(other_xs$geometry[i], color = "cyan")
    # mapview::mapview(other_drop, color = "green") +
    # mapview::mapview(other_xs, color = "red")
    # braids + res_geom + not_braids
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      ),
      dplyr::select(other_xs,
                    -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      )
    )
    
  } else {
    
    message("===== NO 'other_xs' transect lines =====")
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, 
                         -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                         -head_distance, -tail_distance, -head_cuts, -tail_cuts
    )
    
  }
  # mapview::mapview(out, color = "red") + 
  #   mapview::mapview(xs, color = "green") + 
  #   mapview::mapview(braids, color = "dodgerblue") 
  # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
  # to_keep %in% all_xs
  # all_xs %in% to_keep
  
  # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
  transect_lines <-  dplyr::bind_rows(
    # from original transect_lines, remove all of the cross sections on braids,
    dplyr::select(
      dplyr::filter(   
        dplyr::mutate(transect_lines, 
                      tmp_id = paste0(hy_id, "_", cs_id)
        ),
        !tmp_id %in% all_xs
      ),
      -tmp_id
    ),
    # updated braid cross sections
    out
  )
  
  # mapview::mapview(braids, color = "dodgerblue") +
  # mapview::mapview(not_braids, color = "gold") +
  # mapview::mapview(transect_lines, color = "green") +
  # mapview::mapview(transect_lines2, color = "red")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
}

# Given the count of intersections for a line extended from the HEAD and TAIL, determine its relative position within a braid
#  head count: numeric, count of intersections that line had when extending from HEAD
#  head tail_count: numeric, count of intersections that line had when extending from TAIL
# RETURNS: character string "no_intersects", "outer_single", "outer_multi", "inner" , or "in_between"  
check_relative_position <- function(head_count, tail_count) {
  
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  #       INNER scenario intersection count (odd and even cases):
  #         intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - MIDDLE/IN BETWEEN: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THIS!
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT)
  
  # TODO: Consider renaming these as No intersections, 
  # OUTER_SINGLE = SINGLE (the outer braid flowlines)
  # OUTER_MULTI  = SINGLE (the outer braid flowlines)
  # MIDDLE = IN BETWEEN (in between the outer braid flowlines and the actual middle braid flowlines)
  # INNER  = MIDDLE (the actual middle braid flowline)
  
  # boolean that gets flipped to FALSE if any of the other scenarios are detected
  in_between = TRUE
  
  # vector of intersection counts by the extended line,
  # extending out FROM THE HEAD and then FROM THE TAIL
  counts <- c(head_count, tail_count)
  
  # 1. No intersections scenario
  if(all(counts == 0)) {
    
    # relative position of line
    line_position <- "no_intersects"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    return(line_position)
    
  }
  
  # 2. OUTER SINGLE scenario
  if(all(counts == c(1, 0)) | all(counts == c(0, 1))) {
    
    # relative position of line
    line_position <- "outer_single"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    return(line_position)
    
  }
  
  # 3. OUTER MULTI scenario
  # Check if one value is 0 and the other is not zero AND is NOT 1
  if (any(counts == 0) && any(counts > 1)) {
    # if (any(counts == 0) && any(counts != 0)) {  
    
    # relative position of line
    line_position <- "outer_multi"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    # # index of the NON ZERO element 
    # not_zero_idx <- which(counts != 0)
    # message(paste("Index of NON ZERO element:", not_zero_idx))
    
    return(line_position)
  }
  
  # 4. INNER scenario
  # Handle sitation where total intersections is odd or even, if EITHER of below conditions is TRUE (OR condition), then we have inner (middle) line
  # - ODD CASE: If both the count values equal eachother
  # - EVEN CASE: If max(counts) minus 1 EQUALS min(counts)
  # If the counts equal eachother OR max(counts) minus 1 EQUALS min(counts)
  if(counts[1] == counts[2] | max(counts) - 1 == min(counts) ){
    
    # relative position of line
    line_position <- "inner"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    return(line_position)
    
  }
  # 5. IN_BETWEEN scenario
  #  IF NONE OF THE ABOVE CONDITIONS EXECUTED, then we have an IN_BETWEEN line
  if(in_between) {
    
    # relative position of line
    line_position <- "in_between"
    # message("line_position: ", line_position)
    
    # in_between boolean
    # message("in_between: ", in_between)
    return(line_position)
  }
  
  return(line_position)
}
# Extend a transect line outwards by a certain distance from the head and tail directions of the line
# starter_line is the original transect line to extend (geos_geoemtry)
# head_distance: numeric, distance (meters) to extend from HEAD of the line
# tail_distance: numeric, distance (meters) to extend from TAIL of the line
# extra_distance: Any extra distance the line should be extended after the original head/tail distances (THIS IS TYPICALLY GOING TO BE cs_width/2)
geos_extend_transects <- function(
    starter_line, 
    head_distance  = 0, 
    tail_distance  = 0, 
    extra_distance = 0
) {
  
  
  # extra_distance = 100
  # head_distance = 5555
  # tail_distance = 150
  # ifelse(head_distance == 0, 0, extra_distance)
  
  # set head and tail extra values to the 'extra_distance' argument
  head_extra = tail_extra = extra_distance 
  
  # if the HEAD extending distance is 0, also set the 'head_extra' value to 0
  if(head_distance == 0) {
    head_extra = 0
  } 
  
  # if the TAIL extending distance is 0, also set the 'tail_extra' value to 0
  if(tail_distance == 0) {
    tail_extra = 0
  }
  
  # distance to extend head and tail out by
  head_extension <- head_distance + head_extra
  tail_extension <- tail_distance + tail_extra
  
  # head_extension <- head_distance + ifelse(head_distance == 0, 0, extra_distance)
  # tail_extension <- tail_distance + ifelse(tail_distance == 0, 0, extra_distance)
  # head_extension <- head_distance + (cs_width/2)
  # tail_extension <- tail_distance + (cs_width/2)
  
  # first extend the head outwards
  res_geom <- geos_extend_line(
    starter_line,  
    head_extension, 
    "head"
  )
  
  # then extend the tail from the already head extended line 
  res_geom <- geos_extend_line(
    res_geom,  
    tail_extension, 
    "tail"
  )
  
  return(res_geom)
  
}

# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: (geos_geometry), other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# geom_ids: vector of unique IDs for each geoms_to_cut
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
geos_augment_transect <- function(cross_section,
                                  geoms_to_cut, 
                                  geom_ids,
                                  max_distance = NULL,
                                  by = NULL, 
                                  as_df = TRUE, 
                                  carry_geom = TRUE
) {
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  # max_dist <- as.numeric(
  #                 max(
  #                   sf::st_distance(  
  #                     geoms_to_cut, 
  #                     x
  #                   )
  #                 )
  #               )
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
  # geom_ids      = others$comid
  # max_distance  = NULL
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = others
  # max_distance  = NULL
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  
  # cs_line <- geos::as_geos_geometry(cross_section$geometry)
  
  # extract values from cross_section dataframe
  cs_width <- cross_section$cs_widths
  bf_width <- cross_section$bf_width
  id       <- cross_section$hy_id
  
  cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  # cs_line  <- cross_section$geometry
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(max_distance)) {
    max_distance <- max(cs_width * 5)
  }
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(by)) {
    by = bf_width/2
  }
  
  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  # dist_vect <- seq(0, max(c(max_distance, 2000)), multi_transects[i, ]$bf_width)
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  head_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(id), 
    dir           = "head",
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(id), 
    dir           = "tail",
    map           = TRUE
  )
  
  # head_map$as_list()
  # tail_map$as_list()
  
  # # extract the linestringshapes
  # tail_ext <- tail_map$get("line")
  # head_ext <- head_map$get("line")
  
  # mapview::mapview(braids,color = "gold") +  mapview::mapview(geoms_to_cut,color = "dodgerblue") + 
  # mapview::mapview(head_ext,color = "green") +  mapview::mapview(tail_ext,color = "red") +mapview::mapview(cs_line,color = "cyan") 
  
  # get the relative position within the braid of the linestring we are extending our transect out from
  position <- check_relative_position(
    head_count = head_map$get("count"),
    tail_count = tail_map$get("count")
  )
  
  # POSITION VALUES explanation:
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO_INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER_SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER_MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  # INNER scenario intersection count (odd and even cases):
  # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THESE (maybe?) !
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
  
  # if as_df is FALSE, return the line data hashmaps as a list of length 2, 
  # first list element is the head extension data and the second is the tail extension data
  if(!as_df) {
    
    # if NOT AN INNER LINE, postpone processesing
    if(position != "inner") {
      
      # set pending values for these geometries
      head_map$set("pending", TRUE)
      tail_map$set("pending", TRUE)
      
      # set pending values for these geometries
      head_map$set("position", position)
      tail_map$set("position", position)
      
    } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
      
      # set pending values for these geometries
      head_map$set("pending", FALSE)
      tail_map$set("pending", FALSE)
      
      # set pending values for these geometries
      head_map$set("position", position)
      tail_map$set("position", position)
      
    }
    
    # if carry geom is FALSE, remove geometry linestrings from maps before returning
    if(!carry_geom) {
      head_map$remove("line")
      tail_map$remove("line")
    }
    
    return(
      list(
        head = head_map,
        tail = tail_map
      )
    )
    # return(
    #   list(
    #     head = head_map$as_list(),
    #     tail = tail_map$as_list()
    #   )
    # )
    
  }
  
  
  # update "relative_position" column in cross_section to reflect the position of the cross section flowline within the braid value
  cross_section$relative_position <- position
  
  # if NOT AN INNER LINE, postpone processesing
  if(position != "inner") {
    # DON"T UPDATE "pending" value to reflect that this line should be put on hold and processed after the inner flowlines
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    cross_section$head_distance <- head_map$get("total_distance")
    cross_section$tail_distance <- tail_map$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    cross_section$head_cuts <- head_map$get("count")
    cross_section$tail_cuts <- tail_map$get("count")
    
    # if LINE IS A INNER LINE, GET READY TO EXTEND
  } else {
    
    # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
    cross_section$pending <- FALSE
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    cross_section$head_distance <- head_map$get("total_distance")
    cross_section$tail_distance <- tail_map$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    cross_section$head_cuts <- head_map$get("count")
    cross_section$tail_cuts <- tail_map$get("count")
    
  }
  
  # res_geom <- extend_transects(
  #                   starter_line   = cs_line, 
  #                   head_distance  = head_map$get("total_distance"),
  #                   tail_distance  = tail_map$get("total_distance"),
  #                   extra_distance = cs_width/2
  #                 )
  
  return(cross_section)
  
}

# WHILE LOOP IMPLEMENTATION INSTEAD OF RECURSION
# Extend transect line outward and return the minimum linestring that crosses all possible other linestrings (geoms_to_cut) in the given direction
# recursive function that recursively applies the binary search algorithm to extend
# the transect lines outward in both directions until no more braid lines CAN be found/intersected with
# Arguments:
# x = start index of distances vector
# line = transect line that should be extended
# distances = numeric vector of distance values in ascending order (sorted)
# geoms_to_cut = other linestrings (all linestrings other than 'line') that should be cut across
# ids = vector id of the 'line' argument
# dir = character, either "head" or "tail", indicating which direction to extend 'line' out
geos_extend_out <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    geom_ids,
    ids, 
    dir = "head",
    map = FALSE
) {
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
  # geom_ids      = others$comid
  # max_distance  = NULL
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  # cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  # 
  # x             = 1
  # line          = cs_line
  # distances     = dist_vect
  # # geoms_to_cut  = geoms_to_cut
  # geom_ids      = geom_ids
  # ids           = c(id)
  # dir           = "tail"
  # map           = TRUE
  
  
  # # if NOT a geos_geometry class, coerce
  # if(!inherits(line, "geos_geometry")) {
  #   # convert to geos geometry
  #   line <- geos::as_geos_geometry(line)
  #   # geoms_to_cut <- geos::as_geos_geometry(others)
  # }
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(geoms_to_cut, "geos_geometry")) {
    # convert to geos geometry
    geoms_to_cut <- geos::as_geos_geometry(geoms_to_cut)
    # geoms_to_cut <- geos::as_geos_geometry(others)
  }
  
  
  if(map) {
    dmap <- fastmap::fastmap()
  }
  
  # count interesections
  count <- 0
  dcount <- 0
  
  # while (TRUE) {
  while (TRUE) {
    # while (x < length(distances)) {
    # message("x: ", x)
    # message("distances[x]: ", distances[x])
    
    xx <- geos_bs_distance(
      distances    = distances, 
      line         = line, 
      geoms_to_cut = geoms_to_cut,
      direction    = dir
    )
    
    # count        <- count + 1
    # dcount       <- dcount + distances[xx]
    
    # message("xx: ", xx)
    # message("distances[xx]: ", distances[xx])
    # message("ids: ", ids)
    
    if (xx >= length(distances)) {
      # message("!!!!!!!!!!!!!!!! ")
      # message("!!!!!!!! xx >= distance: ", xx, " >= ", length(distances), " !!!!!!!! ")
      # message("!!!!!!!!!!!!!!!! ")
      break
    }
    
    # extend line out to midpoint of distances vector
    crosser <- geos_extend_line(line, distances[xx], end = dir)
    
    # # extend line out to where the binary search first hit a line
    # crossersf <- st_extend_line(sf::st_as_sf(line), 
    #                             distances[xx], end = dir)  
    # mapview::mapview(sf::st_as_sf(crosser), color = "dodgerblue") + mapview::mapview(crossersf, olor = "red")
    # others$geometry <- geos::as_geos_geometry(others$geometry)
    
    
    # Get the 'new_comid' that will be added to "ids" variable and then passed to the next iteration
    # - excluding the IDs in 'ids', determine what geometries in 'geoms_to_cut' are intersecting with our extended 'crosser' line
    # - then index 'geom_ids' based on the boolean vector returned from geos_intersects(), to then get the newly intersected ID (new_comid)
    new_comid <- geom_ids[
      geos::geos_intersects(
        crosser,
        geoms_to_cut[
          !geom_ids %in% ids
        ]
      )
    ]
    
    # # get the comid of the flow line that was intersected, 
    # # new comid that should be added to "ids" variable and passed to the next iteration
    # new_comid <- geoms_to_cut$comid[
    #                     unlist(sf::st_intersects(
    #                       crosser,
    #                       dplyr::filter(geoms_to_cut, 
    #                                     !comid %in% ids
    #                                     )))
    #                     ]
    
    # Update all the variables for next iteration of while loop
    
    # update 'line'
    line         <- crosser
    
    
    # # set the geometries within c(ids, new_comid) to empty (essentially filtering them out)
    # geoms_to_cut[geom_ids %in% c(ids, new_comid)] <- geos::geos_empty()
    
    # # update geom_ids, removing ids and the new_comid
    # geom_ids <- geom_ids[!geom_ids %in% c(ids, new_comid)] 
    
    # update 'geoms_to_cut' and drop the newly added 'new_comid' 
    geoms_to_cut <- geoms_to_cut[
      !geom_ids %in% c(ids, new_comid)
    ]
    
    # update 'geom_ids', removing ids and the new_comid
    geom_ids <- geom_ids[
      !geom_ids %in% c(ids, new_comid)
    ] 
    
    # update 'ids' w/ new_comid
    ids          <- c(ids, new_comid)
    
    # update x (index) value
    x            <- xx
    
    # increment count and continue summing distances
    count        <- count + 1
    dcount       <- dcount + distances[xx]
    
    # message("FINAL x: ", x)
    # message("=======================")
    
  }
  
  # # if specified, return distance map of info and line
  if(map) {
    
    # decrement count by 1 if non zero
    # count <- ifelse(count == 0, count, count-1)
    
    dmap$mset(
      index           = x, 
      distance        = distances[x], 
      total_distance  = dcount,
      line            = line,
      cut_ids         = ids,
      count           = count,
      direction       = dir
    )
    
    return(dmap)
  }
  
  # otherwise just return the line
  return(line)
  
  # # if specified, return the distance index of line
  # if (index) {
  #   return(x)
  #   } 
  # 
  # return(line)
}

# Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
# distances: numeric vector sorted in ascending order
# line: linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
# geoms_to_cut: geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
# direction: character, either "head" or "tail", indicating which end of the line to extend out.
geos_bs_distance <- function(
    distances, 
    line,
    geoms_to_cut, 
    direction = "head"
) {
  
  # distances    = distances
  # line         = line
  # geoms_to_cut = geoms_to_cut
  # direction    = dir
  
  # sftmp <- st_extend_line(xs[i, ], distances[M], end = dir)
  # mapview::mapview(geos_tmp, color = "red") +
  #   mapview::mapview(xs[i, ], color = "dodgerblue") +
  #   mapview::mapview(sftmp, color = "green")
  
  # distances    = distances
  # line         = line
  # geoms_to_cut = geoms_to_cut
  # direction    = dir
  
  
  
  # Left and right pointers (start and end of distances vector)
  L = 1
  R = length(distances)
  
  # While left pointer (L) is less than or equal to the right pointer (R), run binary search. 
  # Each iteration:
  # - the midpoint value gets calculated (M)
  # - M is the index of the 'distances' vector that we will use as the distance value to extend 'line'
  # - if the new extended line ('new_line') intersects with 'geoms_to_cut', then we decrease the distance value (DECREMENT RIGHT POINTER to the MIDPOINT - 1), 
  # - if NOT we increase the distance value (INCREMENT LEFT POINTER to the MIDPOINT + 1)
  while(L <= R) {
    
    # calculate midpoint between left and right pointers
    M = (L + R) %/% 2
    
    # message("L: ", L)
    # message("M: ", M)
    # message("R: ", R)
    # message("x[L]: ", distances[L])
    # message("x[M]: ", distances[M])
    # message("x[R]: ", distances[R])
    
    if(M == 0 | M == length(distances)) {
      # message("EARLY STOPPING bc M = ", M)
      # message("RETURNING L = ", L)
      return(L)
    }
    
    # extend line out to midpoint of distances vector
    new_line <- geos_extend_line(line, distances[M], end = direction)
    # new_line_sf <- st_extend_line(sf::st_as_sf(line), distances[M], end = direction)
    
    # geos::geos_intersects(geoms_to_cut, new_line)
    # sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)
    # geos::geos_intersects(geoms_to_cut, new_line)
    # lengths( sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)) > 0
    # any(geos::geos_intersects(geoms_to_cut, new_line))
    # any( lengths(sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)) > 0)
    # plot(new_line, col = "red", lwd= 5, add = F)
    # plot(line, col = "green", lwd= 5, add = T)
    
    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    
    # if ANY of the geometries in geoms_to_cut are intersected by the new extended line
    if(
      any(geos::geos_intersects(geoms_to_cut, new_line))
    ) {
      
      # then DECREMENT RIGHT pointer (DECREASE DISTANCE VALUE) to the midpoint - 1
      R = M - 1
      
      # otherwise IF NO intersections occur:
    } else {
      
      # then INCREMENT LEFT pointer (INCREASE DISTANCE VALUE) to the midpoint + 1
      L = M + 1
      
    }
    # message("=======================")
  }
  
  # l_line = st_extend_line(ls, x[L])
  # return(l_line)
  
  return(L)
}

# mapview::mapview(line, color = "red") +
# mapview::mapview(ms_xs, color = "red") +
#   # mapview::mapview(crosser, color = "dodgerblue") +
#   mapview::mapview(singles, color = "dodgerblue") +
#   mapview::mapview(multis, color = "green") +
#   transects
# mapview::mapview(cross_pt, col.regions = "red") +
# start + end  + boi + ms_xs + transects + singles
#   dplyr::filter(boi, !comid %in% com)
binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
  # x  = distances
  # ls = line
  # other     = geoms_to_cut
  # direction = dir
  # distances    = distances
  # line         = line
  # geoms_to_cut = geoms_to_cut
  # direction    = dir
  
  # distances2    = distances
  # line2         = line
  # geoms_to_cut2 = geoms_to_cut
  # direction2    = dir
  
  # direction = "head"
  
  L = 1
  R = length(distances)
  
  # while(L <= R & !flag) {
  while(L <= R) {
    
    M = (L + R) %/% 2
    
    # message("L: ", L)
    # message("M: ", M)
    # message("R: ", R)
    # message("x[L]: ", x[L])
    # message("x[M]: ", x[M])
    # message("x[R]: ", x[R])
    
    if(M == 0 | M == length(distances)) {
      # message("EARLY STOPPING bc M = ", M)
      # message("RETURNING L = ", L)
      return(L)
    }
    
    new_line <- st_extend_line(line, distances[M], end = direction)
    # geos_extend_line(line, distances[M], end = end)
    # st_extend_line(line,    distances[0], end = direction)
    # sf::st_intersects(others, new_line)
    # mapview::mapview(others) + new_line + ls
    # (any(lengths(sf::st_intersects(others, new_line)) > 0))
    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    if(any(lengths(sf::st_intersects(geoms_to_cut, new_line)) > 0)) {
      # message("DECREM RIGHT.--> need smaller value")
      # message("R = R - 1 = : ", M - 1)
      # decrement right pointer to middle - 1
      R = M - 1
    } else {
      # message("DECREM RIGHT.--> need smaller value")
      # message("L = M + 1 = : ", M + 1)
      # increment left pointer to middle + 1
      L = M + 1
    }
    # message("=======================")
    # mapview::mapview(new_line) + boi + line + singles + others <- 
  }
  # l_line = st_extend_line(ls, x[L])
  # return(l_line)
  return(L)
}
# transects <- geos_empty()
# line <- as_geos_geometry(net[j,])
# 
# vertices <- wk_vertices(line)
# 
# edges <- as_geos_geometry(
#   wk_linestring(
#     vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#     feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#   )
# )
# function for finding direction each line end point is pointing
geos_linestring_dir <- function(line) {
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- coords[c("x", "y", "feature_id")]
  
  # dimensions
  k <- c(1, - 1)
  i <- c(2, nrow(coords) - 1)
  
  dirs <- mapply(i, k, FUN = function(i, k) {
    x1 <- coords[i-k, 1]
    y1 <- coords[i-k, 2]
    x2 <- coords[i, 1]
    y2 <- coords[i, 2]
    unname(atan2(y1 - y2, x1 - x2))
  })
  
  return(dirs)
  
}

# function which extends the line (one or both ends) by a given distance (in unit distance):
# line: sf or geos linestring
# distance: numeric value in meters or a vector of length 2 if 'end = "both"' where 
#       the first value in the vector will extend that tail by that value and the second value extends the head by that value c(tail, head).
#       If a single value is given when end = "both", the value is recycled and used to extend both ends
# end: character, determines whether to extend the linestring from the tail, head or both ends
geos_extend_line <- function(line, distance, end = "both", with_crs = TRUE) {
  # line <- xs[1, ]
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  if(!end %in% c("head", "tail", "both")) {
    stop("Invalid input 'end' must either'head', 'tail', or 'both'")
  }
  # crs <- wk::wk_crs(line)
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- as.matrix(coords[c("x", "y")])
  # coords <- coords[c("x", "y")]
  
  # which index to keep
  to_keep <- end != c("tail", "head")
  
  # end coords index we want to keep
  ends <- c(1, nrow(coords))[to_keep]
  
  # get directions of the direction of interest
  directions <- geos_linestring_dir(line)[to_keep]
  
  # if only a single distance, duplicate it, otherwise reverse the first 2 distances
  distances <- if (length(distance) == 1) {
    rep(distance, 2) 
  } else {
    rev(distance[1:2])
  }
  
  # adjust end point coordinates 
  coords[ends, ]  <- coords[ends, ] + distances[to_keep] * c(cos(directions), sin(directions))
  
  # whether to return with a CRS or not
  if(with_crs) {
    
    # # make a new linestring WITH CRS
    line <- geos::geos_make_linestring(
      x   = coords[, 1],
      y   = coords[, 2],
      crs = wk::wk_crs(line)
    )
    
    return(line)
    
  } 
  
  # else {
  #   # # make a new linestring WITHOUT CRS
  #   line <- geos::geos_make_linestring(x   = coords[, 1], y   = coords[, 2])
  #   return(line)
  # }
  
  # line <- sf::st_sfc( sf::st_linestring(coords), crs = sf::st_crs(line)) 
  # mapview::mapview(curr, color = "red") + mapview::mapview(newline, color = "green")
  # plot(line,  lwd = 6, add = T)
  # plot(curr$geometry,col = "green", lwd = 6, add = T)
  
  # # make a new linestring WITHOUT CRS
  line <- geos::geos_make_linestring(
    x = coords[, 1], 
    y = coords[, 2]
  )
  
  return(line)
}

# Apply flowline braid length threshold to braided network dataset 
# Return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset
# x: braided flowlines
# originals: not braided flowlines from the same network
# threshold: braid_threshold numeric value to remove braids with a total braid flowline length greater than 'threshold'
braid_thresholder <- function(x, 
                              originals,
                              threshold = NULL, 
                              verbose   = TRUE
) {
  
  # x         = braids
  # originals = not_braids
  # threshold = 30000
  # verbose = TRUE
  
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
  
  # unpack nested braid_id column0
  unpacked <- unnpack_braids(x)
  
  # calculate total length of all the linestrings in each braid_id
  unpacked <- 
    unpacked %>% 
    dplyr::group_by(braid_id) %>%
    dplyr::mutate(
      braid_length = as.numeric(
        sum(sf::st_length(geometry), na.rm = T))
    ) %>%
    dplyr::ungroup()
  # dplyr::mutate(braid_length = sf::st_length(geometry)) %>%
  # dplyr::mutate(braid_length = as.numeric(sum(braid_length, na.rm = T))) %>%
  
  # check to make sure some braids are over threshold and can be removed, if NOT then just return original data
  if(all(unpacked$braid_length <= threshold)) {
    
    message("Removing: 0 braids from braided dataset\n", 
            "Keeping: All braids as all braids have total flowline lengths less than or equal to threshold value: ", threshold)
    
    return(list(
      braids     = x,
      not_braids = originals)
    )
    
  } 
  
  # # table of TRUEs and FALSE for braids to keep/remove given 'threshold'
  # threshold_tbl <- table(unpacked$braid_length <= threshold)
  # if(verbose) { message("Removing: ",  threshold_tbl["FALSE"],  
  # " braids from braided dataset\nKeeping: ", threshold_tbl["TRUE"],
  #           " braids that have total flowline lengths less than or equal to threshold value: ", threshold)}
  
  # comids to keep (total length of braid linestrings is less than or equal to braid_threshold value)
  to_keep <- dplyr::filter(unpacked, braid_length <= threshold)$comid
  
  # # COMIDs that are too large, add them back to the "not_braids" data
  # to_drop <- dplyr::filter(x, !comid %in% to_keep)
  
  # keep track of keeping and removing count
  orig_nrows <- nrow(originals)
  x_nrows  <- nrow(x)
  
  # add the "too big braid COMIDs" back to original "not_braids" data 
  # and set these comids braid_ids to "no_braid" and is_multibraid = FALSE
  originals <- dplyr::bind_rows(
    originals,
    dplyr::select(
      dplyr::mutate(
        dplyr::filter(
          x, !comid %in% to_keep
        ),
        braid_id      = "no_braid", 
        is_multibraid = FALSE
      ),
      -has_mainstem
    )
  )
  
  new_orig_nrows <- nrow(originals)
  
  # filter out braid_ids/COMIDs that are too big
  x <- dplyr::filter(x, comid %in% to_keep)
  
  # updating count of keeping and removing 
  new_orig_nrows <- nrow(originals)
  new_x_nrows <- nrow(x)
  
  if(verbose) {
    message("Removing: ", new_orig_nrows - orig_nrows, 
            " braids from braided dataset\nKeeping: ",   new_x_nrows,
            " braids that have total flowline lengths less than or equal to threshold value: ", threshold)
  }
  
  return(list(
    braids     = x,
    not_braids = originals)
  )
  
}

# *********************************************************
# *********************************************************

# ****************************************
# ------------- LATEST (sf) ------------
# ****************************************

# *********************************************************
# *********************************************************

# *********************************
# ---------- NOT LATEST -----------
# *********************************

# *********************************************************
# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
# fix_extensions <- function(extensions, geoms_to_cut) {
fix_extensions <- function(xline,
                           head, 
                           tail, 
                           braid_class,
                           geoms_to_cut, 
                           head_extension_dist,
                           tail_extension_dist, 
                           cs_width,
                           bf_width
                           ) {
  # others <- geoms_to_cut
  # extensions <- xs_group
  # i = 5
  # xline <- xs_group[i, ]$geometry
  # 
  # head <- xs_group[i, ]$head_geom
  # tail <- xs_group[i, ]$tail_geom
  # tail
  # 
  # head_extension_dist <- xs_group[i, ]$head_dist
  # tail_extension_dist <- xs_group[i, ]$tail_dist
  # 
  # geoms_to_cut <- others
  # cs_width <- xs_group[i, ]$cs_widths
  # bf_width <- xs_group[i, ]$bf_width
  # braid_class <- xs_group[i, ]$braid_class
  # extensions <- 
  #   extensions %>% 
  #   dplyr::group_by(priority)  %>% 
  #   dplyr::arrange(priority, -totdasqkm, .by_group = TRUE) %>% 
  #   dplyr::ungroup()
  # 
  # max_dist <- as.numeric(
  #   max(
  #     sf::st_distance(  
  #       geoms_to_cut, 
  #       xline
  #     )
  #   )
  # )
  # # sequence from 0 to the max possible extension distance 
  # dist_vect <- seq(0, max(c(max_dist, 2000)), bf_width/2)
  # if neither direction had any intersections, skip this iteration:
  if(is.na(braid_class) | !braid_class %in% c("middle", "head", "tail") ) {
    message("---------------------------------")
    message("--- NO INTERSECT AFTER EXTENDING TRANSECT ---")
    message("--- CONTINUING TO NEXT TRANSECT ---")
    message("---------------------------------")
    return(NULL)
  }
  # # if neither direction had any intersections, skip this iteration:
  # if(is.na(braid_class) | braid_class == "no_intersects") {
  #   message("---------------------------------")
  #   message("--- NO INTERSECT AFTER EXTENDING TRANSECT ---")
  #   message("--- CONTINUING TO NEXT TRANSECT ---")
  #   message("---------------------------------")
  #   return(NULL)
  # }
  
  # now we know that there is atleast 1 intersection, 
  # first we'll check if its only in one direction or if intersections occur in BOTH directions
  if(braid_class %in% c("head", "tail")) {
    message("---------------------------------")
    message("--- ONE DIRECT INTERSECT ! ---")
    message("--- direction: ", braid_class ,"---")
    message("---------------------------------")
    # mapview::mapview(res_geom[which(count_intersects != 0)])
    
    # set res_geom to whichever direction has intersections
    # res_geom <-
    
    # get the hashmap of the direction that needs to be extended (i.e. the direction that has more than 0 intersections)
    # ext_map <- map_lst[[which(count_intersects != 0)]]
    
    # to_extend <- map_lst[[which(count_intersects != 0)]]
    # message("---------------------------------")
    # message("--- ONE DIRECT INTERSECT ! ---")
    # message("--- direction: ", ext_map$get("direction") ,"---")
    # message("---------------------------------")
    
    # ext_map$as_list()
    
    # direction to extend out
    direction <- braid_class
    # direction   <- ext_map$get("direction")
    # direction   <- ext_map$get("direction")
    
    # line to extend
    if(braid_class == "head") { 
      
      extend_line <- head 
      
      } else{  
        
      extend_line <- tail
      
      }
    
    
    # extend_line <- ext_map$get("line")
    
    # dist_vect[222]
    # length(dist_vect)
    # 307-171/2
    
    # start and end points of HEAD extended line
    start <- lwgeom::st_startpoint(extend_line)
    end   <- lwgeom::st_endpoint(extend_line)
    
    # mapview::mapview(others, color = "dodgerblue") +
    #   mapview::mapview(extend_line, color = "red") +
    #   mapview::mapview(x, color = "green")  +
    #   start + end + all_cross_pts + last_pt +res_geom
    
    # points that extended line crosses over other flowlines in the braid
    all_cross_pts <- sf::st_intersection(extend_line, geoms_to_cut)
    
    # mapview::mapview(res_geom) + tline + others + all_cross_pts
    
    # get the outtermost point that the line extended from HEAD crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_pt <- all_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(xline), all_cross_pts)))
    ]
    
    # minimum distance between start and end points of extended line and the furthest possible intersection point. 
    diff_distance   <- min(c(
      as.numeric(sf::st_distance(last_pt, start)),
      as.numeric(sf::st_distance(last_pt, end))
    ))
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    
    # extra distance to extend line in direction determined above
    extra <- (cs_width/2) - diff_distance
    
    # # first extend out the head
    # res_geom <- st_extend_line(
    #                   x,  
    #                   ext_map$get("distance") + extra,
    #                   # 1440,
    #                   ext_map$get("direction")
    #                 )
    
    res_geom <- st_extend_line(
      extend_line,  
      extra, 
      direction
    )
    
    
    return(res_geom)
    # mapview::mapview(others, color = "dodgerblue") +
    #   mapview::mapview(extend_line, color = "red") +
    #   mapview::mapview(xs_group, color = "green") + res_geom 
    
  }
  
  if(braid_class == "middle") {
    
    message("---------------------------------")
    message("--- BOTH DIRECT INTERSECT ! ---")
    message("--- direction: ", braid_class ,"---")
    message("---------------------------------")
    # head_ext <- extensions[i, ]$head_geom
    # tail_ext <- extensions[i, ]$tail_geom
    
    # start and end points of HEAD extended line
    start_head <- lwgeom::st_startpoint(head)
    end_head   <- lwgeom::st_endpoint(head)
    
    # start and end points of TAIL extended line
    start_tail <- lwgeom::st_startpoint(tail)
    end_tail   <- lwgeom::st_endpoint(tail)
    
    # points that HEAD extended line crosses over
    head_cross_pts <- sf::st_intersection(head, geoms_to_cut)
    
    # points that TAIL extended line crosses over
    tail_cross_pts <- sf::st_intersection(tail, geoms_to_cut)
    
    # mapview::mapview(res_geom) + tline + others + all_cross_pts
    
    # get the outtermost point that the line extended from HEAD crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_head_pt <- head_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(xline), head_cross_pts)))
    ]
    
    # get the outtermost point that the line extended from TAIL crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_tail_pt <- tail_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(xline), tail_cross_pts)))
    ]
    
    # these are the distances that the extended HEAD/LINE line is crossing over the outer most flowline of the transect, 
    # we will subtract this value by cs_widths/2 in order to get the distance we need to extend the HEAD/TAIL line out 
    # in order to have cs_widths/2 on both sides of the transect
    
    # distance for HEAD extended line
    head_dist   <- min(c(
      as.numeric(sf::st_distance(last_head_pt, start_head)),
      as.numeric(sf::st_distance(last_head_pt, end_head))
    ))
    
    # distance for TAIL extended line
    tail_dist <- min(c(
      as.numeric(sf::st_distance(last_tail_pt, start_tail)),
      as.numeric(sf::st_distance(last_tail_pt, end_tail))
    ))
    
    # set distances to 0 if no crossing point is on top of the start/end
    head_dist   <- ifelse(length(head_dist) == 0, 0, head_dist)
    tail_dist   <- ifelse(length(tail_dist) == 0, 0,  tail_dist)
    
    # check to make sure that extending the line made an intersection
    if (head_dist == 0 & tail_dist == 0) {
      # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      return(NULL)
      # next
    }
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    
    # extra distance to extend HEAD
    extra_head <- (cs_width/2) - head_dist
    
    # extra distance to extend TAIL
    extra_tail <- (cs_width/2) - tail_dist
    
    # first extend out the head
    res_geom <- st_extend_line(
      xline,  
      head_extension_dist + extra_head, 
      end = "head"
    )
    
    # then use the head extended line from above and extend the tail
    res_geom <- st_extend_line(
      res_geom,  
      tail_extension_dist + extra_tail, 
      end = "tail"
    )
    
    return(res_geom)
    # mapview::mapview(geoms_to_cut, color = "dodgerblue") +
    #   mapview::mapview(xline, color = "gold") +
    #   mapview::mapview(res_geom, color = "red") +
    #   mapview::mapview(res_geom2, color = "green") 
  }
  
  # if (braid_class == "outer") {
  #   message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  #   message("!!!!!  SKIPPING OUTER BRAID ! !!!!!!!!!")
  #   message("--- direction: ", braid_class ,"---")
  #   message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  #   return(NULL)
  # }

}


# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
augment_transect2 <- function(x, id, geoms_to_cut, cs_width, bf_width) {
  
  # x            = tline
  # id           = com
  # geoms_to_cut = others
  # cs_width     = xs[i, ]$cs_widths
  # bf_width     = xs[i, ]$bf_width
  # mapview::mapview(others, color = "dodgerblue") +
  #   mapview::mapview(tline, color = "green") +
  #   mapview::mapview(braids, color = "red")
  
  # lengths(sf::st_intersects(geoms_to_cut, x))
  
  # if 'x' intersects with flowlines from the start, return NULL immediately
  if(lengths(sf::st_intersects(x, geoms_to_cut)) != 0) {
    message("---------------------------------")
    message("!!!!! SKIPPING initial transect already intersects a flowline !!!!!")
    message("---> intersection count: ", lengths(sf::st_intersects(x, geoms_to_cut)) , " ---")
    message("---------------------------------")
    return(NULL)
  }
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  max_dist <- as.numeric(
                max(
                  sf::st_distance(  
                    geoms_to_cut, 
                    x
                  )
                )
              )

  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_dist, 2000)), bf_width/2)
  # dist_vect <- seq(0, max(c(max_dist, 2000)), multi_transects[i, ]$bf_width)
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
  head_map <- extend_out2(
    x             = 1,
    line          = x, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "head",
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
  tail_map <- extend_out2(
    x             = 1,
    line          = x, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "tail",
    map           = TRUE
  )
  
  # extract the linestring shapes
  tail_ext <- tail_map$get("line")
  head_ext <- head_map$get("line")
  
  # mapview::mapview(others, color = "dodgerblue") +
  #   mapview::mapview(tline, color = "green") +
  #   mapview::mapview(braids, color = "red") +
  #   mapview::mapview(tail_ext, color = "green") +
  #   mapview::mapview(head_ext, color = "red")
  
  # TODO CHECK which extended line should be selected when number of interesections is the same
  # IF: number of. intersection for each extended line is TIED, select the shorter of the two? NOT SURE WHAT THE CALL IS HERE
  # ELSE: return the one with more interesections (which.max)
  count_intersects <- c(
                        lengths(sf::st_intersects(head_ext, geoms_to_cut)), 
                        lengths(sf::st_intersects(tail_ext, geoms_to_cut))
                      )
  # In fixbraidstransect:
  #   Instead of doing the Xs and DIcvxs split, instead keep all lines (0, 1, 2 divergences)
  # 
  
  # Augmentline function:
  #   If both == 0:
  #   Return null 
  # 
  # If any == 0 and intersect[which(x, y) != 0] == 1:
  #   
  #   # Do single direction extension
  #   
  # If x == y or max(x, y) -1 == min(x, y): 
  #   # We found a middle, do double sided extension
  #   
  #   Else: 
  #   Return null 
  # make a list of head and tail map extensions
  map_lst <- list(head_map, tail_map)
  
  # mapview::mapview(head_ext) + tail_ext + others + tline
  
  # create simple feature collection of head and tail extended lines
  # res_geom <- sf::st_sfc(c(head_ext, tail_ext))
  
  # if neither direction had any intersections, skip this iteration:
  if(all(count_intersects == 0)) {
    message("---------------------------------")
    message("--- NO INTERSECT AFTER EXTENDING TRANSECT ---")
    message("--- CONTINUING TO NEXT TRANSECT ---")
    message("---------------------------------")
    return(NULL)
  }

  # which(count_intersects == 1)
  # count_intersects[which(count_intersects != 0)] == 1
  # map_lst[[which(count_intersects != 0)]]
  
  # now we know that there is atleast 1 intersection, 
  # first we'll check if its only in one direction or if intersections occur in BOTH directions
  # if(any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1) {
  
  # now we know that there is atleast 1 intersection, 
  # first we'll check if its only in one direction or if intersections occur in BOTH directions
  
  # if intersection one of the extended lines does not intersect (0) 
  # AND
  # the other other extended line intersects EXACTLY 1 other flowline,
  # then this is a single braid that we will extend across
  if(any(count_intersects == 0)) {

    # if the NON ZERO intersection extended line, intersects EXACTLY 1 other flowline, then do single extension out
    if(count_intersects[which(count_intersects != 0)] == 1) {
      
      
      # if()
      # if(any(count_intersects == 0)) {
      
      # Do single direction extension
      
      # mapview::mapview(res_geom[which(count_intersects != 0)])
      
      # Do single direction extension
      
      # set res_geom to whichever direction has intersections
      # res_geom <-
      
      # get the hashmap of the direction that needs to be extended (i.e. the direction that has more than 0 intersections)
      ext_map <- map_lst[[which(count_intersects != 0)]]
      
      # to_extend <- map_lst[[which(count_intersects != 0)]]
      message("---------------------------------")
      message("--- ONE DIRECT INTERSECT ! ---")
      message("---> count_intersect[1]: ", count_intersects[1] , " ---")
      message("---> count_intersect[2]: ", count_intersects[2] , " ---")
      message("--- direction: ", ext_map$get("direction") ,"---")
      message("---------------------------------")
      
      # ext_map$as_list()
      
      # direction to extend out
      direction   <- ext_map$get("direction")
      
      # line to extend
      extend_line <- ext_map$get("line")
      
      # dist_vect[222]
      # length(dist_vect)
      # 307-171/2
      
      # start and end points of HEAD extended line
      start <- lwgeom::st_startpoint(extend_line)
      end   <- lwgeom::st_endpoint(extend_line)
      
      # mapview::mapview(others, color = "dodgerblue") +
      #   mapview::mapview(extend_line, color = "red") +
      #   mapview::mapview(x, color = "green")  +
      #   start + end + all_cross_pts + last_pt +res_geom
      
      # points that extended line crosses over other flowlines in the braid
      all_cross_pts <- sf::st_intersection(extend_line, geoms_to_cut)
      
      # mapview::mapview(res_geom) + tline + others + all_cross_pts
      
      # get the outtermost point that the line extended from HEAD crosses other braid flowlines
      # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
      last_pt <- all_cross_pts[
        which.max(as.numeric(sf::st_distance(sf::st_centroid(x), all_cross_pts)))
      ]
      
      # minimum distance between start and end points of extended line and the furthest possible intersection point. 
      diff_distance   <- min(c(
        as.numeric(sf::st_distance(last_pt, start)),
        as.numeric(sf::st_distance(last_pt, end))
      ))
      
      # END_PT--------------- CROSSER_PT ----------------------------START_PT
      # |------------------------|
      # ^^^^ SMALLER SECTION ^^^^
      # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
      # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
      
      # extra distance to extend line in direction determined above
      extra <- (cs_width/2) - diff_distance
      
      # # first extend out the head
      # res_geom <- st_extend_line(
      #                   x,  
      #                   ext_map$get("distance") + extra,
      #                   # 1440,
      #                   ext_map$get("direction")
      #                 )
      
      res_geom <- st_extend_line(
        ext_map$get("line"),  
        extra, 
        ext_map$get("direction")
      )
      
      return(res_geom)
    
    }
    # mapview::mapview(others, color = "dodgerblue") +
    #   mapview::mapview(extend_line, color = "red") +
    #   mapview::mapview(x, color = "green")  +
    #   start + end + all_cross_pts + last_pt +res_geom + res_geom2 + net3
    # If x == y or max(x, y) -1 == min(x, y): 
    #   # We found a middle, do double sided extension
    #   
    #   Else: 
    #   Return null 
  } 
  
  # if the number of intersections is NOT 0 and equal eachother 
  # OR
  # the maximum intersection count - 1 is equal to the minimum intersection count
  # THEN we have found a MIDDLE flowline in a braid
  if(count_intersects[1] == count_intersects[2] | max(count_intersects) - 1 == min(count_intersects)) {
    
    
  
  # if count_intersects[1]
  # else {
    message("---------------------------------")
    message("--- BOTH DIRECT INTERSECT ! ---")
    message("---> count_intersect[1]: ", count_intersects[1] , " ---")
    message("---> count_intersect[2]: ", count_intersects[2] , " ---")
    message("---------------------------------")
    #   direction = "both"
    # }
    # Note from 07/22:
    #  I am working on handling the situation where the line 
    # should be extended out in both directions, in that case I am trying to merge the head_ext and tail_ext objects so I can then take the endpoints, and from those endpoints find the closest intersection points in "all_cross_pts". 
    # The CLOSEST 'alL_cross_pts' will give me information on how to calculate the extra extension 
    # length that the final line needs to be extended out in both directions
    
    # the other idea i have is to do a final line extension WITHIN the extend_out function... 
    # so then I don't even need to deal with the final line extending because it will return the lines 
    # in a ready-to-go format, all i need to then do is select if I am picking the head, tail, or BOTH
    
    # One more note: 
    # IF this method I am working on within this current function DOES WORK, 
    # then I will probably need to find a better way of iterating through these transects,
    # basically either just ordering each COMID by lowest to highest divergence is my best idea. 
    # I just need a way to "prioritize" lower divergence values (mainstems) when it 
    # comes to choosing which transect flowlines will be
    # kept when there is a transect intersecting transect situation.
    
    # if (direction == "both") {
    
    # start and end points of HEAD extended line
    start_head <- lwgeom::st_startpoint(head_ext)
    end_head   <- lwgeom::st_endpoint(head_ext)
    
    # start and end points of TAIL extended line
    start_tail <- lwgeom::st_startpoint(tail_ext)
    end_tail   <- lwgeom::st_endpoint(tail_ext)
    
    # points that HEAD extended line crosses over
    head_cross_pts <- sf::st_intersection(head_ext, geoms_to_cut)
    
    # points that TAIL extended line crosses over
    tail_cross_pts <- sf::st_intersection(tail_ext, geoms_to_cut)
    
    # mapview::mapview(res_geom) + tline + others + all_cross_pts
    
    # get the outtermost point that the line extended from HEAD crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_head_pt <- head_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(x), head_cross_pts)))
    ]
    
    # get the outtermost point that the line extended from TAIL crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_tail_pt <- tail_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(x), tail_cross_pts)))
    ]
    
    # these are the distances that the extended HEAD/LINE line is crossing over the outer most flowline of the transect, 
    # we will subtract this value by cs_widths/2 in order to get the distance we need to extend the HEAD/TAIL line out 
    # in order to have cs_widths/2 on both sides of the transect
    
    # distance for HEAD extended line
    head_dist   <- min(c(
      as.numeric(sf::st_distance(last_head_pt, start_head)),
      as.numeric(sf::st_distance(last_head_pt, end_head))
    ))
    
    # distance for TAIL extended line
    tail_dist <- min(c(
      as.numeric(sf::st_distance(last_tail_pt, start_tail)),
      as.numeric(sf::st_distance(last_tail_pt, end_tail))
    ))
    
    # set distances to 0 if no crossing point is on top of the start/end
    head_dist   <- ifelse(length(head_dist) == 0, 0, head_dist)
    tail_dist   <- ifelse(length(tail_dist) == 0, 0,  tail_dist)
    
    # check to make sure that extending the line made an intersection
    if (head_dist == 0 & tail_dist == 0) {
      # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      return(NULL)
      # next
    }
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    
    # extra distance to extend HEAD
    extra_head <- (cs_width/2) - head_dist
    
    # extra distance to extend TAIL
    extra_tail <- (cs_width/2) - tail_dist
    
    # first extend out the head
    res_geom <- st_extend_line(
      x,  
      head_map$get("distance") + extra_head, 
      head_map$get("direction")
    )

    # then use the head extended line from above and extend the tail
    res_geom <- st_extend_line(
      res_geom,  
      tail_map$get("distance") + extra_tail, 
      tail_map$get("direction")
    )

    # mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(x, color = "red") +
    #   mapview::mapview(geoms_to_cut, color = "green") +
    #   mapview::mapview(res_geom, color = "red") +res_geom2
    
    return(res_geom)
    
  } else {
    message("---------------------------------")
    message("!!!!! SKIPPING BECAUSE EDGE OF MULTIBRAID !!!!!")
    message("---> count_intersect[1]: ", count_intersects[1] , " ---")
    message("---> count_intersect[2]: ", count_intersects[2] , " ---")
    message("---------------------------------")
    
    return(NULL)
    
  }
  
}

# Apply flowline braid length threshold to braided network dataset 
# Return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset
# x: braided flowlines
# originals: not braided flowlines from the same network
# threshold: braid_threshold numeric value to remove braids with a total braid flowline length greater than 'threshold'
braid_thresholder2 <- function(x, 
                              originals,
                              threshold = NULL, 
                              verbose   = TRUE
) {
  
  # x         = braids
  # originals = not_braids
  # threshold = 30000
  # verbose = TRUE
  
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
  
  # unpack nested braid_id column0
  unpacked <- unnpack_braids(x)
  
  # calculate total length of all the linestrings in each braid_id
  unpacked <- 
    unpacked %>% 
    dplyr::group_by(braid_id) %>%
    dplyr::mutate(
      braid_length = as.numeric(
        sum(sf::st_length(geometry), na.rm = T))
    ) %>%
    dplyr::ungroup()
  # dplyr::mutate(braid_length = sf::st_length(geometry)) %>%
  # dplyr::mutate(braid_length = as.numeric(sum(braid_length, na.rm = T))) %>%
  
  # check to make sure some braids are over threshold and can be removed, if NOT then just return original data
  if(all(unpacked$braid_length <= threshold)) {
    
    message("Removing: 0 braids from braided dataset\n", 
            "Keeping: All braids as all braids have total flowline lengths less than or equal to threshold value: ", threshold)
    
    return(list(
      braids     = x,
      not_braids = originals)
    )
    
  } 
  
  # # table of TRUEs and FALSE for braids to keep/remove given 'threshold'
  # threshold_tbl <- table(unpacked$braid_length <= threshold)
  # if(verbose) { message("Removing: ",  threshold_tbl["FALSE"],  
  # " braids from braided dataset\nKeeping: ", threshold_tbl["TRUE"],
  #           " braids that have total flowline lengths less than or equal to threshold value: ", threshold)}
  
  # comids to keep (total length of braid linestrings is less than or equal to braid_threshold value)
  to_keep <- dplyr::filter(unpacked, braid_length <= threshold)$comid
  
  # # COMIDs that are too large, add them back to the "not_braids" data
  # to_drop <- dplyr::filter(x, !comid %in% to_keep)
  
  # keep track of keeping and removing count
  orig_nrows <- nrow(originals)
  x_nrows  <- nrow(x)
  
  # add the "too big braid COMIDs" back to original "not_braids" data 
  # and set these comids braid_ids to "no_braid" and is_multibraid = FALSE
  originals <- dplyr::bind_rows(
    originals,
    dplyr::select(
      dplyr::mutate(
        dplyr::filter(
          x, !comid %in% to_keep
        ),
        braid_id      = "no_braid", 
        is_multibraid = FALSE
      ),
      -has_mainstem
    )
  )
  
  new_orig_nrows <- nrow(originals)
  
  # filter out braid_ids/COMIDs that are too big
  x <- dplyr::filter(x, comid %in% to_keep)
  
  # updating count of keeping and removing 
  new_orig_nrows <- nrow(originals)
  new_x_nrows <- nrow(x)
  
  if(verbose) {
    message("Removing: ", new_orig_nrows - orig_nrows, 
            " braids from braided dataset\nKeeping: ",   new_x_nrows,
            " braids that have total flowline lengths less than or equal to threshold value: ", threshold)
  }
  
  return(list(
    braids     = x,
    not_braids = originals)
  )
  
}
#' Find the middle flowlines in a braided system
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects_mid <- function(
    net, 
    transect_lines,
    braid_threshold = NULL
) {
  
  # transect_lines <-  transects_nofix
  # net <- net3
  # braid_threshold = NULL
  # braid_threshold = 25000
  # braid_threshold = NULL
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  if (all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <-
  braids <-  
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    # dplyr::group_by(comid) %>% 
    # dplyr::mutate(ncomid = n()) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # view data on map
  # mapview::mapview(not_braids, color = "dodgerblue") +
  # mapview::mapview(only_braids, color = "red") 
  # braid_threshold = NULL
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x         = braids, 
      originals = not_braids, 
      threshold = braid_threshold,
      verbose   = TRUE
    )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
  }
  
  # # unique braid_ids/COMIDs
  # ubraids <- unique(only_braids$braid_id)
  # ucoms <- unique(only_braids$comid)
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) %>% 
    # dplyr::filter(divergence == 0)
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # keep track of all original crossections
  all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
  
  # # there are sometimes braids that don't have any divergence == 0 transects, 
  # # so we need to use whatever transect is avalaible with the lowest 'divergence' value 
  # # (hopefully its divergence == 1, but sometimes its divergence == 2)
  # div_xs <- 
  #   xs %>% 
  #   dplyr::filter(
  #     !has_mainstem
  #     # hy_id %in% extra_comids,
  #   ) %>% 
  #   dplyr::group_by(braid_id) %>% 
  #   dplyr::slice_min(divergence, with_ties = FALSE) %>% 
  #   dplyr::ungroup()
  # 
  # # from original cross sections, only keep divergence == 0 and remove the comids that are in "div_xs"
  # # filter to just divergence == 0 and comids NOT in 'div_xs'
  # xs <- dplyr::filter(xs, 
  #                     divergence == 0, 
  #                     !hy_id %in% div_xs$hy_id
  # )
  
  # flag determining whether transect should be replaced
  xs$changed <- FALSE
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    
    message("===== NO 'xs' transect lines =====")
    message("===== returning original data =====")
    
    return(transect_lines)
    
  } else {
    message("===== ", nrow(xs) , " 'xs' transect lines =====")
  }
  
  # determine if a cross section is a middle, outer, single or no intersection 
  xs <- classify_transects(x = braids, transects = xs)
  # xs$braid_class %>% unique()
  
  # NON middle transects
  other_xs <- xs %>% dplyr::filter(braid_class == "single")
  # other_xs <- xs %>% dplyr::filter(braid_class != "middle")
  
  # keep only middle transect
  xs <- xs %>% dplyr::filter(braid_class == "middle")
  
  # order from largest to smallest totdasqkm
  xs <-  dplyr::arrange(xs, -totdasqkm)
  
  # mapview::mapview(xs, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") +
  #   mapview::mapview(not_braids, color = "gold") +
  #   mapview::mapview(other_xs, color = "red") +
  #   mapview::mapview(mid_xs, color = "green") 
  
  for(i in 1:nrow(xs)) {
    
    # message("i: ", i, "/", nrow(xs))
    
    # transect line
    tline <- xs[i, ]$geometry
    
    # comid of transect line
    com <- xs[i, ]$hy_id
    
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    
    # get all linestrings that are apart of the braid_ids of interest
    bids_check <- sapply(1:length(braids$braid_id), function(x) {
      any(
        strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
      )
    })
    
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      braids,
      braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                             unlist(strsplit(braids[bids_check, ]$braid_id, ", "))
      )
      ),
      # braid_id %in% Reduce(c, strsplit(braids[bids_check, ]$braid_id, ", ")),
      comid != com
    )
    
    # resulting geometry after extension
    res_geom <- augment_transect(
      x            = tline,
      id           = com,
      geoms_to_cut = others,
      cs_width     = xs[i, ]$cs_widths,
      bf_width     = xs[i, ]$bf_width
    )
    
    # if augment_transect returns a NULL geometry, was NOT updated, 
    # so we can skip this iteration because no matter how far you extend out this transect, 
    # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
    if(is.null(res_geom)) {
      # message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      # message("=================")
      next
    }
    
    # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
    if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
      
      # # # message stating that replacement was made
      # message("----> REPLACING ", i, " transect")
      
      # updatem geometry with new, extended cross section
      xs[i,]$geometry <- sf::st_geometry(res_geom)
      
      # flag determining whether transect should be replaced
      xs[i, ]$changed <- TRUE
      
    }
    # message("=================")
  }
  
  # # keep track of cross sections to drop
  # xs_drop <- dplyr::filter(xs,!changed)

  # keep only the transects that were changed/extended
  xs <- dplyr::filter(xs, changed)
  # xs_d <- dplyr::filter(xs, changed)
  
  # mapview::mapview(xs, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") +
  #   mapview::mapview(not_braids, color = "gold") +
  #   mapview::mapview(other_xs, color = "red") +
  #   mapview::mapview(xs_d, color = "red") +     mapview::mapview(xs2, color = "red")

  
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- sf::st_intersects(not_braids, xs)
  # # other_intersects <- sf::st_intersects(other_xs, xs)
  
  # any(lengths(net_intersects) > 0) | any(lengths(other_intersects) > 0)
  
  # if there ARE some intersections, remove those intersecting lines from 'xs'
  if(any(lengths(net_intersects) > 0)) {
  # if(any(lengths(net_intersects) > 0) | any(lengths(other_intersects) > 0)) {
    message("Removing ", length(unlist(net_intersects)) + length(unlist(other_intersects)), " transect lines from 'xs'")
    drops <- unlist(net_intersects)
    # drops <- unique(c(unlist(net_intersects), unlist(other_intersects)))
    
    # drop div_xs transects that are overlapping with 'xs' transects
    xs <- xs[-drops, ]
    
  }
  
  # nrow(braids)
  # nrow(not_braids)
  # nrow(braids) + nrow(not_braids)
  # nrow(braids) + nrow(not_braids) == nrow(net)
  # tmpy <- xs2[lengths(sf::st_intersects(xs2, not_braids)) > 0, ]
  # mapview::mapview(xs2, color = "green") +
  #   mapview::mapview(tmpy, color = "gold") +
  #   mapview::mapview(not_braids, color = "dodgerblue") + 
  #   mapview::mapview(braids, color = "red") +
  # mapview::mapview(xs, color = "green")
  
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  other_intersects <- sf::st_intersects(xs, other_xs)
  
  # if there ARE some intersections, remove those intersecting lines from 'div_xs'
  if(any(lengths(other_intersects) > 0)) {
    message("Removing ", length(unlist(other_intersects)), " transect lines from 'other_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[-unlist(other_intersects), ]
  }
  
  # flag determining whether transect should be replaced
  other_xs$changed <- FALSE
  
  if (nrow(other_xs) > 0) {
    
    message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
    
    for (i in 1:nrow(other_xs)) {
      
      # message("i: ", i, "/", nrow(other_xs))
      
      # transect line
      tline <- other_xs[i, ]$geometry
      
      # comid of transect line
      com <- other_xs[i, ]$hy_id
      
      # # check if geom intersects 
      # if(any(lengths(sf::st_intersects(tline, xs)) > 0)) {
      #   message("!!!!! SKIPPING, other_xs[i, ] ALREADY INTERSECTS WITH 'xs' !!!!! ")
      #   message("=================")
      #   next
      # }
      
      # braid IDs of interest
      bids <- strsplit(other_xs[i, ]$braid_id, ", ")[[1]]
      
      # get all linestrings that are apart of the braid_ids of interest
      bids_check <- sapply(1:length(braids$braid_id), function(x) {
        any(
          strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
        )
      })
      
      
      # braid flowlines other than self that are within our given braid id or are nearby (the unique() filtering part)
      others <- dplyr::filter(
        braids,
        braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                               unlist(strsplit(braids[bids_check, ]$braid_id, ", ")))),
        comid != com
      )
      
      # resulting geometry after extension
      res_geom <- augment_transect(
        x            = tline,
        id           = com,
        geoms_to_cut = others,
        cs_width     = other_xs[i, ]$cs_widths,
        bf_width     = other_xs[i, ]$bf_width
      )
      
      # if augment_transect returns a NULL geometry, was NOT updated, 
      # so we can skip this iteration because no matter how far you extend out this transect, 
      # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
      if(is.null(res_geom)) {
        # message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
        # message("--- CONTINUING TO NEXT TRANSECT ---")
        # message("=================")
        next
      }
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
      # AND it does NOT intersect with any other transects in 'xs' (the rest of the main transects lines)
      if(
        !any(lengths(sf::st_intersects(res_geom, other_xs[-i,])) > 0) & 
        !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
      ) {
        
        # # # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # replace geometry with extended line
        other_xs[i,]$geometry <- sf::st_geometry(res_geom)
        
        # flag determining whether transect should be replaced
        other_xs[i, ]$changed <- TRUE
        
      }
      # message("=================")
    }
    
    # # keep only the transects that were changed/extended
    # div_drop <- dplyr::filter(other_xs, !changed)
    
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -braid_id, -is_multibraid, -has_mainstem, -changed),
      dplyr::select(other_xs,
                    -braid_id, -is_multibraid, -has_mainstem, -changed)
    )
    
    # mapview::mapview(other_xs, color = "green") +
    #   # mapview::mapview(tmpy, color = "gold") +
    #   mapview::mapview(not_braids, color = "gold") +
    #   mapview::mapview(braids, color = "dodgerblue") +
    # mapview::mapview(other_xs2, color = "green") +
    #   mapview::mapview(out, color = "red")
    
  } else {
    
    message("===== NO 'other_xs' transect lines =====")
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, -braid_id, -is_multibraid, -has_mainstem, -changed)
    
  }
  
  
  return(out)
  # mapview::mapview(other_xs, color = "green") +
  #   # mapview::mapview(tmpy, color = "gold") +
  #   mapview::mapview(not_braids, color = "gold") +
  #   mapview::mapview(braids, color = "dodgerblue") +
  #   mapview::mapview(xs, color = "green") +
  #   mapview::mapview(out, color = "red")
  
}
classify_transects2 <- function(x, transects) {
  x = braids
  transects = xs

  # i = 1
  
  # make groups for each braided section
  braid_groups <- lapply(1:nrow(transects), function(i) {
    
    # message("i: ", i, "/", nrow(transects))
    
    # braid IDs of interest
    bids <- strsplit(transects[i, ]$braid_id, ", ")[[1]]
    
    # get all linestrings that are apart of the braid_ids of interest
    bids_check <- sapply(1:length(x$braid_id), function(y) {
      any(
        strsplit(x$braid_id[y], ", ")[[1]] %in% bids
      )
    })
    
    sort(unique(c(x[bids_check, ]$braid_id,
                  unlist(strsplit(x[bids_check, ]$braid_id, ", "))
    )))
    
  }) %>% 
    unique()
  out <- lapply(1:length(braid_groups), function(k) {
    # k = 8
    
    # group of cross sections
    xs_group <- dplyr::filter(transects, braid_id %in% braid_groups[[k]])
    message("nrow: ", nrow(xs_group))
    nrow(xs_group)
  })
  sum(unlist(out))
  # for(i in 1:length(braid_groups)) {
  # for(i in 1:length(braid_groups)) {
  out <- lapply(1:length(braid_groups), function(k) {
    # k = 8
    
    # group of cross sections
    xs_group <- dplyr::filter(transects, braid_id %in% braid_groups[[k]])
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      x,
      braid_id %in% braid_groups[[k]]
      # braid_id %in% unique(c(x[bids_check, ]$braid_id,
      #                        unlist(strsplit(x[bids_check, ]$braid_id, ", ")))),
      # comid != com
    )
    
    # max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- max(xs_group$cs_widths * 3)
    # max_dist <- max(xs_group$cs_widths * 10)
    
    # mapview::mapview(transects, color = "green") +
    #   mapview::mapview(x, color = "red") +
    #   mapview::mapview(not_braids, color = "dodgerblue") +    
    #   mapview::mapview(xs_group, color = "green")  + others
    
    xs_group$tmp_id <- NA
    xs_group$braid_class <- NA
    xs_group$priority <- NA
    
    xs_group$head_count <-NA
    xs_group$tail_count <- NA
    
    xs_group$head_dist <- NA
    xs_group$tail_dist <- NA
    
    xs_group$head_geom <- NA
    xs_group$tail_geom <- NA
    
    for (z in 1:nrow(xs_group)) {
      # z = 1
      message(z, "/", nrow(xs_group))
      
      com <- xs_group$hy_id[z]
      tline <- xs_group$geometry[z]
      bf_width <- xs_group$bf_width[z]
      cs_width <- xs_group$cs_widths[z]
      
      # others except self
      geoms_to_cut <- dplyr::filter(others, comid != com)
      
      # if 'x' intersects with flowlines from the start, return NULL immediately
      if(lengths(sf::st_intersects(tline, geoms_to_cut)) != 0) {
        message("---------------------------------")
        message("!!!!! SKIPPING initial transect already intersects a flowline !!!!!")
        message("---> intersection count: ", lengths(sf::st_intersects(tline, geoms_to_cut)) , " ---")
        message("---------------------------------")
        # return(NULL)
        next
      }
      
      # # max distance from transect of interest and rest of braid flowlines 
      # # TODO (need a better method of determing max possible extension of flowline)
      # max_dist <- as.numeric(
      #   max(
      #     sf::st_distance(  
      #       geoms_to_cut, 
      #       x
      #     )
      #   )
      # )
      
      # sequence from 0 to the max possible extension distance 
      dist_vect <- seq(0, max(c(max_dist, 2000)), bf_width/2)
      # dist_vect <- seq(0, max(c(max_dist, 2000)), multi_transects[i, ]$bf_width)
      
      # EXTEND OUT lines 
      # extend transect line out in both directions and find the side that interests with m
      # extend line out from HEAD side of line 
      # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
      head_map <- extend_out2(
        x             = 1,
        line          = tline, 
        distances     = dist_vect,
        geoms_to_cut  = geoms_to_cut, 
        ids           = c(com), 
        dir           = "head",
        map           = TRUE
      )
      
      # extend line out from TAIL side of line 
      # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
      tail_map <- extend_out2(
        x             = 1,
        line          = tline, 
        distances     = dist_vect,
        geoms_to_cut  = geoms_to_cut, 
        ids           = c(com), 
        dir           = "tail",
        map           = TRUE
      )
      
      # extract the linestring shapes
      tail_ext <- tail_map$get("line")
      head_ext <- head_map$get("line")

      # head_ext
      # message(z, "/", nrow(xs_group))
      # extend head out
      # head_ext <-
      #   st_extend_line(xs_group$geometry[z], max_dist, "head") %>% 
      #   sf::st_as_sf() %>% 
      #   dplyr::mutate(hy_id = xs_group$hy_id[z]) %>% 
      #   dplyr::rename(geometry = x)
      # 
      # # extend tail out
      # tail_ext <-
      #   st_extend_line(xs_group$geometry[z], max_dist, "tail") %>% 
      #   sf::st_as_sf() %>% 
      #   dplyr::mutate(hy_id = xs_group$hy_id[z]) %>% 
      #   dplyr::rename(geometry = x)
      
      # count interesections
      count_intersects <- c(
        lengths(sf::st_intersects(head_ext,  geoms_to_cut)),
        lengths(sf::st_intersects(tail_ext, geoms_to_cut))
        # lengths(sf::st_intersects(head_ext$geometry,  
        #                           dplyr::filter(others, comid != head_ext$hy_id) )),
        # lengths(sf::st_intersects(tail_ext$geometry, 
        #                           dplyr::filter(others, comid != tail_ext$hy_id)))
      )
      
      
      
      # count_intersects = c(0, 0)
      # count_intersects = c(0, 5 )
      # any((any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1))
      # zero_count["0"]
      type <- NULL
      
      if(all(count_intersects == 0)) {
        
        type = "no_intersects"
        priority = 3
      } else {
        
        
        # if intersection one of the extended lines does not intersect (0) 
        # AND
        # the other other extended line intersects EXACTLY 1 other flowline,
        # then this is a single braid that we will extend across
        # if(any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1) {
        if(any((any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1))) {
          
          type = ifelse(which(count_intersects != 0) == 1, "head", "tail")
          # type = "single"
          priority = 2
          # if the NON ZERO intersection extended line, intersects EXACTLY 1 other flowline, then do single extension out
          # if(count_intersects[which(count_intersects != 0)] == 1) {
          #   
          #   type = "single"
          # }
          
        } else if (count_intersects[1] == count_intersects[2] | max(count_intersects) - 1 == min(count_intersects)) {
          
          type = "middle"
          priority = 1
          
        } else {
          
          type = "outer"
          priority = 3
        }
        
      }
      
      if(is.null(type)) {
        type = "outer"
        priority = 3
      }
      # xs_group[z, ] <- 
      #   xs_group[z, ] %>% 
      #   dplyr::mutate(
      #     tmp_id = paste0(xs_group$hy_id[z], "_", xs_group$cs_id[z]),
      #     braid_class = type,
      #     head_count = count_intersects[1],
      #     tail_count = count_intersects[2],
      #     head_geom = sf::st_geometry(head_ext),
      #     tail_geom = sf::st_geometry(tail_ext)
      #   )
      xs_group[z, ]$tmp_id      <- paste0(xs_group$hy_id[z], "_", xs_group$cs_id[z])
      xs_group[z, ]$braid_class <- type
      xs_group[z, ]$priority    <- priority
      xs_group[z, ]$head_count  <- count_intersects[1]
      xs_group[z, ]$tail_count  <- count_intersects[2]
      xs_group[z, ]$head_dist   <- head_map$get("distance")
      xs_group[z, ]$tail_dist   <- tail_map$get("distance")
      xs_group[z, ]$head_geom   <- sf::st_geometry(head_ext)
      xs_group[z, ]$tail_geom   <- sf::st_geometry(tail_ext)

      # stash_df <- data.frame(
      #                 hy_id = xs_group$hy_id[z],
      #                 cs_id = xs_group$cs_id[z],
      #                 tmp_id = paste0(xs_group$hy_id[z], "_", xs_group$cs_id[z]),
      #                 braid_class = type,
      #                 head_count = count_intersects[1],
      #                 tail_count = count_intersects[2],
      #                 head_geom = sf::st_geometry(head_ext),
      #                 tail_geom = sf::st_geometry(tail_ext)
      #               ) %>% 
      #   sf::st_as_sf() 
      # stash_df
    }
    
    
    xs_group <- 
      xs_group %>% 
      dplyr::group_by(priority)  %>% 
      dplyr::arrange(priority, -totdasqkm, .by_group = TRUE) %>% 
      dplyr::ungroup()
    
    ### ---- PUT FIX EXTENSIONS 2 FUNCTION IN A LOOP HERE -----
    
    for(i in 1:nrow(xs_group)) {
      # i = 1
      message(i, "/", nrow(xs_group))
          # xline <- xs_group[i, ]$geometry
          # 
          # head <- xs_group[i, ]$head_geom
          # tail <- xs_group[i, ]$tail_geom
          # tail
          # 
          # head_extension_dist <- xs_group[i, ]$head_dist
          # tail_extension_dist <- xs_group[i, ]$tail_dist
          # 
          # geoms_to_cut <- others
          # cs_width <- xs_group[i, ]$cs_widths
          # bf_width <- xs_group[i, ]$bf_width
          # braid_class <- xs_group[i, ]$braid_class
          
      res_geom <- fix_extensions(xline = xs_group[i, ]$geometry,
                                   head = xs_group[i, ]$head_geom, 
                                   tail = xs_group[i, ]$tail_geom, 
                                   braid_class = xs_group[i, ]$braid_class,
                                   geoms_to_cut = others, 
                                   head_extension_dist = xs_group[i, ]$head_dist,
                                   tail_extension_dist = xs_group[i, ]$tail_dist, 
                                   cs_width = xs_group[i, ]$cs_widths,
                                   bf_width = xs_group[i, ]$bf_width
                                   ) 
      if(is.null(res_geom)) {
        # message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
        # message("--- CONTINUING TO NEXT TRANSECT ---")
        # message("=================")
        next
      }
      # dplyr::filter(xs_group[-i,], braid_class == "middle")
      # lengths(sf::st_intersects(res_geom,     
      #                           dplyr::filter(xs_group[-i,], braid_class == "middle"))
      #         )
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
      if(!any(lengths(sf::st_intersects(res_geom, xs_group[-i,])) > 0)){
      # if(!any(lengths(sf::st_intersects(res_geom, dplyr::filter(xs_group[-i,], braid_class == "middle"))) > 0)){
        # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # updatem geometry with new, extended cross section
        xs_group[i,]$geometry <- sf::st_geometry(res_geom)
        
        # flag determining whether transect should be replaced
        xs_group[i, ]$changed <- TRUE
        
      }
      
      
    }
    xs_group
    
    # head_df <- xs_group %>% dplyr::select(-geometry, -tail_geom)
    # mapview::mapview(others) +
    # mapview::mapview(xs_group$geometry, color = "dodgerblue")+
    #   mapview::mapview(xs_group$head_geom, color = "red") + 
    # mapview::mapview(xs_group$tail_geom, color = "green")
    # plot(xs_group$geometry)
    # }) %>% 
    #   dplyr::bind_rows()
    # extenders <- lapply(1:nrow(xs_group), function(z) {
    #   # z = 1
    #   
    #   com <- xs_group$hy_id[z]
    #   xline <- xs_group$geometry[z]
    #   bf_width <- xs_group$bf_width[z]
    #   cs_width <- xs_group$cs_widths[z]
    #   
    #   # others except self
    #   geoms_to_cut <- dplyr::filter(others, comid != com)
    #   
    #   # if 'x' intersects with flowlines from the start, return NULL immediately
    #   if(lengths(sf::st_intersects(xline, geoms_to_cut)) != 0) {
    #     message("---------------------------------")
    #     message("!!!!! SKIPPING initial transect already intersects a flowline !!!!!")
    #     message("---> intersection count: ", lengths(sf::st_intersects(x, geoms_to_cut)) , " ---")
    #     message("---------------------------------")
    #     return(NULL)
    #   }
      
      # # max distance from transect of interest and rest of braid flowlines 
      # # TODO (need a better method of determing max possible extension of flowline)
      # max_dist <- as.numeric(
      #   max(
      #     sf::st_distance(  
      #       geoms_to_cut, 
      #       x
      #     )
      #   )
      # )
      
      # # sequence from 0 to the max possible extension distance 
      # dist_vect <- seq(0, max(c(max_dist, 2000)), bf_width/2)
      # # dist_vect <- seq(0, max(c(max_dist, 2000)), multi_transects[i, ]$bf_width)
      # 
      # # EXTEND OUT lines 
      # # extend transect line out in both directions and find the side that interests with m
      # # extend line out from HEAD side of line 
      # # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
      # head_map <- extend_out2(
      #   x             = 1,
      #   line          = xline, 
      #   distances     = dist_vect,
      #   geoms_to_cut  = geoms_to_cut, 
      #   ids           = c(com), 
      #   dir           = "head",
      #   map           = TRUE
      # )
      # 
      # # extend line out from TAIL side of line 
      # # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
      # tail_map <- extend_out2(
      #   x             = 1,
      #   line          = xline, 
      #   distances     = dist_vect,
      #   geoms_to_cut  = geoms_to_cut, 
      #   ids           = c(com), 
      #   dir           = "tail",
      #   map           = TRUE
      # )
      # 
      # # extract the linestring shapes
      # tail_ext <- tail_map$get("line")
      # head_ext <- head_map$get("line")
      # # head_ext
      # # message(z, "/", nrow(xs_group))
      # # extend head out
      # # head_ext <-
      # #   st_extend_line(xs_group$geometry[z], max_dist, "head") %>% 
      # #   sf::st_as_sf() %>% 
      # #   dplyr::mutate(hy_id = xs_group$hy_id[z]) %>% 
      # #   dplyr::rename(geometry = x)
      # # 
      # # # extend tail out
      # # tail_ext <-
      # #   st_extend_line(xs_group$geometry[z], max_dist, "tail") %>% 
      # #   sf::st_as_sf() %>% 
      # #   dplyr::mutate(hy_id = xs_group$hy_id[z]) %>% 
      # #   dplyr::rename(geometry = x)
      # 
      # # count interesections
      # count_intersects <- c(
      #   lengths(sf::st_intersects(head_ext,  geoms_to_cut)),
      #   lengths(sf::st_intersects(tail_ext, geoms_to_cut))
      #   # lengths(sf::st_intersects(head_ext$geometry,  
      #   #                           dplyr::filter(others, comid != head_ext$hy_id) )),
      #   # lengths(sf::st_intersects(tail_ext$geometry, 
      #   #                           dplyr::filter(others, comid != tail_ext$hy_id)))
      # )
      # 
      # 
      # # count_intersects = c(0, 0)
      # # count_intersects = c(0, 5 )
      # # any((any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1))
      # # zero_count["0"]
      # 
      # if(all(count_intersects == 0)) {
      #   
      #   type = "no_intersects"
      #   
      # } else {
      #   
      #   
      #   # if intersection one of the extended lines does not intersect (0) 
      #   # AND
      #   # the other other extended line intersects EXACTLY 1 other flowline,
      #   # then this is a single braid that we will extend across
      #   # if(any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1) {
      #   if(any((any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1))) {
      #     type = "single"
      #   
      #     # if the NON ZERO intersection extended line, intersects EXACTLY 1 other flowline, then do single extension out
      #     # if(count_intersects[which(count_intersects != 0)] == 1) {
      #     #   
      #     #   type = "single"
      #     # }
      #     
      #   } else if (count_intersects[1] == count_intersects[2] | max(count_intersects) - 1 == min(count_intersects)) {
      #     
      #     type = "middle"
      #     
      #   } else {
      #     
      #     type = "outer"
      #     
      #   }
      #   
      # }
      # 
      # xs_group[z, ] <- 
      #   xs_group[z, ] %>% 
      #   dplyr::mutate(
      #     tmp_id = paste0(xs_group$hy_id[z], "_", xs_group$cs_id[z]),
      #     braid_class = type,
      #     head_count = count_intersects[1],
      #     tail_count = count_intersects[2],
      #     head_geom = sf::st_geometry(head_ext),
      #     tail_geom = sf::st_geometry(tail_ext)
      #   )
      # 
      # # stash_df <- data.frame(
      # #                 hy_id = xs_group$hy_id[z],
      # #                 cs_id = xs_group$cs_id[z],
      # #                 tmp_id = paste0(xs_group$hy_id[z], "_", xs_group$cs_id[z]),
      # #                 braid_class = type,
      # #                 head_count = count_intersects[1],
      # #                 tail_count = count_intersects[2],
      # #                 head_geom = sf::st_geometry(head_ext),
      # #                 tail_geom = sf::st_geometry(tail_ext)
      # #               ) %>% 
      # #   sf::st_as_sf() 
      #   # dplyr::rename(head_geom = geometry,
      #   #               tail_geom = geometry.1) 
      # # stash_df$geometry.1
      # # list(
      # #   hy_id = xs_group$hy_id[z],
      # #   cs_id = xs_group$cs_id[z],
      # #   type = type,
      # #   intersection_counts = count_intersects
      # #   )
      # # stash_df
      
    }) %>% 
      dplyr::bind_rows()
    
    
    mapview::mapview(out) + braids
  # }) %>% 
  #   dplyr::bind_rows()
  # })
  # out %>% 
  #   dplyr::group_by(tmp_id) %>% 
  #   dplyr::slice(1)
  
  # remove duplicate tmp_id rows
  out <- out[!duplicated(out$tmp_id), ] 
  
  # join transects data with the braid classification data and return this
  out <- dplyr::select(
    dplyr::left_join(
      dplyr::mutate(
        transects,
        tmp_id = paste0(hy_id, "_", cs_id)
      ),
      dplyr::select(out, tmp_id, braid_class, head_count, tail_count),
      by = "tmp_id"
    ),
    -tmp_id
  )
  
  return(out)
  
}
classify_transects <- function(x, transects) {
  # x = braids
  # transects = xs
  # # 
  # i = 1
  
  # make groups for each braided section
  braid_groups <- lapply(1:nrow(transects), function(i) {
    
    # message("i: ", i, "/", nrow(transects))
    
    # braid IDs of interest
    bids <- strsplit(transects[i, ]$braid_id, ", ")[[1]]
    
    # get all linestrings that are apart of the braid_ids of interest
    bids_check <- sapply(1:length(x$braid_id), function(y) {
      any(
        strsplit(x$braid_id[y], ", ")[[1]] %in% bids
      )
    })
    
    sort(unique(c(x[bids_check, ]$braid_id,
                  unlist(strsplit(x[bids_check, ]$braid_id, ", "))
    )))
    
  }) %>% 
    unique()
  
  # for(i in 1:length(braid_groups)) {
  # for(i in 1:length(braid_groups)) {
  out <- lapply(1:length(braid_groups), function(k) {
    k = 5
    # group of cross sections
    xs_group <- dplyr::filter(transects, braid_id %in% braid_groups[[k]])
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      x,
      braid_id %in% braid_groups[[k]]
      # braid_id %in% unique(c(x[bids_check, ]$braid_id,
      #                        unlist(strsplit(x[bids_check, ]$braid_id, ", ")))),
      # comid != com
    )
    
    # max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- max(xs_group$cs_widths * 2.5)
    # max_dist <- max(xs_group$cs_widths * 10)
    
    # mapview::mapview(transects, color = "green") +
    #   mapview::mapview(x, color = "red") +
    #   mapview::mapview(not_braids, color = "dodgerblue") +    
    #   mapview::mapview(xs_group, color = "green")  + others
    
    extenders <- lapply(1:nrow(xs_group), function(z) {
      # z = 1
      
      # xs_group$geometry[z]
      # message(z, "/", nrow(xs_group))
      # extend head out
      head_ext <-
        st_extend_line(xs_group$geometry[z], max_dist, "head") %>% 
        sf::st_as_sf() %>% 
        dplyr::mutate(hy_id = xs_group$hy_id[z]) %>% 
        dplyr::rename(geometry = x)
      
      # extend tail out
      tail_ext <-
        st_extend_line(xs_group$geometry[z], max_dist, "tail") %>% 
        sf::st_as_sf() %>% 
        dplyr::mutate(hy_id = xs_group$hy_id[z]) %>% 
        dplyr::rename(geometry = x)
      
      # count interesections
      count_intersects <- c(
        lengths(sf::st_intersects(head_ext$geometry,  
                                  dplyr::filter(others, comid != head_ext$hy_id) )),
        lengths(sf::st_intersects(tail_ext$geometry, 
                                  dplyr::filter(others, comid != tail_ext$hy_id)))
      )
      
      if(all(count_intersects == 0)) {
        
        type = "no_intersects"
        
      } else {
        
        
        # if intersection one of the extended lines does not intersect (0) 
        # AND
        # the other other extended line intersects EXACTLY 1 other flowline,
        # then this is a single braid that we will extend across
        if(any(count_intersects == 0)) {
          
          # if the NON ZERO intersection extended line, intersects EXACTLY 1 other flowline, then do single extension out
          if(count_intersects[which(count_intersects != 0)] == 1) {
            
            type = "single"
          }
          
        } else if (count_intersects[1] == count_intersects[2] | max(count_intersects) - 1 == min(count_intersects)) {
          
          type = "middle"
          
        } else {
          
          type = "outer"
          
        }
        
      }
      
      data.frame(
        hy_id = xs_group$hy_id[z],
        cs_id = xs_group$cs_id[z],
        tmp_id = paste0(xs_group$hy_id[z], "_", xs_group$cs_id[z]),
        braid_class = type,
        head_count = count_intersects[1],
        tail_count = count_intersects[2]
      )
      # list(
      #   hy_id = xs_group$hy_id[z],
      #   cs_id = xs_group$cs_id[z],
      #   type = type,
      #   intersection_counts = count_intersects
      #   )
      
      
    }) %>% 
      dplyr::bind_rows()
    
    extenders
    
  }) %>% 
    dplyr::bind_rows()
  # })
  # out %>% 
  #   dplyr::group_by(tmp_id) %>% 
  #   dplyr::slice(1)
  
  # remove duplicate tmp_id rows
  out <- out[!duplicated(out$tmp_id), ] 
  
  # join transects data with the braid classification data and return this
  out <- dplyr::select(
    dplyr::left_join(
      dplyr::mutate(
        transects,
        tmp_id = paste0(hy_id, "_", cs_id)
      ),
      dplyr::select(out, tmp_id, braid_class, head_count, tail_count),
      by = "tmp_id"
    ),
    -tmp_id
  )
  
  return(out)
  
}
# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
select_middle <- function(x, id, geoms_to_cut, cs_width, bf_width) {
  
  # x            = tline
  # id           = com
  # geoms_to_cut = others
  # cs_width     = xs[i, ]$cs_widths
  # bf_width     = xs[i, ]$bf_width
  # mapview::mapview(others, color = "dodgerblue") +
  #   mapview::mapview(tline, color = "green") +
  #   mapview::mapview(braids, color = "red")
  
  # lengths(sf::st_intersects(geoms_to_cut, x))
  
  # if 'x' intersects with flowlines from the start, return NULL immediately
  if(lengths(sf::st_intersects(x, geoms_to_cut)) != 0) {
    message("---------------------------------")
    message("!!!!! SKIPPING initial transect already intersects a flowline !!!!!")
    message("---> intersection count: ", lengths(sf::st_intersects(x, geoms_to_cut)) , " ---")
    message("---------------------------------")
    return(NULL)
  }
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  max_dist <- as.numeric(
    max(
      sf::st_distance(  
        geoms_to_cut, 
        x
      )
    )
  )
  
  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_dist, 2000)), bf_width/2)
  # dist_vect <- seq(0, max(c(max_dist, 2000)), multi_transects[i, ]$bf_width)
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
  head_map <- extend_out2(
    x             = 1,
    line          = x, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "head",
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
  tail_map <- extend_out2(
    x             = 1,
    line          = x, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "tail",
    map           = TRUE
  )
  
  # extract the linestring shapes
  tail_ext <- tail_map$get("line")
  head_ext <- head_map$get("line")
  
  # mapview::mapview(others, color = "dodgerblue") +
  #   mapview::mapview(tline, color = "green") +
  #   mapview::mapview(braids, color = "red") +
  #   mapview::mapview(tail_ext, color = "green") +
  #   mapview::mapview(head_ext, color = "red")
  
  # TODO CHECK which extended line should be selected when number of interesections is the same
  # IF: number of. intersection for each extended line is TIED, select the shorter of the two? NOT SURE WHAT THE CALL IS HERE
  # ELSE: return the one with more interesections (which.max)
  count_intersects <- c(
    lengths(sf::st_intersects(head_ext, geoms_to_cut)), 
    lengths(sf::st_intersects(tail_ext, geoms_to_cut))
  )
  # In fixbraidstransect:
  #   Instead of doing the Xs and DIcvxs split, instead keep all lines (0, 1, 2 divergences)
  # 
  
  # Augmentline function:
  #   If both == 0:
  #   Return null 
  # 
  # If any == 0 and intersect[which(x, y) != 0] == 1:
  #   
  #   # Do single direction extension
  #   
  # If x == y or max(x, y) -1 == min(x, y): 
  #   # We found a middle, do double sided extension
  #   
  #   Else: 
  #   Return null 
  # make a list of head and tail map extensions
  map_lst <- list(head_map, tail_map)
  
  # mapview::mapview(head_ext) + tail_ext + others + tline
  
  # create simple feature collection of head and tail extended lines
  # res_geom <- sf::st_sfc(c(head_ext, tail_ext))
  
  # if neither direction had any intersections, skip this iteration:
  if(all(count_intersects == 0)) {
    message("---------------------------------")
    message("--- NO INTERSECT AFTER EXTENDING TRANSECT ---")
    message("--- CONTINUING TO NEXT TRANSECT ---")
    message("---------------------------------")
    return(NULL)
  }
  
  # which(count_intersects == 1)
  # count_intersects[which(count_intersects != 0)] == 1
  # map_lst[[which(count_intersects != 0)]]
  
  # now we know that there is atleast 1 intersection, 
  # first we'll check if its only in one direction or if intersections occur in BOTH directions
  # if(any(count_intersects == 0) & count_intersects[which(count_intersects != 0)] == 1) {
  
  # now we know that there is atleast 1 intersection, 
  # first we'll check if its only in one direction or if intersections occur in BOTH directions
  
  # if intersection one of the extended lines does not intersect (0) 
  # AND
  # the other other extended line intersects EXACTLY 1 other flowline,
  # then this is a single braid that we will extend across
  if(any(count_intersects == 0)) {
    
    # if the NON ZERO intersection extended line, intersects EXACTLY 1 other flowline, then do single extension out
    if(count_intersects[which(count_intersects != 0)] == 1) {
      
      
      # if()
      # if(any(count_intersects == 0)) {
      
      # Do single direction extension
      
      # mapview::mapview(res_geom[which(count_intersects != 0)])
      
      # Do single direction extension
      
      # set res_geom to whichever direction has intersections
      # res_geom <-
      
      # get the hashmap of the direction that needs to be extended (i.e. the direction that has more than 0 intersections)
      ext_map <- map_lst[[which(count_intersects != 0)]]
      
      # to_extend <- map_lst[[which(count_intersects != 0)]]
      message("---------------------------------")
      message("--- ONE DIRECT INTERSECT ! ---")
      message("---> count_intersect[1]: ", count_intersects[1] , " ---")
      message("---> count_intersect[2]: ", count_intersects[2] , " ---")
      message("--- direction: ", ext_map$get("direction") ,"---")
      message("---------------------------------")
      
      # ext_map$as_list()
      
      # direction to extend out
      direction   <- ext_map$get("direction")
      
      # line to extend
      extend_line <- ext_map$get("line")
      
      # dist_vect[222]
      # length(dist_vect)
      # 307-171/2
      
      # start and end points of HEAD extended line
      start <- lwgeom::st_startpoint(extend_line)
      end   <- lwgeom::st_endpoint(extend_line)
      
      # mapview::mapview(others, color = "dodgerblue") +
      #   mapview::mapview(extend_line, color = "red") +
      #   mapview::mapview(x, color = "green")  +
      #   start + end + all_cross_pts + last_pt +res_geom
      
      # points that extended line crosses over other flowlines in the braid
      all_cross_pts <- sf::st_intersection(extend_line, geoms_to_cut)
      
      # mapview::mapview(res_geom) + tline + others + all_cross_pts
      
      # get the outtermost point that the line extended from HEAD crosses other braid flowlines
      # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
      last_pt <- all_cross_pts[
        which.max(as.numeric(sf::st_distance(sf::st_centroid(x), all_cross_pts)))
      ]
      
      # minimum distance between start and end points of extended line and the furthest possible intersection point. 
      diff_distance   <- min(c(
        as.numeric(sf::st_distance(last_pt, start)),
        as.numeric(sf::st_distance(last_pt, end))
      ))
      
      # END_PT--------------- CROSSER_PT ----------------------------START_PT
      # |------------------------|
      # ^^^^ SMALLER SECTION ^^^^
      # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
      # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
      
      # extra distance to extend line in direction determined above
      extra <- (cs_width/2) - diff_distance
      
      # # first extend out the head
      # res_geom <- st_extend_line(
      #                   x,  
      #                   ext_map$get("distance") + extra,
      #                   # 1440,
      #                   ext_map$get("direction")
      #                 )
      
      res_geom <- st_extend_line(
        ext_map$get("line"),  
        extra, 
        ext_map$get("direction")
      )
      
      return(res_geom)
      
    }
    # mapview::mapview(others, color = "dodgerblue") +
    #   mapview::mapview(extend_line, color = "red") +
    #   mapview::mapview(x, color = "green")  +
    #   start + end + all_cross_pts + last_pt +res_geom + res_geom2 + net3
    # If x == y or max(x, y) -1 == min(x, y): 
    #   # We found a middle, do double sided extension
    #   
    #   Else: 
    #   Return null 
  } 
  
  # if the number of intersections is NOT 0 and equal eachother 
  # OR
  # the maximum intersection count - 1 is equal to the minimum intersection count
  # THEN we have found a MIDDLE flowline in a braid
  if(count_intersects[1] == count_intersects[2] | max(count_intersects) - 1 == min(count_intersects)) {
    
    
    
    # if count_intersects[1]
    # else {
    message("---------------------------------")
    message("--- BOTH DIRECT INTERSECT ! ---")
    message("---> count_intersect[1]: ", count_intersects[1] , " ---")
    message("---> count_intersect[2]: ", count_intersects[2] , " ---")
    message("---------------------------------")
    #   direction = "both"
    # }
    # Note from 07/22:
    #  I am working on handling the situation where the line 
    # should be extended out in both directions, in that case I am trying to merge the head_ext and tail_ext objects so I can then take the endpoints, and from those endpoints find the closest intersection points in "all_cross_pts". 
    # The CLOSEST 'alL_cross_pts' will give me information on how to calculate the extra extension 
    # length that the final line needs to be extended out in both directions
    
    # the other idea i have is to do a final line extension WITHIN the extend_out function... 
    # so then I don't even need to deal with the final line extending because it will return the lines 
    # in a ready-to-go format, all i need to then do is select if I am picking the head, tail, or BOTH
    
    # One more note: 
    # IF this method I am working on within this current function DOES WORK, 
    # then I will probably need to find a better way of iterating through these transects,
    # basically either just ordering each COMID by lowest to highest divergence is my best idea. 
    # I just need a way to "prioritize" lower divergence values (mainstems) when it 
    # comes to choosing which transect flowlines will be
    # kept when there is a transect intersecting transect situation.
    
    # if (direction == "both") {
    
    # start and end points of HEAD extended line
    start_head <- lwgeom::st_startpoint(head_ext)
    end_head   <- lwgeom::st_endpoint(head_ext)
    
    # start and end points of TAIL extended line
    start_tail <- lwgeom::st_startpoint(tail_ext)
    end_tail   <- lwgeom::st_endpoint(tail_ext)
    
    # points that HEAD extended line crosses over
    head_cross_pts <- sf::st_intersection(head_ext, geoms_to_cut)
    
    # points that TAIL extended line crosses over
    tail_cross_pts <- sf::st_intersection(tail_ext, geoms_to_cut)
    
    # mapview::mapview(res_geom) + tline + others + all_cross_pts
    
    # get the outtermost point that the line extended from HEAD crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_head_pt <- head_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(x), head_cross_pts)))
    ]
    
    # get the outtermost point that the line extended from TAIL crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_tail_pt <- tail_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(x), tail_cross_pts)))
    ]
    
    # these are the distances that the extended HEAD/LINE line is crossing over the outer most flowline of the transect, 
    # we will subtract this value by cs_widths/2 in order to get the distance we need to extend the HEAD/TAIL line out 
    # in order to have cs_widths/2 on both sides of the transect
    
    # distance for HEAD extended line
    head_dist   <- min(c(
      as.numeric(sf::st_distance(last_head_pt, start_head)),
      as.numeric(sf::st_distance(last_head_pt, end_head))
    ))
    
    # distance for TAIL extended line
    tail_dist <- min(c(
      as.numeric(sf::st_distance(last_tail_pt, start_tail)),
      as.numeric(sf::st_distance(last_tail_pt, end_tail))
    ))
    
    # set distances to 0 if no crossing point is on top of the start/end
    head_dist   <- ifelse(length(head_dist) == 0, 0, head_dist)
    tail_dist   <- ifelse(length(tail_dist) == 0, 0,  tail_dist)
    
    # check to make sure that extending the line made an intersection
    if (head_dist == 0 & tail_dist == 0) {
      # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      return(NULL)
      # next
    }
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    
    # extra distance to extend HEAD
    extra_head <- (cs_width/2) - head_dist
    
    # extra distance to extend TAIL
    extra_tail <- (cs_width/2) - tail_dist
    
    # first extend out the head
    res_geom <- st_extend_line(
      x,  
      head_map$get("distance") + extra_head, 
      head_map$get("direction")
    )
    
    # then use the head extended line from above and extend the tail
    res_geom <- st_extend_line(
      res_geom,  
      tail_map$get("distance") + extra_tail, 
      tail_map$get("direction")
    )
    
    # mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(x, color = "red") +
    #   mapview::mapview(geoms_to_cut, color = "green") +
    #   mapview::mapview(res_geom, color = "red") +res_geom2
    
    return(res_geom)
    
  } else {
    message("---------------------------------")
    message("!!!!! SKIPPING BECAUSE EDGE OF MULTIBRAID !!!!!")
    message("---> count_intersect[1]: ", count_intersects[1] , " ---")
    message("---> count_intersect[2]: ", count_intersects[2] , " ---")
    message("---------------------------------")
    
    return(NULL)
    
  }
  
}
# Fix transects found on braided river sections
# net: sf object of NHDplusv2 data
# transect_lines: sf linestring dataframe, containing cross sections of flowlines in 'net'
#                 the output of "cut_cross_sections2()" function
# braid_threshold: numeric value, value of the total length of all flowlines in a braid. 
#             Only braids with total flowline lengths less than or equal to the threshold will be considered by function
#             (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#             Default is NULL, which will attempt to fix all the braid transects in the data

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects_mid2 <- function(
    net, 
    transect_lines,
    braid_threshold = NULL
) {
  
  transect_lines <-  transects_nofix
  net <- net3
  braid_threshold = NULL
  # braid_threshold = 25000
  # braid_threshold = NULL
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  if (all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <-
  braids <-  
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    # dplyr::group_by(comid) %>% 
    # dplyr::mutate(ncomid = n()) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # view data on map
  # mapview::mapview(not_braids, color = "dodgerblue") +
  # mapview::mapview(only_braids, color = "red") 
  
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x         = braids, 
      originals = not_braids, 
      threshold = braid_threshold,
      verbose   = TRUE
    )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
  }
  
  # # unique braid_ids/COMIDs
  # ubraids <- unique(only_braids$braid_id)
  # ucoms <- unique(only_braids$comid)
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) %>% 
    # dplyr::filter(divergence == 0)
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # keep track of all original crossections
  all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
  
  # # there are sometimes braids that don't have any divergence == 0 transects, 
  # # so we need to use whatever transect is avalaible with the lowest 'divergence' value 
  # # (hopefully its divergence == 1, but sometimes its divergence == 2)
  # div_xs <- 
  #   xs %>% 
  #   dplyr::filter(
  #     !has_mainstem
  #     # hy_id %in% extra_comids,
  #   ) %>% 
  #   dplyr::group_by(braid_id) %>% 
  #   dplyr::slice_min(divergence, with_ties = FALSE) %>% 
  #   dplyr::ungroup()
  # 
  # # from original cross sections, only keep divergence == 0 and remove the comids that are in "div_xs"
  # # filter to just divergence == 0 and comids NOT in 'div_xs'
  # xs <- dplyr::filter(xs, 
  #                     divergence == 0, 
  #                     !hy_id %in% div_xs$hy_id
  # )
  
  # flag determining whether transect should be replaced
  xs$changed <- FALSE
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    
    message("===== NO 'xs' transect lines =====")
    message("===== returning original data =====")
    
    return(transect_lines)
    
  } else {
    message("===== ", nrow(xs) , " 'xs' transect lines =====")
    message("===== returning original data =====")
  }
  
  # mapview::mapview(xs, color = "green") +
  #   mapview::mapview(braids, color = "red") +
  #   mapview::mapview(not_braids, color = "dodgerblue")
  # 1078427
  # i = 59
  for(i in 1:nrow(xs)) {
  # for(i in 1:7) {
    # i = 8
    # for(i in 1:58) {
    message("i: ", i, "/", nrow(xs))
    
    # transect line
    tline <- xs[i, ]$geometry
    
    # comid of transect line
    com <- xs[i, ]$hy_id
    
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    
    # get all linestrings that are apart of the braid_ids of interest
    bids_check <- sapply(1:length(braids$braid_id), function(x) {
      any(
        strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
      )
    })
    
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      braids,
      braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                             unlist(strsplit(braids[bids_check, ]$braid_id, ", "))
      )
      ),
      # braid_id %in% Reduce(c, strsplit(braids[bids_check, ]$braid_id, ", ")),
      comid != com
    )
    # mapview::mapview(others, color = "dodgerblue") + 
    #   mapview::mapview(tline, color = "green") +
    # mapview::mapview(braids, color = "red")
    # resulting geometry after extension
    res_geom <- augment_transect2(
      x            = tline,
      id           = com,
      geoms_to_cut = others,
      cs_width     = xs[i, ]$cs_widths,
      bf_width     = xs[i, ]$bf_width
    )
    
    # if augment_transect returns a NULL geometry, was NOT updated, 
    # so we can skip this iteration because no matter how far you extend out this transect, 
    # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
    if(is.null(res_geom)) {
      # message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      # message("=================")
      next
    }
    
    # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
    if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
      
      # # message stating that replacement was made
      # message("----> REPLACING ", i, " transect")
      
      # updatem geometry with new, extended cross section
      xs[i,]$geometry <- sf::st_geometry(res_geom)
      
      # flag determining whether transect should be replaced
      xs[i, ]$changed <- TRUE
      
    }
    # message("=================")
  }
  mapview::mapview(xs, color = "green") +
    mapview::mapview(braids, color = "red") +
    mapview::mapview(not_braids, color = "dodgerblue") + 
    mapview::mapview(xs, color = "green") +
    mapview::mapview(xs_drop, color = "gold") +
    mapview::mapview(xs_changed, color = "green") 
  # # keep track of cross sections to drop
  # xs_drop <- dplyr::filter(xs,!changed)
  # xs_changed <- dplyr::filter(xs,changed)
  
  # keep only the transects that were changed/extended
  xs <- dplyr::filter(xs, changed)
  
  # not_braids <- braids[!braids$comid %in% braids$comid, ]
  # any(lengths(sf::st_intersects(xs, not_braids)) > 0)
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- sf::st_intersects(not_braids, xs)
  
  # if there ARE some intersections, remove those intersecting lines from 'xs'
  if(any(lengths(net_intersects) > 0)) {
    message("Removing ", length(unlist(net_intersects)), " transect lines from 'xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    xs <- xs[-unlist(net_intersects), ]
  }
  
  # nrow(braids)
  # nrow(not_braids)
  # nrow(braids) + nrow(not_braids)
  # nrow(braids) + nrow(not_braids) == nrow(net)
  # tmpy <- xs2[lengths(sf::st_intersects(xs2, not_braids)) > 0, ]
  # mapview::mapview(xs2, color = "green") +
  #   mapview::mapview(tmpy, color = "gold") +
  #   mapview::mapview(not_braids, color = "dodgerblue") + 
  #   mapview::mapview(braids, color = "red") +
  # mapview::mapview(xs, color = "green")
  
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  div_intersects <- sf::st_intersects(xs, div_xs)
  
  # if there ARE some intersections, remove those intersecting lines from 'div_xs'
  if(any(lengths(div_intersects) > 0)) {
    message("Removing ", length(unlist(div_intersects)), " transect lines from 'div_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    div_xs <- div_xs[-unlist(div_intersects), ]
  }
  
  # flag determining whether transect should be replaced
  div_xs$changed <- FALSE
  
  if (nrow(div_xs) > 0) {
    
    message("===== ", nrow(div_xs)  ," 'div_xs' transect lines =====")
    
    for (i in 1:nrow(div_xs)) {
      
      message("i: ", i, "/", nrow(div_xs))
      
      # transect line
      tline <- div_xs[i, ]$geometry
      
      # comid of transect line
      com <- div_xs[i, ]$hy_id
      
      # # check if geom intersects 
      # if(any(lengths(sf::st_intersects(tline, xs)) > 0)) {
      #   message("!!!!! SKIPPING, div_xs[i, ] ALREADY INTERSECTS WITH 'xs' !!!!! ")
      #   message("=================")
      #   next
      # }
      
      # braid IDs of interest
      bids <- strsplit(div_xs[i, ]$braid_id, ", ")[[1]]
      
      # get all linestrings that are apart of the braid_ids of interest
      bids_check <- sapply(1:length(braids$braid_id), function(x) {
        any(
          strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
        )
      })
      
      
      # braid flowlines other than self that are within our given braid id or are nearby (the unique() filtering part)
      others <- dplyr::filter(
        braids,
        braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                               unlist(strsplit(braids[bids_check, ]$braid_id, ", ")))),
        comid != com
      )
      
      # resulting geometry after extension
      res_geom <- augment_transect(
        x            = tline,
        id           = com,
        geoms_to_cut = others,
        cs_width     = div_xs[i, ]$cs_widths,
        bf_width     = div_xs[i, ]$bf_width
      )
      
      # if augment_transect returns a NULL geometry, was NOT updated, 
      # so we can skip this iteration because no matter how far you extend out this transect, 
      # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
      if(is.null(res_geom)) {
        message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
        message("--- CONTINUING TO NEXT TRANSECT ---")
        message("=================")
        next
      }
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
      # AND it does NOT intersect with any other transects in 'xs' (the rest of the main transects lines)
      if(
        !any(lengths(sf::st_intersects(res_geom, div_xs[-i,])) > 0) & 
        !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
      ) {
        
        # # # message stating that replacement was made
        message("----> REPLACING ", i, " transect")
        
        # replace geometry with extended line
        div_xs[i,]$geometry <- sf::st_geometry(res_geom)
        
        # flag determining whether transect should be replaced
        div_xs[i, ]$changed <- TRUE
        
      }
      message("=================")
    }
    
    # # keep only the transects that were changed/extended
    # div_drop <- dplyr::filter(div_xs, !changed)
    
    # keep only the transects that were changed/extended
    div_xs <- dplyr::filter(div_xs, changed)
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -braid_id, -is_multibraid, -has_mainstem, -changed),
      dplyr::select(div_xs,
                    -braid_id, -is_multibraid, -has_mainstem, -changed)
    )
    
  } else {
    
    message("===== NO 'div_xs' transect lines =====")
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, -braid_id, -is_multibraid, -has_mainstem, -changed)
    
  }
  
  # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
  # to_keep %in% all_xs
  # all_xs %in% to_keep
  
  # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
  transect_lines <-  dplyr::bind_rows(
    # from original transect_lines, remove all of the cross sections on braids,
    dplyr::select(
      dplyr::filter(   
        dplyr::mutate(transect_lines, 
                      tmp_id = paste0(hy_id, "_", cs_id)
        ),
        !tmp_id %in% all_xs
      ),
      -tmp_id
    ),
    # updated braid cross sections
    out
  )
  
  # mapview::mapview(braids, color = "dodgerblue") +
  # mapview::mapview(not_braids, color = "gold") +
  # mapview::mapview(transect_lines, color = "green") +
  # mapview::mapview(transect_lines2, color = "red")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
}

# ----- BEST AUGMENT (WORK IN PROGRESS) ----
# # function for extending/updating transect cross section linestrings 
# # Description: Specifically to be used for situations where a river network is braided. 
# # x: transect line to try and extend to cover braided river sections 
# # id: unique identifier (COMID/hy_id) of transect line 
# # geoms_to_cut: other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# # cs_width: numeric, cross section width
# # bf_width: numeric, bankful width
# augment_transect_df <- function(cross_section,
#                                 geoms_to_cut, 
#                                 max_distance = NULL,
#                                 by = NULL, 
#                                 as_df = TRUE, 
#                                 carry_geom = TRUE
#                                 ) {
#   
#   # max distance from transect of interest and rest of braid flowlines 
#   # TODO (need a better method of determing max possible extension of flowline)
#   # max_dist <- as.numeric(
#   #                 max(
#   #                   sf::st_distance(  
#   #                     geoms_to_cut, 
#   #                     x
#   #                   )
#   #                 )
#   #               )
# 
#   # cross_section = xs[i, ]
#   # geoms_to_cut <- others
#   # max_distance = NULL
#   # by = 1
# 
#   # extract values from cross_section dataframe
#   cs_width <- cross_section$cs_widths
#   bf_width <- cross_section$bf_width
#   id       <- cross_section$hy_id
#   cs_line  <- cross_section$geometry
#   
#   # if no "by" argument is given, then the default becomes bf_width/2
#   if(is.null(max_distance)) {
#     max_distance <- max(cs_width * 5)
#   }
#   
#   # if no "by" argument is given, then the default becomes bf_width/2
#   if(is.null(by)) {
#     by = bf_width/2
#   }
#   
#   # sequence from 0 to the max possible extension distance 
#   dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
#   # dist_vect <- seq(0, max(c(max_distance, 2000)), multi_transects[i, ]$bf_width)
#   
#   # EXTEND OUT lines 
#   # extend transect line out in both directions and find the side that interests with m
#   # extend line out from HEAD side of line 
#   # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
#   head_map <- extend_out2(
#     x             = 1,
#     line          = cs_line, 
#     distances     = dist_vect,
#     geoms_to_cut  = geoms_to_cut, 
#     ids           = c(id), 
#     dir           = "head",
#     map           = TRUE
#   )
#   
#   # extend line out from TAIL side of line 
#   # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
#   tail_map <- extend_out2(
#     x             = 1,
#     line          = cs_line, 
#     distances     = dist_vect,
#     geoms_to_cut  = geoms_to_cut, 
#     ids           = c(id), 
#     dir           = "tail",
#     map           = TRUE
#   )
#   
#   # # extract the linestringshapes
#   # tail_ext <- tail_map$get("line")
#   # head_ext <- head_map$get("line")
# 
#   # mapview::mapview(braids,color = "gold") +  mapview::mapview(geoms_to_cut,color = "dodgerblue") + 
#   # mapview::mapview(head_ext,color = "green") +  mapview::mapview(tail_ext,color = "red") +mapview::mapview(cs_line,color = "cyan") 
#   
#   # get the relative position within the braid of the linestring we are extending our transect out from
#   position <- check_relative_position(
#                 head_count = head_map$get("count"),
#                 tail_count = tail_map$get("count")
#                 )
#   
#   # POSITION VALUES explanation:
#   # given the count of interesections from the head and tail of a linestring, return whether the line has:
#   # - NO_INTERSECTION:: (after extending linestring out to max distance)
#   # - OUTER_SINGLE: extending linestring out in both directions yielded 
#            # zero intersections in one direction AND exactly one intersection in the other direction
#   # - OUTER_MULTI: extending linestring out in both directions yielded 
#           # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
#   # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
#          # INNER scenario intersection count (odd and even cases):
#               # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
#                 # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
#   # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
#   # ----> SKIP THESE (maybe?) !
#   # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
#   
#   # if as_df is FALSE, return the line data hashmaps as a list of length 2, 
#   # first list element is the head extension data and the second is the tail extension data
#   if(!as_df) {
#     
#     # if NOT AN INNER LINE, postpone processesing
#     if(position != "inner") {
#       
#       # set pending values for these geometries
#       head_map$set("pending", TRUE)
#       tail_map$set("pending", TRUE)
#       
#       # set pending values for these geometries
#       head_map$set("position", position)
#       tail_map$set("position", position)
#       
#     } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
#       
#       # set pending values for these geometries
#       head_map$set("pending", FALSE)
#       tail_map$set("pending", FALSE)
#       
#       # set pending values for these geometries
#       head_map$set("position", position)
#       tail_map$set("position", position)
#       
#     }
#     
#     # if carry geom is FALSE, remove geometry linestrings from maps before returning
#     if(!carry_geom) {
#       head_map$remove("line")
#       tail_map$remove("line")
#     }
#     
#     return(
#       list(
#         head = head_map,
#         tail = tail_map
#       )
#     )
#     # return(
#     #   list(
#     #     head = head_map$as_list(),
#     #     tail = tail_map$as_list()
#     #   )
#     # )
#     
#   }
# 
# 
#   # update "relative_position" column in cross_section to reflect the position of the cross section flowline within the braid value
#   cross_section$relative_position <- position
#   
#   # if NOT AN INNER LINE, postpone processesing
#   if(position != "inner") {
#     # DON"T UPDATE "pending" value to reflect that this line should be put on hold and processed after the inner flowlines
#     
#     # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#     cross_section$head_distance <- head_map$get("total_distance")
#     cross_section$tail_distance <- tail_map$get("total_distance")
#     
#     # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#     cross_section$head_cuts <- head_map$get("count")
#     cross_section$tail_cuts <- tail_map$get("count")
# 
#   # if LINE IS A INNER LINE, GET READY TO EXTEND
#   } else {
#     
#     # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#     cross_section$pending <- FALSE
#     
#     # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#     cross_section$head_distance <- head_map$get("total_distance")
#     cross_section$tail_distance <- tail_map$get("total_distance")
#     
#     # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#     cross_section$head_cuts <- head_map$get("count")
#     cross_section$tail_cuts <- tail_map$get("count")
#     
#   }
#   
#   # res_geom <- extend_transects(
#   #                   starter_line   = cs_line, 
#   #                   head_distance  = head_map$get("total_distance"),
#   #                   tail_distance  = tail_map$get("total_distance"),
#   #                   extra_distance = cs_width/2
#   #                 )
#   
#   return(cross_section)
#   
# }

# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
augment_transect <- function(x, id, geoms_to_cut, cs_width, bf_width, max_distance = NULL, by = NULL) {

  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  # max_dist <- as.numeric(
  #                 max(
  #                   sf::st_distance(  
  #                     geoms_to_cut, 
  #                     x
  #                   )
  #                 )
  #               )
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(max_distance)) {
    max_distance <- max(cs_width * 5)
  }
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(by)) {
    by = bf_width/2
  }
  
  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  # dist_vect <- seq(0, max(c(max_distance, 2000)), multi_transects[i, ]$bf_width)
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  head_map <- extend_out2(
    x             = 1,
    line          = x, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "head",
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- extend_out2(
    x             = 1,
    line          = x, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "tail",
    map           = TRUE
  )
  
  # extract the linestring shapes
  tail_ext <- tail_map$get("line")
  head_ext <- head_map$get("line")
  
  # TODO CHECK which extended line should be selected when number of interesections is the same
  # IF: number of. intersection for each extended line is TIED, select the shorter of the two? NOT SURE WHAT THE CALL IS HERE
  # ELSE: return the one with more interesections (which.max)
  count_intersects <- c(
                        lengths(sf::st_intersects(head_ext, geoms_to_cut)), 
                        lengths(sf::st_intersects(tail_ext, geoms_to_cut))
                      )
  
  # make a list of head and tail map extensions
  map_lst <- list(head_map, tail_map)
  
  # mapview::mapview(head_ext) + tail_ext + others + tline
  
  # create simple feature collection of head and tail extended lines
  # res_geom <- sf::st_sfc(c(head_ext, tail_ext))
  
  # if neither direction had any intersections, skip this iteration:
  if(all(count_intersects == 0)) {
    # message("---------------------------------")
    # message("--- NO INTERSECT AFTER EXTENDING TRANSECT ---")
    # message("--- CONTINUING TO NEXT TRANSECT ---")
    # message("---------------------------------")
    return(NULL)
  }
  
  # now we know that there is atleast 1 intersection, 
  # first we'll check if its only in one direction or if intersections occur in BOTH directions
  if(any(count_intersects == 0)) {
    # message("---------------------------------")
    # message("--- ONE DIRECT INTERSECT ! ---")
    # message("--- direction: ", ext_map$get("direction") ,"---")
    # message("---------------------------------")
    # mapview::mapview(res_geom[which(count_intersects != 0)])
    
    # set res_geom to whichever direction has intersections
    # res_geom <-
    
    # get the hashmap of the direction that needs to be extended (i.e. the direction that has more than 0 intersections)
    ext_map <- map_lst[[which(count_intersects != 0)]]
    
    # to_extend <- map_lst[[which(count_intersects != 0)]]
    # message("---------------------------------")
    # message("--- ONE DIRECT INTERSECT ! ---")
    # message("--- direction: ", ext_map$get("direction") ,"---")
    # message("---------------------------------")
    
    # ext_map$as_list()
    
    # direction to extend out
    direction   <- ext_map$get("direction")
    
    # line to extend
    extend_line <- ext_map$get("line")

    # dist_vect[222]
    # length(dist_vect)
    # 307-171/2
    
    # start and end points of HEAD extended line
    start <- lwgeom::st_startpoint(extend_line)
    end   <- lwgeom::st_endpoint(extend_line)
    
    # mapview::mapview(others, color = "dodgerblue") +
    #   mapview::mapview(extend_line, color = "red") +
    #   mapview::mapview(x, color = "green")  +
    #   start + end + all_cross_pts + last_pt +res_geom
    
    # points that extended line crosses over other flowlines in the braid
    all_cross_pts <- sf::st_intersection(extend_line, geoms_to_cut)
    
    # mapview::mapview(res_geom) + tline + others + all_cross_pts
    
    # get the outtermost point that the line extended from HEAD crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_pt <- all_cross_pts[
                  which.max(as.numeric(sf::st_distance(sf::st_centroid(x), all_cross_pts)))
                ]
    
    # minimum distance between start and end points of extended line and the furthest possible intersection point. 
    diff_distance   <- min(c(
                          as.numeric(sf::st_distance(last_pt, start)),
                          as.numeric(sf::st_distance(last_pt, end))
                        ))
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    
    # extra distance to extend line in direction determined above
    extra <- (cs_width/2) - diff_distance

    # # first extend out the head
    # res_geom <- st_extend_line(
    #                   x,  
    #                   ext_map$get("distance") + extra,
    #                   # 1440,
    #                   ext_map$get("direction")
    #                 )
    
    res_geom <- st_extend_line(
                      ext_map$get("line"),  
                      extra, 
                      ext_map$get("direction")
                    )
    return(res_geom)
    # mapview::mapview(others, color = "dodgerblue") +
    #   mapview::mapview(extend_line, color = "red") +
    #   mapview::mapview(x, color = "green")  +
    #   start + end + all_cross_pts + last_pt +res_geom + res_geom2 + net3
    
  } else {
    # message("---------------------------------")
    # message("--- BOTH DIRECT INTERSECT ! ---")
    # message("---------------------------------")
  #   direction = "both"
  # }
  # Note from 07/22:
  #  I am working on handling the situation where the line 
  # should be extended out in both directions, in that case I am trying to merge the head_ext and tail_ext objects so I can then take the endpoints, and from those endpoints find the closest intersection points in "all_cross_pts". 
  # The CLOSEST 'alL_cross_pts' will give me information on how to calculate the extra extension 
  # length that the final line needs to be extended out in both directions
  
  # the other idea i have is to do a final line extension WITHIN the extend_out function... 
  # so then I don't even need to deal with the final line extending because it will return the lines 
  # in a ready-to-go format, all i need to then do is select if I am picking the head, tail, or BOTH
  
  # One more note: 
  # IF this method I am working on within this current function DOES WORK, 
  # then I will probably need to find a better way of iterating through these transects,
  # basically either just ordering each COMID by lowest to highest divergence is my best idea. 
  # I just need a way to "prioritize" lower divergence values (mainstems) when it 
  # comes to choosing which transect flowlines will be
  # kept when there is a transect intersecting transect situation.
  
  # if (direction == "both") {
    
    # start and end points of HEAD extended line
    start_head <- lwgeom::st_startpoint(head_ext)
    end_head   <- lwgeom::st_endpoint(head_ext)
    
    # start and end points of TAIL extended line
    start_tail <- lwgeom::st_startpoint(tail_ext)
    end_tail   <- lwgeom::st_endpoint(tail_ext)
    
    # points that HEAD extended line crosses over
    head_cross_pts <- sf::st_intersection(head_ext, geoms_to_cut)
    
    # points that TAIL extended line crosses over
    tail_cross_pts <- sf::st_intersection(tail_ext, geoms_to_cut)
    
    # mapview::mapview(res_geom) + tline + others + all_cross_pts
    
    # get the outtermost point that the line extended from HEAD crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_head_pt <- head_cross_pts[
          which.max(as.numeric(sf::st_distance(sf::st_centroid(x), head_cross_pts)))
        ]
    
    # get the outtermost point that the line extended from TAIL crosses other braid flowlines
    # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
    last_tail_pt <- tail_cross_pts[
          which.max(as.numeric(sf::st_distance(sf::st_centroid(x), tail_cross_pts)))
        ]
    
    # these are the distances that the extended HEAD/LINE line is crossing over the outer most flowline of the transect, 
    # we will subtract this value by cs_widths/2 in order to get the distance we need to extend the HEAD/TAIL line out 
    # in order to have cs_widths/2 on both sides of the transect
    
    # distance for HEAD extended line
    head_dist   <- min(c(
                        as.numeric(sf::st_distance(last_head_pt, start_head)),
                        as.numeric(sf::st_distance(last_head_pt, end_head))
                      ))
    
    # distance for TAIL extended line
    tail_dist <- min(c(
                        as.numeric(sf::st_distance(last_tail_pt, start_tail)),
                        as.numeric(sf::st_distance(last_tail_pt, end_tail))
                      ))
    
    # set distances to 0 if no crossing point is on top of the start/end
    head_dist   <- ifelse(length(head_dist) == 0, 0, head_dist)
    tail_dist   <- ifelse(length(tail_dist) == 0, 0,  tail_dist)
    
    # check to make sure that extending the line made an intersection
    if (head_dist == 0 & tail_dist == 0) {
      # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      return(NULL)
      # next
    }
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    
    # extra distance to extend HEAD
    extra_head <- (cs_width/2) - head_dist
    
    # extra distance to extend TAIL
    extra_tail <- (cs_width/2) - tail_dist
    
    # first extend out the head
    res_geom <- st_extend_line(
                    x,  
                    head_map$get("distance") + extra_head, 
                    head_map$get("direction")
                    )
    
    # then use the head extended line from above and extend the tail
    res_geom <- st_extend_line(
                    res_geom,  
                    tail_map$get("distance") + extra_tail, 
                    tail_map$get("direction")
                    )
    
    return(res_geom)
  }
  
}

# Fix transects found on braided river sections
# net: sf object of NHDplusv2 data
# transect_lines: sf linestring dataframe, containing cross sections of flowlines in 'net'
#                 the output of "cut_cross_sections2()" function
# braid_threshold: numeric value, value of the total length of all flowlines in a braid. 
#             Only braids with total flowline lengths less than or equal to the threshold will be considered by function
#             (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#             Default is NULL, which will attempt to fix all the braid transects in the data

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects5000 <- function(
    net, 
    transect_lines,
    braid_threshold = NULL
) {
  
  # transect_lines <-  transects_nofix
  # net <- net3
  # braid_threshold = 25000
  # braid_threshold = NULL
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  if(all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <-
  braids <-  
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    # dplyr::group_by(comid) %>% 
    # dplyr::mutate(ncomid = n()) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # view data on map
  # mapview::mapview(not_braids, color = "dodgerblue") +
  # mapview::mapview(only_braids, color = "red") 
  
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x         = braids, 
      originals = not_braids, 
      threshold = braid_threshold,
      verbose   = TRUE
    )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
  }
  
  # # unique braid_ids/COMIDs
  # ubraids <- unique(only_braids$braid_id)
  # ucoms <- unique(only_braids$comid)
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) %>% 
    # dplyr::filter(divergence == 0)
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # keep track of all original crossections
  all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
  
  # there are sometimes braids that don't have any divergence == 0 transects, 
  # so we need to use whatever transect is avalaible with the lowest 'divergence' value 
  # (hopefully its divergence == 1, but sometimes its divergence == 2)
  div_xs <- 
    xs %>% 
    dplyr::filter(
      !has_mainstem
      # hy_id %in% extra_comids,
    ) %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::slice_min(divergence, with_ties = FALSE) %>% 
    dplyr::ungroup()
  
  # from original cross sections, only keep divergence == 0 and remove the comids that are in "div_xs"
  # filter to just divergence == 0 and comids NOT in 'div_xs'
  xs <- dplyr::filter(xs, 
                      divergence == 0, 
                      !hy_id %in% div_xs$hy_id
  )
  
  # flag determining whether transect should be replaced
  xs$changed <- FALSE
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    
    message("===== NO 'xs' transect lines =====")
    message("===== returning original data =====")
    
    return(transect_lines)
    
  } else {
    message("===== ", nrow(xs) , " 'xs' transect lines =====")
    message("===== returning original data =====")
  }
  
  for(i in 1:nrow(xs)) {
    
    # message("i: ", i, "/", nrow(xs))
    
    # transect line
    tline <- xs[i, ]$geometry
    
    # comid of transect line
    com <- xs[i, ]$hy_id
    
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    
    # get all linestrings that are apart of the braid_ids of interest
    bids_check <- sapply(1:length(braids$braid_id), function(x) {
      any(
        strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
      )
    })
    
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      braids,
      braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                             unlist(strsplit(braids[bids_check, ]$braid_id, ", "))
      )
      ),
      # braid_id %in% Reduce(c, strsplit(braids[bids_check, ]$braid_id, ", ")),
      comid != com
    )
    
    # resulting geometry after extension
    res_geom <- augment_transect(
      x            = tline,
      id           = com,
      geoms_to_cut = others,
      cs_width     = xs[i, ]$cs_widths,
      bf_width     = xs[i, ]$bf_width,
      max_distance = NULL,
      by           = 1
    )
    
    # if augment_transect returns a NULL geometry, was NOT updated, 
    # so we can skip this iteration because no matter how far you extend out this transect, 
    # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
    if(is.null(res_geom)) {
      # message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      # message("=================")
      next
    }
    
    # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
    if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
      
      # # message stating that replacement was made
      # message("----> REPLACING ", i, " transect")
      
      # updatem geometry with new, extended cross section
      xs[i,]$geometry <- sf::st_geometry(res_geom)
      
      # flag determining whether transect should be replaced
      xs[i, ]$changed <- TRUE
      
    }
    # message("=================")
  }
  
  
  # keep only the transects that were changed/extended
  xs <- dplyr::filter(xs, changed)
  
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- sf::st_intersects(not_braids, xs)
  
  # if there ARE some intersections, remove those intersecting lines from 'xs'
  if(any(lengths(net_intersects) > 0)) {
    message("Removing ", length(unlist(net_intersects)), " transect lines from 'xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    xs <- xs[-unlist(net_intersects), ]
  }

  # mapview::mapview(xs2, color = "green") +
  #   mapview::mapview(tmpy, color = "gold") +
  #   mapview::mapview(not_braids, color = "dodgerblue") + 
  #   mapview::mapview(braids, color = "red") +
  # mapview::mapview(xs, color = "green")
  
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  div_intersects <- sf::st_intersects(xs, div_xs)
  
  # if there ARE some intersections, remove those intersecting lines from 'div_xs'
  if(any(lengths(div_intersects) > 0)) {
    message("Removing ", length(unlist(div_intersects)), " transect lines from 'div_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    div_xs <- div_xs[-unlist(div_intersects), ]
  }
  
  # flag determining whether transect should be replaced
  div_xs$changed <- FALSE
  
  if (nrow(div_xs) > 0) {
    
    message("===== ", nrow(div_xs)  ," 'div_xs' transect lines =====")
    
    for (i in 1:nrow(div_xs)) {
      
      message("i: ", i, "/", nrow(div_xs))
      
      # transect line
      tline <- div_xs[i, ]$geometry
      
      # comid of transect line
      com <- div_xs[i, ]$hy_id
      
      # # check if geom intersects 
      # if(any(lengths(sf::st_intersects(tline, xs)) > 0)) {
      #   message("!!!!! SKIPPING, div_xs[i, ] ALREADY INTERSECTS WITH 'xs' !!!!! ")
      #   message("=================")
      #   next
      # }
      
      # braid IDs of interest
      bids <- strsplit(div_xs[i, ]$braid_id, ", ")[[1]]
      
      # get all linestrings that are apart of the braid_ids of interest
      bids_check <- sapply(1:length(braids$braid_id), function(x) {
        any(
          strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
        )
      })
      
      
      # braid flowlines other than self that are within our given braid id or are nearby (the unique() filtering part)
      others <- dplyr::filter(
        braids,
        braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                               unlist(strsplit(braids[bids_check, ]$braid_id, ", ")))),
        comid != com
      )
      
      # resulting geometry after extension
      res_geom <- augment_transect(
        x            = tline,
        id           = com,
        geoms_to_cut = others,
        cs_width     = div_xs[i, ]$cs_widths,
        bf_width     = div_xs[i, ]$bf_width,
        max_distance = NULL,
        by           = 1
      )
      
      # if augment_transect returns a NULL geometry, was NOT updated, 
      # so we can skip this iteration because no matter how far you extend out this transect, 
      # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
      if(is.null(res_geom)) {
        message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
        message("--- CONTINUING TO NEXT TRANSECT ---")
        message("=================")
        next
      }
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
      # AND it does NOT intersect with any other transects in 'xs' (the rest of the main transects lines)
      if(
        !any(lengths(sf::st_intersects(res_geom, div_xs[-i,])) > 0) & 
        !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
      ) {
        
        # # # message stating that replacement was made
        message("----> REPLACING ", i, " transect")
        
        # replace geometry with extended line
        div_xs[i,]$geometry <- sf::st_geometry(res_geom)
        
        # flag determining whether transect should be replaced
        div_xs[i, ]$changed <- TRUE
        
      }
      message("=================")
    }
    
    # # keep only the transects that were changed/extended
    # div_drop <- dplyr::filter(div_xs, !changed)
    
    # keep only the transects that were changed/extended
    div_xs <- dplyr::filter(div_xs, changed)
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -braid_id, -is_multibraid, -has_mainstem, -changed),
      dplyr::select(div_xs,
                    -braid_id, -is_multibraid, -has_mainstem, -changed)
    )
    
  } else {
    
    message("===== NO 'div_xs' transect lines =====")
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, -braid_id, -is_multibraid, -has_mainstem, -changed)
    
  }
  
  # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
  # to_keep %in% all_xs
  # all_xs %in% to_keep
  
  # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
  transect_lines <-  dplyr::bind_rows(
    # from original transect_lines, remove all of the cross sections on braids,
    dplyr::select(
      dplyr::filter(   
        dplyr::mutate(transect_lines, 
                      tmp_id = paste0(hy_id, "_", cs_id)
        ),
        !tmp_id %in% all_xs
      ),
      -tmp_id
    ),
    # updated braid cross sections
    out
  )
  
  # mapview::mapview(braids, color = "dodgerblue") +
  # mapview::mapview(not_braids, color = "gold") +
  # mapview::mapview(transect_lines, color = "green") +
  # mapview::mapview(transect_lines2, color = "red")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
}

# ----- CURRENT BEST (WORK IN PROGRESS) ----
#' # Fix transects found on braided river sections
#' # net: sf object of NHDplusv2 data
#' # transect_lines: sf linestring dataframe, containing cross sections of flowlines in 'net'
#' #                 the output of "cut_cross_sections2()" function
#' # braid_threshold: numeric value, value of the total length of all flowlines in a braid. 
#' #             Only braids with total flowline lengths less than or equal to the threshold will be considered by function
#' #             (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' #             Default is NULL, which will attempt to fix all the braid transects in the data
#' 
#' #' Fix transects found on braided river sections
#' #'
#' #' @param net sf object of NHDplusv2 data
#' #' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' #' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' #' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' #' Default is NULL, which will attempt to fix all the braid transects in the data
#' #'
#' #' @return sf object of transect linestrings
#' #' @export
#' #'
#' #' @examples
#' fix_braid_transects_latest<- function(
#'     net, 
#'     transect_lines,
#'     braid_threshold = NULL
#' ) {
#'   
#'   # transect_lines <-  transects_nofix
#'   # net <- net3
#'   # braid_threshold = NULL
#'   # braid_threshold = 25000
#'   
#'   # keep track of the original CRS of the inputs to retransform return 
#'   start_crs1 <- sf::st_crs(net, parameters = T)$epsg
#'   start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
#'   
#'   message("Start CRS: ", start_crs1)
#'   
#'   # check if net CRS is 5070, if not, transform it to 5070
#'   if(start_crs1 != 5070) {
#'     # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
#'     message("Transforming CRS to EPSG: 5070")
#'     net <- sf::st_transform(net, 5070) 
#'   }
#'   
#'   # check if net CRS is 5070, if not, transform it to 5070
#'   if(start_crs2 != 5070) {
#'     # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
#'     message("Transforming CRS to EPSG: 5070")
#'     transect_lines <- sf::st_transform(transect_lines, 5070) 
#'   }
#'   
#'   message("Identifying braids...")
#'   
#'   # add braid_id column to network
#'   braids <- find_braids(
#'     network   = net,
#'     return_as = "dataframe",
#'     nested    = TRUE,
#'     # nested    = FALSE,
#'     add       = TRUE
#'   )
#'   
#'   if(all(braids$braid_id == "no_braid")) {
#'     
#'     message("No braids identified, returning original transects")
#'     
#'     # transform CRS back to input CRS
#'     if(start_crs2 != 5070) {
#'       message("Transforming CRS back to EPSG: ", start_crs2)
#'       transect_lines <- sf::st_transform(transect_lines, start_crs2)
#'     }
#'     
#'     return(transect_lines)
#'   }
#'   
#'   message("Fixing braid transects...")
#'   # not braided flowlines
#'   not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
#'   # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
#'   
#'   # trim down network to just the braided parts, and add a comid count to separate out multibraids
#'   # only_braids <-
#'   braids <-  
#'     braids %>% 
#'     dplyr::filter(braid_id != "no_braid") %>% 
#'     # dplyr::group_by(comid) %>% 
#'     # dplyr::mutate(ncomid = n()) %>% 
#'     # dplyr::ungroup() %>% 
#'     dplyr::group_by(braid_id) %>% 
#'     dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
#'     dplyr::ungroup()
#'   
#'   # view data on map
#'   # mapview::mapview(not_braids, color = "dodgerblue") +
#'   # mapview::mapview(only_braids, color = "red") 
#'   
#'   if(!is.null(braid_threshold)) {
#'     
#'     # remove braids that have a total flowline length greater than braid_threshold
#'     braids <- braid_thresholder(
#'       x         = braids, 
#'       originals = not_braids, 
#'       threshold = braid_threshold,
#'       verbose   = TRUE
#'     )
#'     
#'     # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
#'     not_braids <- braids$not_braids
#'     braids     <- braids$braids
#'   }
#'   
#'   # # unique braid_ids/COMIDs
#'   # ubraids <- unique(only_braids$braid_id)
#'   # ucoms <- unique(only_braids$comid)
#'   
#'   # join cross sections w/ braid flowlines
#'   xs <- 
#'     transect_lines %>%
#'     dplyr::filter(hy_id %in% braids$comid) %>%
#'     dplyr::left_join(
#'       sf::st_drop_geometry(
#'         dplyr::select(
#'           braids, comid, braid_id, is_multibraid
#'         )
#'       ),
#'       by = c("hy_id" = "comid")
#'     ) %>% 
#'     # dplyr::filter(divergence == 0)
#'     dplyr::group_by(braid_id) %>% 
#'     dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
#'     dplyr::ungroup() %>% 
#'     dplyr::arrange(-totdasqkm)
#'   
#'   # keep track of all original crossections
#'   all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
#'   
#'   # column to store the relative position within the braid of the flowline we're on 
#'   xs$relative_position <- NA
#'   
#'   # flag determining whether transect should/has been replaced
#'   xs$changed <- FALSE
#'   
#'   # flag determining whether transect is to be processed in a future step after middle flowlines are processed
#'   xs$pending <- TRUE
#'   
#'   # flag determining whether transect is to be processed in a future step after middle flowlines are processed
#'   xs$pending <- TRUE
#' 
#'   # empty columns to store number of head/tail intersections
#'   xs$head_cuts     <- NA
#'   xs$tail_cuts     <- NA
#'   
#'   # empty columns to store distance needed to extend from head/tail of line
#'   xs$head_distance <- NA
#'   xs$tail_distance <- NA
#'     
#'   # check if any transects exist, if not, just return the original transects
#'   if (nrow(xs) == 0) {
#'     
#'     message("===== NO 'xs' transect lines =====")
#'     message("===== returning original data =====")
#'     
#'     return(transect_lines)
#'     
#'   } else {
#'     message("===== ", nrow(xs) , " 'xs' transect lines =====")
#'     message("===== returning original data =====")
#'   }
#'   
#'   
#'   # mapview::mapview(braids, color = "dodgerblue") +
#'   #   mapview::mapview(xs, color = "red") +
#'     # mapview::mapview(xs[i, ], color = "green")
#'   
#'   # Loop through every single cross section and determine:
#'   # 1. its relative position
#'   # 2. how far to extend the line
#'   # 3. in what order should transects be extended, 
#'   # 4. in what direction to extend the transect
#'   for(i in 1:nrow(xs)) {
#'   # for(i in 1:17) {
#'     # message("i: ", i, "/", nrow(xs))
#'     # i = 18 
#'     # # transect line
#'     # tline <- xs[i, ]$geometry
#' 
#'     # comid of transect line
#'     com <- xs$hy_id[i]
#'     
#'     # braid IDs of interest
#'     bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
#'     
#'     # get neighboring braid ID for our current braid
#'     neighbor_braids <- get_neighbor_braids(x = braids, ids = bids, only_unique = T)
#' 
#'     # braid flowlines other than self that are within our given braid id or are nearby
#'     others <- dplyr::filter(
#'       braids,
#'       braid_id %in% neighbor_braids,
#'       comid != com
#'     )
#'     # INPUTS INTO NEW AUGMENT TRANSECTS DF FUNCTION
#'     # cross_section = xs[i, ]
#'     # geoms_to_cut <- others
#'     # max_distance = NULL
#'     # by = 1
#'     
#'     extend_maps <- augment_transect_df(
#'                 cross_section = xs[i, ],
#'                 geoms_to_cut  = others,
#'                 max_distance  = NULL, 
#'                 by            = 1, 
#'                 as_df         = FALSE,
#'                 carry_geom    = FALSE
#'                 )
#'     
#'     # extend_maps$head$as_list()
#'     position <- extend_maps$head$get("position")
#'     
#'     # message("----> position: ", position)
#'     
#'     # if(is.na(position)) {
#'     #   message("!!!!!! !!!!!!!!!!!!!!!!! !!!!!!!!!")
#'     #   message("!!!!!! FOUND AN NA POSITION VALUE !!!!!!!!!")
#'     #   message("!!!!!! !! iter: ", i ," !!!!!!!!!")
#'     # }
#'     
#'     # if a flowline on the inner portion of a braid, make extension and insert
#'     if(position == "inner") {
#'       # message("Extending ", i, " and checking if valid replacement...")
#'       # extend line out by total distance key values in head and tail maps
#'       res_geom <- extend_transects(
#'                       starter_line   = xs$geometry[i],
#'                       head_distance  = extend_maps$head$get("total_distance"),
#'                       tail_distance  = extend_maps$tail$get("total_distance"),
#'                       extra_distance = xs$cs_widths[i]/2
#'                     )
#'       
#'       # mapview::mapview(others) + braids + res_geom + not_braids
#' 
#'       # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
#'       if(!any(lengths(sf::st_intersects(res_geom, dplyr::filter(xs[-i,], changed))) > 0)) {
#'           # if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
#'           # # message stating that replacement was made
#'           # message("----> REPLACING ", i, " transect")
#'           
#'           # updatem geometry with new, extended cross section
#'           xs$geometry[i] <- sf::st_geometry(res_geom)
#'           
#'           # flag determining whether transect should be replaced
#'           xs$changed[i] <- TRUE
#'           # xs[i, ]$changed <- TRUE
#'         }
#'         
#'           # update relative position column
#'           xs$relative_position[i] <- extend_maps$head$get("position")
#'           
#'           # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#'           xs$pending[i] <- extend_maps$head$get("pending")
#'           
#'           # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#'           xs$head_distance[i] <- extend_maps$head$get("total_distance")
#'           xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
#'           
#'           # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#'           xs$head_cuts[i] <- extend_maps$head$get("count")
#'           xs$tail_cuts[i] <- extend_maps$tail$get("count")
#'       
#' 
#'     } else {
#'       message("Postpone processing: ", i)
#'       
#'         # update relative position column
#'         xs$relative_position[i] <- extend_maps$head$get("position")
#'       
#'         # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#'         xs$pending[i] <- extend_maps$head$get("pending")
#'         
#'         # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#'         xs$head_distance[i] <- extend_maps$head$get("total_distance")
#'         xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
#'         
#'         # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#'         xs$head_cuts[i] <- extend_maps$head$get("count")
#'         xs$tail_cuts[i] <- extend_maps$tail$get("count")
#'       
#'     }
#'     
#'     message("=================")
#'   }
#'   
#'   # tmp <- xs %>% dplyr::filter(is.na(relative_position))
#'   # mapview::mapview(xs, color = "red") +
#'   #   mapview::mapview(transect_lines, color = "green") +
#'   #   mapview::mapview(braids, color = "dodgerblue") + 
#'   #   mapview::mapview(tmp, color = "green")
#'   # net_intersects <- sf::st_intersects(not_braids, xs)
#'   # lengths(net_intersects)
#'   
#'   
#'   # # keep only the transects that were changed/extended
#'   # to_keep <- dplyr::filter(xs, changed)
#'   
#'   # # keep only the transects that were changed/extended
#'   # xs <- dplyr::filter(xs, changed)
#'   
#'   # check intersection of keeps and NOT BRAID
#'   # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
#'   net_intersects <- sf::st_intersects(not_braids, xs)
#'   # net_intersects <- sf::st_intersects(not_braids, to_keep)
#'   
#'   # lengths(net_intersects)
#'   
#'   # if there ARE some intersections, remove those intersecting lines from 'xs'
#'   if(any(lengths(net_intersects) > 0)) {
#'     # message("Removing ", length(unlist(net_intersects)), " transect lines from 'xs'")
#'     
#'     # drop div_xs transects that are overlapping with 'xs' transects
#'     xs <- xs[-unlist(net_intersects), ]
#'     # to_keep <- to_keep[-unlist(net_intersects), ]
#'   }
#'   
#'   # mapview::mapview(xs2, color = "green") +
#'   #   mapview::mapview(tmpy, color = "gold") +
#'   #   mapview::mapview(not_braids, color = "dodgerblue") + 
#'   #   mapview::mapview(braids, color = "red") +
#'   # mapview::mapview(xs, color = "green")
#'   
#'   # select the other cross sections that have NOT been changed yet and are NOT inner 
#'   # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
#'   other_xs = dplyr::filter(xs, 
#'                            !changed, 
#'                            relative_position != "inner"
#'                            )
#'   # other_xs = dplyr::filter(xs, !changed)
#'   # tt <- dplyr::filter(xs, !changed, relative_position != "inner")
#'   # tt <- dplyr::filter(xs, !changed)
#'   
#'   # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
#'   # # keep only the transects that were changed/extended
#'   # xs <- dplyr::filter(xs, changed)
#'   # dplyr::filter(xs, changed)
#'   # dplyr::filter(xs, !changed, relative_position == "inner")
#'   
#'   # inner transects that haven't been changed
#'   unchanged_inners <- dplyr::filter(xs, 
#'                                     !changed,
#'                                     relative_position == "inner")
#'   
#'   # keep only changed flowlines
#'   xs <- dplyr::filter(xs, changed) 
#'   
#'   # add back into "xs" the unchanged inner transects that do NOT intersect with our updated/extended inner transect lines
#'   xs <- dplyr::bind_rows(
#'                   xs,
#'                   unchanged_inners[-unlist(sf::st_intersects(
#'                                                 xs,
#'                                                 # dplyr::filter(xs, changed),
#'                                                 unchanged_inners
#'                                                 )
#'                                               ), 
#'                                             ]
#'                     )
#' 
#'   # # # # keep ALL "inner" transects, both the ones that were extended ("changed" == TRUE) and not changed inners
#'   # xs <- dplyr::filter(xs, changed | relative_position == "inner")
#'   
#'   # check intersection of keeps xs with other_xs
#'   
#'   # indices of other_xs transects that now intersect with the updated/extended 'xs' transects. All the cross section lines in "xs" are now "inner" lines that were extended
#'   other_intersects <- sf::st_intersects(xs, other_xs)
#'   
#'   # lengths(other_intersects)
#'   
#'   # if there ARE some intersections, remove those intersecting lines from 'div_xs'
#'   if(any(lengths(other_intersects) > 0)) {
#'     # message("Removing ", length(unique(unlist(other_intersects))), " transect lines from 'other_xs'")
#'     
#'     # drop div_xs transects that are overlapping with 'xs' transects
#'     other_xs <- other_xs[-unlist(other_intersects), ]
#'   }
#'   
#'   # # flag determining whether transect should be replaced
#'   # other_xs$changed <- FALSE
#' 
#'   # if there are still other (non "inner") transects, do extension processing
#'   if (nrow(other_xs) > 0) {
#'     
#'     # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
#'     # loop through the remaining transects that were NOT "inner" lines, and do extensions
#'     for (i in 1:nrow(other_xs)) {
#'       
#'       # message("i: ", i, "/", nrow(other_xs))
#'       
#'       # other_xs$relative_position[i]
#'       # other_xs$head_distance[i]
#'       # other_xs$tail_distance[i]
#'       # other_xs$head_cuts[i]
#'       # other_xs$tail_cuts[i]
#'       # other_xs$cs_widths[i]
#'       
#'       # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
#'       res_geom <- extend_transects(
#'                       starter_line   = other_xs$geometry[i],
#'                       head_distance  = other_xs$head_distance[i],
#'                       tail_distance  = other_xs$tail_distance[i],
#'                       extra_distance = other_xs$cs_widths[i]/2
#'                     )
#'       
#'       # mapview::mapview(res_geom, color = "green") +
#'       #   mapview::mapview(braids, color = "dodgerblue") +
#'       #   mapview::mapview(xs, color = "red") +
#'       #   mapview::mapview(other_xs$geometry[i], color = "cyan")
#'       #   braids + res_geom + not_braids
#'       
#'       # sf::st_intersects(res_geom, xs)
#'       # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
#'       # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom, 
#'       #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
#'       
#'       # ONLY UPDATE geometry if it does NOT intersect with any of the other ALREADY EXTENDED transects in "xs" 
#'       # OR any of the already updated (OR just any of them, not sure if I should limit it to only the CHANGED 'other_xs') transects in "other_xs" (AND LEAVE OUT SELF)
#'       if (
#'         !any(lengths(sf::st_intersects(res_geom, xs)) > 0) &
#'         !any(lengths(sf::st_intersects(res_geom, other_xs[-i, ])) > 0)
#'         # !any(lengths(sf::st_intersects(res_geom, xs)) > 0) &
#'         # !any(lengths(sf::st_intersects(res_geom,
#'         #                                dplyr::filter(other_xs[-i, ], changed))) > 0)
#'         ) {
#'         # # # message stating that replacement was made
#'         # message("----> REPLACING ", i, " transect")
#'         
#'         # replace geometry with extended line
#'         other_xs$geometry[i] <- sf::st_geometry(res_geom)
#'         
#'         # flag determining whether transect should be replaced
#'         other_xs$changed[i] <- TRUE
#'         
#'       }
#'       
#'       # message("=================")
#'     }
#'     
#'     # # # keep only the transects that were changed/extended
#'     # other_drop <- dplyr::filter(other_xs, !changed)
#'     # 
#'     # keep only the transects that were changed/extended
#'     other_xs <- dplyr::filter(other_xs, changed)
#'     # mapview::mapview(res_geom, color = "green") +
#'     #   mapview::mapview(braids, color = "dodgerblue") +
#'     #   mapview::mapview(xs, color = "red") +
#'     #   # mapview::mapview(other_xs$geometry[i], color = "cyan")
#'     # mapview::mapview(other_drop, color = "green") +
#'     # mapview::mapview(other_xs, color = "red")
#'     # braids + res_geom + not_braids
#'     
#'     # bind together final updated transect lines
#'     out <- dplyr::bind_rows(
#'                 dplyr::select(xs, 
#'                               -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#'                               -head_distance, -tail_distance, -head_cuts, -tail_cuts
#'                               ),
#'                 dplyr::select(other_xs,
#'                               -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#'                               -head_distance, -tail_distance, -head_cuts, -tail_cuts
#'                               )
#'               )
#'     
#'   } else {
#'     
#'     message("===== NO 'other_xs' transect lines =====")
#'     
#'     # bind together final updated transect lines
#'     out <- dplyr::select(xs, 
#'                          -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#'                          -head_distance, -tail_distance, -head_cuts, -tail_cuts
#'                          )
#'     
#'   }
#'   
#'   # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
#'   # to_keep %in% all_xs
#'   # all_xs %in% to_keep
#'   
#'   # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
#'   transect_lines <-  dplyr::bind_rows(
#'                           # from original transect_lines, remove all of the cross sections on braids,
#'                           dplyr::select(
#'                             dplyr::filter(   
#'                               dplyr::mutate(transect_lines, 
#'                                             tmp_id = paste0(hy_id, "_", cs_id)
#'                               ),
#'                               !tmp_id %in% all_xs
#'                             ),
#'                             -tmp_id
#'                           ),
#'                           # updated braid cross sections
#'                           out
#'                         )
#'   
#'   # mapview::mapview(braids, color = "dodgerblue") +
#'   # mapview::mapview(not_braids, color = "gold") +
#'   # mapview::mapview(transect_lines, color = "green") +
#'   # mapview::mapview(transect_lines2, color = "red")
#'   
#'   # transform CRS back to input CRS
#'   if(start_crs2 != 5070) {
#'     message("Transforming CRS back to EPSG: ", start_crs2)
#'     transect_lines <- sf::st_transform(transect_lines, start_crs2)
#'   }
#'   
#'   return(transect_lines)
#'   
#' }
# Fix transects found on braided river sections
# net: sf object of NHDplusv2 data
# transect_lines: sf linestring dataframe, containing cross sections of flowlines in 'net'
#                 the output of "cut_cross_sections2()" function
# braid_threshold: numeric value, value of the total length of all flowlines in a braid. 
#             Only braids with total flowline lengths less than or equal to the threshold will be considered by function
#             (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#             Default is NULL, which will attempt to fix all the braid transects in the data

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects <- function(
    net, 
    transect_lines,
    braid_threshold = NULL
    ) {
  
  # transect_lines <-  transects_nofix
  # net <- net3
  # braid_threshold = 25000
  # braid_threshold = NULL
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  if(all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <-
  braids <-  
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    # dplyr::group_by(comid) %>% 
    # dplyr::mutate(ncomid = n()) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # view data on map
  # mapview::mapview(not_braids, color = "dodgerblue") +
  # mapview::mapview(only_braids, color = "red") 
  
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
                x         = braids, 
                originals = not_braids, 
                threshold = braid_threshold,
                verbose   = TRUE
                )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
  }
  
  # # unique braid_ids/COMIDs
  # ubraids <- unique(only_braids$braid_id)
  # ucoms <- unique(only_braids$comid)
  
  # join cross sections w/ braid flowlines
   xs <- 
     transect_lines %>%
      dplyr::filter(hy_id %in% braids$comid) %>%
      dplyr::left_join(
        sf::st_drop_geometry(
          dplyr::select(
            braids, comid, braid_id, is_multibraid
          )
        ),
        by = c("hy_id" = "comid")
      ) %>% 
     # dplyr::filter(divergence == 0)
     dplyr::group_by(braid_id) %>% 
     dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
     dplyr::ungroup()
   
   # keep track of all original crossections
   all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
   
   # there are sometimes braids that don't have any divergence == 0 transects, 
   # so we need to use whatever transect is avalaible with the lowest 'divergence' value 
   # (hopefully its divergence == 1, but sometimes its divergence == 2)
   div_xs <- 
     xs %>% 
     dplyr::filter(
       !has_mainstem
       # hy_id %in% extra_comids,
       ) %>% 
     dplyr::group_by(braid_id) %>% 
     dplyr::slice_min(divergence, with_ties = FALSE) %>% 
     dplyr::ungroup()
 
   # from original cross sections, only keep divergence == 0 and remove the comids that are in "div_xs"
   # filter to just divergence == 0 and comids NOT in 'div_xs'
   xs <- dplyr::filter(xs, 
                       divergence == 0, 
                       !hy_id %in% div_xs$hy_id
                       )
   
   # flag determining whether transect should be replaced
   xs$changed <- FALSE
   
   # check if any transects exist, if not, just return the original transects
   if (nrow(xs) == 0) {
     
     message("===== NO 'xs' transect lines =====")
     message("===== returning original data =====")
     
     return(transect_lines)
     
   } else {
     message("===== ", nrow(xs) , " 'xs' transect lines =====")
     message("===== returning original data =====")
   }
   
   for(i in 1:nrow(xs)) {
     
     # message("i: ", i, "/", nrow(xs))
     
     # transect line
     tline <- xs[i, ]$geometry
     
     # comid of transect line
     com <- xs[i, ]$hy_id
     
     # braid IDs of interest
     bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]

     # get all linestrings that are apart of the braid_ids of interest
     bids_check <- sapply(1:length(braids$braid_id), function(x) {
       any(
         strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
         )
     })


      # braid flowlines other than self that are within our given braid id or are nearby
      others <- dplyr::filter(
                  braids,
                  braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                                         unlist(strsplit(braids[bids_check, ]$braid_id, ", "))
                                         )
                                       ),
                  # braid_id %in% Reduce(c, strsplit(braids[bids_check, ]$braid_id, ", ")),
                  comid != com
                  )
      
      # resulting geometry after extension
      res_geom <- augment_transect(
                      x            = tline,
                      id           = com,
                      geoms_to_cut = others,
                      cs_width     = xs[i, ]$cs_widths,
                      bf_width     = xs[i, ]$bf_width
                      )
      
      # if augment_transect returns a NULL geometry, was NOT updated, 
      # so we can skip this iteration because no matter how far you extend out this transect, 
      # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
      if(is.null(res_geom)) {
        # message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
        # message("--- CONTINUING TO NEXT TRANSECT ---")
        # message("=================")
        next
      }
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
      if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
        
        # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # updatem geometry with new, extended cross section
        xs[i,]$geometry <- sf::st_geometry(res_geom)
      
        # flag determining whether transect should be replaced
        xs[i, ]$changed <- TRUE
        
      }
      # message("=================")
   }
   
   # # keep track of cross sections to drop
   # xs_drop <- dplyr::filter(xs,!changed)
   
   # keep only the transects that were changed/extended
   xs <- dplyr::filter(xs, changed)
   
   # not_braids <- braids[!braids$comid %in% braids$comid, ]
   # any(lengths(sf::st_intersects(xs, not_braids)) > 0)
   # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
   net_intersects <- sf::st_intersects(not_braids, xs)
   
   # if there ARE some intersections, remove those intersecting lines from 'xs'
   if(any(lengths(net_intersects) > 0)) {
     message("Removing ", length(unlist(net_intersects)), " transect lines from 'xs'")
     
     # drop div_xs transects that are overlapping with 'xs' transects
     xs <- xs[-unlist(net_intersects), ]
   }
   
   # nrow(braids)
   # nrow(not_braids)
   # nrow(braids) + nrow(not_braids)
   # nrow(braids) + nrow(not_braids) == nrow(net)
   # tmpy <- xs2[lengths(sf::st_intersects(xs2, not_braids)) > 0, ]
   # mapview::mapview(xs2, color = "green") +
   #   mapview::mapview(tmpy, color = "gold") +
   #   mapview::mapview(not_braids, color = "dodgerblue") + 
   #   mapview::mapview(braids, color = "red") +
   # mapview::mapview(xs, color = "green")
   
   # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
   div_intersects <- sf::st_intersects(xs, div_xs)
   
   # if there ARE some intersections, remove those intersecting lines from 'div_xs'
   if(any(lengths(div_intersects) > 0)) {
     message("Removing ", length(unlist(div_intersects)), " transect lines from 'div_xs'")
     
     # drop div_xs transects that are overlapping with 'xs' transects
     div_xs <- div_xs[-unlist(div_intersects), ]
   }

   # flag determining whether transect should be replaced
   div_xs$changed <- FALSE
   
   if (nrow(div_xs) > 0) {
     
     message("===== ", nrow(div_xs)  ," 'div_xs' transect lines =====")
     
     for (i in 1:nrow(div_xs)) {
       
       message("i: ", i, "/", nrow(div_xs))
       
       # transect line
       tline <- div_xs[i, ]$geometry
       
       # comid of transect line
       com <- div_xs[i, ]$hy_id
       
       # # check if geom intersects 
       # if(any(lengths(sf::st_intersects(tline, xs)) > 0)) {
       #   message("!!!!! SKIPPING, div_xs[i, ] ALREADY INTERSECTS WITH 'xs' !!!!! ")
       #   message("=================")
       #   next
       # }
       
       # braid IDs of interest
       bids <- strsplit(div_xs[i, ]$braid_id, ", ")[[1]]
       
       # get all linestrings that are apart of the braid_ids of interest
       bids_check <- sapply(1:length(braids$braid_id), function(x) {
         any(
           strsplit(braids$braid_id[x], ", ")[[1]] %in% bids
           )
       })
       
       
       # braid flowlines other than self that are within our given braid id or are nearby (the unique() filtering part)
       others <- dplyr::filter(
                     braids,
                     braid_id %in% unique(c(braids[bids_check, ]$braid_id,
                                            unlist(strsplit(braids[bids_check, ]$braid_id, ", ")))),
                     comid != com
                   )
       
       # resulting geometry after extension
       res_geom <- augment_transect(
                       x            = tline,
                       id           = com,
                       geoms_to_cut = others,
                       cs_width     = div_xs[i, ]$cs_widths,
                       bf_width     = div_xs[i, ]$bf_width
                     )
                     
       # if augment_transect returns a NULL geometry, was NOT updated, 
       # so we can skip this iteration because no matter how far you extend out this transect, 
       # it does NOT end up interesecting any of the other flowlines in this set of braided flowlines
       if(is.null(res_geom)) {
         message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
         message("--- CONTINUING TO NEXT TRANSECT ---")
         message("=================")
         next
       }
       
       # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
       # AND it does NOT intersect with any other transects in 'xs' (the rest of the main transects lines)
       if(
         !any(lengths(sf::st_intersects(res_geom, div_xs[-i,])) > 0) & 
         !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
          ) {
         
         # # # message stating that replacement was made
         message("----> REPLACING ", i, " transect")
         
         # replace geometry with extended line
         div_xs[i,]$geometry <- sf::st_geometry(res_geom)
         
         # flag determining whether transect should be replaced
         div_xs[i, ]$changed <- TRUE
         
         }
       message("=================")
     }
     
     # # keep only the transects that were changed/extended
     # div_drop <- dplyr::filter(div_xs, !changed)
     
     # keep only the transects that were changed/extended
     div_xs <- dplyr::filter(div_xs, changed)
     
     # bind together final updated transect lines
     out <- dplyr::bind_rows(
                 dplyr::select(xs, 
                               -braid_id, -is_multibraid, -has_mainstem, -changed),
                 dplyr::select(div_xs,
                               -braid_id, -is_multibraid, -has_mainstem, -changed)
                 )
     
   } else {
     
     message("===== NO 'div_xs' transect lines =====")
     
     # bind together final updated transect lines
     out <- dplyr::select(xs, -braid_id, -is_multibraid, -has_mainstem, -changed)
     
   }
   
   # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
   # to_keep %in% all_xs
   # all_xs %in% to_keep

   # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
   transect_lines <-  dplyr::bind_rows(
                         # from original transect_lines, remove all of the cross sections on braids,
                         dplyr::select(
                           dplyr::filter(   
                             dplyr::mutate(transect_lines, 
                                           tmp_id = paste0(hy_id, "_", cs_id)
                             ),
                             !tmp_id %in% all_xs
                           ),
                           -tmp_id
                         ),
                         # updated braid cross sections
                         out
                       )
   
   # mapview::mapview(braids, color = "dodgerblue") +
   # mapview::mapview(not_braids, color = "gold") +
   # mapview::mapview(transect_lines, color = "green") +
   # mapview::mapview(transect_lines2, color = "red")
   
   # transform CRS back to input CRS
   if(start_crs2 != 5070) {
     message("Transforming CRS back to EPSG: ", start_crs2)
     transect_lines <- sf::st_transform(transect_lines, start_crs2)
   }
  
   return(transect_lines)
   
}

   # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
   # to_keep %in% all_xs
   # all_xs %in% to_keep

  #  mapview::mapview(braids, color = "dodgerblue") +
  #    mapview::mapview(only_braids, color = "cyan")+
  #    # mapview::mapview(to_drop, color = "dodgerblue") +
  #    mapview::mapview(out, color = "red") + 
  #    mapview::mapview(transect_lines, color = "gold") +
  #    mapview::mapview(transect_lines2, color = "green") 
  #  
  # mapview::mapview(others, color = "cyan") +
  #   mapview::mapview(only_braids, color = "dodgerblue")+
  #   mapview::mapview(xs, color = "red") +
  #   mapview::mapview(div_xs, color = "green") 
    # mapview::mapview(tline, color = "red") +
    # mapview::mapview(res_geom, color = "green") 
  
  
   #    message("=================")
   #    # max distance from transect of interest and rest of braid flowlines 
   #    # TODO (need a better method of determing max possible extension of flowline)
   #    max_dist <- as.numeric(
   #                    max(
   #                      sf::st_distance(  
   #                        others, 
   #                        tline
   #                      )
   #                    )
   #                  )
   #    
   #    # sequence from 0 to the max possible extension distance 
   #    dist_vect <- seq(0, max(c(max_dist, 2000)), xs[i, ]$bf_width/2)
   #    # dist_vect <- seq(0, max(c(max_dist, 2000)), multi_transects[i, ]$bf_width)
   #    
   #    # EXTEND OUT lines 
   #    # extend transect line out in both directions and find the side that interests with m
   #    # extend line out from HEAD side of line 
   #    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
   #    head_map <- extend_out2(
   #      x             = 1,
   #      line          = tline, 
   #      distances     = dist_vect,
   #      geoms_to_cut  = others, 
   #      ids           = c(com), 
   #      dir           = "head",
   #      map           = TRUE
   #    )
   #    
   #    # extend line out from TAIL side of line 
   #    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
   #    tail_map <- extend_out2(
   #      x             = 1,
   #      line          = tline, 
   #      distances     = dist_vect,
   #      geoms_to_cut  = others, 
   #      ids           = c(com), 
   #      dir           = "tail",
   #      map           = TRUE
   #    )
   #    
   #    # tail_ext$as_list()
   #    # head_ext$as_list()
   #    
   #    tail_ext <- tail_map$get("line")
   #    head_ext <- head_map$get("line")
   #    
   #    # mapview::mapview(tail_ext) + head_ext + tline + others
   #    # TODO CHECK which extended line should be selected when number of interesections is the same
   #    # IF: number of. intersection for each extended line is TIED, select the shorter of the two? NOT SURE WHAT THE CALL IS HERE
   #    # ELSE: return the one with more interesections (which.max)
   #    count_intersects <- c(
   #                          lengths(sf::st_intersects(head_ext, others)), 
   #                          lengths(sf::st_intersects(tail_ext, others))
   #                          )
   #    
   #    # mapview::mapview(head_ext) + tail_ext + others + tline
   #    
   #    # create simple feature collection of head and tail extended lines
   #    # res_geom <- sf::st_sfc(c(head_ext, tail_ext))
   #  
   #    # if neither direction had any intersections, skip this iteration:
   #    if(all(count_intersects == 0)) {
   #      message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
   #      message("--- CONTINUING TO NEXT TRANSECT ---")
   #      next
   #    }
   #    
   #    # now we know that there is atleast 1 intersection, 
   #    # first we'll check if its only in one direction or if intersections occur in BOTH directions
   #    if(any(count_intersects == 0)) {
   #      
   #      # mapview::mapview(res_geom[which(count_intersects != 0)])
   #      
   #      # set res_geom to whichever direction has intersections
   #      res_geom <- res_geom[which(count_intersects != 0)]
   #      
   #      direction <- c("head", "tail")[which(count_intersects != 0)]
   #      
   #    } else {
   #      
   #      direction = "both"
   #    }
   #    # Note from 07/22:
   #    #  I am working on handling the situation where the line 
   #    # should be extended out in both directions, in that case I am trying to merge the head_ext and tail_ext objects so I can then take the endpoints, and from those endpoints find the closest intersection points in "all_cross_pts". 
   #    # The CLOSEST 'alL_cross_pts' will give me information on how to calculate the extra extension 
   #    # length that the final line needs to be extended out in both directions
   #    
   #    # the other idea i have is to do a final line extension WITHIN the extend_out function... 
   #    # so then I don't even need to deal with the final line extending because it will return the lines 
   #    # in a ready-to-go format, all i need to then do is select if I am picking the head, tail, or BOTH
   #  
   #    # One more note: 
   #    # IF this method I am working on within this current function DOES WORK, 
   #    # then I will probably need to find a better way of iterating through these transects,
   #    # basically either just ordering each COMID by lowest to highest divergence is my best idea. 
   #    # I just need a way to "prioritize" lower divergence values (mainstems) when it 
   #    # comes to choosing which transect flowlines will be
   #    # kept when there is a transect intersecting transect situation.
   #    
   #    if (direction == "both") {
   #      
   #      # start and end points of HEAD extended line
   #      start_head <- lwgeom::st_startpoint(head_ext)
   #      end_head <- lwgeom::st_endpoint(head_ext)
   #      
   #      # start and end points of TAIL extended line
   #      start_tail <- lwgeom::st_startpoint(tail_ext)
   #      end_tail <- lwgeom::st_endpoint(tail_ext)
   #      
   #      # points that HEAD extended line crosses over
   #      head_cross_pts <- sf::st_intersection(head_ext, others)
   #      
   #      # points that TAIL extended line crosses over
   #      tail_cross_pts <- sf::st_intersection(tail_ext, others)
   #      
   #      # mapview::mapview(res_geom) + tline + others + all_cross_pts
   #      
   #      # get the outtermost point that the line extended from HEAD crosses other braid flowlines
   #      # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
   #      last_head_pt <- head_cross_pts[
   #        which.max(as.numeric(sf::st_distance(sf::st_centroid(tline), head_cross_pts)))
   #        ]
   #      
   #      # get the outtermost point that the line extended from TAIL crosses other braid flowlines
   #      # by finding the head_cross_pts that is the FURTHEST from the centroid of the original transect line
   #      last_tail_pt <- tail_cross_pts[
   #        which.max(as.numeric(sf::st_distance(sf::st_centroid(tline), tail_cross_pts)))
   #      ]
   #      
   #      # these are the distances that the extended HEAD/LINE line is crossing over the outer most flowline of the transect, 
   #      # we will subtract this value by cs_widths/2 in order to get the distance we need to extend the HEAD/TAIL line out 
   #      # in order to have cs_widths/2 on both sides of the transect
   #      
   #      # distance for HEAD extended line
   #      head_dist   <- min(c(
   #                      as.numeric(sf::st_distance(last_head_pt, start_head)),
   #                      as.numeric(sf::st_distance(last_head_pt, end_head))
   #                      ))
   #      
   #      # distance for TAIL extended line
   #      tail_dist <- min(c(
   #                      as.numeric(sf::st_distance(last_tail_pt, start_tail)),
   #                      as.numeric(sf::st_distance(last_tail_pt, end_tail))
   #                      ))
   #      
   #      # set distances to 0 if no crossing point is on top of the start/end
   #      head_dist   <- ifelse(length(head_dist) == 0, 0, head_dist)
   #      tail_dist   <- ifelse(length(tail_dist) == 0, 0,  tail_dist)
   #      
   #      # check to make sure that extending the line made an intersection
   #      if (end_dist == 0 & start_dist == 0) {
   #        # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
   #        # message("--- CONTINUING TO NEXT TRANSECT ---")
   #        next
   #      }
   #      
   #      # END_PT--------------- CROSSER_PT ----------------------------START_PT
   #      # |------------------------|
   #      # ^^^^ SMALLER SECTION ^^^^
   #      # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
   #      # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
   #      
   #      # extra distance to extend HEAD
   #      extra_head <- (xs[i, ]$cs_widths/2) - head_dist
   #      
   #      # extra distance to extend TAIL
   #      extra_tail <- (xs[i, ]$cs_widths/2) - tail_dist
   #      
   #      # first extend out the head
   #      res_geom <- st_extend_line(
   #                          tline,  
   #                          head_map$get("distance") + extra_head, 
   #                          head_map$get("direction")
   #                        )
   #      # then use the head extended line from above and extend the tail
   #      res_geom <- st_extend_line(
   #                            res_geom,  
   #                            tail_map$get("distance") + extra_tail, 
   #                            tail_map$get("direction")
   #                            )
   #      
   #      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
   #      if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
   #        
   #        
   #      }
   #      # headtmp <- st_extend_line(
   #      #   tline,  
   #      #   head_map$get("distance") + extra_head, 
   #      #   head_map$get("direction")
   #      # )
   #      # 
   #      # tailtmp <- st_extend_line(
   #      #   tline,  
   #      #   tail_map$get("distance") + extra_tail, 
   #      #   tail_map$get("direction")
   #      # )
   #      # mapview::mapview(braids, color = "dodgerblue") +
   #      # mapview::mapview(only_braids, color = "red")+
   #      # mapview::mapview(xs, color = "gold") +
   #      # mapview::mapview(tline, color = "cyan") +
   #      #   mapview::mapview(res_geom, color = "cyan") +
   #      #   mapview::mapview(headtmp, color = "green") +
   #      #   mapview::mapview(tailtmp, color = "red") +
   #      #   mapview::mapview(others, color = "cyan") +
   #      #   mapview::mapview(head_ext, color = "red") +
   #      #   mapview::mapview(tail_ext, color = "red") +
   #      #   end_head + start_head + end_tail + start_tail +
   #      #   tail_cross_pts + head_cross_pts
   #        
   #      res_geom %>%
   #      sf::st_combine()  %>% 
   #      sf::st_line_merge() %>% 
   #      sf::st_snap(. ,tolerance = 0.5)
   #      sf::st_union() 
   #    # %>% 
   #    #   sf::st_line_merge()
   #    ll <- sf::st_union(sf::st_combine(res_geom))
   #    
   #    # linestring start and ends, 
   #    end   <- lwgeom::st_endpoint(      sf::st_union(sf::st_combine(res_geom)))
   #    start <- lwgeom::st_startpoint(      sf::st_union(sf::st_combine(res_geom)))
   #    mapview::mapview(   sf::st_combine(res_geom) ) + start + end + ll
   #  }
   #    # mapview::mapview(res_geom)
   #  which(count_intersects != 0)
   # 
   #  # point that crosses over other flowline
   #  all_cross_pts <- sf::st_intersection(res_geom, others)
   #  
   #  mapview::mapview(res_geom) + tline + others + all_cross_pts
   #  # get our final crosser point by finding the all_cross_pts that is the FURTHEST from the centroid of the original transect line
   #  cross_pt <- all_cross_pts[
   #    which.max(as.numeric(sf::st_distance(sf::st_centroid(tline), all_cross_pts)))
   #  ]
   #  
   #  # linestring start and ends, 
   #  end   <- lwgeom::st_endpoint(res_geom)
   #  start <- lwgeom::st_startpoint(res_geom)
   #  
   #  # distance from crossing point and end/start points. 
   #  # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
   #  end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
   #  start_dist <- as.numeric(sf::st_distance(cross_pt, start))
   #  
   #  # set distances to 0 if no crossing point is on top of the start/end
   #  end_dist   <- ifelse(length(end_dist) == 0, 0, end_dist)
   #  start_dist <- ifelse(length(start_dist) == 0, 0,  start_dist)
   # 
   # }
# }

fix_braid_transects2 <- function(net, transect_lines, drop = FALSE) {
  # transect_lines = transects2
  # net <- net3

  # mapview::mapview(transect_lines) + net
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }

  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    # nested    = TRUE,
    nested    = FALSE,
    add       = TRUE
  )
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  only_braids <- 
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    dplyr::group_by(comid) %>% 
    dplyr::mutate(
      ncomid = n()
    ) %>% 
    dplyr::ungroup() 
    # dplyr::group_by(braid_id)
    # dplyr::group_by(comid, braid_id)
  
  # extract the flowlines/comids that show up more than once (multibraids) and slice to just the lowest divergence value (0 or 1)
  multis <- 
    only_braids %>% 
    dplyr::filter(ncomid > 1)  %>% 
    # dplyr::filter(ncomid > 1 | is_multibraid)
    dplyr::group_by(comid) %>% 
    # dplyr::filter(divergence %in% c(0, 1))
    dplyr::slice_min(divergence, with_ties = FALSE) %>% 
    dplyr::ungroup()
  
  # get the remaining braided flowlines which will be single flowlines
  # filter rest of braids to the flowlines to:
  #  - comids NOT in multis
  #  AND 
  # -  braid_ids NOT in multis
  singles <- dplyr::filter(only_braids, 
                           !comid %in% multis$comid
                           )
  # COUNT number of flowlines in each braid
  singles <- 
    singles %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(
      nbraid = n()
    ) %>% 
    dplyr::ungroup() 
  
  # if any singles only have a single flowline, extract these braids and add to 'multis'
  extra_singles <- dplyr::select(
                          dplyr::filter(singles, nbraid == 1),
                          -nbraid
                          )
  
  # IF there are some extra, braid_ids with only a single flowline braids:
  # subset these singletons and then add to 'multis' set of multibraid flowlines IF:
  # braid_id of the singletons is already in the multis
  # ALSO then remove the singletons from 'singles'
  # then add back any of the remaining 'extra_singles' IF they are NOT found in 'multis'
  if(nrow(extra_singles) > 0) {
    
    message("---> FOUND ", nrow(extra_singles), " EXTRA SINGLES")
    
    # bind extra braids to multis IF they are already in multis
    multis <- dplyr::bind_rows(
                  multis,
                  extra_singles[extra_singles$braid_id %in% multis$braid_id, ]
                )
    
    # remove all extra_singles from original singles
    singles <- dplyr::select(
                        dplyr::filter(singles, nbraid != 1),
                        -nbraid
                      )
    
    # bind extra braids BACK INTO 'singles' IF they were NOT found in multis
    singles <- dplyr::bind_rows(
                  singles,
                  extra_singles[!extra_singles$braid_id %in% multis$braid_id, ]
                )
    
  }
  
  # singles <- dplyr::select(dplyr::filter(singles, nbraid != 1),
  #                         -nbraid)
  
  # match braid flowlines with the transects on each braid
  singles_xs <- get_single_braid_transects(singles, transect_lines)
  multis_xs <- get_multibraid_transects(multis, transect_lines)
  
  # extend transects outwards
  singles_extend <- rectify_singlebraid_transects(singles, singles_xs)
  multis_extend  <- rectify_multibraid_transects(multis, multis_xs)

  # nochange <- extended_lines %>% dplyr::filter(!changed)
  # change <- extended_lines %>% dplyr::filter(changed)
  # mapview::mapview(nochange, color = "red") + change
  # mapview::mapview(extended_lines, color = "green") +
  #   mapview::mapview(change, color = "green") +
  #   mapview::mapview(nochange, color = "red") +
  #   mapview::mapview(transect_lines, color = "gold") +
  #   # mapview::mapview(transect_lines2, color = "red") +
  #   mapview::mapview(braids, color = "dodgerblue") 
  # tmp <- only_braids %>% 
  #   dplyr::filter(!comid %in% extended_lines$hy_id)
  
  # get the extended lines for the single and multibraids
  extended_lines <- 
    dplyr::bind_rows(
            dplyr::mutate(
              singles_extend,
              tmp_id = paste0(hy_id, "_", cs_id)
            ),
            dplyr::mutate(
              multis_extend,
              tmp_id = paste0(hy_id, "_", cs_id)
              )
            ) %>% 
    dplyr::select(-braid_id, -is_multibraid)
  
  # if drop is specified, drop extended braid transects IF they cross with non braided flowlines
  if(drop) {
    
    # filter out transects that cross with non braided river flowlines
    to_drop <- sf::st_filter(
      extended_lines,
      dplyr::filter(braids, braid_id == "no_braid")
    )
    
    message("Dropping ", nrow(to_drop), " extended lines because of overlap with non braided flowlines")
    
    # drop lines that cross over non braided river segments
    extended_lines <- dplyr::filter(extended_lines, !tmp_id %in% to_drop$tmp_id)
    
  }
  
  # tmp1 <- only_braids %>% 
    # dplyr::filter(only_braids,
    #               !comid %in% c(singles_xs$hy_id, multis_xs$hy_id)
    #               )$comid
  
  
  # tmp2 <- only_braids %>% 
    # dplyr::filter(!comid %in% c(extended_lines$hy_id))
    # dplyr::filter(!comid %in%  extended_lines[extended_lines$changed, ]$hy_id)
  
  # mapview::mapv
  # extended_lines[extended_lines$changed, ]$hy_id
  # mapview::mapview(tmp1, color = "green") +
  #   # mapview::mapview(tmp1, color = "gold") +
  #   mapview::mapview(tmp2, color = "red") +
  #   mapview::mapview(braids, color = "dodgerblue") + 
  # mapview::mapview(extended_lines, color = "cyan")
  # %>% 
    # dplyr::select(-braid_id, -is_multibraid)
  
  # keep ONLY lines that were CHANGED, remove the excess braid transects (changed == FALSE)
  # extended_lines[!extended_lines$changed, ]$tmp_id
  # dplyr::filter(extended_lines, changed)
  
  # Steps below:
  # 1. filter out transect lines to tmp_id NOT IN any of the extended lines (i.e. the rest of the data/transects)
  # 2. Next Filter will remove the braided transects that were NOT part of the original transects we tried to extend,
  #       these are typically divergent flowlines (divergence == 2)
  # 3. then after filtering, bind the CHANGED extended lines back with the rest of the data
    # NOTE: we leave out the braided transects that were NOT changed/updated/extended
  # 4. drop the changed and tmp_id columns
  transect_lines <- 
    transect_lines %>% 
    dplyr::mutate(
      tmp_id = paste0(hy_id, "_", cs_id)
    ) %>% 
    dplyr::filter(!tmp_id %in% c(extended_lines$tmp_id)) %>% 
    dplyr::filter(
      !hy_id %in% dplyr::filter(only_braids,
                    !comid %in% c(singles_xs$hy_id, multis_xs$hy_id)
                    )$comid
    ) %>% 
    dplyr::bind_rows( 
      dplyr::filter(extended_lines, changed)
      ) %>% 
    dplyr::select(-changed, -tmp_id)
  
  # # filter out transects that cross with non braided river flowlines
  # only_braids
  #                         dplyr::filter(braids, braid_id == "no_braid")
  # transect_lines <- sf::st_filter(
  #                         # transect_lines2,
  #                         transect_lines,
  #                         only_braids
  #                         # dplyr::filter(braids, braid_id == "no_braid")
  #                       )
    
    # sf::st_filter(
    #   dplyr::filter(braids, braid_id == "no_braid"),
    #   transect_lines2
    # )
  # transect_lines2
    # dplyr::filter(!tmp_id %in% extended_lines[!extended_lines$changed, ]$tmp_id)

  # mapview::mapview(extended_lines, color = "green") +
  #   mapview::mapview(transect_lines, color = "gold") +
  #   mapview::mapview(transect_lines2, color = "red") +
  #   mapview::mapview(braids, color = "dodgerblue")
  # mapview::mapview(tmp, color = "cyan")
  # 
  message("MIN transect length: ", min(as.numeric(sf::st_length(transect_lines))))  
  message("MEAN transect length: ", mean(as.numeric(sf::st_length(transect_lines))))  
  message("MAX transect length: ", max(as.numeric(sf::st_length(transect_lines))))
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG:", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
  # mapview::mapview(braids, color = "dodgerblue") +
  #   mapview::mapview(multis, color = "red") +
  # mapview::mapview(singles, color = "red") +
  #   # mapview::mapview(orig_multis, color = "gold") +
  #   # mapview::mapview(orig_singles, color = "green") +
  #   mapview::mapview(singles_xs, color = "gold") +
  #   mapview::mapview(multis_xs, color = "gold") +
  #   mapview::mapview(new_multi_xs, color = "green")  +
  #   mapview::mapview(new_singles_xs, color = "green") +
  #   mapview::mapview(new_multi_xs2, color = "green")
  # 
  
}

fix_braid_transects3 <- function(net, transect_lines) {
  transect_lines = transects2
  net <- net3

  # mapview::mapview(transect_lines) + net
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  # TODO: GO fix this mistake in "find_braids", this is how multibraids should be defined and is_multibraid column
  # add number of braids in each multibraid
  braids$nbraids <- lengths(strsplit(braids$braid_id, ", "))
  
  braids <-
    braids %>% 
    dplyr::mutate(
      is_multibraid = dplyr::case_when(
        nbraids != 1 ~ TRUE,
        TRUE         ~ FALSE
        )
    ) %>% 
    dplyr::relocate(comid, braid_id, nbraids, is_multibraid)
  
  # # braids1$
  # tmp <- dplyr::filter(braids1,  braid_id %in% c("braid_13, braid_14"))
  # b13 <-  dplyr::filter(braids1, braid_id %in% c("braid_13"))
  # b14 <-  dplyr::filter(braids1,  braid_id %in% c("braid_14"))
  # mapview::mapview(tmp, color = "gold") + braids1 + mapview::mapview(b13, color = "red") +
  #   mapview::mapview(b14, color = "green")
  # braids1 %>% 
  #   dplyr::mutate(
  #     mb_id = braid_id
  #   ) %>% 
  #   dplyr::relocate(mb_id) 
  
  # # add braid_id column to network
  # braids2 <- find_braids(
  #   network   = net,
  #   return_as = "dataframe",
  #   nested    = FALSE,
  #   add       = TRUE
  # )
  
  # add a new unique identifer column
  braids <- dplyr::mutate(
                    braids,
                    new_id = 1:dplyr::n()
                  ) 
  
  # all flowlines that are part of braid
  braid_lines <- dplyr::filter(braids, braid_id != "no_braid")
  
  # flowlines that are NOT single braids and NOT multi braids
  good_to_go <- dplyr::filter(braids, !new_id %in% c(braid_lines$new_id))
  
  # # SINGLE BRAIDS
  # singles <- dplyr::filter(braids, !is_multibraid, braid_id != "no_braid")
  # 
  # # filter braids down to just multibraids
  # multis <- dplyr::filter(braids, is_multibraid)
  # 
  # # flowlines that are NOT single braids and NOT multi braids
  # good_to_go <- dplyr::filter(braids, !new_id %in% c(singles$new_id, multis$new_id))

  # make sure all flowlines are accounted for
  # if(nrow(braids) != nrow(singles) + nrow(multis) + nrow(good_to_go)){
    if(nrow(braids) != nrow(braid_lines) + nrow(good_to_go)){
    message("!!!!! MIGHT BE MISSING SOME new_ids !!!!!")
    # message("nrow(singles): ", nrow(singles))
    # message("nrow(multis): ", nrow(multis))
    message("nrow(braid_lines): ", nrow(braid_lines))
    message("nrow(good_to_go): ", nrow(good_to_go))
    message("nrow(braid_lines) + nrow(good_to_go): ", nrow(braid_lines) + nrow(good_to_go))
    # message("nrow(singles) + nrow(multis) + nrow(good_to_go): ", nrow(singles) + nrow(multis) + nrow(good_to_go))
    message("nrow(braids): ", nrow(braids))
    message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
  } else {
    message("---> ALL GOOD TO GO SO FAR! ")
  }
  
  # HANDLE SINGLE BRAIDS 
  
  # # get a dataframe with all the transects that are on single braid flowlines AND on the MAIN flowline
  # singles_xs <- get_single_braid_transects(
  #                     single_braids  = singles, 
  #                     transects      = transect_lines
  #                     )
  # 
  # # get a dataframe with all the transects that are on single braid flowlines AND on the MAIN flowline
  # multis_xs <- get_multibraid_transects(
  #                     multi_braids   = multis, 
  #                     transects      = transect_lines
  #                     )

  # get a dataframe with all the transects that are on single braid flowlines AND on the MAIN flowline
  xs <- pull_braids(
              b          = braid_lines, 
              transects  = transect_lines
            )
  
  # cnt <- xs %>% 
  #   dplyr::group_by(braid_id) %>% 
  #   dplyr::count(divergence) %>% 
  #   sf::st_drop_geometry() %>% 
  #   tidyr::pivot_wider(id_cols = "braid_id", names_from = "divergence", values_from = "n") %>% 
  #   dplyr::filter(is.na(`0`), is.na(`1`))
  #   cnt
  # dplyr::mutate(one_count = dplyr::case_when(
  #     divergence == 1
  #   ))
  
  # # mapview::mapview(net2) + 
  # mapview::mapview(net, color = "dodgerblue") +
  #   mapview::mapview(transect_lines, color = "gold") +
  #   mapview::mapview(singles, color = "green") +
  #   # mapview::mapview(singles_xs, color = "green") +
  #   mapview::mapview(multis, color = "red") +
  #   # mapview::mapview(multis_xs, color = "red") +
  #   mapview::mapview(xs, color = "gold") +
  #   mapview::mapview(braid_lines, color = "red") 
  
  for (i in 1:nrow(xs)) {
    message(i, " / ", nrow(xs))
    # x = 1
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    
    # comid of transect line
    com <- xs[i, ]$hy_id
    
    # transect line to extend out
    tline <- xs[i, ]$geometry
    
    # get all linestrings that are apart of the braid_ids of interest
    in_bid <- sapply(1:length(xs$braid_id), function(x) {
      any(strsplit(xs$braid_id[x], ", ")[[1]] %in% bids)
      # any(strsplit(multis_xs$braid_id[x], ", ")[[1]] %in% bids)
    })
    
    # # OPTION 1: flowlines within CURRENT multibraid AND NOT SELF
    # # # flowlines within overall multibraid (EXCEPT SELF)
    # others <- dplyr::filter(
    #               multis,
    #               comid %in% multis[in_bid, ]$comid & comid != com
    #               )
    
    # OPTION 2: flowlines within ANY multibraid AND NOT SELF ***(THIS IS BEST OPTION SO FAR i think)***
    # # flowlines within overall multibraid (EXCEPT SELF)
    others <- dplyr::filter(
      braid_lines,
      comid != com
    )
    
    # OPTION 3: flowlines of current braid AND ANY BRAID FLOWLINES THAT TOUCH current braid AND NOT SELF
    # flowlines within overall multibraid (EXCEPT SELF)
    # filter to flowlines that are:
    # 1. in current braid of interest (in_bid)
    # 2. is NOT the current transect of interest (tline)
    # 3. flowlines touching any of these lines
    # neighbor_braids <- 
    # others <- 
    #   multis %>% 
    #   sf::st_filter(
    #     dplyr::filter(multis, comid %in% multis[in_bid, ]$comid & comid != com), 
    #     .predicate = st_touches
    #     ) %>% 
    #   dplyr::filter(comid != com)
    
    # # original count of intersections with "others"
    # first_count <- sum(lengths(sf::st_intersects(others, tline)))
    # message("------> FIRST COUNT OF INTERSECTIONS: ", first_count)
    # if(first_count > 0) {
    #   start_counts <- dplyr::bind_rows(start_counts,  data.frame(comid = com, count = first_count))
    #   }
    
    # OPTION 1:  max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- max(
      as.numeric(
        sf::st_distance(sf::st_centroid(others), tline)
      )
    )
    # # OPTION 2: The maximum distance from the end points of each line and the transect line
    # max_dist <- max(
    #             as.numeric(
    #               sf::st_distance(
    #                 lwgeom::st_endpoint(others), 
    #                 tline)), 
    #             as.numeric(
    #               sf::st_distance(
    #                 lwgeom::st_startpoint(others),
    #                 tline
    #               )))
    
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, max(c(max_dist, 2000)), xs[i, ]$bf_width)
    
    # EXTEND OUT lines 
    # extend transect line out in both directions and find the side that interests with m
    # extend line out from HEAD side of line 
    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
    head_ext <- extend_out(
      x             = 1,
      line          = tline, 
      distances     = dist_vect,
      geoms_to_cut  = others, 
      ids           = c(com), 
      dir           = "head"
    )
    # extend line out from TAIL side of line 
    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
    tail_ext <- extend_out(
      x             = 1,
      line          = tline, 
      distances     = dist_vect,
      geoms_to_cut  = others, 
      ids           = c(com), 
      dir           = "tail"
    )
    
    # TODO CHECK which extended line should be selected when number of interesections is the same
    # IF: number of. intersection for each extended line is TIED, select the shorter of the two? NOT SURE WHAT THE CALL IS HERE
    # ELSE: return the one with more interesections (which.max)
    count_intersects <- c(lengths(sf::st_intersects(head_ext, others)), lengths(sf::st_intersects(tail_ext, others)))
    
    # create simple feature collection of head and tail extended lines
    res_geom <- sf::st_sfc(c(head_ext, tail_ext))
    
    # IF TIED
    if(count_intersects[1] == count_intersects[2]) {
      
      # if number of braid intersections is tied, return the shorter extended line
      res_geom <- res_geom[which.min(c(
        sf::st_length(head_ext),
        sf::st_length(tail_ext)
      ))
      ]
      
      direction <- c("head", "tail")[which.min(c(
        sf::st_length(head_ext),
        sf::st_length(tail_ext)
      ))]
      # otherwise, select the line with most braid intersections
    } else {
      
      # subset to line with max number of interesections
      res_geom  <- res_geom[which.max(count_intersects)]
      direction <- c("head", "tail")[which.max(count_intersects)]
      
    }
    
    # cross_idx <- ifelse(direction == "head", head_lst[[1]], tail_lst[[1]])
    
    # point that crosses over other flowline
    all_cross_pts <- sf::st_intersection(res_geom, others)
    
    # get our final crosser point by finding the all_cross_pts that is the FURTHEST from the centroid of the original transect line
    cross_pt <- all_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(tline), all_cross_pts)))
    ]
    
    # linestring start and ends, 
    end   <- lwgeom::st_endpoint(res_geom)
    start <- lwgeom::st_startpoint(res_geom)
    
    # distance from crossing point and end/start points. 
    # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
    end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
    start_dist <- as.numeric(sf::st_distance(cross_pt, start))
    
    # set distances to 0 if no crossing point is on top of the start/end
    end_dist   <- ifelse(length(end_dist) == 0, 0, end_dist)
    start_dist <- ifelse(length(start_dist) == 0, 0,  start_dist)
    
    # check to make sure that extending the line made an intersection
    if (end_dist == 0 & start_dist == 0) {
      message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      message("--- CONTINUING TO NEXT TRANSECT ---")
      next
    }
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    ext_dist <- (xs[i, ]$cs_widths/2) - min(end_dist, start_dist)
    
    # generate final extended line
    res_geom <- st_extend_line(res_geom,  ext_dist, direction)
    # mapview::mapview(res_geom) + xs + braid_lines + braids + transect_lines + tline
    # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
    if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)) {
      
      # message("ADDING EXTENDED TRANSECT ", i, " TO RES")
      # res <- sf::st_sfc(c(res, res_geom))
      # multis_xs[i,]$geometry <- res_geom
      xs[i,]$geometry <- sf::st_geometry(res_geom)
      
    }
    message("==============")
    
  }
  
  # mapview::mapview(net2) +
  mapview::mapview(net, color = "dodgerblue") +
    mapview::mapview(transect_lines, color = "green") +
    # mapview::mapview(braid_lines, color = "green") +
  # mapview::mapview(singles, color = "green") +
    # mapview::mapview(singles_xs, color = "green") +
  # mapview::mapview(multis, color = "red") +
    # mapview::mapview(multis_xs, color = "red") +
    mapview::mapview(xs, color = "red") +
    mapview::mapview(braid_lines, color = "gold")
  
}

rectify_singlebraid_transects <- function(single_braids, single_transects) {
  message("Extending transects of single braids...")
  
  # single_braids <- singles
  # single_transects <- singles_xs

  # flag determining whether transect should be replaced
  single_transects$changed <- FALSE
  # i = 44
  for (i in 1:nrow(single_transects)) {
  # for (i in 1:43) {
    # message("i: ", i, "/", nrow(single_transects))
    
    # if (i == 22) {
    #   stop()
    # }
    
    # braid transect line
    line <- single_transects[i, ]$geometry
    
    # braid ID
    bid <-  single_transects[i, ]$braid_id
    
    # transect COMID
    com <-  single_transects[i, ]$hy_id
    
    # braid of interest
    boi <-  dplyr::filter(single_braids, braid_id == bid)
    # mapview::mapview(line, color = "green") +
    #   mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(boi, color = "red") 
    # tmpp <- braids %>% dplyr::filter(braid_id )
    
    
    # max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- as.numeric(
                        max(
                          sf::st_distance(  
                              dplyr::filter(boi, !comid %in% com), 
                              line
                            )
                          )
                        )
    
    if(as.numeric(max(
      sf::st_distance(  
        dplyr::filter(boi, !comid %in% com), 
        line
      )
    )) == -Inf) {
      message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
      message("---> !!!!! MAX INFINITY !!!!COMID: ", com)
      message("INDEX: ", i)
      message("!!!!!!!!!!!!!!!!!!!!!!!!!!!!")
    }
    # if max(numeric(0), na.rm = FALSE) == -Inf
    # sequence from 0 to the max possible extension distance 
    distances <- seq(0, max(c(max_dist, 2000)), single_transects[i, ]$bf_width)
    
    # find the distance it takes from the HEAD of the transect string to reach another part of the braid
    head <- binary_search_distance(
      distances      = distances, 
      line           = line,
      geoms_to_cut   = dplyr::filter(boi, !comid %in% com),
      direction      = "head"
    )
    # find the distance it takes from the TAIL of the transect string to reach another part of the braid
    tail <- binary_search_distance(
      distances         = distances, 
      line              = line,
      geoms_to_cut      = dplyr::filter(boi, !comid %in% com),
      direction         = "tail"
    )
    
    # length(distances)
    # if(head > length(distances)) { head = head - 1 }
    # if(tail > length(distances)) { tail = tail - 1 }
    
    # head_line <- st_extend_line(line, distances[head-1], 'head')
    # tail_line <- st_extend_line(line, distances[tail-1], 'tail')
    # mapview::mapview(boi) + line + head_line + tail_line
    
    # pick the extension direction that had the soonest interesection with the rest of the braid
    if(which.min(c(head, tail)) == 1) { 
      
      dir = "head"
      cross_idx <- head
      
    } else {
      
      dir = "tail"
      cross_idx <- tail
      
    }
    
    # the index of the intersection point is out of bounds, set it to the max distance value
    if(cross_idx > length(distances)) {
      
      cross_idx = cross_idx - 1
    }
    
    # primary transect line, still needs other side to be fixed/cleaned up 
    crosser <- st_extend_line(line, distances[cross_idx], dir)
    
    # point that crosses over other flowline
    cross_pt <- sf::st_intersection(crosser, dplyr::filter(boi, !comid %in% com))
    # sf::st_intersection(crosser, boi)
    
    # linestring start and ends, 
    end   <- lwgeom::st_endpoint(crosser)
    start <- lwgeom::st_startpoint(crosser)
    
    # distance from crossing point and end/start points. 
    # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
    end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
    start_dist <- as.numeric(sf::st_distance(cross_pt, start))
    
    # set distances to 0 if no crossing point is on top of the start/end
    end_dist   <- ifelse(length(end_dist) == 0, 0, end_dist)
    start_dist <- ifelse(length(start_dist) == 0, 0, start_dist)
    
    if (end_dist == 0 & start_dist == 0) {
      # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      next
    }
    
    # END_PT------- CROSSER_PT ----------------------------START_PT
    # |-----------------|
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    ext_dist <- (single_transects[i, ]$cs_widths/2) - min(end_dist, start_dist)
    
    # generate final extended line
    res <- st_extend_line(line, distances[cross_idx] + ext_dist, dir)
    
    if (lengths(sf::st_intersects(res, single_transects[-i,])) == 0) {
      
      # insert updated geoemtry
      single_transects[i, ]$geometry <- sf::st_geometry(res)
      
      # mapview::mapview(single_transects, color = "red")  +
      #   mapview::mapview(res, color = "green")  +
      #   mapview::mapview(line, color = "green")  +
      #   mapview::mapview(braids, color = "dodgerblue")
        
      # set change flag to TRUE
      single_transects[i, ]$changed <- TRUE
    }
    
    # message('==============')
    # 1079081
    # mapview::mapview(ms_xs) + boi
    # mapview::mapview(line, color = "red") +
    #   mapview::mapview(crosser, color = "dodgerblue") +
    #   mapview::mapview(res, color = "green") +
    #   # mapview::mapview(cross_pt, col.regions = "red") +
    #   start + end  + boi + ms_xs + transects + singles
  }
  
  return(single_transects)
  
}

rectify_multibraid_transects <- function(multibraids, multi_transects) {
  message("Extending transects of multibraids...")
  
  # flag determining whether transect should be replaced
  multi_transects$changed <- FALSE
  
  max_dist <- max(
    as.numeric(
      multi_transects$cs_widths * 4
    )
  )
  
  for (i in 1:nrow(multi_transects)) {
    # message(i, " / ", nrow(multi_transects))
    
    # braid IDs of interest
    bids <- strsplit(multi_transects[i, ]$braid_id, ", ")[[1]]
    
    # comid of transect line
    com <- multi_transects[i, ]$hy_id
    
    # transect line to extend out
    tline <- multi_transects[i, ]$geometry
    
    # get all linestrings that are apart of the braid_ids of interest
    in_bid <- sapply(1:length(multibraids$braid_id), function(x) {
      any(strsplit(multibraids$braid_id[x], ", ")[[1]] %in% bids)
      # any(strsplit(multi_transects$braid_id[x], ", ")[[1]] %in% bids)
    })
    
    # # OPTION 1: flowlines within CURRENT multibraid AND NOT SELF
    # # # flowlines within overall multibraid (EXCEPT SELF)
    # others <- dplyr::filter(
    #               multibraids,
    #               comid %in% multibraids[in_bid, ]$comid & comid != com
    #               )
    
    # OPTION 2: flowlines within ANY multibraid AND NOT SELF ***(THIS IS BEST OPTION SO FAR i think)***
    # # flowlines within overall multibraid (EXCEPT SELF)
    others <- dplyr::filter(
      multibraids,
      comid != com
    )
    
    # OPTION 3: flowlines of current braid AND ANY BRAID FLOWLINES THAT TOUCH current braid AND NOT SELF
    # flowlines within overall multibraid (EXCEPT SELF)
    # filter to flowlines that are:
    # 1. in current braid of interest (in_bid)
    # 2. is NOT the current transect of interest (tline)
    # 3. flowlines touching any of these lines
    # neighbor_braids <- 
    # others <- 
    #   multibraids %>% 
    #   sf::st_filter(
    #     dplyr::filter(multibraids, comid %in% multibraids[in_bid, ]$comid & comid != com), 
    #     .predicate = st_touches
    #     ) %>% 
    #   dplyr::filter(comid != com)
    
    # # original count of intersections with "others"
    # first_count <- sum(lengths(sf::st_intersects(others, tline)))
    # message("------> FIRST COUNT OF INTERSECTIONS: ", first_count)
    # if(first_count > 0) {
    #   start_counts <- dplyr::bind_rows(start_counts,  data.frame(comid = com, count = first_count))
    #   }
    
    # OPTION 1:  max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    # max_dist <- max(
    #   as.numeric(
    #     sf::st_distance(sf::st_centroid(others), tline)
    #   )
    # )
    # multis_xs$cs_widths * 2
    # (multi_transects[i, ]$cs_widths/2)
    # max_dist <- max(
    #               as.numeric(
    #                 multi_transects$cs_widths * 2.5
    #               )
    #             )
    # # OPTION 2: The maximum distance from the end points of each line and the transect line
    # max_dist <- max(
    #             as.numeric(
    #               sf::st_distance(
    #                 lwgeom::st_endpoint(others), 
    #                 tline)), 
    #             as.numeric(
    #               sf::st_distance(
    #                 lwgeom::st_startpoint(others),
    #                 tline
    #               )))
    
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, max_dist, multi_transects[i, ]$bf_width)
    # dist_vect <- seq(0, max(c(max_dist, 2000)), multi_transects[i, ]$bf_width)
    
    # EXTEND OUT lines 
    # extend transect line out in both directions and find the side that interests with m
    # extend line out from HEAD side of line 
    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
    head_ext <- extend_out2(
      x             = 1,
      line          = tline, 
      distances     = dist_vect,
      geoms_to_cut  = others, 
      ids           = c(com), 
      dir           = "head"
    )
    
    # extend line out from TAIL side of line 
    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
    tail_ext <- extend_out2(
      x             = 1,
      line          = tline, 
      distances     = dist_vect,
      geoms_to_cut  = others, 
      ids           = c(com), 
      dir           = "tail"
    )
    
    # TODO CHECK which extended line should be selected when number of interesections is the same
    # IF: number of. intersection for each extended line is TIED, select the shorter of the two? NOT SURE WHAT THE CALL IS HERE
    # ELSE: return the one with more interesections (which.max)
    count_intersects <- c(lengths(sf::st_intersects(head_ext, others)), lengths(sf::st_intersects(tail_ext, others)))
    
    # create simple feature collection of head and tail extended lines
    res_geom <- sf::st_sfc(c(head_ext, tail_ext))
    
    # IF TIED
    if(count_intersects[1] == count_intersects[2]) {
      
      # if number of braid intersections is tied, return the shorter extended line
      res_geom <- res_geom[which.min(c(
        sf::st_length(head_ext),
        sf::st_length(tail_ext)
      ))
      ]
      
      direction <- c("head", "tail")[which.min(c(
        sf::st_length(head_ext),
        sf::st_length(tail_ext)
      ))]
      # otherwise, select the line with most braid intersections
    } else {
      
      # subset to line with max number of interesections
      res_geom  <- res_geom[which.max(count_intersects)]
      direction <- c("head", "tail")[which.max(count_intersects)]
      
    }
    
    # cross_idx <- ifelse(direction == "head", head_lst[[1]], tail_lst[[1]])
    
    # point that crosses over other flowline
    all_cross_pts <- sf::st_intersection(res_geom, others)
    
    # get our final crosser point by finding the all_cross_pts that is the FURTHEST from the centroid of the original transect line
    cross_pt <- all_cross_pts[
      which.max(as.numeric(sf::st_distance(sf::st_centroid(tline), all_cross_pts)))
    ]
    
    # linestring start and ends, 
    end   <- lwgeom::st_endpoint(res_geom)
    start <- lwgeom::st_startpoint(res_geom)
    
    # distance from crossing point and end/start points. 
    # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
    end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
    start_dist <- as.numeric(sf::st_distance(cross_pt, start))
    
    # set distances to 0 if no crossing point is on top of the start/end
    end_dist   <- ifelse(length(end_dist) == 0, 0, end_dist)
    start_dist <- ifelse(length(start_dist) == 0, 0,  start_dist)
    
    # check to make sure that extending the line made an intersection
    if (end_dist == 0 & start_dist == 0) {
      # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      next
    }
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    ext_dist <- (multi_transects[i, ]$cs_widths/2) - min(end_dist, start_dist)
    
    # generate final extended line
    res_geom <- st_extend_line(res_geom,  ext_dist, direction)
    
    # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
    if(!any(lengths(sf::st_intersects(res_geom, multi_transects[-i,])) > 0)){
      
      # message("ADDING EXTENDED TRANSECT ", i, " TO RES")
      # res <- sf::st_sfc(c(res, res_geom))
      # multi_transects[i,]$geometry <- res_geom
      # sf::st_length(sf::st_geometry(res_geom))
      # sf::st_length( multis_xs[1,]$geometry )
      
      # insert updated geometry
      multi_transects[i,]$geometry <- sf::st_geometry(res_geom)
      
      # set change flag to TRUE
      multi_transects[i, ]$changed <- TRUE
      
    }
    # message("================")
  }
  
  return(multi_transects)
}


fix_multibraid_transects1 <- function(net, transect_lines) {
  
  # transect_lines = transects
  # net <- net3
  
  # mapview::mapview(transect_lines) + net
  
  # keep track of the CRS of the input to retransform return 
  start_crs <- sf::st_crs(net, parameters = T)$epsg
  
  message("Start CRS: ", start_crs)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  transect_lines <- sf::st_transform(transect_lines, 5070) 
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    add       = TRUE
  )
  
  # filter braids down to just multibraids
  multis <- dplyr::filter(braids, is_multibraid)
  
  # multis$braid_id
  # multis %>% dplyr::group_by(braid_id)
  
  # add number of braids in each multibraid
  multis$nbraids <- lengths(strsplit(multis$braid_id, ", "))
  
  # filter multibraids to braid_ids with more than 1 braid_id
  # multis <- dplyr::filter(multis, nbraids != 1)

  # multis %>%
  #   dplyr::group_by(braid_id)
  # # multis <- 
  #   multis %>%
  #   dplyr::mutate(
  #     nbraids = length(strsplit(braid_id, ", ")[1])
  #   ) %>% 
  #   dplyr::filter(nbraids != 1)
  # single braids on the ACTUAL main stem
  main_multis <-
    multis %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(divergence == 0) %>% 
    dplyr::ungroup()
  
  # determine the remaining braids that did NOT have a divergence flag of 0, use divergence == 1 as mainstem
  div_multis <-
    multis %>% 
    dplyr::filter(!braid_id %in% main_multis$braid_id) %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(
      divergence == 1
    ) %>% 
    dplyr::ungroup()
  
  # Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
  # Mainstem single braid cross sections (ms_xs)
  main_xs <-
  # multis_xs <-
    transect_lines %>% 
    dplyr::filter(hy_id %in% main_multis$comid) %>% 
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          main_multis, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) 
  
  # # # Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
  # # # Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
  div_xs <-
    transect_lines %>%
    dplyr::filter(hy_id %in% div_multis$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          div_multis, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )
  # dplyr::left_join(  
  #   dplyr::filter(transect_lines, hy_id %in% div_singles$comid),
  #   sf::st_drop_geometry(
  #     dplyr::select(div_singles, comid, braid_id, is_multibraid)
  #     ), by = c("hy_id" = "comid"))
  
  # the cross sections for all mainstem (and divergence mainstem (i.e. divergence == 1)) MULTIBRAIDS in network
  multis_xs <- dplyr::bind_rows(main_xs, div_xs)
  
  # # arrange cross section lines by total downstream area sqkm (i.e. order from upstream to downstream)
  # multis_xs <-
  #   multis_xs %>%
  #   dplyr::group_by(hy_id) %>%
  #   dplyr::arrange(totdasqkm) %>% 
  #   dplyr::ungroup()
  
  # mapview::mapview(multis_xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(multis, color = "dodgerblue") 
  # mapview::mapview(res, color = "red")

  # # empty simple feature collection to stash new transect lines
  # res <- sf::st_sfc()

  # start_counts <- data.frame()
  
  # We iterate through each transect line in `multis_xs` and extend the transect line out in both directions 
  # to try and cross the rest of the neighboring braided flowlines

  for (i in 1:nrow(multis_xs)) {
    # message(i, " / ", nrow(multis_xs))
    
    # braid IDs of interest
    bids <- strsplit(multis_xs[i, ]$braid_id, ", ")[[1]]

    # comid of transect line
    com <- multis_xs[i, ]$hy_id
    
    # transect line to extend out
    tline <- multis_xs[i, ]$geometry
    
    # get all linestrings that are apart of the braid_ids of interest
    in_bid <- sapply(1:length(multis$braid_id), function(x) {
      any(strsplit(multis$braid_id[x], ", ")[[1]] %in% bids)
      # any(strsplit(multis_xs$braid_id[x], ", ")[[1]] %in% bids)
    })
    
    # # OPTION 1: flowlines within CURRENT multibraid AND NOT SELF
    # # # flowlines within overall multibraid (EXCEPT SELF)
    # others <- dplyr::filter(
    #               multis,
    #               comid %in% multis[in_bid, ]$comid & comid != com
    #               )
    
    # OPTION 2: flowlines within ANY multibraid AND NOT SELF ***(THIS IS BEST OPTION SO FAR i think)***
    # # flowlines within overall multibraid (EXCEPT SELF)
    others <- dplyr::filter(
                      multis,
                      comid != com
                    )
    
    # OPTION 3: flowlines of current braid AND ANY BRAID FLOWLINES THAT TOUCH current braid AND NOT SELF
    # flowlines within overall multibraid (EXCEPT SELF)
    # filter to flowlines that are:
    # 1. in current braid of interest (in_bid)
    # 2. is NOT the current transect of interest (tline)
    # 3. flowlines touching any of these lines
    # neighbor_braids <- 
    # others <- 
    #   multis %>% 
    #   sf::st_filter(
    #     dplyr::filter(multis, comid %in% multis[in_bid, ]$comid & comid != com), 
    #     .predicate = st_touches
    #     ) %>% 
    #   dplyr::filter(comid != com)
    
    # # original count of intersections with "others"
    # first_count <- sum(lengths(sf::st_intersects(others, tline)))
    # message("------> FIRST COUNT OF INTERSECTIONS: ", first_count)
    # if(first_count > 0) {
    #   start_counts <- dplyr::bind_rows(start_counts,  data.frame(comid = com, count = first_count))
    #   }

    # OPTION 1:  max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- max(
                  as.numeric(
                    sf::st_distance(sf::st_centroid(others), tline)
                    )
                  )
    # # OPTION 2: The maximum distance from the end points of each line and the transect line
    # max_dist <- max(
    #             as.numeric(
    #               sf::st_distance(
    #                 lwgeom::st_endpoint(others), 
    #                 tline)), 
    #             as.numeric(
    #               sf::st_distance(
    #                 lwgeom::st_startpoint(others),
    #                 tline
    #               )))
    
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, max(c(max_dist, 2000)), multis_xs[i, ]$bf_width)
    
    # EXTEND OUT lines 
    # extend transect line out in both directions and find the side that interests with m
    # extend line out from HEAD side of line 
    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
    head_ext <- extend_out(
                    x             = 1,
                    line          = tline, 
                    distances     = dist_vect,
                    geoms_to_cut  = others, 
                    ids           = c(com), 
                    dir           = "head"
                    )
    # extend line out from TAIL side of line 
    # the line will extend out until it has gone "max_dist" AND all the possible flowlines have been intersected with 
    tail_ext <- extend_out(
                    x             = 1,
                    line          = tline, 
                    distances     = dist_vect,
                    geoms_to_cut  = others, 
                    ids           = c(com), 
                    dir           = "tail"
                  )
  
    # TODO CHECK which extended line should be selected when number of interesections is the same
    # IF: number of. intersection for each extended line is TIED, select the shorter of the two? NOT SURE WHAT THE CALL IS HERE
    # ELSE: return the one with more interesections (which.max)
    count_intersects <- c(lengths(sf::st_intersects(head_ext, others)), lengths(sf::st_intersects(tail_ext, others)))
    
    # create simple feature collection of head and tail extended lines
    res_geom <- sf::st_sfc(c(head_ext, tail_ext))
    
    # IF TIED
    if(count_intersects[1] == count_intersects[2]) {
      
      # if number of braid intersections is tied, return the shorter extended line
      res_geom <- res_geom[which.min(c(
                                  sf::st_length(head_ext),
                                  sf::st_length(tail_ext)
                                ))
                                ]
      
      direction <- c("head", "tail")[which.min(c(
                                              sf::st_length(head_ext),
                                              sf::st_length(tail_ext)
                                            ))]
    # otherwise, select the line with most braid intersections
    } else {
      
      # subset to line with max number of interesections
      res_geom  <- res_geom[which.max(count_intersects)]
      direction <- c("head", "tail")[which.max(count_intersects)]
      
    }
    
    # cross_idx <- ifelse(direction == "head", head_lst[[1]], tail_lst[[1]])
    
    # point that crosses over other flowline
    all_cross_pts <- sf::st_intersection(res_geom, others)
    
    # get our final crosser point by finding the all_cross_pts that is the FURTHEST from the centroid of the original transect line
    cross_pt <- all_cross_pts[
                      which.max(as.numeric(sf::st_distance(sf::st_centroid(tline), all_cross_pts)))
                      ]
    
    # linestring start and ends, 
    end   <- lwgeom::st_endpoint(res_geom)
    start <- lwgeom::st_startpoint(res_geom)
    
    # distance from crossing point and end/start points. 
    # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
    end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
    start_dist <- as.numeric(sf::st_distance(cross_pt, start))
    
    # set distances to 0 if no crossing point is on top of the start/end
    end_dist   <- ifelse(length(end_dist) == 0, 0, end_dist)
    start_dist <- ifelse(length(start_dist) == 0, 0,  start_dist)
    
    # check to make sure that extending the line made an intersection
    if (end_dist == 0 & start_dist == 0) {
      # message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      # message("--- CONTINUING TO NEXT TRANSECT ---")
      next
    }
    
    # END_PT--------------- CROSSER_PT ----------------------------START_PT
    # |------------------------|
    # ^^^^ SMALLER SECTION ^^^^
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    ext_dist <- (multis_xs[i, ]$cs_widths/2) - min(end_dist, start_dist)
    
    # generate final extended line
    res_geom <- st_extend_line(res_geom,  ext_dist, direction)
    
    # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
    if(!any(lengths(sf::st_intersects(res_geom, multis_xs[-i,])) > 0)){
      
      # message("ADDING EXTENDED TRANSECT ", i, " TO RES")
      # res <- sf::st_sfc(c(res, res_geom))
      # multis_xs[i,]$geometry <- res_geom
      multis_xs[i,]$geometry <- sf::st_geometry(res_geom)
      
    }
    
  }
  
  # replace transect lines with updated 
  transect_lines <- dplyr::bind_rows(
                        sf::st_as_sf(
                          dplyr::left_join(
                            sf::st_drop_geometry(
                              dplyr::filter(transect_lines, hy_id %in% multis_xs$hy_id)
                            ),
                            dplyr::select(multis_xs, geometry, hy_id, cs_id),
                            by = c("hy_id", "cs_id")
                          )
                        ),
                        dplyr::filter(transect_lines, !hy_id %in% multis_xs$hy_id)
                      )
  
  # transform CRS back to input CRS
  if(start_crs != 5070) {
    message("Transforming CRS back to EPSG:", start_crs)
    transect_lines <- sf::st_transform(transect_lines, start_crs)
  }
  # dplyr::select(
  #   dplyr::filter(transect_lines, hy_id %in% multis_xs$hy_id),
  #   -geometry
  # )
  # dplyr::select(multis_xs, geometry, hy_id, cs_id)
  # # transect_lines <- dplyr::bind_rows(
  # #                           dplyr::filter(transect_lines, !hy_id %in% multis_xs$hy_id),
  # #                           dplyr::select(
  # #                             multis_xs, -braid_id, -is_multibraid
  # #                           )
  # #                         )
  #   dplyr::filter(transect_lines, !hy_id %in% multis_xs$hy_id)
    # # # final check on all intersections 
    # final_cross_pts <- sf::st_intersection(res_geom, multis)
    # 
    # # final starting and ending points of res_geom
    # out_end   <- lwgeom::st_endpoint(res_geom)
    # out_start <- lwgeom::st_startpoint(res_geom)
    # # 
    # # # comparing the distance from the END of the final res_geom and its nearest intersection (should be bankful width/2 length)
    # end_width <- as.numeric(
    #               sf::st_distance(
    #                 out_end,
    #                 final_cross_pts[sf::st_nearest_feature(out_end, final_cross_pts)]
    #                 )
    #               )
    # # # comparing the distance from the START of the final res_geom and its nearest intersection (should be bankful width/2 length)
    # start_width <- as.numeric(
    #                   sf::st_distance(
    #                     out_start,
    #                     final_cross_pts[sf::st_nearest_feature(out_start, final_cross_pts)]
    #                     )
    #                 )
    # # cross_section width to compare distances on outsides of last intersections
    # cs <- (multis_xs[i, ]$cs_widths/2)
    # # check if distancce from last flowline intersection to the end of the transect line is approximately equal to the cs_width/2
    # # Arbitrairly selected a 2% threshold requirement
    # # if the distance is NOT approximately equal, the approx_equal function will return the amount it is off by
    # e <- approx_equal(end_width, cs, cs, 2)
    # s <- approx_equal(start_width, cs, cs, 2)
    # 
    # # if start width is off by more than 2%, then extend the line out
    # if(!isTRUE(s)) {
    #   message("START IS NOT EQUAL TO REQUIRED CS_WIDTH: ", cs)
    #   message("off by: ", s)
    #   # extend in the opposite direction that we originally went in
    #   switch_dir <- ifelse(direction == "head", "tail", "head")
    #   # extend line out by s
    #   res_geom <- st_extend_line(res_geom,  s, switch_dir)
    # }
    # # if end width is off by more than 2%, then extend the line out
    # if(!isTRUE(e)) {
    #   message("END IS NOT EQUAL TO REQUIRED CS_WIDTH: ", cs)
    #   message("off by: ", e)
    #   # extend in the opposite direction that we originally went in
    #   switch_dir <- ifelse(direction == "head", "tail", "head")
    #   # extend line out by e
    #   res_geom <- st_extend_line(res_geom,  e, switch_dir)
    # }

    # # first transect, put it as the first geometry in "res"
    # if(length(res) == 0) {
    #   res <- sf::st_sfc(c(res_geom))
    # } else {
    #   res <- sf::st_sfc(c(res, res_geom))
    # }
    # else {
      
    # # if there are already geometries in res (res length greater than 0)
    # if(length(res) > 0) {
      
      # # if there are NOT ANY intersections with the current extended transect (res_geom) and the rest of the updated geoms (res)
      # if(!any(lengths(sf::st_intersects(res, res_geom)) > 0)) {
      
      # if there are NOT ANY intersections with the current extended transect (res_geom) and the rest of the updated geoms (res) 
      # AND NOT ANY interesections with original `multi_xs` transects
      # if(!any(lengths(sf::st_intersects(res, res_geom)) > 0)){
      # if(!any(lengths(sf::st_intersects(res, res_geom)) > 0) & !any(lengths(sf::st_intersects(res_geom, multis_xs[-i,])) > 0)){
      # if(!any(lengths(sf::st_intersects(res_geom, multis_xs[-i,])) > 0)){
      #     
      #   message("ADDING EXTENDED TRANSECT ", i, " TO RES")
      #   # res <- sf::st_sfc(c(res, res_geom))
      #   # multis_xs[i,]$geometry <- res_geom
      #   multis_xs[i,]$geometry <- sf::st_geometry(res_geom)
      # 
      # } else {
      #   message("----> NOT ADDING EXTENDED TRANSECT ", i, " TO RES")
      # }
    
    # if(length(res) == 0) {
    #   res <- sf::st_sfc(c(res_geom))
    # } else {
    #   res <- sf::st_sfc(c(res, res_geom))
    # }
    # message('==============')

    # LEAVING OFF AT 07/14:
    # TO CONTINUE:
      # as each row of this multis_xs dataframe is iterated through
      # and the new multibraid transects lines are created and the correct one is selected (code above)
    # Make sure to do the following when picking back up:
    # 1. Extend the "res_geom" line out so that it is 1/2 the bankful widths length from the last braid line 
          # it crosses over,  out to the 1/2 bankful widths value (when the transect line crosses the outter most 
          #                                                        line on both sides of the braid, the line should
                                                                   # extend out a distance of
          #                                                        1/2 bf_width on both sides
    # 2. Update/Replace each updated transect line with the respective transect line it started out as.
    # Only do 2. IF the updated transect line does NOT interesect with any of the current transect geometries
    # -----> OR we might just need to check and make sure that the updated transect line doe
                # NOT intersect with any of the NEWLY FORMED (EXTENDED) transect lines, NOT SURE YET
    
    # (replace small geometry with new extended one)
    
    # ---- OLD LOGIC FOR CHECKING THE NUMBER OF INTERSECTIONS/WHICH EXTENDED LINE TO USE AS THE UPDATED LINE ----
    # # if ANY of the extended lines do NOT interesect any lines at all (count_intersects == 0), select the one that DOES intersect lines
    # # AND as long as atleast one of the extended lines does some interesection (AND NOT ALL intersection counts equal to 0)
    # if(any(count_intersects == 0) & !all(count_intersects == 0)) {
    #   res_geom <- res_geom[count_intersects != 0]  
    # # if the number of interesections is tied
    # } else if(count_intersects[1] == count_intersects[2]) {
    #   res_geom <- res_geom[which.min(c(sf::st_length(head_ext),
    #                             sf::st_length(tail_ext)))]
    #   # otherwise select the line with the max number of intersections
    # } else {
    #   # subset to line with max number of interesections
    #   res_geom <- res_geom[which.max(count_intersects)]
    # }
  # }
  
}

# start_counts
# 
# already <- 
#   multis_xs %>% 
#   dplyr::filter(hy_id %in% start_counts$comid)
# mapview::mapview(multis, color = "dodgerblue") +
#   mapview::mapview(multis_xs, color = "red") +
#   mapview::mapview(transect_lines, color = "cyan") +
#   # mapview::mapview(others, color = "red") +
#   # mapview::mapview(res, color = "green") +
#   # mapview::mapview(res_geom, color = "green") + 
#   mapview::mapview(already, color = "green")
# 1078587

# *********************************
# ----- FINAL GOOD TO GO WORK -----
# *********************************

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects_latest2 <- function(
    net, 
    transect_lines,
    braid_threshold = NULL
) {
  
  # transect_lines <-  transects_nofix
  # net <- net3
  # braid_threshold = NULL
  # braid_threshold = 25000
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  if(all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  message("Fixing braid transects...")
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <-
  braids <-  
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    # dplyr::group_by(comid) %>% 
    # dplyr::mutate(ncomid = n()) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # view data on map
  # mapview::mapview(not_braids, color = "dodgerblue") +
  # mapview::mapview(only_braids, color = "red") 
  
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x         = braids, 
      originals = not_braids, 
      threshold = braid_threshold,
      verbose   = TRUE
    )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
  }
  
  # # unique braid_ids/COMIDs
  # ubraids <- unique(only_braids$braid_id)
  # ucoms <- unique(only_braids$comid)
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) %>% 
    # dplyr::filter(divergence == 0)
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(-totdasqkm)
  
  # keep track of all original crossections
  all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
  
  # column to store the relative position within the braid of the flowline we're on 
  xs$relative_position <- NA
  
  # flag determining whether transect should/has been replaced
  xs$changed <- FALSE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # empty columns to store number of head/tail intersections
  xs$head_cuts     <- NA
  xs$tail_cuts     <- NA
  
  # empty columns to store distance needed to extend from head/tail of line
  xs$head_distance <- NA
  xs$tail_distance <- NA
  
  # data.table::data.table(xs)[1, ]
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    
    message("===== NO 'xs' transect lines =====")
    message("===== returning original data =====")
    
    return(transect_lines)
    
  } else {
    message("===== ", nrow(xs) , " 'xs' transect lines =====")
    # message("===== returning original data =====")
  }
  
  # braids %>% 
  #   geos_make_collection() %>% 
  #   geos_unary_union() %>% 
  #   st_as_sfc()
  # braids %>% 
  #   dplyr::mutate(
  #     geometry  =  geos::geos_geometry(.)
  #   ) %>% 
  #   dplyr::relocate(geometry2)
  # geos::as_geos_geometry(braids )
  
  # braids$geometry <-  geos::geos_geometry(braids$geometry)
  # mapview::mapview(braids, color = "dodgerblue") +
  #   mapview::mapview(xs, color = "red") +
  # mapview::mapview(xs[i, ], color = "green")
  
  # Loop through every single cross section and determine:
  # 1. its relative position
  # 2. how far to extend the line
  # 3. in what order should transects be extended, 
  # 4. in what direction to extend the transect
  for(i in 1:nrow(xs)) {
    # for(i in 1:17) {
    # message("i: ", i, "/", nrow(xs))
    # i = 18 
    # # transect line
    # tline <- xs[i, ]$geometry
    # i = 18
    # curr <- xs[i, ]
    
    # comid of transect line
    com <- xs$hy_id[i]
    
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    
    # get neighboring braid ID for our current braid
    neighbor_braids <- get_neighbor_braids(x = braids, ids = bids, only_unique = T)
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
                      braids,
                      braid_id %in% neighbor_braids,
                      comid != com
                    )
    
    # # convert "others" geometry to geos_geometry
    # others$geometry <- geos::as_geos_geometry(others$geometry)
    # 
    # geos::as_geos_geometry(others$geometry) %>% sf::st_geometry()
    # 
    # others$geometry <- geos::as_geos_geometry(others$geometry)
    # 
    # sf::st_geometry(others$geometry)
    
    # Error in st_geometry.sf(x) : 
    
    # attr(obj, "sf_column") does not point to a geometry column.
    # Did you rename it, without setting st_geometry(obj) <- "newname"?
    
    # INPUTS INTO NEW AUGMENT TRANSECTS DF FUNCTION
    # cross_section = xs[i, ]
    # curr = xs[i, ]
    # geoms_to_cut <- others
    # max_distance = NULL
    # by = 1
    
    # tree <- geos::geos_strtree(braids[-81, ])
    # gg <- geos::as_geos_geometry(xs[1, ])
    # # tree[1]
    # 
    # geos::geos_strtree_query(tree, gg)
    # ttmp <- braids[81, ]
    # mapview::mapview(ttmp) + xs[1, ]
    # tree
    
    # other_meta <- sf::st_drop_geometry(others)
    # geoms_to_cut <- geos::as_geos_geometry(others$geometry)
    
    # geoms_to_cut  = others
    extend_maps <- geos_augment_transect(
      cross_section = xs[i, ],
      geoms_to_cut  = geos::as_geos_geometry(others$geometry),
      geom_ids      = others$comid,
      max_distance  = NULL, 
      by            = 1, 
      as_df         = FALSE,
      carry_geom    = FALSE
    )
    
    # extend_maps$head$as_list()
    position <- extend_maps$head$get("position")
    
    # message("----> position: ", position)
    
    # if(is.na(position)) {
    #   message("!!!!!! !!!!!!!!!!!!!!!!! !!!!!!!!!")
    #   message("!!!!!! FOUND AN NA POSITION VALUE !!!!!!!!!")
    #   message("!!!!!! !! iter: ", i ," !!!!!!!!!")
    # }
    
    # if a flowline on the inner portion of a braid, make extension and insert
    if(position == "inner") {
      # message("Extending ", i, " and checking if valid replacement...")
      # extend line out by total distance key values in head and tail maps
      res_geom <- geos_extend_transects(
                      starter_line   = geos::as_geos_geometry(xs$geometry[i]),
                      head_distance  = extend_maps$head$get("total_distance"),
                      tail_distance  = extend_maps$tail$get("total_distance"),
                      extra_distance = xs$cs_widths[i]/2
                    )
      
      # mapview::mapview(others) + braids + res_geom + not_braids
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
      if(
        # !any(
        #   lengths(
        #     sf::st_intersects(sf::st_as_sf(res_geom),
        #                     dplyr::filter(xs[-i,], changed)
        #                     )
        #   ) > 0)
        !geos::geos_intersects_any(
          res_geom,
          geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
          )
        ) {
        
        # !geos::geos_intersects_any(
        #   res_geom,
        #   geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        # )
        # geos::geos_intersects(
        #   res_geom,
        #   geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        # )
        # geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        # if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
        # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # updatem geometry with new, extended cross section
        xs$geometry[i] <- sf::st_geometry(
                                  sf::st_as_sf(res_geom)
                                  )
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        # xs[i, ]$changed <- TRUE
      }
      
      # update relative position column
      xs$relative_position[i] <- extend_maps$head$get("position")
      
      # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
      xs$pending[i] <- extend_maps$head$get("pending")
      
      # update head/tail distances values in dataframe w/ values from head/tail hashmaps
      xs$head_distance[i] <- extend_maps$head$get("total_distance")
      xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
      
      # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
      xs$head_cuts[i] <- extend_maps$head$get("count")
      xs$tail_cuts[i] <- extend_maps$tail$get("count")
      
      
    } else {
      # message("Postpone processing: ", i)
      
      # update relative position column
      xs$relative_position[i] <- extend_maps$head$get("position")
      
      # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
      xs$pending[i] <- extend_maps$head$get("pending")
      
      # update head/tail distances values in dataframe w/ values from head/tail hashmaps
      xs$head_distance[i] <- extend_maps$head$get("total_distance")
      xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
      
      # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
      xs$head_cuts[i] <- extend_maps$head$get("count")
      xs$tail_cuts[i] <- extend_maps$tail$get("count")
      
    }
    
    # message("=================")
  }
  
  # tmp <- xs %>% dplyr::filter(is.na(relative_position))
  # mapview::mapview(xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") + other_xs
  #   mapview::mapview(tmp, color = "green")
  # net_intersects <- sf::st_intersects(not_braids, xs)
  # lengths(net_intersects)
  
  
  # # keep only the transects that were changed/extended
  # to_keep <- dplyr::filter(xs, changed)
  
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  # mapview::mapview(xs, color = "red") + braids + not_braids
  
  # check intersection of keeps and NOT BRAID
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- geos::geos_intersects_any(
                            geos::as_geos_geometry(xs),
                            geos::as_geos_geometry(not_braids)
                            )
  # net_intersects <- sf::st_intersects(not_braids, xs)
  
  # remove updated cross sections that intersect with the NOT BRAIDED flowlines
  if(any(net_intersects)) {
    
    message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
    xs <- xs[!net_intersects, ]
    
  }
  
  # mapview::mapview(xs2, color = "green") +
  #   mapview::mapview(tmpy, color = "gold") +
  #   mapview::mapview(not_braids, color = "dodgerblue") + 
  #   mapview::mapview(braids, color = "red") +
  # mapview::mapview(xs, color = "green")
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs = dplyr::filter(xs, 
                           !changed, 
                           relative_position != "inner"
                           )
  # other_xs = dplyr::filter(xs, !changed)
  # tt <- dplyr::filter(xs, !changed, relative_position != "inner")
  # tt <- dplyr::filter(xs, !changed)
  
  # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  # dplyr::filter(xs, changed)
  # dplyr::filter(xs, !changed, relative_position == "inner")
  
  # inner transects that haven't been changed
  unchanged_inners <- dplyr::filter(xs, 
                                    !changed,
                                    relative_position == "inner")
  
  # keep only changed flowlines
  xs <- dplyr::filter(xs, changed) 

  # intersections between updated inner cross sections ("xs") and the remaining inner cross sections that were NOT changed ("unchanged_inners")
  inner_intersects <- geos::geos_intersects_any(
                          geos::as_geos_geometry(unchanged_inners$geometry),
                          geos::as_geos_geometry(xs$geometry)
                        )

  # add back into "xs" the unchanged inner transects that do NOT intersect with our updated/extended inner transect lines
  xs <- dplyr::bind_rows(
            xs,
            unchanged_inners[!inner_intersects, ]
          )
  
  
  # # # # keep ALL "inner" transects, both the ones that were extended ("changed" == TRUE) and not changed inners
  # xs <- dplyr::filter(xs, changed | relative_position == "inner")
  
  # check intersection of keeps xs with other_xs
  
  # indices of other_xs transects that now intersect with the updated/extended 'xs' transects. 
  # All the cross section lines in "xs" are now "inner" lines that were extended
  other_intersects <- geos::geos_intersects_any(
                            geos::as_geos_geometry(other_xs$geometry),
                            geos::as_geos_geometry(xs$geometry)
                          )
  # other_intersects <- sf::st_intersects(xs, other_xs)
  # unlist(sf::st_intersects(xs, other_xs))
  
  # net_intersects <- sf::st_intersects(not_braids, xs)
  # lengths(other_intersects)

  # if there ARE some intersections, remove those intersecting lines from 'div_xs'
  if(any(other_intersects)) {
    message("Removing ", table((unlist(other_intersects)))["TRUE"], " transect lines from 'other_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[!other_intersects, ]
  }
  
  # # flag determining whether transect should be replaced
  # other_xs$changed <- FALSE
  
  # if there are still other (non "inner") transects, do extension processing
  if (nrow(other_xs) > 0) {
    
    # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
    # loop through the remaining transects that were NOT "inner" lines, and do extensions
    for (i in 1:nrow(other_xs)) {
      
      # message("i: ", i, "/", nrow(other_xs))
      
      # other_xs$relative_position[i]
      # other_xs$head_distance[i]
      # other_xs$tail_distance[i]
      # other_xs$head_cuts[i]
      # other_xs$tail_cuts[i]
      # other_xs$cs_widths[i]
      # i = 1
      # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
      if (other_xs$relative_position[i] == "no_intersects") {
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
        next
      }
      
      # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
      res_geom <- geos_extend_transects(
        starter_line   = geos::as_geos_geometry(other_xs$geometry[i]),
        head_distance  = other_xs$head_distance[i],
        tail_distance  = other_xs$tail_distance[i],
        extra_distance = xs$cs_widths[i]/2
      )
      # mapview::mapview(res_geom, color = "green") +
      #   mapview::mapview(braids, color = "dodgerblue") +
      #   mapview::mapview(xs, color = "red") +
      #   mapview::mapview(other_xs$geometry[i], color = "cyan")
      #   braids + res_geom + not_braids
      
      # sf::st_intersects(res_geom, xs)
      # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
      # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom, 
      #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
      
      if(
        !any(
        geos::geos_intersects_any(
          geos::as_geos_geometry(xs),
          geos::as_geos_geometry(res_geom)
          )) &
        !any(geos::geos_intersects_any(
              geos::as_geos_geometry(other_xs[-i, ]),
              geos::as_geos_geometry(res_geom)
            ))
      ) {
        
        # # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # replace geometry with extended line
        other_xs$geometry[i] <- sf::st_geometry(sf::st_as_sf(res_geom))
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
      }

      # message("=================")
    }
    
    # # # keep only the transects that were changed/extended
    # other_drop <- dplyr::filter(other_xs, !changed)
    # 
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)
    # mapview::mapview(res_geom, color = "green") +
    #   mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(xs, color = "red") +
    #   # mapview::mapview(other_xs$geometry[i], color = "cyan")
    # mapview::mapview(other_drop, color = "green") +
    # mapview::mapview(other_xs, color = "red")
    # braids + res_geom + not_braids
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      ),
      dplyr::select(other_xs,
                    -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      )
    )
    
  } else {
    
    message("===== NO 'other_xs' transect lines =====")
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, 
                         -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                         -head_distance, -tail_distance, -head_cuts, -tail_cuts
    )
    
  }
  # mapview::mapview(out, color = "red") + 
  #   mapview::mapview(xs, color = "green") + 
  #   mapview::mapview(braids, color = "dodgerblue") 
  # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
  # to_keep %in% all_xs
  # all_xs %in% to_keep
  
  # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
  transect_lines <-  dplyr::bind_rows(
    # from original transect_lines, remove all of the cross sections on braids,
    dplyr::select(
      dplyr::filter(   
        dplyr::mutate(transect_lines, 
                      tmp_id = paste0(hy_id, "_", cs_id)
        ),
        !tmp_id %in% all_xs
      ),
      -tmp_id
    ),
    # updated braid cross sections
    out
  )
  
  # mapview::mapview(braids, color = "dodgerblue") +
  # mapview::mapview(not_braids, color = "gold") +
  # mapview::mapview(transect_lines, color = "green") +
  # mapview::mapview(transect_lines2, color = "red")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
}

# Given the count of intersections for a line extended from the HEAD and TAIL, determine its relative position within a braid
#  head count: numeric, count of intersections that line had when extending from HEAD
#  head tail_count: numeric, count of intersections that line had when extending from TAIL
# RETURNS: character string "no_intersects", "outer_single", "outer_multi", "inner" , or "in_between"  
check_relative_position <- function(head_count, tail_count) {
  
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  #       INNER scenario intersection count (odd and even cases):
  #         intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - MIDDLE/IN BETWEEN: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THIS!
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT)
  
  # TODO: Consider renaming these as No intersections, 
  # OUTER_SINGLE = SINGLE (the outer braid flowlines)
  # OUTER_MULTI  = SINGLE (the outer braid flowlines)
  # MIDDLE = IN BETWEEN (in between the outer braid flowlines and the actual middle braid flowlines)
  # INNER  = MIDDLE (the actual middle braid flowline)
  
  # boolean that gets flipped to FALSE if any of the other scenarios are detected
  in_between = TRUE
  
  # vector of intersection counts by the extended line,
  # extending out FROM THE HEAD and then FROM THE TAIL
  counts <- c(head_count, tail_count)
  
  # 1. No intersections scenario
  if(all(counts == 0)) {
    
    # relative position of line
    line_position <- "no_intersects"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    return(line_position)
    
  }
  
  # 2. OUTER SINGLE scenario
  if(all(counts == c(1, 0)) | all(counts == c(0, 1))) {
    
    # relative position of line
    line_position <- "outer_single"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    return(line_position)
    
  }
  
  # 3. OUTER MULTI scenario
  # Check if one value is 0 and the other is not zero AND is NOT 1
  if (any(counts == 0) && any(counts > 1)) {
    # if (any(counts == 0) && any(counts != 0)) {  
    
    # relative position of line
    line_position <- "outer_multi"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    # # index of the NON ZERO element 
    # not_zero_idx <- which(counts != 0)
    # message(paste("Index of NON ZERO element:", not_zero_idx))
    
    return(line_position)
  }
  
  # 4. INNER scenario
  # Handle sitation where total intersections is odd or even, if EITHER of below conditions is TRUE (OR condition), then we have inner (middle) line
  # - ODD CASE: If both the count values equal eachother
  # - EVEN CASE: If max(counts) minus 1 EQUALS min(counts)
  # If the counts equal eachother OR max(counts) minus 1 EQUALS min(counts)
  if(counts[1] == counts[2] | max(counts) - 1 == min(counts) ){
    
    # relative position of line
    line_position <- "inner"
    # message("line_position: ", line_position)
    
    # flip in_between boolean to FALSE
    in_between = FALSE
    # message("in_between: ", in_between)
    
    return(line_position)
    
  }
  # 5. IN_BETWEEN scenario
  #  IF NONE OF THE ABOVE CONDITIONS EXECUTED, then we have an IN_BETWEEN line
  if(in_between) {
    
    # relative position of line
    line_position <- "in_between"
    # message("line_position: ", line_position)
    
    # in_between boolean
    # message("in_between: ", in_between)
    return(line_position)
  }
  
  return(line_position)
}
# Extend a transect line outwards by a certain distance from the head and tail directions of the line
# starter_line is the original transect line to extend (geos_geoemtry)
# head_distance: numeric, distance (meters) to extend from HEAD of the line
# tail_distance: numeric, distance (meters) to extend from TAIL of the line
# extra_distance: Any extra distance the line should be extended after the original head/tail distances (THIS IS TYPICALLY GOING TO BE cs_width/2)
geos_extend_transects <- function(
                             starter_line, 
                             head_distance  = 0, 
                             tail_distance  = 0, 
                             extra_distance = 0
                             ) {
  

  # extra_distance = 100
  # head_distance = 5555
  # tail_distance = 150
  # ifelse(head_distance == 0, 0, extra_distance)
  
  # set head and tail extra values to the 'extra_distance' argument
  head_extra = tail_extra = extra_distance 
  
  # if the HEAD extending distance is 0, also set the 'head_extra' value to 0
  if(head_distance == 0) {
    head_extra = 0
  } 
  
  # if the TAIL extending distance is 0, also set the 'tail_extra' value to 0
  if(tail_distance == 0) {
    tail_extra = 0
  }
  
  # distance to extend head and tail out by
  head_extension <- head_distance + head_extra
  tail_extension <- tail_distance + tail_extra
  
  # head_extension <- head_distance + ifelse(head_distance == 0, 0, extra_distance)
  # tail_extension <- tail_distance + ifelse(tail_distance == 0, 0, extra_distance)
  # head_extension <- head_distance + (cs_width/2)
  # tail_extension <- tail_distance + (cs_width/2)
  
  # first extend the head outwards
  res_geom <- geos_extend_line(
                  starter_line,  
                  head_extension, 
                  "head"
                )
  
  # then extend the tail from the already head extended line 
  res_geom <- geos_extend_line(
                  res_geom,  
                  tail_extension, 
                  "tail"
                )
  
  return(res_geom)

}

# Extend a transect line outwards by a certain distance from the head and tail directions of the line
# starter_line is the original transect line to extend
# head_distance: numeric, distance (meters) to extend from HEAD of the line
# tail_distance: numeric, distance (meters) to extend from TAIL of the line
# extra_distance: Any extra distance the line should be extended after the original head/tail distances (THIS IS TYPICALLY GOING TO BE cs_width/2)
extend_transects <- function(starter_line, 
                             head_distance  = 0, 
                             tail_distance  = 0, 
                             extra_distance = 0
) {
  
  
  # distance to extend head and tail out by
  head_extension <- head_distance + extra_distance
  tail_extension <- tail_distance + extra_distance
  # head_extension <- head_distance + (cs_width/2)
  # tail_extension <- tail_distance + (cs_width/2)
  
  # first extend the head outwards
  res_geom <- st_extend_line(
    starter_line,  
    head_extension, 
    "head"
  )
  
  # then extend the tail from the already head extended line 
  res_geom <- st_extend_line(
    res_geom,  
    tail_extension, 
    "tail"
  )
  
  return(res_geom)
  
  #   mapview::mapview(cs_line,color = "cyan") +  
  #   mapview::mapview(h_tmp,color = "green") +
  #   mapview::mapview(t_tmp,color = "red") 
  # # starter_line <- cross_section$geometry[1]
  # starter_line <- cs_line
  # # h_distance <- head_map$get("distance") 
  # # t_distance <- tail_map$get("distance") 
  # h_distance <- head_map$get("totol_distance") 
  # t_distance <- tail_map$get("totol_distance") 
  # cs_width <- cross_section$cs_widths[1]
  # h_distance + (cs_width/2)
  # t_distance + (cs_width/2)
  # h_tmp <- st_extend_line(starter_line,   h_distance + (cs_width/2),  "head")
  # t_tmp <- st_extend_line( starter_line,   t_distance + (cs_width/2),  "tail")
  # return(res_geom)
}

relative_position_test_data <- function() {
  # create intersection test data
  positions <- data.frame(
    head_count = c(0, 1, 0, 0, 0, 2, 0, 2, 3, 2, 2, 4),
    tail_count = c(0, 0, 1, 3, 2, 0, 3, 2, 2, 3, 4, 2),
    relative_position = c(
      "no_intersects",
      "outer_single",
      "outer_single",
      "outer_multi",
      "outer_multi",
      "outer_multi",
      "outer_multi",
      "inner",
      "inner",
      "inner",
      "in_between",
      "in_between"
    )
  )
  return(positions)
}

test_relative_positions <- function() {
  
  positions <- relative_position_test_data()
  
  for(i in 1:nrow(positions)) {
    # i = 1
    message(i, "/", length(positions))
    positions$head_count[i]
    positions$tail_count[i]
    line_position <- check_relative_position(
      head_count = positions$head_count[i],
      tail_count = positions$tail_count[i]
    )
    
    message("line_position: ", line_position)
    message("positions$relative_position[i]: ", positions$relative_position[i])
    if(line_position ==positions$relative_position[i] ) {
      message("GOOD TO GO !!!")
    } else {
      message("-----> BAD MATCH :()")
    }
    message("=============")
  }
  
}

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function(i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects_latest<- function(
    net, 
    transect_lines,
    braid_threshold = NULL
) {
  
  # transect_lines <-  transects_nofix
  # net <- net3
  # braid_threshold = NULL
  # braid_threshold = 25000
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = TRUE,
    # nested    = FALSE,
    add       = TRUE
  )
  
  if(all(braids$braid_id == "no_braid")) {
    
    message("No braids identified, returning original transects")
    
    # transform CRS back to input CRS
    if(start_crs2 != 5070) {
      message("Transforming CRS back to EPSG: ", start_crs2)
      transect_lines <- sf::st_transform(transect_lines, start_crs2)
    }
    
    return(transect_lines)
  }
  
  message("Fixing braid transects...")
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  # only_braids <-
  braids <-  
    braids %>% 
    dplyr::filter(braid_id != "no_braid") %>% 
    # dplyr::group_by(comid) %>% 
    # dplyr::mutate(ncomid = n()) %>% 
    # dplyr::ungroup() %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  # view data on map
  # mapview::mapview(not_braids, color = "dodgerblue") +
  # mapview::mapview(only_braids, color = "red") 
  
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
      x         = braids, 
      originals = not_braids, 
      threshold = braid_threshold,
      verbose   = TRUE
    )
    
    # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
    not_braids <- braids$not_braids
    braids     <- braids$braids
  }
  
  # # unique braid_ids/COMIDs
  # ubraids <- unique(only_braids$braid_id)
  # ucoms <- unique(only_braids$comid)
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) %>% 
    # dplyr::filter(divergence == 0)
    dplyr::group_by(braid_id) %>% 
    dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup() %>% 
    dplyr::arrange(-totdasqkm)
  
  # keep track of all original crossections
  all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
  
  # column to store the relative position within the braid of the flowline we're on 
  xs$relative_position <- NA
  
  # flag determining whether transect should/has been replaced
  xs$changed <- FALSE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # flag determining whether transect is to be processed in a future step after middle flowlines are processed
  xs$pending <- TRUE
  
  # empty columns to store number of head/tail intersections
  xs$head_cuts     <- NA
  xs$tail_cuts     <- NA
  
  # empty columns to store distance needed to extend from head/tail of line
  xs$head_distance <- NA
  xs$tail_distance <- NA
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    
    message("===== NO 'xs' transect lines =====")
    message("===== returning original data =====")
    
    return(transect_lines)
    
  } else {
    message("===== ", nrow(xs) , " 'xs' transect lines =====")
    # message("===== returning original data =====")
  }
  
  
  # mapview::mapview(braids, color = "dodgerblue") +
  #   mapview::mapview(xs, color = "red") +
  # mapview::mapview(xs[i, ], color = "green")
  
  # Loop through every single cross section and determine:
  # 1. its relative position
  # 2. how far to extend the line
  # 3. in what order should transects be extended, 
  # 4. in what direction to extend the transect
  for(i in 1:nrow(xs)) {
    # for(i in 1:17) {
    # message("i: ", i, "/", nrow(xs))
    # i = 18 
    # # transect line
    # tline <- xs[i, ]$geometry
    
    # comid of transect line
    com <- xs$hy_id[i]
    
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    
    # get neighboring braid ID for our current braid
    neighbor_braids <- get_neighbor_braids(x = braids, ids = bids, only_unique = T)
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      braids,
      braid_id %in% neighbor_braids,
      comid != com
    )
    # INPUTS INTO NEW AUGMENT TRANSECTS DF FUNCTION
    # cross_section = xs[i, ]
    # geoms_to_cut <- others
    # max_distance = NULL
    # by = 1
    
    extend_maps <- augment_transect_df(
      cross_section = xs[i, ],
      geoms_to_cut  = others,
      max_distance  = NULL, 
      by            = 1, 
      as_df         = FALSE,
      carry_geom    = FALSE
    )
    
    # extend_maps$head$as_list()
    position <- extend_maps$head$get("position")
    
    # message("----> position: ", position)
    
    # if(is.na(position)) {
    #   message("!!!!!! !!!!!!!!!!!!!!!!! !!!!!!!!!")
    #   message("!!!!!! FOUND AN NA POSITION VALUE !!!!!!!!!")
    #   message("!!!!!! !! iter: ", i ," !!!!!!!!!")
    # }
    
    # if a flowline on the inner portion of a braid, make extension and insert
    if(position == "inner") {
      # message("Extending ", i, " and checking if valid replacement...")
      # extend line out by total distance key values in head and tail maps
      res_geom <- extend_transects(
        starter_line   = xs$geometry[i],
        head_distance  = extend_maps$head$get("total_distance"),
        tail_distance  = extend_maps$tail$get("total_distance"),
        extra_distance = xs$cs_widths[i]/2
      )
      
      # mapview::mapview(others) + braids + res_geom + not_braids
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
      if(!any(lengths(sf::st_intersects(res_geom, dplyr::filter(xs[-i,], changed))) > 0)) {
        # if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
        # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # updatem geometry with new, extended cross section
        xs$geometry[i] <- sf::st_geometry(res_geom)
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        # xs[i, ]$changed <- TRUE
      }
      
      # update relative position column
      xs$relative_position[i] <- extend_maps$head$get("position")
      
      # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
      xs$pending[i] <- extend_maps$head$get("pending")
      
      # update head/tail distances values in dataframe w/ values from head/tail hashmaps
      xs$head_distance[i] <- extend_maps$head$get("total_distance")
      xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
      
      # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
      xs$head_cuts[i] <- extend_maps$head$get("count")
      xs$tail_cuts[i] <- extend_maps$tail$get("count")
      
      
    } else {
      # message("Postpone processing: ", i)
      
      # update relative position column
      xs$relative_position[i] <- extend_maps$head$get("position")
      
      # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
      xs$pending[i] <- extend_maps$head$get("pending")
      
      # update head/tail distances values in dataframe w/ values from head/tail hashmaps
      xs$head_distance[i] <- extend_maps$head$get("total_distance")
      xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
      
      # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
      xs$head_cuts[i] <- extend_maps$head$get("count")
      xs$tail_cuts[i] <- extend_maps$tail$get("count")
      
    }
    
    # message("=================")
  }
  
  # tmp <- xs %>% dplyr::filter(is.na(relative_position))
  # mapview::mapview(xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") + 
  #   mapview::mapview(tmp, color = "green")
  # net_intersects <- sf::st_intersects(not_braids, xs)
  # lengths(net_intersects)
  
  
  # # keep only the transects that were changed/extended
  # to_keep <- dplyr::filter(xs, changed)
  
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  
  # check intersection of keeps and NOT BRAID
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- sf::st_intersects(not_braids, xs)
  # net_intersects <- sf::st_intersects(not_braids, to_keep)
  
  # lengths(net_intersects)
  
  # if there ARE some intersections, remove those intersecting lines from 'xs'
  if(any(lengths(net_intersects) > 0)) {
    # message("Removing ", length(unlist(net_intersects)), " transect lines from 'xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    xs <- xs[-unlist(net_intersects), ]
    # to_keep <- to_keep[-unlist(net_intersects), ]
  }
  
  # mapview::mapview(xs2, color = "green") +
  #   mapview::mapview(tmpy, color = "gold") +
  #   mapview::mapview(not_braids, color = "dodgerblue") + 
  #   mapview::mapview(braids, color = "red") +
  # mapview::mapview(xs, color = "green")
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs = dplyr::filter(xs, 
                           !changed, 
                           relative_position != "inner"
  )
  # other_xs = dplyr::filter(xs, !changed)
  # tt <- dplyr::filter(xs, !changed, relative_position != "inner")
  # tt <- dplyr::filter(xs, !changed)
  
  # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  # dplyr::filter(xs, changed)
  # dplyr::filter(xs, !changed, relative_position == "inner")
  
  # inner transects that haven't been changed
  unchanged_inners <- dplyr::filter(xs, 
                                    !changed,
                                    relative_position == "inner")
  
  # keep only changed flowlines
  xs <- dplyr::filter(xs, changed) 
  
  # add back into "xs" the unchanged inner transects that do NOT intersect with our updated/extended inner transect lines
  xs <- dplyr::bind_rows(
    xs,
    unchanged_inners[-unlist(sf::st_intersects(
      xs,
      # dplyr::filter(xs, changed),
      unchanged_inners
    )
    ), 
    ]
  )
  
  # # # # keep ALL "inner" transects, both the ones that were extended ("changed" == TRUE) and not changed inners
  # xs <- dplyr::filter(xs, changed | relative_position == "inner")
  
  # check intersection of keeps xs with other_xs
  
  # indices of other_xs transects that now intersect with the updated/extended 'xs' transects. All the cross section lines in "xs" are now "inner" lines that were extended
  other_intersects <- sf::st_intersects(xs, other_xs)
  
  # lengths(other_intersects)
  
  # if there ARE some intersections, remove those intersecting lines from 'div_xs'
  if(any(lengths(other_intersects) > 0)) {
    # message("Removing ", length(unique(unlist(other_intersects))), " transect lines from 'other_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[-unlist(other_intersects), ]
  }
  
  # # flag determining whether transect should be replaced
  # other_xs$changed <- FALSE
  
  # if there are still other (non "inner") transects, do extension processing
  if (nrow(other_xs) > 0) {
    
    # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
    # loop through the remaining transects that were NOT "inner" lines, and do extensions
    for (i in 1:nrow(other_xs)) {
      
      # message("i: ", i, "/", nrow(other_xs))
      
      # other_xs$relative_position[i]
      # other_xs$head_distance[i]
      # other_xs$tail_distance[i]
      # other_xs$head_cuts[i]
      # other_xs$tail_cuts[i]
      # other_xs$cs_widths[i]
      
      # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
      if (other_xs$relative_position[i] == "no_intersects") {
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
        next
      }
      
      # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
      res_geom <- extend_transects(
        starter_line   = other_xs$geometry[i],
        head_distance  = other_xs$head_distance[i],
        tail_distance  = other_xs$tail_distance[i],
        extra_distance = other_xs$cs_widths[i]/2
      )
      
      # mapview::mapview(res_geom, color = "green") +
      #   mapview::mapview(braids, color = "dodgerblue") +
      #   mapview::mapview(xs, color = "red") +
      #   mapview::mapview(other_xs$geometry[i], color = "cyan")
      #   braids + res_geom + not_braids
      
      # sf::st_intersects(res_geom, xs)
      # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
      # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom, 
      #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
      
      # ONLY UPDATE geometry if it does NOT intersect with any of the other ALREADY EXTENDED transects in "xs" 
      # OR any of the already updated (OR just any of them, not sure if I should limit it to only the CHANGED 'other_xs') transects in "other_xs" (AND LEAVE OUT SELF)
      if (
        !any(lengths(sf::st_intersects(res_geom, xs)) > 0) &
        !any(lengths(sf::st_intersects(res_geom, other_xs[-i, ])) > 0)
        # !any(lengths(sf::st_intersects(res_geom, xs)) > 0) &
        # !any(lengths(sf::st_intersects(res_geom,
        #                                dplyr::filter(other_xs[-i, ], changed))) > 0)
      ) {
        # # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # replace geometry with extended line
        other_xs$geometry[i] <- sf::st_geometry(res_geom)
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
      }
      
      # message("=================")
    }
    
    # # # keep only the transects that were changed/extended
    # other_drop <- dplyr::filter(other_xs, !changed)
    # 
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)
    # mapview::mapview(res_geom, color = "green") +
    #   mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(xs, color = "red") +
    #   # mapview::mapview(other_xs$geometry[i], color = "cyan")
    # mapview::mapview(other_drop, color = "green") +
    # mapview::mapview(other_xs, color = "red")
    # braids + res_geom + not_braids
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      ),
      dplyr::select(other_xs,
                    -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      )
    )
    
  } else {
    
    message("===== NO 'other_xs' transect lines =====")
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, 
                         -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                         -head_distance, -tail_distance, -head_cuts, -tail_cuts
    )
    
  }
  
  # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
  # to_keep %in% all_xs
  # all_xs %in% to_keep
  
  # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
  transect_lines <-  dplyr::bind_rows(
    # from original transect_lines, remove all of the cross sections on braids,
    dplyr::select(
      dplyr::filter(   
        dplyr::mutate(transect_lines, 
                      tmp_id = paste0(hy_id, "_", cs_id)
        ),
        !tmp_id %in% all_xs
      ),
      -tmp_id
    ),
    # updated braid cross sections
    out
  )
  
  # mapview::mapview(braids, color = "dodgerblue") +
  # mapview::mapview(not_braids, color = "gold") +
  # mapview::mapview(transect_lines, color = "green") +
  # mapview::mapview(transect_lines2, color = "red")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
}

# Apply flowline braid length threshold to braided network dataset 
# Return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset
# x: braided flowlines
# originals: not braided flowlines from the same network
# threshold: braid_threshold numeric value to remove braids with a total braid flowline length greater than 'threshold'
braid_thresholder <- function(x, 
                              originals,
                              threshold = NULL, 
                              verbose   = TRUE
) {
  
  # x         = braids
  # originals = not_braids
  # threshold = 30000
  # verbose = TRUE
  
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
  
  # unpack nested braid_id column0
  unpacked <- unnpack_braids(x)
  
  # calculate total length of all the linestrings in each braid_id
  unpacked <- 
    unpacked %>% 
    dplyr::group_by(braid_id) %>%
    dplyr::mutate(
      braid_length = as.numeric(
        sum(sf::st_length(geometry), na.rm = T))
    ) %>%
    dplyr::ungroup()
  # dplyr::mutate(braid_length = sf::st_length(geometry)) %>%
  # dplyr::mutate(braid_length = as.numeric(sum(braid_length, na.rm = T))) %>%
  
  # check to make sure some braids are over threshold and can be removed, if NOT then just return original data
  if(all(unpacked$braid_length <= threshold)) {
    
    message("Removing: 0 braids from braided dataset\n", 
            "Keeping: All braids as all braids have total flowline lengths less than or equal to threshold value: ", threshold)
    
    return(list(
      braids     = x,
      not_braids = originals)
    )
    
  } 
  
  # # table of TRUEs and FALSE for braids to keep/remove given 'threshold'
  # threshold_tbl <- table(unpacked$braid_length <= threshold)
  # if(verbose) { message("Removing: ",  threshold_tbl["FALSE"],  
  # " braids from braided dataset\nKeeping: ", threshold_tbl["TRUE"],
  #           " braids that have total flowline lengths less than or equal to threshold value: ", threshold)}
  
  # comids to keep (total length of braid linestrings is less than or equal to braid_threshold value)
  to_keep <- dplyr::filter(unpacked, braid_length <= threshold)$comid
  
  # # COMIDs that are too large, add them back to the "not_braids" data
  # to_drop <- dplyr::filter(x, !comid %in% to_keep)
  
  # keep track of keeping and removing count
  orig_nrows <- nrow(originals)
  x_nrows  <- nrow(x)
  
  # add the "too big braid COMIDs" back to original "not_braids" data 
  # and set these comids braid_ids to "no_braid" and is_multibraid = FALSE
  originals <- dplyr::bind_rows(
    originals,
    dplyr::select(
      dplyr::mutate(
        dplyr::filter(
          x, !comid %in% to_keep
        ),
        braid_id      = "no_braid", 
        is_multibraid = FALSE
      ),
      -has_mainstem
    )
  )
  
  new_orig_nrows <- nrow(originals)
  
  # filter out braid_ids/COMIDs that are too big
  x <- dplyr::filter(x, comid %in% to_keep)
  
  # updating count of keeping and removing 
  new_orig_nrows <- nrow(originals)
  new_x_nrows <- nrow(x)
  
  if(verbose) {
    message("Removing: ", new_orig_nrows - orig_nrows, 
            " braids from braided dataset\nKeeping: ",   new_x_nrows,
            " braids that have total flowline lengths less than or equal to threshold value: ", threshold)
  }
  
  return(list(
    braids     = x,
    not_braids = originals)
  )
  
}

# ----- BEST AUGMENT (WORK IN PROGRESS) ----
# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: (geos_geometry), other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# geom_ids: vector of unique IDs for each geoms_to_cut
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
geos_augment_transect <- function(cross_section,
                                geoms_to_cut, 
                                geom_ids,
                                max_distance = NULL,
                                by = NULL, 
                                as_df = TRUE, 
                                carry_geom = TRUE
) {
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  # max_dist <- as.numeric(
  #                 max(
  #                   sf::st_distance(  
  #                     geoms_to_cut, 
  #                     x
  #                   )
  #                 )
  #               )

  # cross_section = xs[i, ]
  # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
  # geom_ids      = others$comid
  # max_distance  = NULL
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = others
  # max_distance  = NULL
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  
  # cs_line <- geos::as_geos_geometry(cross_section$geometry)
  
  # extract values from cross_section dataframe
  cs_width <- cross_section$cs_widths
  bf_width <- cross_section$bf_width
  id       <- cross_section$hy_id
  
  cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  # cs_line  <- cross_section$geometry
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(max_distance)) {
    max_distance <- max(cs_width * 5)
  }
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(by)) {
    by = bf_width/2
  }
  
  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  # dist_vect <- seq(0, max(c(max_distance, 2000)), multi_transects[i, ]$bf_width)
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  head_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(id), 
    dir           = "head",
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(id), 
    dir           = "tail",
    map           = TRUE
  )
  
  # head_map$as_list()
  # tail_map$as_list()
  
  # # extract the linestringshapes
  # tail_ext <- tail_map$get("line")
  # head_ext <- head_map$get("line")
  
  # mapview::mapview(braids,color = "gold") +  mapview::mapview(geoms_to_cut,color = "dodgerblue") + 
  # mapview::mapview(head_ext,color = "green") +  mapview::mapview(tail_ext,color = "red") +mapview::mapview(cs_line,color = "cyan") 
  
  # get the relative position within the braid of the linestring we are extending our transect out from
  position <- check_relative_position(
    head_count = head_map$get("count"),
    tail_count = tail_map$get("count")
  )
  
  # POSITION VALUES explanation:
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO_INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER_SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER_MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  # INNER scenario intersection count (odd and even cases):
  # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THESE (maybe?) !
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
  
  # if as_df is FALSE, return the line data hashmaps as a list of length 2, 
  # first list element is the head extension data and the second is the tail extension data
  if(!as_df) {
    
    # if NOT AN INNER LINE, postpone processesing
    if(position != "inner") {
      
      # set pending values for these geometries
      head_map$set("pending", TRUE)
      tail_map$set("pending", TRUE)
      
      # set pending values for these geometries
      head_map$set("position", position)
      tail_map$set("position", position)
      
    } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
      
      # set pending values for these geometries
      head_map$set("pending", FALSE)
      tail_map$set("pending", FALSE)
      
      # set pending values for these geometries
      head_map$set("position", position)
      tail_map$set("position", position)
      
    }
    
    # if carry geom is FALSE, remove geometry linestrings from maps before returning
    if(!carry_geom) {
      head_map$remove("line")
      tail_map$remove("line")
    }
    
    return(
      list(
        head = head_map,
        tail = tail_map
      )
    )
    # return(
    #   list(
    #     head = head_map$as_list(),
    #     tail = tail_map$as_list()
    #   )
    # )
    
  }
  
  
  # update "relative_position" column in cross_section to reflect the position of the cross section flowline within the braid value
  cross_section$relative_position <- position
  
  # if NOT AN INNER LINE, postpone processesing
  if(position != "inner") {
    # DON"T UPDATE "pending" value to reflect that this line should be put on hold and processed after the inner flowlines
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    cross_section$head_distance <- head_map$get("total_distance")
    cross_section$tail_distance <- tail_map$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    cross_section$head_cuts <- head_map$get("count")
    cross_section$tail_cuts <- tail_map$get("count")
    
    # if LINE IS A INNER LINE, GET READY TO EXTEND
  } else {
    
    # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
    cross_section$pending <- FALSE
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    cross_section$head_distance <- head_map$get("total_distance")
    cross_section$tail_distance <- tail_map$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    cross_section$head_cuts <- head_map$get("count")
    cross_section$tail_cuts <- tail_map$get("count")
    
  }
  
  # res_geom <- extend_transects(
  #                   starter_line   = cs_line, 
  #                   head_distance  = head_map$get("total_distance"),
  #                   tail_distance  = tail_map$get("total_distance"),
  #                   extra_distance = cs_width/2
  #                 )
  
  return(cross_section)
  
}
# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
augment_transect_df <- function(cross_section,
                                geoms_to_cut, 
                                max_distance = NULL,
                                by = NULL, 
                                as_df = TRUE, 
                                carry_geom = TRUE
) {
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  # max_dist <- as.numeric(
  #                 max(
  #                   sf::st_distance(  
  #                     geoms_to_cut, 
  #                     x
  #                   )
  #                 )
  #               )
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = others
  # max_distance  = NULL
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  
  # geos_line <- geos::as_geos_geometry(cross_section$geometry)
  
  # extract values from cross_section dataframe
  cs_width <- cross_section$cs_widths
  bf_width <- cross_section$bf_width
  id       <- cross_section$hy_id
  cs_line  <- cross_section$geometry
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(max_distance)) {
    max_distance <- max(cs_width * 5)
  }
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(by)) {
    by = bf_width/2
  }
  
  # sequence from 0 to the max possible extension distance 
  dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  # dist_vect <- seq(0, max(c(max_distance, 2000)), multi_transects[i, ]$bf_width)
  
  # EXTEND OUT lines 
  # extend transect line out in both directions and find the side that interests with m
  # extend line out from HEAD side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  head_map <- extend_out2(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "head",
    map           = TRUE
  )
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- extend_out2(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    ids           = c(id), 
    dir           = "tail",
    map           = TRUE
  )
  
  # # extract the linestringshapes
  # tail_ext <- tail_map$get("line")
  # head_ext <- head_map$get("line")
  
  # mapview::mapview(braids,color = "gold") +  mapview::mapview(geoms_to_cut,color = "dodgerblue") + 
  # mapview::mapview(head_ext,color = "green") +  mapview::mapview(tail_ext,color = "red") +mapview::mapview(cs_line,color = "cyan") 
  
  # get the relative position within the braid of the linestring we are extending our transect out from
  position <- check_relative_position(
    head_count = head_map$get("count"),
    tail_count = tail_map$get("count")
  )
  
  # POSITION VALUES explanation:
  # given the count of interesections from the head and tail of a linestring, return whether the line has:
  # - NO_INTERSECTION:: (after extending linestring out to max distance)
  # - OUTER_SINGLE: extending linestring out in both directions yielded 
  # zero intersections in one direction AND exactly one intersection in the other direction
  # - OUTER_MULTI: extending linestring out in both directions yielded 
  # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
  # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
  # INNER scenario intersection count (odd and even cases):
  # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
  # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
  # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
  # ----> SKIP THESE (maybe?) !
  # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
  
  # if as_df is FALSE, return the line data hashmaps as a list of length 2, 
  # first list element is the head extension data and the second is the tail extension data
  if(!as_df) {
    
    # if NOT AN INNER LINE, postpone processesing
    if(position != "inner") {
      
      # set pending values for these geometries
      head_map$set("pending", TRUE)
      tail_map$set("pending", TRUE)
      
      # set pending values for these geometries
      head_map$set("position", position)
      tail_map$set("position", position)
      
    } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
      
      # set pending values for these geometries
      head_map$set("pending", FALSE)
      tail_map$set("pending", FALSE)
      
      # set pending values for these geometries
      head_map$set("position", position)
      tail_map$set("position", position)
      
    }
    
    # if carry geom is FALSE, remove geometry linestrings from maps before returning
    if(!carry_geom) {
      head_map$remove("line")
      tail_map$remove("line")
    }
    
    return(
      list(
        head = head_map,
        tail = tail_map
      )
    )
    # return(
    #   list(
    #     head = head_map$as_list(),
    #     tail = tail_map$as_list()
    #   )
    # )
    
  }
  
  
  # update "relative_position" column in cross_section to reflect the position of the cross section flowline within the braid value
  cross_section$relative_position <- position
  
  # if NOT AN INNER LINE, postpone processesing
  if(position != "inner") {
    # DON"T UPDATE "pending" value to reflect that this line should be put on hold and processed after the inner flowlines
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    cross_section$head_distance <- head_map$get("total_distance")
    cross_section$tail_distance <- tail_map$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    cross_section$head_cuts <- head_map$get("count")
    cross_section$tail_cuts <- tail_map$get("count")
    
    # if LINE IS A INNER LINE, GET READY TO EXTEND
  } else {
    
    # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
    cross_section$pending <- FALSE
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    cross_section$head_distance <- head_map$get("total_distance")
    cross_section$tail_distance <- tail_map$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    cross_section$head_cuts <- head_map$get("count")
    cross_section$tail_cuts <- tail_map$get("count")
    
  }
  
  # res_geom <- extend_transects(
  #                   starter_line   = cs_line, 
  #                   head_distance  = head_map$get("total_distance"),
  #                   tail_distance  = tail_map$get("total_distance"),
  #                   extra_distance = cs_width/2
  #                 )
  
  return(cross_section)
  
}

# Extend transect line outward and return the minimum linestring that crosses all possible other linestrings (geoms_to_cut) in the given direction
# recursive function that recursively applies the binary search algorithm to extend
# the transect lines outward in both directions until no more braid lines CAN be found/intersected with
# Arguments:
  # x = start index of distances vector
  # line = transect line that should be extended
  # distances = numeric vector of distance values in ascending order (sorted)
  # geoms_to_cut = other linestrings (all linestrings other than 'line') that should be cut across
  # ids = vector id of the 'line' argument
  # dir = character, either "head" or "tail", indicating which direction to extend 'line' out
extend_out <- function(
                    x,
                    line,
                    distances,
                    geoms_to_cut, 
                    ids, 
                    dir = "head"
                ) {
  
  # x = 1
  # line          = tline
  # distances      = dist_vect
  # geoms_to_cut  = others
  # ids           = c(com)
  # dir           = "head"

  # if binary search returns that no intersection happened (extended all the way out)
  # BASE CASE OF RECURSION
  if(x >= length(distances)) {
    message("---- HIT THE BASE CASE ----")
    # return(list(x, line))
    return(line)
  }
  
  # message("x: ", x)
  # message("length(distances): ", length(distances))
  # message("ids: ", paste0(ids, sep = ", "))
  # message("RUNNING binary search...")
  
  xx <- binary_search_distance(
    distances    = distances, 
    line         = line, 
    geoms_to_cut = geoms_to_cut,
    direction    = dir
  )
  
  # message("binary search distance index: ", xx)
  # message("extend distance (distances[xx]): ", distances[xx])
  
  if (xx >= length(distances)) {
    message("xx >= length(distances): ", xx, " >= ", length(distances))
    message("---- HIT THE BASE CASE ----")
    # return(list(x, line))
    return(line)
  }
  
  # extend line out to where the binary search first hit a line
  crosser <- st_extend_line(line, distances[xx], end = dir)  
  
  # intersection of crosser line and rest of multibraid except current comid(s)
  # sf::st_intersection(crosser,
  #                     dplyr::filter(geoms_to_cut, !comid %in% ids)
  #                     )
  
  # get the comid of the flow line that was interesected, 
  # new comid that should be added to "ids" variable and passed to recursive function call
  new_comid <- geoms_to_cut$comid[
                              unlist(sf::st_intersects(crosser,
                                                       dplyr::filter(geoms_to_cut, !comid %in% ids)
                                                       ) 
                                     )
                                ]

  # x            = xx
  # line         = crosser
  # distances    = distances
  # geoms_to_cut =  dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid))
  # ids          = c(ids, new_comid)
  # dir          = dir
  
  message("====== RECURSIVE STEP ======")
  
  # dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid))
  extend_out(
    x            = xx,
    line         = crosser,
    distances    = distances,
    geoms_to_cut =  dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid)), 
    ids          = c(ids, new_comid), 
    dir          = dir
  )
 
}

# WHILE LOOP IMPLEMENTATION INSTEAD OF RECURSION
# Extend transect line outward and return the minimum linestring that crosses all possible other linestrings (geoms_to_cut) in the given direction
# recursive function that recursively applies the binary search algorithm to extend
# the transect lines outward in both directions until no more braid lines CAN be found/intersected with
# Arguments:
# x = start index of distances vector
# line = transect line that should be extended
# distances = numeric vector of distance values in ascending order (sorted)
# geoms_to_cut = other linestrings (all linestrings other than 'line') that should be cut across
# ids = vector id of the 'line' argument
# dir = character, either "head" or "tail", indicating which direction to extend 'line' out
extend_out2 <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    ids, 
    dir = "head",
    map = FALSE
) {
  
  if(map) {
    dmap <- fastmap::fastmap()
  }
  
  # count interesections
  count <- 0
  dcount <- 0
  
  while (TRUE) {
    
  # while (x < length(distances)) {
    # message("x: ", x)
    # message("distances[x]: ", distances[x])
    
    xx <- binary_search_distance(
      distances    = distances, 
      line         = line, 
      geoms_to_cut = geoms_to_cut,
      direction    = dir
    )
    
    # count        <- count + 1
    # dcount       <- dcount + distances[xx]

    # message("xx: ", xx)
    # message("distances[xx]: ", distances[xx])
    # message("ids: ", ids)
    
    if (xx >= length(distances)) {
      # message("!!!!!!!!!!!!!!!! ")
      # message("!!!!!!!! xx >= distance: ", xx, " >= ", length(distances), " !!!!!!!! ")
      # message("!!!!!!!!!!!!!!!! ")
      break
    }
    
    # extend line out to where the binary search first hit a line
    crosser <- st_extend_line(line, distances[xx], end = dir)  
    
    # get the comid of the flow line that was intersected, 
    # new comid that should be added to "ids" variable and passed to the next iteration
    new_comid <- geoms_to_cut$comid[
                                    unlist(sf::st_intersects(
                                              crosser,
                                              dplyr::filter(geoms_to_cut, !comid %in% ids)
                                              ) 
                                           )
                                    ]
    
    # update variables for the next iteration
    line         <- crosser
    geoms_to_cut <- dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid))
    ids          <- c(ids, new_comid)
    # count        <- count + 1
    # dcount       <- dcount + distances[x]
    x            <- xx
    count        <- count + 1
    dcount       <- dcount + distances[xx]
    # message("FINAL x: ", x)
    # message("=======================")
    
  }
  
  # # if specified, return distance map of info and line
  if(map) {
    
    # decrement count by 1 if non zero
    # count <- ifelse(count == 0, count, count-1)
    
    dmap$mset(
      index           = x, 
      distance        = distances[x], 
      total_distance  = dcount,
      line            = line,
      cut_ids         = ids,
      count           = count,
      direction       = dir
    )
    
    return(dmap)
  }
  
  # otherwise just return the line
  return(line)
  
  # # if specified, return the distance index of line
  # if (index) {
  #   return(x)
  #   } 
  # 
  # return(line)
}

# WHILE LOOP IMPLEMENTATION INSTEAD OF RECURSION ---> VERSION 3
# Extend transect line outward and return the minimum linestring that crosses all possible other linestrings (geoms_to_cut) in the given direction
# recursive function that recursively applies the binary search algorithm to extend
# the transect lines outward in both directions until no more braid lines CAN be found/intersected with
# Arguments:
# x = start index of distances vector
# line = transect line that should be extended
# distances = numeric vector of distance values in ascending order (sorted)
# geoms_to_cut = other linestrings (all linestrings other than 'line') that should be cut across
# ids = vector id of the 'line' argument
# dir = character, either "head" or "tail", indicating which direction to extend 'line' out
extend_out3 <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    ids, 
    dir = "head",
    map = FALSE
) {
  
  if(map) {
    dmap <- fastmap::fastmap()
  }
  
  while (TRUE) {
    # while (x < length(distances)) {
    message("x: ", x)
    message("distances[x]: ", distances[x])
    
    xx <- binary_search_distance(
      distances    = distances, 
      line         = line, 
      geoms_to_cut = geoms_to_cut,
      direction    = dir
    )
    
    message("xx: ", xx)
    message("distances[xx]: ", distances[xx])
    message("ids: ", ids)
    
    if (xx >= length(distances)) {
      message("!!!!!!!!!!!!!!!! ")
      message("!!!!!!!! xx >= distance: ", xx, " >= ", length(distances), " !!!!!!!! ")
      message("!!!!!!!!!!!!!!!! ")
      break
    }
    
    # extend line out to where the binary search first hit a line
    crosser <- st_extend_line(line, distances[xx], end = dir)  
    
    # get the comid of the flow line that was intersected, 
    # new comid that should be added to "ids" variable and passed to the next iteration
    new_comid <- geoms_to_cut$comid[
      unlist(sf::st_intersects(crosser,
                               dplyr::filter(geoms_to_cut, !comid %in% ids)
      ) 
      )
    ]
    
    # Update variables for the next iteration
    line <- crosser
    geoms_to_cut <- dplyr::filter(geoms_to_cut, !comid %in% c(ids, new_comid))
    ids <- c(ids, new_comid)
    x <- xx
    message("FINAL x: ", x)
    message("=======================")
    
  }
  
  if(distances[x] != 0) {
    message("=== distance[x] NOT equal to 0 === ")
    message("FINAL x: ", x)
    message("FINAL xx: ", xx)
    message("---> Setting x from ", x, " to ", (xx +  x) %/% 2)
    message("---> Setting distances[x] from ",  distances[x], " to ",distances[(xx +  x) %/% 2])
    length(dist_vect)
    # (length(distances) -  x) %/% 2
    x <- (xx + x) %/% 2
    # (307+171)  %/% 2
    # # distance  = distances[x]
    # 307+171 /2
  }
  # # if specified, return distance map of info and line
  if(map) {
    
    dmap$mset(
      index     = x, 
      distance  = distances[x], 
      line      = line,
      direction = dir
    )
    
    return(dmap)
  }
  
  # otherwise just return the line
  return(line)
  
  # # if specified, return the distance index of line
  # if (index) {
  #   return(x)
  #   } 
  # 
  # return(line)
}

# WHILE LOOP IMPLEMENTATION INSTEAD OF RECURSION
# Extend transect line outward and return the minimum linestring that crosses all possible other linestrings (geoms_to_cut) in the given direction
# recursive function that recursively applies the binary search algorithm to extend
# the transect lines outward in both directions until no more braid lines CAN be found/intersected with
# Arguments:
# x = start index of distances vector
# line = transect line that should be extended
# distances = numeric vector of distance values in ascending order (sorted)
# geoms_to_cut = other linestrings (all linestrings other than 'line') that should be cut across
# ids = vector id of the 'line' argument
# dir = character, either "head" or "tail", indicating which direction to extend 'line' out
geos_extend_out <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    geom_ids,
    ids, 
    dir = "head",
    map = FALSE
) {
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
  # geom_ids      = others$comid
  # max_distance  = NULL
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  # cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  # 
  # x             = 1
  # line          = cs_line
  # distances     = dist_vect
  # # geoms_to_cut  = geoms_to_cut
  # geom_ids      = geom_ids
  # ids           = c(id)
  # dir           = "tail"
  # map           = TRUE
  
  
  # # if NOT a geos_geometry class, coerce
  # if(!inherits(line, "geos_geometry")) {
  #   # convert to geos geometry
  #   line <- geos::as_geos_geometry(line)
  #   # geoms_to_cut <- geos::as_geos_geometry(others)
  # }
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(geoms_to_cut, "geos_geometry")) {
    # convert to geos geometry
    geoms_to_cut <- geos::as_geos_geometry(geoms_to_cut)
    # geoms_to_cut <- geos::as_geos_geometry(others)
  }
  

  if(map) {
    dmap <- fastmap::fastmap()
  }
  
  # count interesections
  count <- 0
  dcount <- 0
  
  # while (TRUE) {
  while (TRUE) {
    # while (x < length(distances)) {
    # message("x: ", x)
    # message("distances[x]: ", distances[x])
    
    xx <- geos_bs_distance(
      distances    = distances, 
      line         = line, 
      geoms_to_cut = geoms_to_cut,
      direction    = dir
    )
    
    # count        <- count + 1
    # dcount       <- dcount + distances[xx]
    
    # message("xx: ", xx)
    # message("distances[xx]: ", distances[xx])
    # message("ids: ", ids)
    
    if (xx >= length(distances)) {
      # message("!!!!!!!!!!!!!!!! ")
      # message("!!!!!!!! xx >= distance: ", xx, " >= ", length(distances), " !!!!!!!! ")
      # message("!!!!!!!!!!!!!!!! ")
      break
    }
    
    # extend line out to midpoint of distances vector
    crosser <- geos_extend_line(line, distances[xx], end = dir)
    
    # # extend line out to where the binary search first hit a line
    # crossersf <- st_extend_line(sf::st_as_sf(line), 
    #                             distances[xx], end = dir)  
    # mapview::mapview(sf::st_as_sf(crosser), color = "dodgerblue") + mapview::mapview(crossersf, olor = "red")
    # others$geometry <- geos::as_geos_geometry(others$geometry)
    
    
    # Get the 'new_comid' that will be added to "ids" variable and then passed to the next iteration
    # - excluding the IDs in 'ids', determine what geometries in 'geoms_to_cut' are intersecting with our extended 'crosser' line
    # - then index 'geom_ids' based on the boolean vector returned from geos_intersects(), to then get the newly intersected ID (new_comid)
    new_comid <- geom_ids[
                        geos::geos_intersects(
                          crosser,
                          geoms_to_cut[
                            !geom_ids %in% ids
                          ]
                        )
                      ]
    
    # # get the comid of the flow line that was intersected, 
    # # new comid that should be added to "ids" variable and passed to the next iteration
    # new_comid <- geoms_to_cut$comid[
    #                     unlist(sf::st_intersects(
    #                       crosser,
    #                       dplyr::filter(geoms_to_cut, 
    #                                     !comid %in% ids
    #                                     )))
    #                     ]
    
    # Update all the variables for next iteration of while loop
    
    # update 'line'
    line         <- crosser

    
    # # set the geometries within c(ids, new_comid) to empty (essentially filtering them out)
    # geoms_to_cut[geom_ids %in% c(ids, new_comid)] <- geos::geos_empty()
    
    # # update geom_ids, removing ids and the new_comid
    # geom_ids <- geom_ids[!geom_ids %in% c(ids, new_comid)] 

    # update 'geoms_to_cut' and drop the newly added 'new_comid' 
    geoms_to_cut <- geoms_to_cut[
                            !geom_ids %in% c(ids, new_comid)
                            ]
    
    # update 'geom_ids', removing ids and the new_comid
    geom_ids <- geom_ids[
                      !geom_ids %in% c(ids, new_comid)
                      ] 
    
    # update 'ids' w/ new_comid
    ids          <- c(ids, new_comid)
    
    # update x (index) value
    x            <- xx
    
    # increment count and continue summing distances
    count        <- count + 1
    dcount       <- dcount + distances[xx]
    
    # message("FINAL x: ", x)
    # message("=======================")
    
  }
  
  # # if specified, return distance map of info and line
  if(map) {
    
    # decrement count by 1 if non zero
    # count <- ifelse(count == 0, count, count-1)
    
    dmap$mset(
      index           = x, 
      distance        = distances[x], 
      total_distance  = dcount,
      line            = line,
      cut_ids         = ids,
      count           = count,
      direction       = dir
    )
    
    return(dmap)
  }
  
  # otherwise just return the line
  return(line)
  
  # # if specified, return the distance index of line
  # if (index) {
  #   return(x)
  #   } 
  # 
  # return(line)
}

fix_braid_transects4 <- function(net, transect_lines) {
  # transect_lines = transects
  # net <- net3
  
  # keep track of the CRS of the input to retransform return 
  start_crs <- sf::st_crs(net, parameters = T)$epsg
  
  message("Start CRS: ", start_crs)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(sf::st_crs(transect_lines, parameters = T)$epsg != 5070) {
    # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
    message("Transforming CRS to EPSG:5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network   = net,
    return_as = "dataframe",
    nested    = FALSE,
    add       = TRUE
    )
  
  # # check if net CRS is 5070, if not, transform it to 5070
  # if(sf::st_crs(braids, parameters = T)$epsg != 5070) {
  #   # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
  #   message("Transforming CRS to EPSG:5070")
  #   braids <- sf::st_transform(braids, 5070) 
  # }
  
  # braids <- sf::st_transform(braids, 5070)
  # net    <- sf::st_transform(net, 5070)
  
  # # multibraids
  # multis <- dplyr::filter(braids, is_multibraid)
  # mb
  
  # all braids
  # all_braids <-   dplyr::filter(braids, braid_id != "no_braid")
  
  # not braided sections
  # no_braid <- dplyr::filter(braids, braid_id == "no_braid")
  
  # multibraids
  # multis <- dplyr::filter(braids, is_multibraid)
  
  # SINGLE BRAIDS
  singles <- dplyr::filter(braids, !is_multibraid, braid_id != "no_braid")
  
  # DEAL WITH SINGLE BRAIDS
  
  # DETERMINE WHICH FLOWLINE IN EACH BRAID IS MAINSTEM OR if no mainstem, use the divergence == 1 flowlines
  
  # single braids on the ACTUAL main stem
  main_singles <- 
    singles %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(divergence == 0) %>% 
    dplyr::ungroup()
  
  # determine the remaining braids that did NOT have a divergence flag of 0, use divergence == 1 as mainstem
  div_singles <-
    singles %>% 
      dplyr::filter(!braid_id %in% main_singles$braid_id) %>% 
      dplyr::group_by(braid_id) %>% 
      dplyr::filter(
        divergence == 1
      ) %>% 
      dplyr::ungroup()
  
  # check if any left over braids
  other_singles <- dplyr::filter(singles, !braid_id %in% c(main_singles$braid_id, div_singles$braid_id))
  
  if(nrow(other_singles) != 0) {
    message("Still need to determine ", nrow(other_singles), " single braided mainstems for braid_ids")
  } else {
    message("All single braided mainstem braid_ids accounted for")
  }
  
  
  # Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
  # Mainstem single braid cross sections (ms_xs)
  ms_xs <- 
    transect_lines %>% 
    dplyr::filter(hy_id %in% main_singles$comid) %>% 
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          main_singles, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) 
  
  # Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
  # Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
  div_xs <-
    transect_lines %>%
    dplyr::filter(hy_id %in% div_singles$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          div_singles, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )
    # dplyr::left_join(  
    #   dplyr::filter(transect_lines, hy_id %in% div_singles$comid),
    #   sf::st_drop_geometry(
    #     dplyr::select(div_singles, comid, braid_id, is_multibraid)
    #     ), by = c("hy_id" = "comid"))
  
  single_xs <- dplyr::bind_rows(ms_xs, div_xs)
  
  # stash = c()
  
  message("Extending transects of single braids...")
  
  for (i in 1:nrow(single_xs)) {
    message("i: ", i, "/", nrow(single_xs))
    
    # if (i == 22) {
    #   stop()
    # }
    
    # braid transect line
    line <- single_xs[i, ]$geometry
    
    # braid ID
    bid <-  single_xs[i, ]$braid_id
    
    # transect COMID
    com <-  single_xs[i, ]$hy_id
    
    # braid of interest
    boi <-  dplyr::filter(singles, braid_id == bid)
  
    # max distance from transect of interest and rest of braid flowlines 
    # TODO (need a better method of determing max possible extension of flowline)
    max_dist <- as.numeric(max(sf::st_distance(  
                  dplyr::filter(boi, !comid %in% com), 
                  line)
                  ))
    
    # sequence from 0 to the max possible extension distance 
    distances <- seq(0, max(c(max_dist, 2000)), single_xs[i, ]$bf_width)
    
    # find the distance it takes from the HEAD of the transect string to reach another part of the braid
    head <- binary_search_distance(
                      distances      = distances, 
                      line           = line,
                      geoms_to_cut   = dplyr::filter(boi, !comid %in% com),
                      direction      = "head"
                      )
    # find the distance it takes from the TAIL of the transect string to reach another part of the braid
    tail <- binary_search_distance(
                      distances         = distances, 
                      line              = line,
                      geoms_to_cut      = dplyr::filter(boi, !comid %in% com),
                      direction         = "tail"
                      )
    
    # length(distances)
    # if(head > length(distances)) { head = head - 1 }
    # if(tail > length(distances)) { tail = tail - 1 }
    
      # head_line <- st_extend_line(line, distances[head-1], 'head')
      # tail_line <- st_extend_line(line, distances[tail-1], 'tail')
      # mapview::mapview(boi) + line + head_line + tail_line
      
    # pick the extension direction that had the soonest interesection with the rest of the braid
    if(which.min(c(head, tail)) == 1) { 
      
      dir = "head"
      cross_idx <- head
      
      } else {
        
      dir = "tail"
      cross_idx <- tail
      
      }
      
      # the index of the intersection point is out of bounds, set it to the max distance value
      if(cross_idx > length(distances)) {
        
        cross_idx = cross_idx - 1
      }
    
    # primary transect line, still needs other side to be fixed/cleaned up 
    crosser <- st_extend_line(line, distances[cross_idx], dir)
    
    # point that crosses over other flowline
    cross_pt <- sf::st_intersection(crosser, dplyr::filter(boi, !comid %in% com))
    # sf::st_intersection(crosser, boi)
    
    # linestring start and ends, 
    end   <- lwgeom::st_endpoint(crosser)
    start <- lwgeom::st_startpoint(crosser)
    
    # distance from crossing point and end/start points. 
    # We end up taking the minimum distance from the crossing point to the ends of the new "crosser" line we made above.
    end_dist   <- as.numeric(sf::st_distance(cross_pt, end))
    start_dist <- as.numeric(sf::st_distance(cross_pt, start))
  
    # set distances to 0 if no crossing point is on top of the start/end
    end_dist   <- ifelse(length(end_dist) == 0, 0, end_dist)
    start_dist <- ifelse(length(start_dist) == 0, 0, start_dist)
    
    if (end_dist == 0 & start_dist == 0) {
      message("--- NO INTERSECTION AFTER EXTENDING TRANSECT ---")
      message("--- CONTINUING TO NEXT TRANSECT ---")
      next
    }
  
    # END_PT------- CROSSER_PT ----------------------------START_PT
    # |-----------------|
    # That would be the minimum distance, we then want to extend from crosser pt to about half the distance of the cross section width 
    # calculate distance which will be half of one of the CS_widths, minus the smaller distance from the cross section. (ex. above)
    ext_dist <- (single_xs[i, ]$cs_widths/2) - min(end_dist, start_dist)
    
    # generate final extended line
    res <- st_extend_line(line, distances[cross_idx] + ext_dist, dir)
    
    if (lengths(sf::st_intersects(res, single_xs[-i,])) == 0) {
      single_xs[i, ]$geometry <- res
    }
    
    message('==============')
    
    # mapview::mapview(ms_xs) + boi
    # mapview::mapview(line, color = "red") +
    #   mapview::mapview(crosser, color = "dodgerblue") +
    #   mapview::mapview(res, color = "green") +
    #   # mapview::mapview(cross_pt, col.regions = "red") +
    #   start + end  + boi + ms_xs + transects + singles
  }
  
  to_replace <- transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
  
  # single_xs$hy_id %in% to_replace$hy_id
  # to_replace$hy_id %>% duplicated()
  # single_xs$hy_id %>% duplicated()
  
  # sf::st_drop_geometry(
  #   transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
  #   )
  # transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
  # to_replace %>% 
    # sf::st_drop_geometry(to_replace) %>% 
  
  # join and replace original single braided transect lines with the updated/extended lines created above (single_xs)
  to_replace <-  sf::st_as_sf(
                  dplyr::left_join(
                      sf::st_drop_geometry(
                        transect_lines[which(transect_lines$hy_id %in% single_xs$hy_id), ]
                      ),
                      dplyr::select(
                        single_xs, hy_id, cs_id, geometry
                      ),
                      by = c("hy_id", "cs_id")
                      )
                  )
  
  # bind the updated transect lines back with the rest of the dataset
  transect_lines <- dplyr::bind_rows(
                        transect_lines[which(!transect_lines$hy_id %in% single_xs$hy_id), ],
                        to_replace
                        )
  
  # transform CRS back to input CRS
  if(start_crs != 5070) {
    message("Transforming CRS back to EPSG:", start_crs)
    transect_lines <- sf::st_transform(transect_lines, start_crs)
  }
  
  # transect_lines
  # transects
  
  return(transect_lines)
  
  # mapview::mapview(to_replace) + transects + single_xs + singles
  
  # mapview::mapview(to_replace, color = "red") +
  #   # mapview::mapview(crosser, color = "dodgerblue") +
  #   mapview::mapview(singles, color = "dodgerblue") +
  #   mapview::mapview(single_xs, color = "green") +
  #   transects +
  #   single_xs[3, ] + 
  #   single_xs[2, ]
  
  # single_xs
  # table(transect_lines$hy_id %in% single_xs$hy_id)
  # table(single_xs$hy_id %in% transect_lines$hy_id)
  # return(new_lines)  

}

# Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
# distances: numeric vector sorted in ascending order
# line: linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
# geoms_to_cut: geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
# direction: character, either "head" or "tail", indicating which end of the line to extend out.
geos_bs_distance <- function(
    distances, 
    line,
    geoms_to_cut, 
    direction = "head"
    ) {
  
  # distances    = distances
  # line         = line
  # geoms_to_cut = geoms_to_cut
  # direction    = dir
  
  # sftmp <- st_extend_line(xs[i, ], distances[M], end = dir)
  # mapview::mapview(geos_tmp, color = "red") +
  #   mapview::mapview(xs[i, ], color = "dodgerblue") +
  #   mapview::mapview(sftmp, color = "green")
  
  # distances    = distances
  # line         = line
  # geoms_to_cut = geoms_to_cut
  # direction    = dir
  
 
  
  # Left and right pointers (start and end of distances vector)
  L = 1
  R = length(distances)
  
  # While left pointer (L) is less than or equal to the right pointer (R), run binary search. 
  # Each iteration:
  # - the midpoint value gets calculated (M)
  # - M is the index of the 'distances' vector that we will use as the distance value to extend 'line'
  # - if the new extended line ('new_line') intersects with 'geoms_to_cut', then we decrease the distance value (DECREMENT RIGHT POINTER to the MIDPOINT - 1), 
  # - if NOT we increase the distance value (INCREMENT LEFT POINTER to the MIDPOINT + 1)
  while(L <= R) {
    
    # calculate midpoint between left and right pointers
    M = (L + R) %/% 2
    
    # message("L: ", L)
    # message("M: ", M)
    # message("R: ", R)
    # message("x[L]: ", distances[L])
    # message("x[M]: ", distances[M])
    # message("x[R]: ", distances[R])

    if(M == 0 | M == length(distances)) {
      # message("EARLY STOPPING bc M = ", M)
      # message("RETURNING L = ", L)
      return(L)
    }
    
    # extend line out to midpoint of distances vector
    new_line <- geos_extend_line(line, distances[M], end = direction)
    # new_line_sf <- st_extend_line(sf::st_as_sf(line), distances[M], end = direction)

    # geos::geos_intersects(geoms_to_cut, new_line)
    # sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)
    # geos::geos_intersects(geoms_to_cut, new_line)
    # lengths( sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)) > 0
    # any(geos::geos_intersects(geoms_to_cut, new_line))
    # any( lengths(sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)) > 0)
    # plot(new_line, col = "red", lwd= 5, add = F)
    # plot(line, col = "green", lwd= 5, add = T)
    
    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    
    # if ANY of the geometries in geoms_to_cut are intersected by the new extended line
    if(
      any(geos::geos_intersects(geoms_to_cut, new_line))
    ) {
      
      # then DECREMENT RIGHT pointer (DECREASE DISTANCE VALUE) to the midpoint - 1
      R = M - 1
      
    # otherwise IF NO intersections occur:
    } else {
      
      # then INCREMENT LEFT pointer (INCREASE DISTANCE VALUE) to the midpoint + 1
      L = M + 1
      
    }
    # message("=======================")
  }
  
  # l_line = st_extend_line(ls, x[L])
  # return(l_line)
  
  return(L)
}

# mapview::mapview(line, color = "red") +
# mapview::mapview(ms_xs, color = "red") +
#   # mapview::mapview(crosser, color = "dodgerblue") +
#   mapview::mapview(singles, color = "dodgerblue") +
#   mapview::mapview(multis, color = "green") +
#   transects
  # mapview::mapview(cross_pt, col.regions = "red") +
  # start + end  + boi + ms_xs + transects + singles
#   dplyr::filter(boi, !comid %in% com)
binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
  # x  = distances
  # ls = line
  # other     = geoms_to_cut
  # direction = dir
  # distances    = distances
  # line         = line
  # geoms_to_cut = geoms_to_cut
  # direction    = dir

  # distances2    = distances
  # line2         = line
  # geoms_to_cut2 = geoms_to_cut
  # direction2    = dir
  
  # direction = "head"
  
  L = 1
  R = length(distances)
  
  # while(L <= R & !flag) {
  while(L <= R) {
    
    M = (L + R) %/% 2
    
    # message("L: ", L)
    # message("M: ", M)
    # message("R: ", R)
    # message("x[L]: ", x[L])
    # message("x[M]: ", x[M])
    # message("x[R]: ", x[R])

    if(M == 0 | M == length(distances)) {
      # message("EARLY STOPPING bc M = ", M)
      # message("RETURNING L = ", L)
      return(L)
    }
    
    new_line <- st_extend_line(line, distances[M], end = direction)
    # geos_extend_line(line, distances[M], end = end)
    # st_extend_line(line,    distances[0], end = direction)
    # sf::st_intersects(others, new_line)
    # mapview::mapview(others) + new_line + ls
    # (any(lengths(sf::st_intersects(others, new_line)) > 0))
    # check if any of the other braid linestrings get intersected by the extended line:
    # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
    # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
    if(any(lengths(sf::st_intersects(geoms_to_cut, new_line)) > 0)) {
      # message("DECREM RIGHT.--> need smaller value")
      # message("R = R - 1 = : ", M - 1)
      # decrement right pointer to middle - 1
      R = M - 1
    } else {
      # message("DECREM RIGHT.--> need smaller value")
      # message("L = M + 1 = : ", M + 1)
      # increment left pointer to middle + 1
      L = M + 1
    }
    # message("=======================")
    # mapview::mapview(new_line) + boi + line + singles + others <- 
  }
  # l_line = st_extend_line(ls, x[L])
  # return(l_line)
  return(L)
}
# transects <- geos_empty()
# line <- as_geos_geometry(net[j,])
# 
# vertices <- wk_vertices(line)
# 
# edges <- as_geos_geometry(
#   wk_linestring(
#     vertices[c(1, rep(seq_along(vertices)[-c(1, length(vertices))], each = 2), length(vertices))],
#     feature_id = rep(seq_len(length(vertices) - 1), each = 2)
#   )
# )
# function for finding direction each line end point is pointing
geos_linestring_dir <- function(line) {

  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- coords[c("x", "y", "feature_id")]
  
  # dimensions
  k <- c(1, - 1)
  i <- c(2, nrow(coords) - 1)
  
  dirs <- mapply(i, k, FUN = function(i, k) {
    x1 <- coords[i-k,1]
    y1 <- coords[i-k,2]
    x2 <- coords[i,1]
    y2 <- coords[i,2]
    unname(atan2(y1-y2, x1-x2))
  })
  
  return(dirs)
  
}

# function which extends the line (one or both ends) by a given distance (in unit distance):
# line: sf or geos linestring
# distance: numeric value in meters or a vector of length 2 if 'end = "both"' where 
#       the first value in the vector will extend that tail by that value and the second value extends the head by that value c(tail, head).
#       If a single value is given when end = "both", the value is recycled and used to extend both ends
# end: character, determines whether to extend the linestring from the tail, head or both ends
geos_extend_line <- function(line, distance, end = "both", with_crs = TRUE) {
  # line <- xs[1, ]
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  if(!end %in% c("head", "tail", "both")) {
    stop("Invalid input 'end' must either'head', 'tail', or 'both'")
  }
  # crs <- wk::wk_crs(line)
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- as.matrix(coords[c("x", "y")])
  # coords <- coords[c("x", "y")]
  
  # which index to keep
  to_keep <- end != c("tail", "head")
  
  # end coords index we want to keep
  ends <- c(1, nrow(coords))[to_keep]
  
  # get directions of the direction of interest
  directions <- geos_linestring_dir(line)[to_keep]
  
  # if only a single distance, duplicate it, otherwise reverse the first 2 distances
  distances <- if (length(distance) == 1) {
    rep(distance, 2) 
    } else {
    rev(distance[1:2])
    }
  
  # adjust end point coordinates 
  coords[ends, ]  <- coords[ends, ] + distances[to_keep] * c(cos(directions), sin(directions))

  # whether to return with a CRS or not
  if(with_crs) {
    
    # # make a new linestring WITH CRS
    line <- geos::geos_make_linestring(
                          x   = coords[, 1],
                          y   = coords[, 2],
                          crs = wk::wk_crs(line)
                        )
    
    return(line)
    
  } 
  
  # else {
  #   # # make a new linestring WITHOUT CRS
  #   line <- geos::geos_make_linestring(x   = coords[, 1], y   = coords[, 2])
  #   return(line)
  # }

  # line <- sf::st_sfc( sf::st_linestring(coords), crs = sf::st_crs(line)) 
  # mapview::mapview(curr, color = "red") + mapview::mapview(newline, color = "green")
  # plot(line,  lwd = 6, add = T)
  # plot(curr$geometry,col = "green", lwd = 6, add = T)
  
  # # make a new linestring WITHOUT CRS
  line <- geos::geos_make_linestring(
                  x = coords[, 1], 
                  y = coords[, 2]
                )
  
  return(line)
}


# function for finding direction each line end point is pointing
st_ends_heading <- function(line) {
  
  M <- sf::st_coordinates(line)
  i <- c(2, nrow(M) - 1)
  j <- c(1, -1)
  
  headings <- mapply(i, j, FUN = function(i, j) {
    Ax <- M[i-j,1]
    Ay <- M[i-j,2]
    Bx <- M[i,1]
    By <- M[i,2]
    unname(atan2(Ay-By, Ax-Bx))
  })
  
  return(headings)
}

# function which extends the line (one or both ends) by a given distance (in unit distance):
st_extend_line <- function(line, distance, end = "both") {
  
  if (!(end %in% c("both", "head", "tail")) | length(end) != 1){
    stop("'end' must be 'both', 'head' or 'tail'")
  }
  
  M <- sf::st_coordinates(line)[,1:2]
  keep <- !(end == c("tail", "head"))
  
  ends <- c(1, nrow(M))[keep]
  headings <- st_ends_heading(line)[keep]
  distances <- if (length(distance) == 1) rep(distance, 2) else rev(distance[1:2])
  
  M[ends,] <- M[ends,] + distances[keep] * c(cos(headings), sin(headings))
  newline <- sf::st_linestring(M)
  
  # If input is sfc_LINESTRING and not sfg_LINESTRING
  if (is.list(line)) newline <- sf::st_sfc(newline, crs = sf::st_crs(line))
  
  return(newline)
}

# approx_equal <- function(x, y, tolerance = 1e-6) {
#   abs_diff <- abs(x - y)
#   if (abs_diff < tolerance) {
#     return(TRUE)
#   } else {
#     return(abs_diff)
#   }
# }
# 

# Function that identifies the transects on the MAINFLOW line of singly braided flowlines. 
# - single_braids: is an sf datarame of linestrings with comid, divergencec, braid_id, and geometry columns. 
# 'single_braids' is the return results of 'find_braids()' function after removing non braided flowlines
# AND removing multibraid flowlines (is_multibraid == TRUE)
# - transect_lines: is an sf datarame of linestrings with hy_id (comid), bf_width, cs_width, and geometry columns (at least).
# it is the return result of 'cut_cross_sections2()' function
# TODO: add 2 more arguments that allow you to specificy the unique ID to use as comid in single_braids and hy_id in transect_lines
# and an argument that identifies the column name of the 'braid_id' column. 
get_single_braid_transects <- function(
    single_braids, 
    transects
    ) {
  # single_braids  = singles
  # transects      = transect_lines
  
  # single braids on the ACTUAL main stem
  main_singles <- 
    single_braids %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(divergence == 0) %>% 
    dplyr::ungroup()
  
  # determine the remaining braids that did NOT have a divergence flag of 0, use divergence == 1 as mainstem
  div_singles <-
    single_braids %>% 
    dplyr::filter(!braid_id %in% main_singles$braid_id) %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(
      divergence == 1
    ) %>% 
    dplyr::ungroup()
  
  # check if any left over braids
  other_singles <- dplyr::filter(single_braids, !braid_id %in% c(main_singles$braid_id, div_singles$braid_id))
  
  if(nrow(other_singles) != 0) {
    message("Still need to determine ", nrow(other_singles), " single braided mainstems for braid_ids")
  } else {
    message("All single braided mainstem braid_ids accounted for")
  }
  
  
  # Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
  # Mainstem single braid cross sections (ms_xs)
  ms_xs <- 
    transects %>% 
    dplyr::filter(hy_id %in% main_singles$comid) %>% 
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          main_singles, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) 
  
  # Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
  # Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
  div_xs <-
    transects %>%
    dplyr::filter(hy_id %in% div_singles$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          div_singles, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )
  # dplyr::left_join(  
  #   dplyr::filter(transects, hy_id %in% div_singles$comid),
  #   sf::st_drop_geometry(
  #     dplyr::select(div_singles, comid, braid_id, is_multibraid)
  #     ), by = c("hy_id" = "comid"))
  
  # bind together all single braid cross sections
  single_xs <- dplyr::bind_rows(ms_xs, div_xs)
  
  return(single_xs)
}

# Function that identifies the transects on the MAINFLOW line of MULTIBRAIDED flowlines. 
# - multi_braids: is an sf datarame of linestrings with comid, divergencec, braid_id, and geometry columns. 
# 'multi_braids' is the return results of 'find_braids()' function after removing non braided flowlines
# AND removing all singly braided flowlines (is_multibraid == FALSE)
# - transect_lines: is an sf datarame of linestrings with hy_id (comid), bf_width, cs_width, and geometry columns (at least).
# it is the return result of 'cut_cross_sections2()' function
# TODO: add 2 more arguments that allow you to specificy the unique ID to use as comid in single_braids and hy_id in transect_lines
# and an argument that identifies the column name of the 'braid_id' column. 
get_multibraid_transects <- function(
    multi_braids, 
    transects
) {
  
  # multi_braids  = multis
  # transects     = transect_lines
  
  # single braids on the ACTUAL main stem
  main_multis <-
    multi_braids %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(divergence == 0) %>% 
    dplyr::ungroup()
  
  # determine the remaining braids that did NOT have a divergence flag of 0, use divergence == 1 as mainstem
  div_multis <-
    multi_braids %>% 
    dplyr::filter(!braid_id %in% main_multis$braid_id) %>% 
    dplyr::group_by(braid_id) %>% 
    dplyr::filter(
      divergence == 1
    ) %>% 
    dplyr::ungroup()
  
  # Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
  # Mainstem single braid cross sections (ms_xs)
  main_xs <-
    # multis_xs <-
    transects %>% 
    dplyr::filter(hy_id %in% main_multis$comid) %>% 
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          main_multis, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) 
  
  # # # Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
  # # # Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
  div_xs <-
    transects %>%
    dplyr::filter(hy_id %in% div_multis$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          div_multis, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )
  # dplyr::left_join(  
  #   dplyr::filter(transects, hy_id %in% div_singles$comid),
  #   sf::st_drop_geometry(
  #     dplyr::select(div_singles, comid, braid_id, is_multibraid)
  #     ), by = c("hy_id" = "comid"))
  
  # bind together all MULTI braid cross sections
  # the cross sections for all mainstem (and divergence mainstem (i.e. divergence == 1)) MULTIBRAIDS in network
  multis_xs <- dplyr::bind_rows(main_xs, div_xs)

  return(multis_xs)
}

# Function that identifies the transects on the MAINFLOW line of MULTIBRAIDED flowlines. 
# - multi_braids: is an sf datarame of linestrings with comid, divergencec, braid_id, and geometry columns. 
# 'multi_braids' is the return results of 'find_braids()' function after removing non braided flowlines
# AND removing all singly braided flowlines (is_multibraid == FALSE)
# - transect_lines: is an sf datarame of linestrings with hy_id (comid), bf_width, cs_width, and geometry columns (at least).
# it is the return result of 'cut_cross_sections2()' function
# TODO: add 2 more arguments that allow you to specificy the unique ID to use as comid in single_braids and hy_id in transect_lines
# and an argument that identifies the column name of the 'braid_id' column. 
pull_braids <- function(
    b, 
    transects
) {
  # b          = braid_lines
  # transects  = transect_lines
  # b   = braid_lines
  # transects      = transect_lines
  # multi_braids  =    multi_braids   = braid_lines, 
  # transects      = transect_lines multis
  # transects     = transect_lines
  
  # xs <- 
  #   transects %>%
  #   dplyr::filter(hy_id %in% b$comid) %>%
  #   dplyr::left_join(
  #     sf::st_drop_geometry(
  #       dplyr::select(
  #         b, comid, braid_id, is_multibraid
  #       )
  #     ),
  #     by = c("hy_id" = "comid")
  #   )
  # return(xs)
# }
  # mains <- dplyr::filter(b, divergence == 0)
  # 
  # divs <- 
  #   b %>% 
  #   dplyr::filter(!braid_id %in% mains$braid_id) %>% 
  #   dplyr::filter(divergence == 1 )
  # 
  # # extras <-
  #   
  #   b %>% 
  #   dplyr::filter(!braid_id %in% c(mains$braid_id, divs$braid_id)) 
  # 
  # mains$comid
  # 
  # out <- dplyr::filter(b, divergence != 2)
  # mains <- dplyr::filter(b, divergence == 2)
  # min_div <- 
  #   b %>% 
  #   dplyr::group_by(braid_id) %>% 
  #   dplyr::filter(divergence == min(divergence))
  # 
  # 
  # xs <- 
  #   transects %>%
  #   dplyr::filter(hy_id %in% out$comid) %>%
  #   dplyr::left_join(
  #     sf::st_drop_geometry(
  #       dplyr::select(
  #         out, comid, braid_id, is_multibraid
  #       )
  #     ),
  #     by = c("hy_id" = "comid")
  #   )
  # 
  # xs$braid_id %in% b$braid_id
  # b[!b$braid_id %in% xs$braid_id,] %>% 
  #   dplyr::group_by(braid_id) %>% 
  #   dplyr::filter(divergence != 0)
  
 # return(xs)
  singles <- dplyr::filter(b, !is_multibraid)

  multis <- dplyr::filter(b, is_multibraid)

  main_singles <-
    singles %>%
    dplyr::group_by(braid_id) %>%
    dplyr::filter(divergence == 0) %>%
    dplyr::ungroup()

  div_singles <-
    singles %>%
    dplyr::filter(!braid_id %in% main_singles$braid_id) %>%
    dplyr::group_by(braid_id) %>%
    dplyr::filter(
      divergence == 1
    ) %>%
    dplyr::ungroup()

  # check if any left over braids
  other_singles <- dplyr::filter(
                          singles,
                          !braid_id %in% c(main_singles$braid_id, div_singles$braid_id)
                        )

  if(nrow(other_singles) != 0) {
    message("Still need to determine ", nrow(other_singles), " single braided mainstems for braid_ids")
  } else {
    message("All single braided mainstem braid_ids accounted for")
  }

  # single braids on the ACTUAL main stem
  main_multis <-
    multis %>%
    dplyr::group_by(braid_id) %>%
    dplyr::filter(divergence == 0) %>%
    dplyr::ungroup()

  # determine the remaining braids that did NOT have a divergence flag of 0, use divergence == 1 as mainstem
  div_multis <-
    multis %>%
    dplyr::filter(!braid_id %in% main_multis$braid_id) %>%
    dplyr::group_by(braid_id) %>%
    dplyr::filter(
      divergence == 1
    ) %>%
    dplyr::ungroup()

  # # check if any left over braids
  # other_singles <- dplyr::filter(
  #   b,
  #   !braid_id %in% c(main_multis$braid_id, div_multis$braid_id)
  #   )

  # mapview::mapview(net2) +
  # mapview::mapview(net, color = "dodgerblue") +
  #   mapview::mapview(transect_lines, color = "gold") +
  #   mapview::mapview(singles, color = "green") +
  #   mapview::mapview(singles_xs, color = "green") +
  #   mapview::mapview(multis, color = "red") +
  #   mapview::mapview(multis_xs, color = "red") +
  #   mapview::mapview(xs, color = "gold") +
  #   mapview::mapview(braid_lines, color = "red")  +
  #   mapview::mapview(other_singles, color = "green")


  # Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
  # Mainstem single braid cross sections (ms_xs)
  ms_xs <-
    transects %>%
    dplyr::filter(hy_id %in% main_singles$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          main_singles, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )

  # Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
  # Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
  single_div_xs <-
    transects %>%
    dplyr::filter(hy_id %in% div_singles$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          div_singles, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )

  # Get the transects that are on the divergence == 0 single braided mainstem sections ^^^ (main_singles)
  # Mainstem single braid cross sections (ms_xs)
  main_xs <-
    # multis_xs <-
    transects %>%
    dplyr::filter(hy_id %in% main_multis$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          main_multis, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )

  # # # Get the transects that are on the divergence == 1 single braided mainstem sections ^^^ (div_singles)
  # # # Pseudo mainstem sections (using divergence == 1 as mainstem) single braid cross sections (ms_xs)
  div_xs <-
    transects %>%
    dplyr::filter(hy_id %in% div_multis$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          div_multis, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    )

  # bind together all MULTI braid cross sections
  # the cross sections for all mainstem (and divergence mainstem (i.e. divergence == 1)) MULTIBRAIDS in network
  xs <- dplyr::bind_rows(ms_xs, main_xs, single_div_xs, div_xs)

  return(xs)
}

approx_equal <- function(x, y, reference, percent_tolerance = 1) {
  tolerance <- percent_tolerance / 100 * abs(reference)
  abs_diff <- abs(x - y)
  if (abs_diff < tolerance) {
    return(TRUE)
  } else {
    return(abs_diff)
  }
}

# *******************************
# -------- TEST DATASETS --------
# *******************************

# net2 <- 
#   nhdplusTools::navigate_network(start = 1079041, mode = "UT",  distance_km = 40) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100 )
# net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 500 )
# net2 <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 100)
# net1 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 10) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 50) %>%
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))

# net3 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 100) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net4 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 150) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net5 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 200) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net6 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 250) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net7 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 300) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net8 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 400) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 
# net9 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 550) %>% 
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# net10 <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 50)
# net11 <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 100)
# net12 <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 150)
# net13 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100 )
# net14 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 250 )
# net15 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 400 )
# net16 <- nhdplusTools::navigate_network(start = 3179642, mode = "UT",  distance_km = 50)
# net17 <- nhdplusTools::navigate_network(start = 3179642, mode = "UT",  distance_km = 150)
# net18 <- nhdplusTools::navigate_network(start = 3179642, mode = "UT",  distance_km = 250)
# net19 <- nhdplusTools::navigate_network(start = 3179642, mode = "UT",  distance_km = 350)

# net18$geometry %>% plot()
# nhdplusTools::navigate_network()
# gj <- AOI::aoi_get("Grand Junction, CO")
# 
# nn <- nhdplusTools::get_nhdplus(gj)
# 
# mapview(gj) + nn
# net13 = area3_net1 
# net14 = area3_net2 
# net15 = area3_net3 
# plot(area2_net4$geometry)
# plot(net9$geometry)

# test_data <- list(net1, net2, net3, net4, net5, net6, net7, net8, net9,
#                   net10, net11, net12, net13, net14, net15, net16, net17, net18, net19)
# 
# data_info <- lapply(1:length(test_data), function(i) {
#   message(i, "/", length(test_data))
#   bs <- find_braids(network = test_data[[i]], return_as = "list")
# 
#   tibble::tibble(
#        dataset  = paste0("n", i),
#        nrows = nrow(test_data[[i]]),
#        npoints = mapview::npts(test_data[[i]]),
#        braid_count =   length(bs),
#        braid_comids =   length(unique(unname(unlist(bs))))
#        )
#   
# }) %>% 
#   dplyr::bind_rows()
# 
# data_info %>% 
# ggplot2::ggplot() +
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = npoints))
# # ggplot2::geom_point(ggplot2::aes(x = nrows, y = braid_comids))
#   ggplot2::geom_point(ggplot2::aes(x = nrows, y = braid_count))
# 
# cut_cross_sections2()
# # 
# # net2 <- dplyr::select(net2, comid, divergence, totdasqkm, fromnode, tonode)
# plot(net5$geometry)
# # # add bf_width column to network
# net3 <-
#   net2 %>%
#   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# # # 
# # transects_fixed = cut_cross_sections2(
# #   net       = net3,
# #   id        = "comid",
# #   cs_widths = pmax(50, net3$bf_width * 7),
# #   num       = 5,
# #   fix_braids = F,
# #   add       = TRUE
# #   )
# # 
# 
# 
# # Benchmarking multiple functions
# result <- microbenchmark::microbenchmark(
#   n1 = cut_cross_sections2(
#     net        = net1,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n2 = cut_cross_sections2(
#     net        = net2,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n3 = cut_cross_sections2(
#     net        = net3,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n4 = cut_cross_sections2(
#     net        = net4,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n5 = cut_cross_sections2(
#     net        = net5,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n6 = cut_cross_sections2(
#     net        = net6,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n7 = cut_cross_sections2(
#     net        = net7,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n8 = cut_cross_sections2(
#     net        = net8,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n9 = cut_cross_sections2(
#     net        = net9,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n10 = cut_cross_sections2(
#     net        = net10,
#     id         = "comid",
#     cs_widths  = pmax(50, net10$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n11 = cut_cross_sections2(
#     net        = net11,
#     id         = "comid",
#     cs_widths  = pmax(50, net11$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n12 = cut_cross_sections2(
#     net        = net12,
#     id         = "comid",
#     cs_widths  = pmax(50, net12$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n13 = cut_cross_sections2(
#     net        = net13,
#     id         = "comid",
#     cs_widths  = pmax(50, net13$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n14 = cut_cross_sections2(
#     net        = net14,
#     id         = "comid",
#     cs_widths  = pmax(50, net14$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n15 = cut_cross_sections2(
#     net        = net15,
#     id         = "comid",
#     cs_widths  = pmax(50, net15$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n16 = cut_cross_sections2(
#     net        = net16,
#     id         = "comid",
#     cs_widths  = pmax(50, net16$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n17 = cut_cross_sections2(
#     net        = net17,
#     id         = "comid",
#     cs_widths  = pmax(50, net17$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n18 = cut_cross_sections2(
#     net        = net18,
#     id         = "comid",
#     cs_widths  = pmax(50, net18$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n19 = cut_cross_sections2(
#     net        = net19,
#     id         = "comid",
#     cs_widths  = pmax(50, net19$bf_width * 7),
#     num        = 5,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   times = 2  # Number of repetitions for each function
# )
# 
# result_fewer <- microbenchmark::microbenchmark(
#   n1 = cut_cross_sections2(
#     net        = net1,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n2 = cut_cross_sections2(
#     net        = net2,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n3 = cut_cross_sections2(
#     net        = net3,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n4 = cut_cross_sections2(
#     net        = net4,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n5 = cut_cross_sections2(
#     net        = net5,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n6 = cut_cross_sections2(
#     net        = net6,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n7 = cut_cross_sections2(
#     net        = net7,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n8 = cut_cross_sections2(
#     net        = net8,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n9 = cut_cross_sections2(
#     net        = net9,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n10 = cut_cross_sections2(
#     net        = net10,
#     id         = "comid",
#     cs_widths  = pmax(50, net10$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n11 = cut_cross_sections2(
#     net        = net11,
#     id         = "comid",
#     cs_widths  = pmax(50, net11$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n12 = cut_cross_sections2(
#     net        = net12,
#     id         = "comid",
#     cs_widths  = pmax(50, net12$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n13 = cut_cross_sections2(
#     net        = net13,
#     id         = "comid",
#     cs_widths  = pmax(50, net13$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n14 = cut_cross_sections2(
#     net        = net14,
#     id         = "comid",
#     cs_widths  = pmax(50, net14$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n15 = cut_cross_sections2(
#     net        = net15,
#     id         = "comid",
#     cs_widths  = pmax(50, net15$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n16 = cut_cross_sections2(
#     net        = net16,
#     id         = "comid",
#     cs_widths  = pmax(50, net16$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n17 = cut_cross_sections2(
#     net        = net17,
#     id         = "comid",
#     cs_widths  = pmax(50, net17$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n18 = cut_cross_sections2(
#     net        = net18,
#     id         = "comid",
#     cs_widths  = pmax(50, net18$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   n19 = cut_cross_sections2(
#     net        = net19,
#     id         = "comid",
#     cs_widths  = pmax(50, net19$bf_width * 7),
#     num        = 2,
#     fix_braids = FALSE,
#     add        = TRUE
#   ),
#   times = 2  # Number of repetitions for each function
# )
# 
# # print(result)
# avg_results <-
#   result_fewer %>%
#   dplyr::group_by(expr) %>%
#   dplyr::summarise(time = mean(time))
# # 
# # data_info <- 
# #   data_info %>% 
# #   dplyr::left_join(
# #     avg_results,
# #     by = c("dataset" = "expr")
# #   )
# # data_info %>% 
# #   ggplot2::ggplot() + # ggplot2::geom_point(ggplot2::aes(x = nrows, y = time))
# 
# # Benchmarking multiple functions
# result_fixed <- microbenchmark::microbenchmark(
#   n1 = cut_cross_sections2(
#     net        = net1,
#     id         = "comid",
#     cs_widths  = pmax(50, net1$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n2 = cut_cross_sections2(
#     net        = net2,
#     id         = "comid",
#     cs_widths  = pmax(50, net2$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n3 = cut_cross_sections2(
#     net        = net3,
#     id         = "comid",
#     cs_widths  = pmax(50, net3$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n4 = cut_cross_sections2(
#     net        = net4,
#     id         = "comid",
#     cs_widths  = pmax(50, net4$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n5 = cut_cross_sections2(
#     net        = net5,
#     id         = "comid",
#     cs_widths  = pmax(50, net5$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n6 = cut_cross_sections2(
#     net        = net6,
#     id         = "comid",
#     cs_widths  = pmax(50, net6$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n7 = cut_cross_sections2(
#     net        = net7,
#     id         = "comid",
#     cs_widths  = pmax(50, net7$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n8 = cut_cross_sections2(
#     net        = net8,
#     id         = "comid",
#     cs_widths  = pmax(50, net8$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n9 = cut_cross_sections2(
#     net        = net9,
#     id         = "comid",
#     cs_widths  = pmax(50, net9$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n10 = cut_cross_sections2(
#     net        = net10,
#     id         = "comid",
#     cs_widths  = pmax(50, net10$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n11 = cut_cross_sections2(
#     net        = net11,
#     id         = "comid",
#     cs_widths  = pmax(50, net11$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n12 = cut_cross_sections2(
#     net        = net12,
#     id         = "comid",
#     cs_widths  = pmax(50, net12$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n13 = cut_cross_sections2(
#     net        = net13,
#     id         = "comid",
#     cs_widths  = pmax(50, net13$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n14 = cut_cross_sections2(
#     net        = net14,
#     id         = "comid",
#     cs_widths  = pmax(50, net14$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n15 = cut_cross_sections2(
#     net        = net15,
#     id         = "comid",
#     cs_widths  = pmax(50, net15$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n16 = cut_cross_sections2(
#     net        = net16,
#     id         = "comid",
#     cs_widths  = pmax(50, net16$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n17 = cut_cross_sections2(
#     net        = net17,
#     id         = "comid",
#     cs_widths  = pmax(50, net17$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n18 = cut_cross_sections2(
#     net        = net18,
#     id         = "comid",
#     cs_widths  = pmax(50, net18$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   n19 = cut_cross_sections2(
#     net        = net19,
#     id         = "comid",
#     cs_widths  = pmax(50, net19$bf_width * 7),
#     num        = 5,
#     fix_braids = TRUE,
#     add        = TRUE
#   ),
#   times = 2  # Number of repetitions for each function
# )
# avg_results
# 121770.0885 / 1000
# 
# 125000000/60
# result_fixed
# test_data <- list(net1, net2, net3, net4, net5, net6, net7, net8, net9,
#                   net10, net11, net12, net13, net14, net15, net16, net17, net18, net19)
# 
# data_info <- lapply(1:length(test_data), function(i) {
#   message(i, "/", length(test_data))
#   bs <- find_braids(network = test_data[[i]], return_as = "list")
#   
#   tibble::tibble(
#     dataset  = paste0("n", i),
#     nrows = nrow(test_data[[i]]),
#     npoints = mapview::npts(test_data[[i]]),
#     braid_count =   length(bs),
#     braid_comids =   length(unique(unname(unlist(bs))))
#   )
#   
# }) %>% 
#   dplyr::bind_rows()
# 
# data_info %>% 
#   ggplot2::ggplot() +
#   ggplot2::geom_point(ggplot2::aes(x = nrows, y = npoints))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = braid_comids))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = braid_count))
# 
# avg_fewer_results <-
#   result_fewer %>%
#   dplyr::group_by(expr) %>%
#   dplyr::summarise(time = mean(time)) %>% 
#   dplyr::mutate(
#     fixed = "unfixed_fewer"
#   )  
# avg_fixed_results <- 
#   result_fixed %>% 
#   dplyr::group_by(expr) %>% 
#   dplyr::summarise(time = mean(time)) %>% 
#   dplyr::mutate(
#     fixed = "fixed"
#   )  
#   # dplyr::rename(
#   #   fixed_time = time
#   # )
# 
# avg_results <- 
#   result %>% 
#   dplyr::group_by(expr) %>% 
#   dplyr::summarise(time = mean(time)) %>% 
#   dplyr::mutate(
#     fixed = "unfixed"
#   ) 
#   # dplyr::rename(
#   #   unfixed_time = time
#   # )
# 
# 
# res <- dplyr::bind_rows(avg_fixed_results, avg_results, avg_fewer_results)
# 
# data_info <-
#   data_info %>% 
#   dplyr::left_join(
#     res,
#     by = c("dataset" = "expr")
#   )
# 
# data_info %>% 
#   ggplot2::ggplot() +
#   # ggplot2::geom_line(ggplot2::aes(x = nrows, y = time, color = fixed))
#   # ggplot2::geom_line(ggplot2::aes(x = npoints, y = time, color = fixed))
#   # ggplot2::geom_line(ggplot2::aes(x = braid_count, y = time, color = fixed))
#   ggplot2::geom_line(ggplot2::aes(x = braid_comids, y = time, color = fixed))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = npoints)) 
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = braid_comids))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = time, color = fixed))
#   # ggplot2::geom_point(ggplot2::aes(x = nrows, y = time, color = fixed))

# mapview::mapview(singles, color = "dodgerblue") + 
#   mapview::mapview(main_singles, color = "red") +
#   mapview::mapview(ms_xs, color = "yellow") +
#   mapview::mapview(div_singles, color = "green") +
#   mapview::mapview(div_xs, color = "cyan") + 
#   mapview::mapview(transects, color = "hotpink") +
#   net2
# 
# 
# main_flines <- 
#   b %>% 
#   dplyr::group_by(braid_id) %>% 
#   # dplyr::mutate(
#   #   mainstem = ifelse(divergence == 0, "mainstem", "div")
#   # ) %>% 
#   # dplyr::filter(mainstem == "mainstem")
#   dplyr::filter(divergence == 0) %>% 
#   dplyr::ungroup()
# 
# mapview::mapview(main_flines) + 
#   mapview::mapview(b, color = "red") +
#   mapview::mapview(mb, color = "green") +
#   mapview::mapview(mb, color = "green") +
#   transects
# # cross sections on braided river segments
# braid_xs <- 
#   transects %>% 
#   dplyr::filter(hy_id %in% dplyr::filter(braids, braid_id != "no_braid")$comid) %>% 
#   dplyr::left_join(
#     sf::st_drop_geometry(
#       dplyr::select(
#       dplyr::filter(braids, braid_id != "no_braid"), comid, braid_id, is_multibraid
#       )
#     ),
#     by = c("hy_id" = "comid")
#   )
# mapview::mapview(singles) + 
#   mapview::mapview(main_singles, color = "red") +
#   mapview::mapview(div_singles, color = "green") +
#   mapview::mapview(braid_xs, color = "cyan")
# mapview::mapview(no_braid) + 
#   mapview::mapview(b, color = "red") +
#   mapview::mapview(mb, color = "green") +
#   mapview::mapview(braid_xs, color = "green")
# 
# 
# braid_ids <- unique(Reduce(c, strsplit(unique(braid_xs$braid_id), ", ")))
# 
# bxs <- lapply(1:length(braid_ids), function(k) {
#   
#   k = 6
#   bid <- braid_ids[k]
#   bid
#   blines <- 
#     braids %>% 
#     dplyr::filter(braid_id == bid)
#   tmp <- braid_xs %>% 
#     dplyr::filter(braid_id == bid)
#   
#   ucomids <- unique(tmp$hy_id)
#   
#   mapview::mapview(braids, color = "dodgerblue")  +
#     mapview::mapview(blines, color = "red") + 
#     tmp 
#   for (i in ucomids) {
#     i = 1
#     
#     ucomids[i]
#   
#    toi <- dplyr::filter(tmp, hy_id %in% ucomids[i])
#    others <- dplyr::filter(tmp, !hy_id %in% ucomids[i])
#    
#    match_lines <- others[sf::st_nearest_feature(toi, others) , ]
#    mapview::mapview(braids, color = "dodgerblue")  +
#      mapview::mapview(blines, color = "red") + 
#      tmp + 
#      toi +
#      others + 
#      mapview::mapview(match_lines, color = "red") 
#    mapview::mapview()   
#   }
#   tmp
#   
#   mapview::mapview(braids, color = "dodgerblue")  + mapview::mapview(blines, color = "red") + tmp 
#   
#   })

# make_braids_gif(
#   network    = find_braids(network = net2,return_as = "dataframe",nested = F,add = T),
#   save_path  = "D:/gif/braid_gif6.gif",
#   height     = 8,
#   width      = 12,
#   gif_width  = 1900,
#   gif_height = 1600,
#   delay      = 0.80,
#   verbose    = T
# )




