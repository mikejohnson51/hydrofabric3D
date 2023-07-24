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

# *********************************************
# ---- Test data for fix_braid_transects() ----
# *********************************************

# net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 150)
# net2 <- nhdplusTools::navigate_network(start = 17608987, mode = "UT", distance_km = 100)
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT", distance_km = 200)

# net2 <- dplyr::select(net2, comid, divergence, totdasqkm, fromnode, tonode)

# add bf_width column to network
net3 <-
  net2 %>% 
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>% 
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm))) 

transects_fixed = cut_cross_sections2(
  net       = net3,
  id        = "comid",
  cs_widths = pmax(50, net3$bf_width * 7),
  num       = 5,
  fix_braids = TRUE,
  add       = TRUE
  ) 

transects_nofix = cut_cross_sections2(
  net       = net3,
  id        = "comid",
  cs_widths = pmax(50, net3$bf_width * 7),
  num       = 5,
  fix_braids = FALSE,
  add       = TRUE
) 

mapview::mapview(transects_fixed, color = "red") + 
  mapview::mapview(transects_nofix, color = "green") +
  mapview::mapview(net3, color = "dodgerblue")

# *********************************************
# *********************************************

# function for extending/updating transect cross section linestrings 
# Description: Specifically to be used for situations where a river network is braided. 
# x: transect line to try and extend to cover braided river sections 
# id: unique identifier (COMID/hy_id) of transect line 
# geoms_to_cut: other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across 
# cs_width: numeric, cross section width
# bf_width: numeric, bankful width
augment_transect <- function(x, id, geoms_to_cut, cs_width, bf_width) {

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
  
  # head_map$as_list()
  # tail_map$as_list()
  
  tail_ext <- tail_map$get("line")
  head_ext <- head_map$get("line")
  
  # mapview::mapview(geoms_to_cut, color = "dodgerblue") +
  #   mapview::mapview(x, color = "gold")+
  #   mapview::mapview(tail_ext, color = "red") +
  #   mapview::mapview(head_ext, color = "green")
  # mapview::mapview(res_geom, color = "cyan") +
  # mapview::mapview(tail_ext) + head_ext + tline + others
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

fix_braid_transects <- function(net, transect_lines) {
  
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
  
  # not braided flowlines
  not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
  
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
  
  # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
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
   
   for(i in 1:nrow(div_xs)) {
     
     # message("i: ", i, "/", nrow(div_xs))
     
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
       # message("--- SKIPPING - NO INTERSECTION AFTER EXTENDING TRANSECT ---")
       # message("--- CONTINUING TO NEXT TRANSECT ---")
       # message("=================")
       next
     }
     
     # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects (EXCEPT SELF)
     # AND it does NOT intersect with any other transects in 'xs' (the rest of the main transects lines)
     if(
       !any(lengths(sf::st_intersects(res_geom, div_xs[-i,])) > 0) & 
       !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
        ) {
       
       # # # message stating that replacement was made
       # message("----> REPLACING ", i, " transect")
       
       # replace geometry with extended line
       div_xs[i,]$geometry <- sf::st_geometry(res_geom)
       
       # flag determining whether transect should be replaced
       div_xs[i, ]$changed <- TRUE
       
       }
     # message("=================")
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
   
   # transform CRS back to input CRS
   if(start_crs2 != 5070) {
     message("Transforming CRS back to EPSG:", start_crs2)
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

start_counts

already <- 
  multis_xs %>% 
  dplyr::filter(hy_id %in% start_counts$comid)
mapview::mapview(multis, color = "dodgerblue") +
  mapview::mapview(multis_xs, color = "red") +
  mapview::mapview(transect_lines, color = "cyan") +
  # mapview::mapview(others, color = "red") +
  # mapview::mapview(res, color = "green") +
  # mapview::mapview(res_geom, color = "green") + 
  mapview::mapview(already, color = "green")
# 1078587
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
    # message("FINAL x: ", x)
    # message("=======================")
    
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

mapview::mapview(singles, color = "dodgerblue") + 
  mapview::mapview(main_singles, color = "red") +
  mapview::mapview(ms_xs, color = "yellow") +
  mapview::mapview(div_singles, color = "green") +
  mapview::mapview(div_xs, color = "cyan") + 
  mapview::mapview(transects, color = "hotpink") +
  net2


main_flines <- 
  b %>% 
  dplyr::group_by(braid_id) %>% 
  # dplyr::mutate(
  #   mainstem = ifelse(divergence == 0, "mainstem", "div")
  # ) %>% 
  # dplyr::filter(mainstem == "mainstem")
  dplyr::filter(divergence == 0) %>% 
  dplyr::ungroup()

mapview::mapview(main_flines) + 
  mapview::mapview(b, color = "red") +
  mapview::mapview(mb, color = "green") +
  mapview::mapview(mb, color = "green") +
  transects
# cross sections on braided river segments
braid_xs <- 
  transects %>% 
  dplyr::filter(hy_id %in% dplyr::filter(braids, braid_id != "no_braid")$comid) %>% 
  dplyr::left_join(
    sf::st_drop_geometry(
      dplyr::select(
      dplyr::filter(braids, braid_id != "no_braid"), comid, braid_id, is_multibraid
      )
    ),
    by = c("hy_id" = "comid")
  )
mapview::mapview(singles) + 
  mapview::mapview(main_singles, color = "red") +
  mapview::mapview(div_singles, color = "green") +
  mapview::mapview(braid_xs, color = "cyan")
mapview::mapview(no_braid) + 
  mapview::mapview(b, color = "red") +
  mapview::mapview(mb, color = "green") +
  mapview::mapview(braid_xs, color = "green")


braid_ids <- unique(Reduce(c, strsplit(unique(braid_xs$braid_id), ", ")))

bxs <- lapply(1:length(braid_ids), function(k) {
  
  k = 6
  bid <- braid_ids[k]
  bid
  blines <- 
    braids %>% 
    dplyr::filter(braid_id == bid)
  tmp <- braid_xs %>% 
    dplyr::filter(braid_id == bid)
  
  ucomids <- unique(tmp$hy_id)
  
  mapview::mapview(braids, color = "dodgerblue")  +
    mapview::mapview(blines, color = "red") + 
    tmp 
  for (i in ucomids) {
    i = 1
    
    ucomids[i]
  
   toi <- dplyr::filter(tmp, hy_id %in% ucomids[i])
   others <- dplyr::filter(tmp, !hy_id %in% ucomids[i])
   
   match_lines <- others[sf::st_nearest_feature(toi, others) , ]
   mapview::mapview(braids, color = "dodgerblue")  +
     mapview::mapview(blines, color = "red") + 
     tmp + 
     toi +
     others + 
     mapview::mapview(match_lines, color = "red") 
   mapview::mapview()   
  }
  tmp
  
  mapview::mapview(braids, color = "dodgerblue")  + mapview::mapview(blines, color = "red") + tmp 
  
  })

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




