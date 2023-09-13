# ***************************************
# ------------- THRESHOLDING ------------
# ***************************************
net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 300)
# net3 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 150)
net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 60)
net2$geometry %>% plot()
# net3$geometry %>% plot()
net2 <-
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 

system.time({
  transects = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = F,
    # terminal_id = NULL,
    braid_threshold = NULL,
    terminal_id = "terminalpa",
    add       = TRUE
  )
})
plot(transects$geometry)
net <- net2
transect_lines <- transects
# plot(transects$geometry)
terminal_id = "terminalpa"
max_transect = 2000

braid_threshold = NULL

no_thresh <- test_thresholder(net = net, 
                           transect_lines = transects, terminal_id = "terminalpa",
                           max_transect = NULL
                           )
plot(no_thresh$geometry)

thresh <- test_thresholder(
  net = net, 
  transect_lines = transects,
  terminal_id = "terminalpa",
  # max_transect = NULL
  max_transect = 2000
  )
plot(thresh$geometry)
no_thresh <- test_thresholder(
  net = net, 
  transect_lines = transects,
  terminal_id = "terminalpa",
  # max_transect = NULL
  max_transect = NULL
)
no_thresh
plot(no_thresh$geometry)
thresh$geometry %>% sf::st_length()
thresh$geometry %>% sf::st_length() %>% max()
thresh$lengthm %>% max()
sf::st_length(thresh$geometry )

test_thresholder <- function(
    net, 
    transect_lines,
    terminal_id = NULL,
    braid_threshold = NULL,
    max_transect = NULL
) {
  
  # # # *******************************
  # # # ---- Test data for braids  ----
  # # # *******************************
  # 
  # # net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 300)
  # # net3 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 150)
  # net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 60)
  # net2$geometry %>% plot()
  # # net3$geometry %>% plot()
  # net2 <-
  #   net2 %>%
  #   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  #   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
  # # 
  # 
  # system.time({
  #   transects = cut_cross_sections(
  #     net       = net2,
  #     id        = "comid",
  #     cs_widths = pmax(50, net2$bf_width * 7),
  #     num       = 6,
  #     densify = 2,
  #     fix_braids = F,
  #     # terminal_id = NULL,
  #     braid_threshold = NULL,
  #     terminal_id = "terminalpa",
  #     add       = TRUE
  #   )
  # })
  # net <- net2
  # transect_lines <- transects
  # # plot(transects$geometry)
  # terminal_id = "terminalpa"
  # max_transect = 2000
  # 
  # braid_threshold = NULL
  
  # net <- net2
  # transect_lines <- transects
  # # plot(transects$geometry)
  # terminal_id = "terminalpa"
  # max_transect = 2000
  # # max_transect = NULL
  # braid_threshold = NULL
  
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
  
  # # add braid_id column to network
  # braids <- find_braids(
  #   network   = net,
  #   return_as = "dataframe",
  #   nested    = TRUE,
  #   # nested    = FALSE,
  #   add       = TRUE
  # )
  
  # add braid_id column to network
  braids <- find_braids(
    network     = net, 
    terminal_id = terminal_id,
    add         = TRUE,
    nested      = TRUE,
    verbose     = FALSE
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
    
    # message("===== NO 'xs' transect lines =====")
    # message("===== returning original data =====")
    message("No transect lines intersect with braided flowlines, returning original transect lines")
    return(transect_lines)
    
  } else {
    message( "Fixing ", nrow(xs) , " transect lines intersecting with braided flowlines lines")
    # message("===== ", nrow(xs) , " 'xs' transect lines =====")
    # message("===== returning original data =====")
  }
  
  
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
    message("i: ", i, "/", nrow(xs))

    # curr <- xs[i, ]
    # i = 1
    # i = 83
    # mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(xs, color = "red") +
    # mapview::mapview(xs[i, ], color = "green")
    
    
    # comid of transect line
    com <- xs$hy_id[i]
    # com
    # 1079071
    
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
    # xs$lengthm
    # geoms_to_cut  = others
    extend_maps <- geos_augment_transect2(
      cross_section = xs[i, ],
      geoms_to_cut  = geos::as_geos_geometry(others$geometry),
      geom_ids      = others$comid,
      # max_distance  = NULL,
      max_distance  = max_transect,
      by            = 1,
      keep_geom = FALSE
      # keep_geom     = FALSE
    )
    
    # # geoms_to_cut  = others
    # extend_maps <- geos_augment_transect(
    #   cross_section = xs[i, ],
    #   geoms_to_cut  = geos::as_geos_geometry(others$geometry),
    #   geom_ids      = others$comid,
    #   # max_distance  = NULL,
    #   max_distance  = max_transect,
    #   by            = 1,
    #   as_df = FALSE,
    #   carry_geom = FALSE
    #   # keep_geom     = FALSE
    # )
    
    # extend_maps$head$get("total_distance")
    # extend_maps$tail$get("total_distance")
    # extend_maps$head$as_list()
    
    # tmpy <- geos_extend_transects(
    #   starter_line   = geos::as_geos_geometry(xs$geometry[i]),
    #   head_distance  = extend_maps$head$get("total_distance"),
    #   tail_distance  = extend_maps$tail$get("total_distance"),
    #   extra_distance = xs$cs_widths[i]/2
    # ) %>% 
    #   sf::st_as_sf() %>% 
    #   dplyr::mutate(
    #     new="newline"
    #   )
    # tmpy
    # 2081 + 358 + 179
    # mapview::mapview(braids, color = "dodgerblue") +
    #   mapview::mapview(xs, color = "red") +
    # mapview::mapview(xs[i, ], color = "green") +
    #   mapview::mapview(tmpy, color = "red") 
    # xs[i, ]$cs_widths
    # tmpy %>% sf::st_length()
    
    if(!is.null(max_transect)) {
      
    
      if(xs[i, ]$cs_widths/2 +  extend_maps$head$get("total_distance") >= max_transect | xs[i, ]$cs_widths/2 + extend_maps$tail$get("total_distance")  >= max_transect) {
        message("----> i: ", i)
        message("----> EXTEND DISTANCE IS GREATER THAN max_transect")
        
        message("----> head extension: ", xs[i, ]$cs_widths/2 +  extend_maps$head$get("total_distance"))
        message("----> tail extension: ", xs[i, ]$cs_widths/2 + extend_maps$tail$get("total_distance"))
        message('========================================')
      }
    }
    # 
    # extend_maps$tail$get("total_distance") + extend_maps$head$get("total_distance")

    # extend_maps$head$as_list()
    position <- extend_maps$head$get("position")
    
    # extend_maps$head$as_list()
    # 
    # within_threshold(
    #   base_distance   = xs[i, ]$cs_widths,
    #   head_distance  = extend_maps$head$get("total_distance"),
    #   tail_distance  = extend_maps$tail$get("total_distance"),
    #   threshold  = max_transect
    # )
    
    # extend_maps$head$get("is_thresholded") | extend_maps$tail$get("is_thresholded")
    if(any(extend_maps$head$get("is_thresholded"), extend_maps$tail$get("is_thresholded"))) {
      
      message("TRANSECT ", i, " IS THRESHOLDED (longer than max_transects: ", max_transect, ")")
      
    }
    
    # if a flowline on the inner portion of a braid, make extension and insert
    # or if trhye extended line would be longer than the max_transects threshold value
    # if(position == "inner") {
    if(position == "inner" | any(extend_maps$head$get("is_thresholded"), extend_maps$tail$get("is_thresholded"))) {
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
        
        # updatem geometry with new, extended cross section
        xs$geometry[i] <- sf::st_geometry(
          sf::st_as_sf(res_geom)
        )
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        
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
  }
  
  # mapview::mapview(xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") + other_xs
  #   mapview::mapview(tmp, color = "green")
  
  # net_intersects <- sf::st_intersects(not_braids, xs)
  
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
  
  # dropped <- xs[net_intersects, ]
  # kept <- xs[!net_intersects, ]
  # xs %>%
  #   dplyr::filter(hy_id == "1078527", cs_id == 2) %>%
  #   sf::st_length()
  # 
  # mapview::mapview(kept, color = "red") +
  #   mapview::mapview(dropped, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue")  +
  #   mapview::mapview(not_braids, color = "gold") +
  #   mapview::mapview(xs, color = "gold")

  # remove updated cross sections that intersect with the NOT BRAIDED flowlines
  if(any(net_intersects)) {
    
    # message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
    xs <- xs[!net_intersects, ]
    
  }
  
  
  # dplyr::filter(xs, 
  #               !changed,
  #               !relative_position %in% c("inner", "no_intersects")
  # )
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs <- dplyr::filter(xs, 
                           !changed,
                           relative_position != "inner"
                           )
  
  # other_xs = dplyr::filter(xs, !changed)
  
  # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  
  # # inner transects that haven't been changed
  unchanged_inners <- dplyr::filter(xs,
                                    !changed,
                                    relative_position == "inner")
  # dplyr::filter(xs, 
  #               !changed,
  #               relative_position == "inner")

  # unchanged_inners <- dplyr::filter(xs,
  #                         !changed,
  #                         relative_position %in% c("inner", "no_intersects")
  #                         )
  # 
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
    # message("Removing ", table((unlist(other_intersects)))["TRUE"], " transect lines from 'other_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[!other_intersects, ]
  }

  # if there are still other (non "inner") transects, do extension processing
  if (nrow(other_xs) == 0) {
    
      # bind together final updated transect lines
      out <- dplyr::select(xs, 
                           -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                           -head_distance, -tail_distance, -head_cuts, -tail_cuts
                           )
    
    } else {
    # i = 28
      # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
      # loop through the remaining transects that were NOT "inner" lines, and do extensions
      for (i in 1:nrow(other_xs)) {
        
        message("i: ", i, "/", nrow(other_xs))
        
        # other_xs[i, ]$hy_id
        # other_xs[i, ]$cs_id
        
        # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
        if (other_xs$relative_position[i] == "no_intersects") {
          message("==== SKIPPING: ", i, " ==== ")
          message("==== other_xs$hy_id[i]: ",  other_xs$hy_id[i], " ====")
  
          # flag determining whether transect should be replaced
          other_xs$changed[i] <- TRUE
          
          next
        }
        
        # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
        res_geom <- geos_extend_transects(
          starter_line   = geos::as_geos_geometry(other_xs$geometry[i]),
          head_distance  = other_xs$head_distance[i],
          tail_distance  = other_xs$tail_distance[i],
          extra_distance = other_xs$cs_widths[i]/2
        )
        
        
        # sf::st_intersects(res_geom, xs)
        # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
        # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom, 
        #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
        
        # geos::as_geos_geometry(other_xs$geometry[i])
        # !any(
        #   geos::geos_intersects_any(
        #     geos::as_geos_geometry(xs),
        #     geos::as_geos_geometry( 
        #       geos::as_geos_geometry(other_xs$geometry[i])
        #       )
        #   )) &
        #   !any(geos::geos_intersects_any(
        #     geos::as_geos_geometry(other_xs[-i, ]),
        #     geos::as_geos_geometry(
        #       geos::as_geos_geometry(other_xs$geometry[i])
        #       )
        #   ))
        
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
        
      }
      
      # # # keep only the transects that were changed/extended
      # other_drop <- dplyr::filter(other_xs, !changed)
      final_intersects <- geos::geos_intersects_any(
                                geos::as_geos_geometry(other_xs),
                                geos::as_geos_geometry(not_braids)
                              )
      
      other_xs <- other_xs[!final_intersects, ]
      
      # # # # # # keep only the transects that were changed/extended
      # other_xs <- dplyr::filter(other_xs, changed)
      
      # changed_other <- dplyr::filter(other_xs, changed)
      # 
      # out2 <- dplyr::bind_rows(
      #   dplyr::select(xs, 
      #                 -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
      #                 -head_distance, -tail_distance, -head_cuts, -tail_cuts
      #   ),
      #   dplyr::select(changed_other,
      #                 -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
      #                 -head_distance, -tail_distance, -head_cuts, -tail_cuts
      #   )
      # )
      # mapview::mapview(braids, color = "dodgerblue") +
      #   mapview::mapview(out2, color = "red") +
      # mapview::mapview(other_xs, color = "green") +
      #   mapview::mapview(changed_other, color = "red") + xs
      
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
      
  } 
  # else {
    
  #   # message("===== NO 'other_xs' transect lines =====")
  #   
  #   # # bind together final updated transect lines
  #   # out <- dplyr::select(xs, 
  #   #                      -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
  #   #                      -head_distance, -tail_distance, -head_cuts, -tail_cuts
  #   # )
  #   
  # }
  
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
  
  # # keep track of all original crossections
  # all_xs <- paste0(out$hy_id, "_", out$cs_id)
  
  # fin <-  dplyr::bind_rows(
  #                       # from original transect_lines, remove all of the cross sections on braids,
  #                       dplyr::select(
  #                         dplyr::filter(   
  #                           dplyr::mutate(transect_lines, 
  #                                         tmp_id = paste0(hy_id, "_", cs_id)
  #                           ),
  #                           !tmp_id %in% all_xs
  #                         ),
  #                         -tmp_id
  #                       ),
  #                       # updated braid cross sections
  #                       out
  #                     )
  
  
  # mapview::mapview(braids, color = "dodgerblue") +
  # mapview::mapview(not_braids, color = "gold") +
  # mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(fin, color = "red") + out
  # mapview::mapview(transect_lines2, color = "red")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
}


#' Determine the relative position of a transect line within a braid given the count of intersecting lines 
#' Determine the relative position of a transect line within a braid, given the count of intersections that occur after extending the transect line in both direction (from the head and tail of the transect line) 
#' @param head_count numeric, count of intersections extending from the "head" end of an extended transect line  
#' @param tail_count numeric, count of intersections extending from the "tail" end of an extended transect line  
#'
#' @return character ("no_intersects", "outer_single", "outer_multi", "inner" , or "in_between")
#' @export
#'
#' @examples
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

#' Extend a transect line outwards by a certain distance from the head and tail directions of the line
#' Internal function
#' @param starter_line geos_geometry, original transect line to extend outwards
#' @param head_distance numeric, distance (meters) to extend from "head" of the line
#' @param tail_distance numeric, distance (meters) to extend from "tail" of the line
#' @param extra_distance numeric, any extra distance (meters) the line should be extended after the original head/tail distances (this is typically going to be the cross section width divded by 2)
#'
#' @return geos_geometry, extended by specified distance
#' @export
#'
#' @examples
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

#' Determine the distances needed to extend a transect linestring geometry across neighboring flowlines
#' Internal function that takes a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
#' extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided. 
#' @param cross_section geos_geometry, transect line to try and extend to cover braided river sections. Dataframe row must contain a "cs_width", "bf_width", "hy_id", and a "geometry" column 
#' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across 
#' @param geom_ids character, unique identifier (comid/hy_id) of transect line 
#' @param max_distance numeric, maximum distance (meters) to extend line out by
#' @param by numeric, distance to incrementelly extend out transect line. 
#' @param as_df logical, whether to return data as dataframe or a list of fastmap::fastmap(). Default is TRUE, returns as a dataframe.
#' If FALSE, then the return is a list of 2 fastmap::fastmap() containing information on how the head and tail extensions for the transect geometries
#' @param carry_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns. 
#'
#' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line. 
#' @export
#'
#' @examples
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
  # cross_section$lengthm
  # cross_section$geometry %>% sf::st_length()
  # max_distance = 2000
  # max_transect
  # max_distance <- (max_distance - cross_section$lengthm)/2
  # 820 + 820 + 360
  # max_distance  = max_transect
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
  # geom_ids      = others$comid
  # # max_distance  = NULL
  # max_distance  = max_transect
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  
  # extract values from cross_section dataframe
  cs_width <- cross_section$cs_widths
  bf_width <- cross_section$bf_width
  id       <- cross_section$hy_id
  
  # convert cross section line to geos_geometry
  cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(by)) {
    by = bf_width/2
  }
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(max_distance)) {
    
    total_distance <- max(cs_width * 5)
    # max_distance <- max(cs_width * 5)
    # max_distance <- 10000
    
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, max(c(total_distance, 2000), by = by))
    
  } else {

    # calculate the max distance that a line should possibly be extended from both ends
    total_distance <- abs((max_distance - cross_section$lengthm)/2)
    # max_distance <- abs((max_distance - cross_section$lengthm)/2)
    
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, total_distance, by = by)
    
  }
  
  # # calculate the max distance that a line should possibly be extended from both ends 
  # max_distance <- (max_distance - cross_section$lengthm)/2
  
  # # sequence from 0 to the max possible extension distance 
  # dist_vect <- seq(0, total_distance, by = by)
  # dist_vect <- seq(0, max_distance, by = by)
  # dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  
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
    threshold     = max_distance,
    dir           = "head",
    map           = TRUE
  )
  
  # head_map$as_list()
  # head_map$get("is_thresholded")
  
  if(head_map$get("is_thresholded")) {
    message("----> id: ", id, " was thresholded in HEAD direction")
  }
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(id), 
    threshold     = max_distance,
    dir           = "tail",
    map           = TRUE
  )
  
  # tail_map$as_list()
  # tail_map$get("is_thresholded")
  
  if(tail_map$get("is_thresholded")) {
    message("----> id: ", id, " was thresholded in TAIL direction")
  }
 
  # within_threshold(
  #   base_distance = cs_width,
  #   head_distance = head_map$get("total_distance"),
  #   tail_distance = tail_map$get("total_distance"),
  #   threshold     = max_distance
  #                  )
  # head_map$get("total_distance")
  # tail_map$get("total_distance")
  
  
  # head_map$as_list()
  # tail_map$as_list()
  # cross_section$lengthm
  # 359 + 820
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
  
  # # if the total distance in either direction is greater than the max distance, set the total distance to 0
  # head_map$set("total_distance",
  #              ifelse(head_map$get("total_distance") > max_distance, 
  #                                          # max_distance, 
  #                                          0,
  #                                          head_map$get("total_distance")
  #                                          )
  #                                  )
  # tail_map$set("total_distance", 
  #              ifelse(tail_map$get("total_distance") > max_distance, 
  #                                         # max_distance, 
  #                                         0,
  #                                         tail_map$get("total_distance")
  #                                         )
  #                                  )
  
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

#' Determine the distances needed to extend a transect linestring geometry across neighboring flowlines v2
#' Internal function that takes a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
#' extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided. 
#' @param cross_section geos_geometry, transect line to try and extend to cover braided river sections. Dataframe row must contain a "cs_width", "bf_width", "hy_id", and a "geometry" column 
#' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across 
#' @param geom_ids character, unique identifier (comid/hy_id) of transect line 
#' @param max_distance numeric, maximum distance (meters) to extend line out by
#' @param by numeric, distance to incrementelly extend out transect line. 
#' @param keep_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns. 
#'
#' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line. 
#' @export
#'
#' @examples
geos_augment_transect2 <- function(cross_section,
                                  geoms_to_cut, 
                                  geom_ids,
                                  max_distance = NULL,
                                  by = NULL, 
                                  keep_geom = TRUE
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
  # cross_section$lengthm
  # cross_section$geometry %>% sf::st_length()
  # max_distance = 2000
  # max_transect
  # max_distance <- (max_distance - cross_section$lengthm)/2
  # 820 + 820 + 360
  # max_distance  = max_transect
  
  # cross_section = xs[i, ]
  # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
  # geom_ids      = others$comid
  # # max_distance  = NULL
  # max_distance  = max_transect
  # by            = 1
  # as_df         = FALSE
  # carry_geom    = FALSE
  
  # extract values from cross_section dataframe
  cs_width <- cross_section$cs_widths
  bf_width <- cross_section$bf_width
  id       <- cross_section$hy_id
  
  # convert cross section line to geos_geometry
  cs_line  <- geos::as_geos_geometry(cross_section$geometry)
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(by)) {
    by = bf_width/2
  }
  
  # if no "by" argument is given, then the default becomes bf_width/2
  if(is.null(max_distance)) {
    
    total_distance <- max(cs_width * 5)
    # max_distance <- max(cs_width * 5)
    # max_distance <- 10000
    
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, max(c(total_distance, 2000), by = by))
    
  } else {
    
    # calculate the max distance that a line should possibly be extended from both ends
    total_distance <- abs((max_distance - cross_section$lengthm)/2)
    # max_distance <- abs((max_distance - cross_section$lengthm)/2)
    
    # sequence from 0 to the max possible extension distance 
    dist_vect <- seq(0, total_distance, by = by)
    
  }
  
  # # calculate the max distance that a line should possibly be extended from both ends 
  # max_distance <- (max_distance - cross_section$lengthm)/2
  
  # # sequence from 0 to the max possible extension distance 
  # dist_vect <- seq(0, total_distance, by = by)
  # dist_vect <- seq(0, max_distance, by = by)
  # dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
  
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
    threshold     = max_distance,
    dir           = "head",
    map           = TRUE
  )
  
  # head_map$as_list()
  # head_map$get("is_thresholded")
  
  if(head_map$get("is_thresholded")) {
    message("----> id: ", id, " was thresholded in HEAD direction")
  }
  
  # extend line out from TAIL side of line 
  # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with 
  tail_map <- geos_extend_out(
    x             = 1,
    line          = cs_line, 
    distances     = dist_vect,
    geoms_to_cut  = geoms_to_cut, 
    geom_ids      = geom_ids,
    ids           = c(id), 
    threshold     = max_distance,
    dir           = "tail",
    map           = TRUE
  )
  
  # tail_map$as_list()
  # tail_map$get("is_thresholded")
  
  if(tail_map$get("is_thresholded")) {
    message("----> id: ", id, " was thresholded in TAIL direction")
  }
  
  head_map$as_list()
  tail_map$as_list()
  
  # check if the final line is within the maximum transect line distance (threshold)
  if(!within_threshold(
                    base_distance = cs_width,
                    head_distance = head_map$get("total_distance"),
                    tail_distance = tail_map$get("total_distance"),
                    threshold     = max_distance
                  )
     ) {
    
    # head_map$as_list()
    # tail_map$as_list()
    message("==== id: ", id, " ====")
    message("==== THRESHOLDING LINE ====")
    # reset the values in the head and tail maps to make it so the line is NOT going to be extended,
    # because extending the line will make it longer than the maximum transect distance threshold
    reset_line_map(head_map)
    reset_line_map(tail_map)

    # head_map$as_list()
    # tail_map$as_list()
    
  }
  # head_map$get("total_distance")
  # tail_map$get("total_distance")
  
  
  # head_map$as_list()
  # tail_map$as_list()
  # cross_section$lengthm
  # 359 + 820
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
  
  # # if the total distance in either direction is greater than the max distance, set the total distance to 0
  # head_map$set("total_distance",
  #              ifelse(head_map$get("total_distance") > max_distance, 
  #                                          # max_distance, 
  #                                          0,
  #                                          head_map$get("total_distance")
  #                                          )
  #                                  )
  # tail_map$set("total_distance", 
  #              ifelse(tail_map$get("total_distance") > max_distance, 
  #                                         # max_distance, 
  #                                         0,
  #                                         tail_map$get("total_distance")
  #                                         )
  #                                  )
  
  # set the 'position' and 'pending' values for head_map and tail_map
  # then drop geometries if carry_geom == FALSE
  
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
  
  # if keep_geom is FALSE, remove geometry linestrings from maps before returning
  if(!keep_geom) {
    head_map$remove("line")
    tail_map$remove("line")
  }
  
  # return list with 2 elements. First is the head extension data and the second is the tail extension data
  return(
    list(
      head = head_map,
      tail = tail_map
    )
  )
    
  
  
}

# check if the base line length + extension from head and tail is
# less than or equal to the threshold distance value
# base_distance: numeric, length (meters) of the line
# head_distance: numeric, minimum length (meters) to extend line from HEAD direction to cross all neighboring flowlines
# tail_distance: numeric, minimum length (meters) to extend line from TAIL direction to cross all neighboring flowlines
# threshold: numeric, maximum length of the final, fully extended line. Default is NULL (which will return TRUE)
# Returns a boolean, TRUE if line is within the threshold value, FALSE otherwise
within_threshold <- function(
    base_distance, 
    head_distance  = 0, 
    tail_distance  = 0, 
    threshold = NULL
) {
  
  # # inputs to function
  # base_distance = cs_width
  # head_distance  =  head_map$get("total_distance")
  # tail_distance  =  tail_map$get("total_distance")
  # threshold = max_distance
  
  # if no threshold is given (NULL), return TRUE, the line extensions WILL be within the threshold
  if(is.null(threshold)) {
    return(TRUE)
  }
  
  # the base_distance is the length of the line,
  # so the extra_distance is 1/2 the base_distance (i.e. the distance from the midpoint to either end of the line)
  extra_distance <- base_distance/2
  
  # head/tail distance + the extra distance that the line has to go after the head/tail extension. 
  # Reminder: 
  # the head/tail extension distance will extend the line out the minimum distance needed to 
  # cross over all flowlines in that direction ('head' or 'tail' direction), thus there is still an extra_distance that the line needs
  # to extend out further in order for the final line to cross the final intersected flowline and still 
  # have a cross section distancce AFTER the final flow line intersection
  # IF the initial head_distance/tail_distance is 0, 
  # - that means that the line is NOT going to extend out in that direction, thus we can set the extra_head/extra_tail distancce to 0
  # - otherwise extra_head/extra_tail is the head_distance/tail_distance + 1/2 the initial 'base_distance'
  extra_head <- ifelse(head_distance > 0, head_distance + extra_distance, 0)
  extra_tail <- ifelse(tail_distance > 0, tail_distance + extra_distance, 0)
  
  # total length of line
  total_distance <- extra_head + extra_tail + base_distance
  
  # boolean of whether total_distance is valid (less than the 'threshold')
  is_valid_distance <- total_distance < threshold
  
  return(is_valid_distance)
  
}

# Reset fastmap containing the extension data for a line
# Used for when the extension is going to be greater than the max transect length threshold
# Set distance, total_distance, count to 0
# Set index to 1
# Set cut_ids to just the original lines id
# Set is_thresholded to TRUE
# line_map is a fastmap with the following keys: index, distance, total_distance, cut_ids, count, and is_thresholded
# id: numeric or character ID of the original line to set the "cut_ids" key to. Default is NULL which will use the first id in "cut_ids"
reset_line_map <- function(line_map, id = NULL) {
  
  # set index to 1
  line_map$set("index", 1)
  
  # set distance and total_distance to 0
  line_map$set("distance", 0)
  line_map$set("total_distance", 0)

  # if id is NULL (not provided in function call), then use the FIRST id in 'cut_ids' key
  if(is.null(id)) {
    line_map$set("cut_ids", line_map$get("cut_ids")[1])
  } else {
    line_map$set("cut_ids", id)
  }
  # set count to 0  
  line_map$set("count", 0)
  
  # set is_thresholded to TRUE
  line_map$set("is_thresholded", TRUE)
  
  # return(line_map)
  
}

# WHILE LOOP IMPLEMENTATION INSTEAD OF RECURSION

#' Extend a linestring outward and return the minimum linestring (or details of minimum linestring) that crosses all possible other linestrings (geoms_to_cut) for a the given direction
#' Internal Function
#' @param x start index of distances vector
#' @param line transect line that should be extended
#' @param distances numeric vector of distance values (meters) in ascending order (sorted)
#' @param geoms_to_cut geos_geometry, all other linestrings (all linestrings other than 'line') that should be cut across (typically other linestrings making up a braided section of river)
#' @param geom_ids character or numeric vector of unique identifers for each linestring in 'geoms_to_cut'
#' @param ids character or numeric vector of unique identifier of the 'line' argument
#' @param dir character, either "head" or "tail", indicating which direction to extend 'line' out
#' @param map logical, whether to return a fastmap::fastmap() containing details about
#'  the extending line (distance, number of intersections, IDs of intersected geometries, etc.) 
#'  or to just return the extended line. Default is TRUE, which will return a fastmap::fastmap() that can be used 
#'  later on to extend the line the necessary distance.  If FALSE, a geos_geometry of the extended linestring is returned
#'
#' @return fastmap::fastmap() with details on line extension, or a geos_geometry of the extended line
#' @export
#'
#' @examples
geos_extend_out <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    geom_ids,
    ids, 
    threshold = NULL,
    dir = "head",
    map = TRUE
) {
  
  # x             = 1
  # line          = cs_line
  # distances     = dist_vect
  # geoms_to_cut  = geoms_to_cut
  # geom_ids      = geom_ids
  # threshold = 2000
  # ids           = c(id)
  # dir           = "head"
  # map           = TRUE
  
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
  
  # store the original 'line' in case it needs to be returned
  stash_line <- line
  
  # store the original 'ids' in case it needs to be returned
  stash_ids <- ids
  
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
    # message("dcount: ", dcount)
    
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
    
    # if a threshold is given and the total distance count is greater than the threshold, stop iteration
    if (!is.null(threshold) && dcount > threshold) {
      # message("!!!!!!!!!!!!!!!! ")
      # message("!!!!!!!! dcount > threshold ", dcount, " > ", threshold, " !!!!!!!! ")
      # message("!!!!!!!!!!!!!!!! ")
      
      # # if specified, return distance map of info and line
      if(map) {
        dmap$mset(
          index           = 1, 
          distance        = 0, 
          total_distance  = 0,
          line            = stash_line,
          cut_ids         = stash_ids,
          count           = 0,
          direction       = dir,
          is_thresholded  = TRUE
        )
        
        return(dmap)
      }
      
      # otherwise just return the orginal line
      return(stash_line)
      # break
    }
    
    if (xx >= length(distances)) {
      # message("!!!!!!!!!!!!!!!! ")
      # message("!!!!!!!! xx >= distance: ", xx, " >= ", length(distances), " !!!!!!!! ")
      # message("!!!!!!!!!!!!!!!! ")
      break
    }
    
    # extend line out to midpoint of distances vector
    crosser <- geos_extend_line(line, distances[xx], dir = dir)
    
    # # extend line out to where the binary search first hit a line
    # crossersf <- st_extend_line(sf::st_as_sf(line), 
    #                             distances[xx], end = dir)  
    # mapview::mapview(sf::st_as_sf(crosser), color = "green") +
    #   mapview::mapview(sf::st_as_sf(line), color = "green") +
    #   mapview::mapview(xs[i, ], color = "red") +
    #   mapview::mapview(braids, color = "dodgerblue")
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
      direction       = dir,
      is_thresholded  = FALSE
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

#' Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
#'
#' @param distances numeric vector sorted in ascending order
#' @param line geos_geometry, linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
#' @param geoms_to_cut geos_geometry, geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
#' @param direction character, either "head" or "tail", indicating which end of the line to extend out.
#'
#' @return index of 'distance' vector, representing the minimum extension distance for a line to intersect nearby geometries
#' @export
#'
#' @examples
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
    new_line <- geos_extend_line(line, distances[M], dir = direction)
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


#' Extend a linestring out and determine the minimum extension distance to cross all possible other geometries
#' Internal function, implements a binary search algorithm to determine the minimum distance the geos_geometry linestring 'line' must be extended to cross all possible geometries in 'geoms_to_cut'
#' @param distances numeric vector in ascending order
#' @param line geos_geometry linestring
#' @param geoms_to_cut geos_geomtry linestrings to try and interesect by extending 'line'
#' @param direction character, direction to extend linestring from. Either "head", "tail" or "both". Default is "head".
#'
#' @return numeric value indicating the index of the value in 'distances' that is the minimum extension distance to intersect all possible geometries in 'geoms_to_cut'
binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
  
  # left and right pointers at the start and end of the 'distances' vector, respectively
  L = 1
  R = length(distances)
  
  # while left pointer is less than or equal to the right pointer, run binary search
  while(L <= R) {
    
    # calculate midpoint
    M = (L + R) %/% 2
    
    # if midpoint is at the end or start of the 'distances' vector, return left pointer
    if(M == 0 | M == length(distances)) {
      # message("EARLY STOPPING bc M = ", M)
      # message("RETURNING L = ", L)
      return(L)
    }
    
    # extend linestring by distance value at midpoint (M pointer)
    new_line <- st_extend_line(line, distances[M], dir = direction)
    
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
  }
  
  return(L)
}



#' Find the direction of the endpoints of a linestring
#'
#' @param line geos_geometry, linestring
#'
#' @return numeric vector of angle directions of a given linestring
#' @export
#'
#' @examples
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

#' Extend a geos_geometry linestring from, one or both ends, by a given distance (meters)
#'
#' @param line sf linestring or geos_geometry linestring to extend
#' @param distance numeric value in meters or a vector of length 2 if 'end = "both"' where 
#       the first value in the vector will extend that tail by that value and the second value extends the head by that value c(tail, head).
#       If a single value is given when end = "both", the value is recycled and used to extend both ends
#' @param end character, determines whether to extend the linestring from the 'tail', 'head' or 'both' ends
#' @param with_crs logical, whether a CRS should be prescribed to extended output geos_geometry linestring
#'
#' @return geos_geometry linestring extended by 'distance' from either the 'head', 'tail' or 'both' ends of the original linestring
#' @export
#'
#' @examples
geos_extend_line <- function(line, 
                             distance,
                             dir = "both", with_crs = TRUE) {
  # line <- xs[1, ]
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  if(!dir %in% c("head", "tail", "both")) {
    stop("Invalid input 'dir' must either be 'head', 'tail', or 'both'")
  }
  
  # crs <- wk::wk_crs(line)
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- as.matrix(coords[c("x", "y")])
  # coords <- coords[c("x", "y")]
  
  # which index to keep
  to_keep <- dir != c("tail", "head")
  
  # dir coords index we want to keep
  dirs <- c(1, nrow(coords))[to_keep]
  
  # get directions of the direction of interest
  directions <- geos_linestring_dir(line)[to_keep]
  
  # if only a single distance, duplicate it, otherwise reverse the first 2 distances
  distances <- if (length(distance) == 1) {
    rep(distance, 2) 
  } else {
    rev(distance[1:2])
  }
  
  # adjust dir point coordinates 
  coords[dirs, ]  <- coords[dirs, ] + distances[to_keep] * c(cos(directions), sin(directions))
  
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

#' Apply flowline braid length threshold to braided network dataset 
#' Internal function
#' @param x sf object of braided flowlines (output of find_braids())
#' @param originals sf object, non braided flowlines from the same network
#' @param threshold numeric, remove braids with a total braid flowline length greater than 'threshold'
#' @param verbose logical, whether to output progress messages. If TRUE (default), messages are outputted
#'
#' @return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset, a list of 2 sf objects containing the updated braids with braids removed that are greater than the threshold value, and an sf object containing the original remaining network linestrings
#' @export
#'
#' @examples
braid_thresholder <- function(x, 
                              originals,
                              threshold = NULL, 
                              verbose   = TRUE
) {
  
  # x         = braids
  # originals = not_braids
  # threshold = 30000
  # verbose   = TRUE
  
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
  to_keep <- dplyr::filter(unpacked, 
                           braid_length <= threshold)$comid
  
  # # COMIDs that are too large, add them back to the "not_braids" data
  # to_drop <- dplyr::filter(x, !comid %in% to_keep)
  
  # keep track of keeping and removing count
  orig_nrows <- nrow(originals)
  x_nrows  <- nrow(x)
  
  # add the "too big braid COMIDs" back to original "not_braids" data 
  # and set these comids braid_ids to "no_braid" and is_multibraid = FALSE
  originals <- dplyr::bind_rows(
                  originals,
                  # dplyr::select(
                  dplyr::mutate(
                    dplyr::filter(
                      x, 
                      !comid %in% to_keep
                    ),
                    braid_id      = "no_braid", 
                    is_multibraid = FALSE
                  )
                  #   -has_mainstem
                  # )
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
    not_braids = originals
    )
  )
  
}

#' Find the total length of all flowlines of each braid in a NHDPlus dataset
#' Internal function
#' @param x sf object of braided flowlines (output of find_braids()). Requires 'braid_id' and 'comid' column.
#' @param keep_geom logical, whether to keep geometries or not. Default is FALSE to drop geometries
#' @param verbose logical, whether to output progress messages. If TRUE (default), messages are outputted
#'
#' @return dataframe with 3 columns, the braid_id, length (meters) of the total flowline length, and comids within the braid, and optionally a sf geometry
#' @export
braid_lengths <- function(x, 
                          keep_geom = FALSE,
                          verbose   = TRUE
                          ) {
  
  # input check for input 'x'
  if(is.null(x)) {
    stop("missing 'x' input argument")
  }
  
  # unpack nested braid_id column0
  unpacked <- unnpack_braids(x)

  unpacked <-
    unpacked %>% 
    dplyr::group_by(braid_id) %>%
    dplyr::summarize(
      braid_length = as.numeric(
        sum(sf::st_length(geometry), na.rm = T)
      ),
      comids = paste0(c(comid), collapse = ", ")
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(-braid_length)
  
  # drop geometries if keep_geom is FALSE
  if(!keep_geom) {
    
    unpacked <- sf::st_drop_geometry(unpacked)
  }
  
  return(unpacked)
  
}

# *****************************
# ------------- v2 ------------
# *****************************
net2 <- nhdplusTools::navigate_network(start = 15175471, mode = "UT",  distance_km = 450)
# net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 150)
# net2 <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 250)
# rm(net3)
net2$geometry %>% plot()
# "15175471" %in% net2$comid 
# net3$geometry %>% plot()
net2 <-
  net2 %>%
  dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
  dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
# 

system.time({
  transects = cut_cross_sections(
    net       = net2,
    id        = "comid",
    cs_widths = pmax(50, net2$bf_width * 7),
    num       = 5,
    densify = 2,
    fix_braids = F,
    # terminal_id = NULL,
    braid_threshold = NULL,
    terminal_id = "terminalpa",
    add       = TRUE
  )
})

plot(transects$geometry)

net <- net2
transect_lines <- transects
terminal_id = "terminalpa"
# max_transect = 2000
braid_threshold = 20000

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param terminal_id character, column name containing a unique identifier, delineating seperate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects2 <- function(
    net, 
    transect_lines,
    terminal_id = NULL,
    braid_threshold = NULL
) {
  
  net <- net2
  transect_lines <- transects
  terminal_id = "terminalpa"
  # max_transect = 2000
  braid_threshold = 20000
  
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
  
  # # add braid_id column to network
  # braids <- find_braids(
  #   network   = net,
  #   return_as = "dataframe",
  #   nested    = TRUE,
  #   # nested    = FALSE,
  #   add       = TRUE
  # )
  
  # add braid_id column to network
  braids <- find_braids(
    network     = net, 
    terminal_id = terminal_id,
    add         = TRUE,
    nested      = TRUE,
    verbose     = FALSE
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
    # dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
    dplyr::ungroup()
  
  ###### BRAID LENGTH CHECKING
  # braid_sizes <- braid_lengths(braids, keep_geom = TRUE)
  # braid_sizes$braid_length
  # hist(braid_sizes$braid_length)
  # hist(braid_sizes$braid_length, breaks = 20)
  # n = 1
  # braid_sizes$geometry[n] %>% plot()
  # braid_sizes$geometry[n+1] %>% plot()
  # braid_sizes <- dplyr::mutate(braid_sizes, 
  #                   braid_cat = dplyr::case_when(
  #                       braid_length >= 20000 ~ "big",
  #                       TRUE                  ~ "small"))

  
  if(!is.null(braid_threshold)) {
    
    # remove braids that have a total flowline length greater than braid_threshold
    braids <- braid_thresholder(
                  x         = braids, 
                  originals = not_braids, 
                  threshold = braid_threshold,
                  # new_braid_ids = "thresholded",
                  verbose   = TRUE
                )
  
    # tmp_small <- braids2$braids
    # tmp_bigs <- braids2$not_braids
    # ggplot2::ggplot() +
    #   # ggplot2::geom_sf(
    #   #   # data = tmp_bigs,
    #   #   data = dplyr::filter(tmp_bigs, braid_id == "thresholded"),
    #   #   color = "dodgerblue",lwd = 1
    #   #   ) +
    #   # ggplot2::geom_sf(data = braids,
    #   #                  color = "green", lwd = 1)+
    #   # ggplot2::geom_sf(data = tmp_small, color = "red", alpha = 0, lwd = 1.5 )
    # ggplot2::geom_sf(data = tmp_small, color = "red", alpha = 1, lwd = 1.5 ) +
    # ggplot2::geom_sf(
    #   # data = tmp_bigs,
    #   data = dplyr::filter(tmp_bigs, braid_id == "thresholded"),
    #   color = "dodgerblue",  alpha = 0.7, lwd = 1
    # )
    
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
    
    # message("===== NO 'xs' transect lines =====")
    # message("===== returning original data =====")
    message("No transect lines intersect with braided flowlines, returning original transect lines")
    return(transect_lines)
    
  } else {
    message( "Fixing ", nrow(xs) , " transect lines intersecting with braided flowlines lines")
    # message("===== ", nrow(xs) , " 'xs' transect lines =====")
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
  aug_time = 0
  ext_time = 0
  inner_time = 0
  
  extc <- c()
  augc <- c()
  # i = 1
  
  # i = 279
  # components <- find_connected_components(braids)
  # cuid <- components$component_id %>% unique()
  # cuid
  # # i = 279
  # k = 110
  # cuid[k]
  # components %>% 
  # ggplot2::ggplot() + 
  #   ggplot2::geom_sf(ggplot2::aes(color = component_id))+
  #   gghighlight::gghighlight(component_id == cuid[k])
  # 
  # components$component_id %>% unique()
  system.time({
  # for(i in 1:100) {
  for(i in 1:nrow(xs)) {
    # for(i in 1:17) {
    message("i: ", i, "/", nrow(xs))
    # i = 18 
    # # transect line
    # tline <- xs[i, ]$geometry
    # i = 2
    # curr <- xs[i, ]
    # i
    # i = 279
    i = 665
    # comid of transect line
    com <- xs$hy_id[i]
    
    # braid IDs of interest
    bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
    bids
    components <- find_connected_components(braids)
    
    braid_list <- strsplit(components$braid_id, ", ")
    
    bid_in_braids <- unlist(lapply(braid_list, function(vec) all(vec %in% bids)))
    
    # Here what ive done is used the find_connected_components on the braids 
    # object and then I've located the connected componment IDs of the braids that 
    # contains ALL and ONLY ALL of the braid_ids in "bids"
    # maybe in addition to using this i can find additional neighbors by looking for braid_ids that contains my BIDs and ONLY my bids but also extra braid_ids (this would only be for cases when the "bid" is a single braid_id? )
    
    # bid_in_braids
    components[bid_in_braids, ]
    # unique(components[bid_in_braids, ]$component_id)
    tmp_comps %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_sf(ggplot2::aes(color = component_id))+
      gghighlight::gghighlight(component_id %in%  unique(components[bid_in_braids, ]$component_id))
      # gghighlight::gghighlight(component_id == cuid[k])
    
    cuid <- components$component_id %>% unique()
    cuid
    components$braid_id
    # 708-712
    # 648 - 649
    braid_list <- strsplit(components$braid_id, ", ")
    # logical_vector <- lapply(braid_list, function(vec) all(vec %in% bids))
    bid_in_braids <- lapply(braid_list, function(vec) vec %in% bids)
    bid_in_braids <- unlist(lapply(braid_list, function(vec) all(vec %in% bids)))
    components[bid_in_braids, ]
    bid_in_braids
    bids %in% strsplit(components$braid_id, ", ") 
    strsplit(components$braid_id, ", ") 
    strsplit(components$braid_id, ", ") %in% bids
    components[strsplit(components$braid_id, ", ") %in% bids, ]
    tmp_comps <- unnpack_braids(components)
    components %>% 
      # dplyr::filter(braid_id %in% bids)
      dplyr::filter(!grepl(paste0(bids, collapse = "|"), braid_id))
    tmp <- 
      components %>% 
      # dplyr::filter(braid_id %in% bids)
      dplyr::filter(grepl(paste0(bids, collapse = "|"), braid_id)) %>% 
      dplyr::mutate(
        braid_id_list = strsplit(braid_id, ", ")
      ) %>% 
      dplyr::mutate(
        # braid_id_bool =  braid_id_list %in% bids
        braid_id_bool =  list(bids %in% braid_id_list)
      ) %>% 
      dplyr::relocate(braid_id_list, braid_id_bool)
      # dplyr::filter(braid_id_list %in% )
    
    tmp$braid_id_list %>% class()
    
    strsplit(tmp$braid_id, ", ")[[1]]
    # i = 279
    k = 115
    cuid[k]
    tmp_comps %>% 
      ggplot2::ggplot() + 
      ggplot2::geom_sf(ggplot2::aes(color = component_id))+
      gghighlight::gghighlight(component_id == cuid[k])
    
    components$component_id %>% unique()
    
    # get neighboring braid ID for our current braid
    neighbor_braids <- get_neighbor_braids(x = braids, ids = bids, only_unique = T)
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- dplyr::filter(
      braids,
      braid_id %in% neighbor_braids,
      comid != com
    )
    braids$braid_id
    others2 <- dplyr::filter(
      braids,
      grepl(bids, braid_id),
      comid != com
    )
    others2 <- dplyr::filter(
      braids,
      grepl(    paste0(bids, collapse = "|"), braid_id),
      comid != com
    )
    others
    others2
    others2$braid_id
    paste0(bids, collapse = "|")
    others2$braid_id
    mapview::mapview(xs[i, ], color = "green") +
      mapview::mapview(others, color = "red") +
      mapview::mapview(others2, color = "red") +
      mapview::mapview(braids, color = "green") +
      mapview::mapview(not_braids, color = "dodgerblue")  
    aug_time1 <- Sys.time()
    
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
    
    aug_time2 <- Sys.time()
    
    aug_final_time <- aug_time2 - aug_time1
    augc <- c(augc, aug_final_time)
    aug_time = aug_time + as.numeric(aug_final_time)
    
    # paste0(round(aug_final_time, 1), " ",  units(aug_final_time))
    
    # extend_maps$head$as_list()
    position <- extend_maps$head$get("position")
    
    ext_time1 <- Sys.time()
    
    # if a flowline on the inner portion of a braid, make extension and insert
    if(position == "inner") {
      # message("Extending ", i, " and checking if valid replacement...")
      
      inner_time1 <- Sys.time()
      
      # extend line out by total distance key values in head and tail maps
      res_geom <- geos_extend_transects(
        starter_line   = geos::as_geos_geometry(xs$geometry[i]),
        head_distance  = extend_maps$head$get("total_distance"),
        tail_distance  = extend_maps$tail$get("total_distance"),
        extra_distance = xs$cs_widths[i]/2
      )
      
      inner_time2 <- Sys.time()
      inner_final_time <- inner_time2 - inner_time1
      inner_time = inner_time + as.numeric(inner_final_time)
      
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
        
        # updatem geometry with new, extended cross section
        xs$geometry[i] <- sf::st_geometry(
          sf::st_as_sf(res_geom)
        )
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        
      }
      
      # # update relative position column
      # xs$relative_position[i] <- extend_maps$head$get("position")
      # 
      # # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
      # xs$pending[i] <- extend_maps$head$get("pending")
      # 
      # # update head/tail distances values in dataframe w/ values from head/tail hashmaps
      # xs$head_distance[i] <- extend_maps$head$get("total_distance")
      # xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
      # 
      # # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
      # xs$head_cuts[i] <- extend_maps$head$get("count")
      # xs$tail_cuts[i] <- extend_maps$tail$get("count")
    } 
    # else {
      
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
    # }
      
      ext_time2 <- Sys.time()
      ext_final_time <- ext_time2 - ext_time1
      extc <- c(extc, ext_final_time)
      ext_time = ext_time +  as.numeric(ext_final_time)
      }
    })
  
  # mapview::mapview(xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") + other_xs
  #   mapview::mapview(tmp, color = "green")
  
  # net_intersects <- sf::st_intersects(not_braids, xs)
  
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
    
    # message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
    xs <- xs[!net_intersects, ]
    
  }
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs = dplyr::filter(xs, 
                           !changed,
                           relative_position != "inner"
                           )
  
  # other_xs = dplyr::filter(xs, !changed)
  
  # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  
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
    # message("Removing ", table((unlist(other_intersects)))["TRUE"], " transect lines from 'other_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[!other_intersects, ]
  }
  
  # if there are still other (non "inner") transects, do extension processing
  if (nrow(other_xs) == 0) { 
  # if (nrow(other_xs) > 0) {
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, 
                         -braid_id, -is_multibraid,
                         # -has_mainstem, 
                         -changed, -pending,
                         -head_distance, -tail_distance, -head_cuts, -tail_cuts
    )
    
  } else {
    
    # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
    
    system.time({

    # loop through the remaining transects that were NOT "inner" lines, and do extensions
    for (i in 1:nrow(other_xs)) {
      
      message("i: ", i, "/", nrow(other_xs))
      
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
        extra_distance = other_xs$cs_widths[i]/2
      )
      
      
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
      
    }
      
      
    })
    
    # # # keep only the transects that were changed/extended
    # other_drop <- dplyr::filter(other_xs, !changed)
    
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    -braid_id, -is_multibraid, 
                    # -has_mainstem,
                    -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      ),
      dplyr::select(other_xs,
                    -braid_id, -is_multibraid,
                    # -has_mainstem, 
                    -changed, -pending,
                    -head_distance, -tail_distance, -head_cuts, -tail_cuts
      )
    )
    
  }
  
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
  # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
  fin <-  dplyr::bind_rows(
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
  mapview::mapview(braids, color = "dodgerblue") +
  mapview::mapview(not_braids, color = "gold") +
  # mapview::mapview(transect_lines, color = "green") +
  mapview::mapview(fin, color = "red")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
}

# Extend a transect line outwards by a certain distance from the head and tail directions of the line
# starter_line is the original transect line to extend (geos_geoemtry)
# head_distance: numeric, distance (meters) to extend from HEAD of the line
# tail_distance: numeric, distance (meters) to extend from TAIL of the line
# extra_distance: Any extra distance the line should be extended after the original head/tail distances (THIS IS TYPICALLY GOING TO BE cs_width/2)

#' Extend a transect line outwards by a certain distance from the head and tail directions of the line
#' Internal function
#' @param starter_line geos_geometry, original transect line to extend outwards
#' @param head_distance numeric, distance (meters) to extend from "head" of the line
#' @param tail_distance numeric, distance (meters) to extend from "tail" of the line
#' @param extra_distance numeric, any extra distance (meters) the line should be extended after the original head/tail distances (this is typically going to be the cross section width divded by 2)
#'
#' @return geos_geometry, extended by specified distance
#' @export
#'
#' @examples
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

#' Determine the distances needed to extend a transect linestring geometry across neighboring flowlines
#' Internal function that takes a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
#' extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided. 
#' @param cross_section geos_geometry, transect line to try and extend to cover braided river sections. Dataframe row must contain a "cs_width", "bf_width", "hy_id", and a "geometry" column 
#' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across 
#' @param geom_ids character, unique identifier (comid/hy_id) of transect line 
#' @param max_distance numeric, maximum distance (meters) to extend line out by
#' @param by numeric, distance to incrementelly extend out transect line. 
#' @param as_df logical, whether to return data as dataframe or a list of fastmap::fastmap(). Default is TRUE, returns as a dataframe.
#' If FALSE, then the return is a list of 2 fastmap::fastmap() containing information on how the head and tail extensions for the transect geometries
#' @param carry_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns. 
#'
#' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line. 
#' @export
#'
#' @examples
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

#' Extend a linestring outward and return the minimum linestring (or details of minimum linestring) that crosses all possible other linestrings (geoms_to_cut) for a the given direction
#' Internal Function
#' @param x start index of distances vector
#' @param line transect line that should be extended
#' @param distances numeric vector of distance values (meters) in ascending order (sorted)
#' @param geoms_to_cut geos_geometry, all other linestrings (all linestrings other than 'line') that should be cut across (typically other linestrings making up a braided section of river)
#' @param geom_ids character or numeric vector of unique identifers for each linestring in 'geoms_to_cut'
#' @param ids character or numeric vector of unique identifier of the 'line' argument
#' @param dir character, either "head" or "tail", indicating which direction to extend 'line' out
#' @param map logical, whether to return a fastmap::fastmap() containing details about
#'  the extending line (distance, number of intersections, IDs of intersected geometries, etc.) 
#'  or to just return the extended line. Default is TRUE, which will return a fastmap::fastmap() that can be used 
#'  later on to extend the line the necessary distance.  If FALSE, a geos_geometry of the extended linestring is returned
#'
#' @return fastmap::fastmap() with details on line extension, or a geos_geometry of the extended line
#' @export
#'
#' @examples
geos_extend_out <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    geom_ids,
    ids, 
    dir = "head",
    map = TRUE
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
    crosser <- geos_extend_line(line, distances[xx], dir = dir)
    
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

#' Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
#'
#' @param distances numeric vector sorted in ascending order
#' @param line geos_geometry, linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
#' @param geoms_to_cut geos_geometry, geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
#' @param direction character, either "head" or "tail", indicating which end of the line to extend out.
#'
#' @return index of 'distance' vector, representing the minimum extension distance for a line to intersect nearby geometries
#' @export
#'
#' @examples
geos_bs_distance2 <- function(
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
    new_line <- geos_extend_line(line, distances[M], dir = direction)
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


# ****************************************
# ------------- LATEST (geos) ------------
# ****************************************

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param terminal_id character, column name containing a unique identifier, delineating seperate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#'
#' @return sf object of transect linestrings
#' @export
#'
#' @examples
fix_braid_transects <- function(
    net, 
    transect_lines,
    terminal_id = NULL,
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
  
  # # add braid_id column to network
  # braids <- find_braids(
  #   network   = net,
  #   return_as = "dataframe",
  #   nested    = TRUE,
  #   # nested    = FALSE,
  #   add       = TRUE
  # )
  
  # add braid_id column to network
  braids <- find_braids(
                  network     = net, 
                  terminal_id = terminal_id,
                  add         = TRUE,
                  nested      = TRUE,
                  verbose     = FALSE
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
    
    # message("===== NO 'xs' transect lines =====")
    # message("===== returning original data =====")
    message("No transect lines intersect with braided flowlines, returning original transect lines")
    return(transect_lines)
    
  } else {
    message( "Fixing ", nrow(xs) , " transect lines intersecting with braided flowlines lines")
    # message("===== ", nrow(xs) , " 'xs' transect lines =====")
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
        
        # updatem geometry with new, extended cross section
        xs$geometry[i] <- sf::st_geometry(
          sf::st_as_sf(res_geom)
        )
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        
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
  }
  
  # mapview::mapview(xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") + other_xs
  #   mapview::mapview(tmp, color = "green")
  
  # net_intersects <- sf::st_intersects(not_braids, xs)

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
    
    # message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
    xs <- xs[!net_intersects, ]
    
  }
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs = dplyr::filter(xs, 
                           !changed,
                           relative_position != "inner"
                           )
  
  # other_xs = dplyr::filter(xs, !changed)

  # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  
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
    # message("Removing ", table((unlist(other_intersects)))["TRUE"], " transect lines from 'other_xs'")
    
    # drop div_xs transects that are overlapping with 'xs' transects
    other_xs <- other_xs[!other_intersects, ]
  }
  
  # if there are still other (non "inner") transects, do extension processing
  if (nrow(other_xs) > 0) {
    
    # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
    # loop through the remaining transects that were NOT "inner" lines, and do extensions
    for (i in 1:nrow(other_xs)) {
      
      # message("i: ", i, "/", nrow(other_xs))

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
                      extra_distance = other_xs$cs_widths[i]/2
                      )

      
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
      
    }
    
    # # # keep only the transects that were changed/extended
    # other_drop <- dplyr::filter(other_xs, !changed)
    
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)
    
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
    
    # message("===== NO 'other_xs' transect lines =====")
    
    # bind together final updated transect lines
    out <- dplyr::select(xs, 
                         -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
                         -head_distance, -tail_distance, -head_cuts, -tail_cuts
    )
    
  }
  
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

#' Determine the relative position of a transect line within a braid given the count of intersecting lines 
#' Determine the relative position of a transect line within a braid, given the count of intersections that occur after extending the transect line in both direction (from the head and tail of the transect line) 
#' @param head_count numeric, count of intersections extending from the "head" end of an extended transect line  
#' @param tail_count numeric, count of intersections extending from the "tail" end of an extended transect line  
#'
#' @return character ("no_intersects", "outer_single", "outer_multi", "inner" , or "in_between")
#' @export
#'
#' @examples
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

#' Extend a transect line outwards by a certain distance from the head and tail directions of the line
#' Internal function
#' @param starter_line geos_geometry, original transect line to extend outwards
#' @param head_distance numeric, distance (meters) to extend from "head" of the line
#' @param tail_distance numeric, distance (meters) to extend from "tail" of the line
#' @param extra_distance numeric, any extra distance (meters) the line should be extended after the original head/tail distances (this is typically going to be the cross section width divded by 2)
#'
#' @return geos_geometry, extended by specified distance
#' @export
#'
#' @examples
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

#' Determine the distances needed to extend a transect linestring geometry across neighboring flowlines
#' Internal function that takes a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
#' extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided. 
#' @param cross_section geos_geometry, transect line to try and extend to cover braided river sections. Dataframe row must contain a "cs_width", "bf_width", "hy_id", and a "geometry" column 
#' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across 
#' @param geom_ids character, unique identifier (comid/hy_id) of transect line 
#' @param max_distance numeric, maximum distance (meters) to extend line out by
#' @param by numeric, distance to incrementelly extend out transect line. 
#' @param as_df logical, whether to return data as dataframe or a list of fastmap::fastmap(). Default is TRUE, returns as a dataframe.
#' If FALSE, then the return is a list of 2 fastmap::fastmap() containing information on how the head and tail extensions for the transect geometries
#' @param carry_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns. 
#'
#' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line. 
#' @export
#'
#' @examples
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

#' Extend a linestring outward and return the minimum linestring (or details of minimum linestring) that crosses all possible other linestrings (geoms_to_cut) for a the given direction
#' Internal Function
#' @param x start index of distances vector
#' @param line transect line that should be extended
#' @param distances numeric vector of distance values (meters) in ascending order (sorted)
#' @param geoms_to_cut geos_geometry, all other linestrings (all linestrings other than 'line') that should be cut across (typically other linestrings making up a braided section of river)
#' @param geom_ids character or numeric vector of unique identifers for each linestring in 'geoms_to_cut'
#' @param ids character or numeric vector of unique identifier of the 'line' argument
#' @param dir character, either "head" or "tail", indicating which direction to extend 'line' out
#' @param map logical, whether to return a fastmap::fastmap() containing details about
#'  the extending line (distance, number of intersections, IDs of intersected geometries, etc.) 
#'  or to just return the extended line. Default is TRUE, which will return a fastmap::fastmap() that can be used 
#'  later on to extend the line the necessary distance.  If FALSE, a geos_geometry of the extended linestring is returned
#'
#' @return fastmap::fastmap() with details on line extension, or a geos_geometry of the extended line
#' @export
#'
#' @examples
geos_extend_out <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    geom_ids,
    ids, 
    dir = "head",
    map = TRUE
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
    crosser <- geos_extend_line(line, distances[xx], dir = dir)
    
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

#' Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
#'
#' @param distances numeric vector sorted in ascending order
#' @param line geos_geometry, linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
#' @param geoms_to_cut geos_geometry, geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array 
#' @param direction character, either "head" or "tail", indicating which end of the line to extend out.
#'
#' @return index of 'distance' vector, representing the minimum extension distance for a line to intersect nearby geometries
#' @export
#'
#' @examples
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
    new_line <- geos_extend_line(line, distances[M], dir = direction)
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


#' Extend a linestring out and determine the minimum extension distance to cross all possible other geometries
#' Internal function, implements a binary search algorithm to determine the minimum distance the geos_geometry linestring 'line' must be extended to cross all possible geometries in 'geoms_to_cut'
#' @param distances numeric vector in ascending order
#' @param line geos_geometry linestring
#' @param geoms_to_cut geos_geomtry linestrings to try and interesect by extending 'line'
#' @param direction character, direction to extend linestring from. Either "head", "tail" or "both". Default is "head".
#'
#' @return numeric value indicating the index of the value in 'distances' that is the minimum extension distance to intersect all possible geometries in 'geoms_to_cut'
binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
  
  # left and right pointers at the start and end of the 'distances' vector, respectively
  L = 1
  R = length(distances)
  
  # while left pointer is less than or equal to the right pointer, run binary search
  while(L <= R) {
  
    # calculate midpoint
    M = (L + R) %/% 2
    
    # if midpoint is at the end or start of the 'distances' vector, return left pointer
    if(M == 0 | M == length(distances)) {
      # message("EARLY STOPPING bc M = ", M)
      # message("RETURNING L = ", L)
      return(L)
    }
    
    # extend linestring by distance value at midpoint (M pointer)
    new_line <- st_extend_line(line, distances[M], dir = direction)

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
  }

  return(L)
}



#' Find the direction of the endpoints of a linestring
#'
#' @param line geos_geometry, linestring
#'
#' @return numeric vector of angle directions of a given linestring
#' @export
#'
#' @examples
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

#' Extend a geos_geometry linestring from, one or both ends, by a given distance (meters)
#'
#' @param line sf linestring or geos_geometry linestring to extend
#' @param distance numeric value in meters or a vector of length 2 if 'end = "both"' where 
#       the first value in the vector will extend that tail by that value and the second value extends the head by that value c(tail, head).
#       If a single value is given when end = "both", the value is recycled and used to extend both ends
#' @param end character, determines whether to extend the linestring from the 'tail', 'head' or 'both' ends
#' @param with_crs logical, whether a CRS should be prescribed to extended output geos_geometry linestring
#'
#' @return geos_geometry linestring extended by 'distance' from either the 'head', 'tail' or 'both' ends of the original linestring
#' @export
#'
#' @examples
geos_extend_line <- function(line, 
                             distance,
                             dir = "both", with_crs = TRUE) {
  # line <- xs[1, ]
  
  # if NOT a geos_geometry class, coerce
  if(!inherits(line, "geos_geometry")) {
    # convert to geos geometry
    line <- geos::as_geos_geometry(line)
  }
  
  if(!dir %in% c("head", "tail", "both")) {
    stop("Invalid input 'dir' must either be 'head', 'tail', or 'both'")
  }
  
  # crs <- wk::wk_crs(line)
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- as.matrix(coords[c("x", "y")])
  # coords <- coords[c("x", "y")]
  
  # which index to keep
  to_keep <- dir != c("tail", "head")
  
  # dir coords index we want to keep
  dirs <- c(1, nrow(coords))[to_keep]
  
  # get directions of the direction of interest
  directions <- geos_linestring_dir(line)[to_keep]
  
  # if only a single distance, duplicate it, otherwise reverse the first 2 distances
  distances <- if (length(distance) == 1) {
    rep(distance, 2) 
  } else {
    rev(distance[1:2])
  }
  
  # adjust dir point coordinates 
  coords[dirs, ]  <- coords[dirs, ] + distances[to_keep] * c(cos(directions), sin(directions))
  
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

#' Apply flowline braid length threshold to braided network dataset 
#' Internal function
#' @param x sf object of braided flowlines (output of find_braids())
#' @param originals sf object, non braided flowlines from the same network
#' @param threshold numeric, remove braids with a total braid flowline length greater than 'threshold'
#' @param new_braid_ids character, what to name braid_id column for braids that were thresholded and removed. Default is "no_braid".
#' @param verbose logical, whether to output progress messages. If TRUE (default), messages are outputted
#'
#' @return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset, a list of 2 sf objects containing the updated braids with braids removed that are greater than the threshold value, and an sf object containing the original remaining network linestrings
#' @export
#'
#' @examples
braid_thresholder <- function(x, 
                              originals,
                              threshold = NULL, 
                              new_braid_ids = "no_braid",
                              verbose   = TRUE
) {
  
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
                    # dplyr::select(
                    dplyr::mutate(
                      dplyr::filter(
                        x, !comid %in% to_keep
                      ),
                      braid_id      = new_braid_ids,
                      # braid_id      = "no_braid",
                      # braid_id      = "thresholded",
                      is_multibraid = FALSE
                    )
                      # -has_mainstem
                    # )
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

#' Find the total length of all flowlines of each braid in a NHDPlus dataset
#' Internal function
#' @param x sf object of braided flowlines (output of find_braids()). Requires 'braid_id' and 'comid' column.
#' @param keep_geom logical, whether to keep geometries or not. Default is FALSE to drop geometries
#' @param verbose logical, whether to output progress messages. If TRUE (default), messages are outputted
#'
#' @return dataframe with 3 columns, the braid_id, length (meters) of the total flowline length, and comids within the braid, and optionally a sf geometry
#' @export
braid_lengths <- function(x, 
                          keep_geom = FALSE,
                          verbose   = TRUE
) {
  
  # input check for input 'x'
  if(is.null(x)) {
    stop("missing 'x' input argument")
  }
  
  # unpack nested braid_id column0
  unpacked <- unnpack_braids(x)
  
  unpacked <-
    unpacked %>% 
    dplyr::group_by(braid_id) %>%
    dplyr::summarize(
      braid_length = as.numeric(
        sum(sf::st_length(geometry), na.rm = T)
      ),
      comids = paste0(c(comid), collapse = ", ")
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(-braid_length)
  
  # drop geometries if keep_geom is FALSE
  if(!keep_geom) {
    
    unpacked <- sf::st_drop_geometry(unpacked)
  }
  
  return(unpacked)
  
}

#' 
#' # ****************************************
#' # ------------- SECOND (geos) ------------
#' # ****************************************
#' 
#' #' Fix transects found on braided river sections
#' #'
#' #' @param net sf object of NHDplusv2 data
#' #' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' #' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline
#' #' lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' #' Default is NULL, which will attempt to fix all the braid transects in the data
#' #'
#' #' @return sf object of transect linestrings
#' #' @export
#' #'
#' #' @examples
#' fix_braid_transects <- function(
    #'     net,
#'     transect_lines,
#'     braid_threshold = NULL
#' ) {
#'   # ref_net <- nhdplusTools::navigate_network(start = 101, mode = "UT",  distance_km = 100)
#'   # # net2 <- nhdplusTools::navigate_network(start = 3555480, mode = "UT",  distance_km = 50)
#'   # net <-
#'   #   ref_net %>%
#'   #   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode) %>%
#'   #   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
#' 
#'   # ref_net <- sf::read_sf("/Users/anguswatters/Downloads/01_reference_features.gpkg", layer = "flowlines")
#'   # names(ref_net) <- tolower(names(ref_net))
#'   #
#'   # ref_net <-
#'   #   ref_net %>%
#'   #   dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
#'   #   dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
#'   # net <-
#'   #   ref_net %>%
#'   #   dplyr::filter(terminalpa == 1921053)
#'   # # dplyr::filter(terminalpa == ends[i])
#' 
#'   # transect_lines = cut_cross_sections3(
#'   #   net       = net,
#'   #   id        = "comid",
#'   #   cs_widths = pmax(50, net$bf_width * 7),
#'   #   num       = 5,
#'   #   fix_braids = F,
#'   #   add       = TRUE,
#'   #   use_original = T
#'   # )
#'   # braid_threshold <- NULL
#'   
#'   net <- nhdplusTools::navigate_network(start = 719018, mode = "UT",  distance_km = 6)
#'   net <- 
#'     net %>% 
#'     dplyr::select(comid, divergence, totdasqkm, fromnode, tonode, terminalpa) %>%
#'     dplyr::mutate(bf_width = exp(0.700    + 0.365* log(totdasqkm)))
#'   
#'   transect_lines = cut_cross_sections3(
#'     net       = net,
#'     id        = "comid",
#'     cs_widths = pmax(50, net$bf_width * 7),
#'     num       = 5,
#'     fix_braids = F,
#'     add       = TRUE,
#'     use_original = T
#'   )
#'   plot(net$geometry)
#'   plot(transect_lines$geometry, col = "red", add = T)
#'   net2 <- net
#' 
#'   # set geometry name of network to "geometry"
#'   net <- nhdplusTools::rename_geometry(net, "geometry")
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
#'   # check if there are any braids, if there aren't retransform and return original transect lines
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
#' 
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
#'   mapview::mapview(not_braids, color = "dodgerblue") +
#'   mapview::mapview(braids, color = "red")
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
#'   # data.table::data.table(xs)[1, ]
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
#'     # message("===== returning original data =====")
#'   }
#' 
#'   # braids %>%
#'   #   geos_make_collection() %>%
#'   #   geos_unary_union() %>%
#'   #   st_as_sfc()
#'   # braids %>%
#'   #   dplyr::mutate(
#'   #     geometry  =  geos::geos_geometry(.)
#'   #   ) %>%
#'   #   dplyr::relocate(geometry2)
#'   # geos::as_geos_geometry(braids )
#' 
#'   # braids$geometry <-  geos::geos_geometry(braids$geometry)
#'   mapview::mapview(braids, color = "dodgerblue") +
#'     mapview::mapview(xs, color = "red")
#'   i = 1
#'   # mapview::mapview(xs[i, ], color = "green")
#' 
#'   # Loop through every single cross section and determine:
#'   # 1. its relative position
#'   # 2. how far to extend the line
#'   # 3. in what order should transects be extended,
#'   # 4. in what direction to extend the transect
#'   for(i in 1:nrow(xs)) {
#'     # for(i in 1:17) {
#'     message("i: ", i, "/", nrow(xs))
#'     # i = 18
#'     # # transect line
#'     # tline <- xs[i, ]$geometry
#'     # i = 18
#'     # curr <- xs[i, ]
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
#'     # mapview::mapview(braids, color = "dodgerblue") +
#'     #   mapview::mapview(others, color = "red") +
#'     #   mapview::mapview(xs[i, ], color = "green")
#'     # # convert "others" geometry to geos_geometry
#'     # others$geometry <- geos::as_geos_geometry(others$geometry)
#'     #
#'     # geos::as_geos_geometry(others$geometry) %>% sf::st_geometry()
#'     #
#'     # others$geometry <- geos::as_geos_geometry(others$geometry)
#'     #
#'     # sf::st_geometry(others$geometry)
#' 
#'     # Error in st_geometry.sf(x) :
#' 
#'     # attr(obj, "sf_column") does not point to a geometry column.
#'     # Did you rename it, without setting st_geometry(obj) <- "newname"?
#' 
#'     # INPUTS INTO NEW AUGMENT TRANSECTS DF FUNCTION
#'     # cross_section = xs[i, ]
#'     # curr = xs[i, ]
#'     # geoms_to_cut <- others
#'     # max_distance = NULL
#'     # by = 1
#' 
#'     # tree <- geos::geos_strtree(braids[-81, ])
#'     # gg <- geos::as_geos_geometry(xs[1, ])
#'     # # tree[1]
#'     #
#'     # geos::geos_strtree_query(tree, gg)
#'     # ttmp <- braids[81, ]
#'     # mapview::mapview(ttmp) + xs[1, ]
#'     # tree
#' 
#'     # other_meta <- sf::st_drop_geometry(others)
#'     # geoms_to_cut <- geos::as_geos_geometry(others$geometry)
#' 
#'     # geoms_to_cut  = others
#'     extend_maps <- geos_augment_transect(
#'       cross_section = xs[i, ],
#'       geoms_to_cut  = geos::as_geos_geometry(others$geometry),
#'       geom_ids      = others$comid,
#'       max_distance  = NULL,
#'       by            = 1,
#'       as_df         = FALSE,
#'       carry_geom    = FALSE
#'     )
#'     # extend_maps$head$as_list()
#'     # extend_maps$tail$as_list()
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
#'       res_geom <- geos_extend_transects(
#'         starter_line   = geos::as_geos_geometry(xs$geometry[i]),
#'         head_distance  = extend_maps$head$get("total_distance"),
#'         tail_distance  = extend_maps$tail$get("total_distance"),
#'         extra_distance = xs$cs_widths[i]/2
#'       )
#' 
#'       # mapview::mapview(others) + braids + res_geom + not_braids
#' 
#'       # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
#'       if(
#'         # !any(
#'         #   lengths(
#'         #     sf::st_intersects(sf::st_as_sf(res_geom),
#'         #                     dplyr::filter(xs[-i,], changed)
#'         #                     )
#'         #   ) > 0)
#'         !geos::geos_intersects_any(
#'           res_geom,
#'           geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
#'         )
#'       ) {
#' 
#'         # !geos::geos_intersects_any(
#'         #   res_geom,
#'         #   geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
#'         # )
#'         # geos::geos_intersects(
#'         #   res_geom,
#'         #   geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
#'         # )
#'         # geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
#'         # if(!any(lengths(sf::st_intersects(res_geom, xs[-i,])) > 0)){
#'         # # message stating that replacement was made
#'         # message("----> REPLACING ", i, " transect")
#' 
#'         # updatem geometry with new, extended cross section
#'         xs$geometry[i] <- sf::st_geometry(
#'           sf::st_as_sf(res_geom)
#'         )
#' 
#'         # flag determining whether transect should be replaced
#'         xs$changed[i] <- TRUE
#'         # xs[i, ]$changed <- TRUE
#'       }
#' 
#'       # update relative position column
#'       xs$relative_position[i] <- extend_maps$head$get("position")
#' 
#'       # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#'       xs$pending[i] <- extend_maps$head$get("pending")
#' 
#'       # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#'       xs$head_distance[i] <- extend_maps$head$get("total_distance")
#'       xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
#' 
#'       # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#'       xs$head_cuts[i] <- extend_maps$head$get("count")
#'       xs$tail_cuts[i] <- extend_maps$tail$get("count")
#' 
#' 
#'     } else {
#'       # message("Postpone processing: ", i)
#' 
#'       # update relative position column
#'       xs$relative_position[i] <- extend_maps$head$get("position")
#' 
#'       # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#'       xs$pending[i] <- extend_maps$head$get("pending")
#' 
#'       # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#'       xs$head_distance[i] <- extend_maps$head$get("total_distance")
#'       xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
#' 
#'       # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#'       xs$head_cuts[i] <- extend_maps$head$get("count")
#'       xs$tail_cuts[i] <- extend_maps$tail$get("count")
#' 
#'     }
#' 
#'     # message("=================")
#'   }
#' 
#'   # tmp <- xs %>% dplyr::filter(is.na(relative_position))
#'   mapview::mapview(xs, color = "red") +
#'     mapview::mapview(transect_lines, color = "green") +
#'     mapview::mapview(braids, color = "dodgerblue") + net
#'     # mapview::mapview(other, color = "green")
#'   # net_intersects <- sf::st_intersects(not_braids, xs)
#'   # lengths(net_intersects)
#' 
#' 
#'   # # keep only the transects that were changed/extended
#'   # to_keep <- dplyr::filter(xs, changed)
#' 
#'   # # keep only the transects that were changed/extended
#'   # xs <- dplyr::filter(xs, changed)
#'   # mapview::mapview(xs, color = "red") + braids + not_braids
#' 
#'   # check intersection of keeps and NOT BRAID
#'   # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
#'   net_intersects <- geos::geos_intersects_any(
#'     geos::as_geos_geometry(xs),
#'     geos::as_geos_geometry(not_braids)
#'   )
#' 
#'   # message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
#'   # # net_intersects <- sf::st_intersects(not_braids, xs)
#'   # xs2 <- xs[!net_intersects, ]
#'   # mapview::mapview(net) +
#'   # mapview::mapview(xs, color  = "red") +
#'   #   mapview::mapview(xs2, color  = "green")
#' 
#'   # remove updated cross sections that intersect with the NOT BRAIDED flowlines
#'   if(any(net_intersects)) {
#' 
#'     message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
#'     xs <- xs[!net_intersects, ]
#' 
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
#'   # other_xs = dplyr::filter(xs,
#'   #                          !changed,
#'   #                          relative_position != "inner"
#'   #                          )
#'   other_xs = dplyr::filter(xs,
#'                            !changed,
#'                            relative_position != "inner"
#'                            )
#' 
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
#'   # intersections between updated inner cross sections ("xs") and the remaining inner cross sections that were NOT changed ("unchanged_inners")
#'   inner_intersects <- geos::geos_intersects_any(
#'     geos::as_geos_geometry(unchanged_inners$geometry),
#'     geos::as_geos_geometry(xs$geometry)
#'   )
#' 
#'   # add back into "xs" the unchanged inner transects that do NOT intersect with our updated/extended inner transect lines
#'   xs <- dplyr::bind_rows(
#'     xs,
#'     unchanged_inners[!inner_intersects, ]
#'   )
#' 
#'   # mapview::mapview(    xs, color  = "blue") +
#'   # mapview::mapview(    unchanged_inners, color  = "green") +
#'   #   mapview::mapview(    unchanged_inners[!inner_intersects, ], color  = "red")
#' 
#'   # # # # keep ALL "inner" transects, both the ones that were extended ("changed" == TRUE) and not changed inners
#'   # xs <- dplyr::filter(xs, changed | relative_position == "inner")
#' 
#'   # check intersection of keeps xs with other_xs
#' 
#'   # indices of other_xs transects that now intersect with the updated/extended 'xs' transects.
#'   # All the cross section lines in "xs" are now "inner" lines that were extended
#'   other_intersects <- geos::geos_intersects_any(
#'                                 geos::as_geos_geometry(other_xs$geometry),
#'                                 geos::as_geos_geometry(xs$geometry)
#'                               )
#'   # other_intersects <- sf::st_intersects(xs, other_xs)
#'   # unlist(sf::st_intersects(xs, other_xs))
#' 
#'   # net_intersects <- sf::st_intersects(not_braids, xs)
#'   # lengths(other_intersects)
#' 
#'   # if there ARE some intersections, remove those intersecting lines from 'div_xs'
#'   if(any(other_intersects)) {
#'     message("Removing ", table((unlist(other_intersects)))["TRUE"], " transect lines from 'other_xs'")
#' 
#'     # drop div_xs transects that are overlapping with 'xs' transects
#'     other_xs <- other_xs[!other_intersects, ]
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
#'       # i = 1
#'       # other_xs$relative_position[i]
#'       # other_xs$head_distance[i]
#'       # other_xs$tail_distance[i]
#'       # other_xs$head_cuts[i]
#'       # other_xs$tail_cuts[i]
#'       # other_xs$cs_widths[i]
#'       # i = 1
#'       # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
#'       if (other_xs$relative_position[i] == "no_intersects") {
#' 
#'         # flag determining whether transect should be replaced
#'         other_xs$changed[i] <- TRUE
#' 
#'         next
#'       }
#' 
#'       # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
#'       res_geom <- geos_extend_transects(
#'         starter_line   = geos::as_geos_geometry(other_xs$geometry[i]),
#'         head_distance  = other_xs$head_distance[i],
#'         tail_distance  = other_xs$tail_distance[i],
#'         extra_distance = xs$cs_widths[i]/2
#'       )
#'       
#'       mapview::mapview(sf::st_as_sf(res_geom), color = "green") +
#'         mapview::mapview(braids, color = "dodgerblue") +
#'         # mapview::mapview(xs, color = "red") +
#'         mapview::mapview(other_xs, color = "cyan")
#'         braids + res_geom + not_braids
#' 
#'       # sf::st_intersects(res_geom, xs)
#'       # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
#'       # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom,
#'       #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
#' 
#'       if(
#'         !any(
#'           geos::geos_intersects_any(
#'             geos::as_geos_geometry(xs),
#'             geos::as_geos_geometry(res_geom)
#'           )) &
#'         !any(geos::geos_intersects_any(
#'           geos::as_geos_geometry(other_xs[-i, ]),
#'           geos::as_geos_geometry(res_geom)
#'         ))
#'       ) {
#' 
#'         # # # message stating that replacement was made
#'         # message("----> REPLACING ", i, " transect")
#' 
#'         # replace geometry with extended line
#'         other_xs$geometry[i] <- sf::st_geometry(sf::st_as_sf(res_geom))
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
#'       dplyr::select(xs,
#'                     -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#'                     -head_distance, -tail_distance, -head_cuts, -tail_cuts
#'       ),
#'       dplyr::select(other_xs,
#'                     -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#'                     -head_distance, -tail_distance, -head_cuts, -tail_cuts
#'       )
#'     )
#' 
#'   } else {
#' 
#'     message("===== NO 'other_xs' transect lines =====")
#' 
#'     # bind together final updated transect lines
#'     out <- dplyr::select(xs,
#'                          -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#'                          -head_distance, -tail_distance, -head_cuts, -tail_cuts
#'     )
#' 
#'   }
#'   # mapview::mapview(out, color = "red") +
#'   #   mapview::mapview(xs, color = "green") +
#'   #   mapview::mapview(braids, color = "dodgerblue")
#'   # to_keep <- paste0(xs$hy_id, "_", xs$cs_id)
#'   # to_keep %in% all_xs
#'   # all_xs %in% to_keep
#' 
#'   # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
#'   transect_lines <-  dplyr::bind_rows(
#'     # from original transect_lines, remove all of the cross sections on braids,
#'     dplyr::select(
#'       dplyr::filter(
#'         dplyr::mutate(transect_lines,
#'                       tmp_id = paste0(hy_id, "_", cs_id)
#'         ),
#'         !tmp_id %in% all_xs
#'       ),
#'       -tmp_id
#'     ),
#'     # updated braid cross sections
#'     out
#'   )
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
#' 
#' #' Determine the relative position of a transect line within a braid given the count of intersecting lines
#' #' Determine the relative position of a transect line within a braid, given the count of intersections that occur after extending the transect line in both direction (from the head and tail of the transect line)
#' #' @param head_count numeric, count of intersections extending from the "head" end of an extended transect line
#' #' @param tail_count numeric, count of intersections extending from the "tail" end of an extended transect line
#' #'
#' #' @return character ("no_intersects", "outer_single", "outer_multi", "inner" , or "in_between")
#' #' @export
#' #'
#' #' @examples
#' check_relative_position <- function(head_count, tail_count) {
#' 
#'   # given the count of interesections from the head and tail of a linestring, return whether the line has:
#'   # - NO INTERSECTION:: (after extending linestring out to max distance)
#'   # - OUTER SINGLE: extending linestring out in both directions yielded
#'   # zero intersections in one direction AND exactly one intersection in the other direction
#'   # - OUTER MULTI: extending linestring out in both directions yielded
#'   # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
#'   # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
#'   #       INNER scenario intersection count (odd and even cases):
#'   #         intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
#'   # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
#'   # - MIDDLE/IN BETWEEN: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
#'   # ----> SKIP THIS!
#'   # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT)
#' 
#'   # TODO: Consider renaming these as No intersections,
#'   # OUTER_SINGLE = SINGLE (the outer braid flowlines)
#'   # OUTER_MULTI  = SINGLE (the outer braid flowlines)
#'   # MIDDLE = IN BETWEEN (in between the outer braid flowlines and the actual middle braid flowlines)
#'   # INNER  = MIDDLE (the actual middle braid flowline)
#' 
#'   # boolean that gets flipped to FALSE if any of the other scenarios are detected
#'   in_between = TRUE
#' 
#'   # vector of intersection counts by the extended line,
#'   # extending out FROM THE HEAD and then FROM THE TAIL
#'   counts <- c(head_count, tail_count)
#' 
#'   # 1. No intersections scenario
#'   if(all(counts == 0)) {
#' 
#'     # relative position of line
#'     line_position <- "no_intersects"
#'     # message("line_position: ", line_position)
#' 
#'     # flip in_between boolean to FALSE
#'     in_between = FALSE
#'     # message("in_between: ", in_between)
#' 
#'     return(line_position)
#' 
#'   }
#' 
#'   # 2. OUTER SINGLE scenario
#'   if(all(counts == c(1, 0)) | all(counts == c(0, 1))) {
#' 
#'     # relative position of line
#'     line_position <- "outer_single"
#'     # message("line_position: ", line_position)
#' 
#'     # flip in_between boolean to FALSE
#'     in_between = FALSE
#'     # message("in_between: ", in_between)
#' 
#'     return(line_position)
#' 
#'   }
#' 
#'   # 3. OUTER MULTI scenario
#'   # Check if one value is 0 and the other is not zero AND is NOT 1
#'   if (any(counts == 0) && any(counts > 1)) {
#'     # if (any(counts == 0) && any(counts != 0)) {
#' 
#'     # relative position of line
#'     line_position <- "outer_multi"
#'     # message("line_position: ", line_position)
#' 
#'     # flip in_between boolean to FALSE
#'     in_between = FALSE
#'     # message("in_between: ", in_between)
#' 
#'     # # index of the NON ZERO element
#'     # not_zero_idx <- which(counts != 0)
#'     # message(paste("Index of NON ZERO element:", not_zero_idx))
#' 
#'     return(line_position)
#'   }
#' 
#'   # 4. INNER scenario
#'   # Handle sitation where total intersections is odd or even, if EITHER of below conditions is TRUE (OR condition), then we have inner (middle) line
#'   # - ODD CASE: If both the count values equal eachother
#'   # - EVEN CASE: If max(counts) minus 1 EQUALS min(counts)
#'   # If the counts equal eachother OR max(counts) minus 1 EQUALS min(counts)
#'   if(counts[1] == counts[2] | max(counts) - 1 == min(counts) ){
#' 
#'     # relative position of line
#'     line_position <- "inner"
#'     # message("line_position: ", line_position)
#' 
#'     # flip in_between boolean to FALSE
#'     in_between = FALSE
#'     # message("in_between: ", in_between)
#' 
#'     return(line_position)
#' 
#'   }
#'   # 5. IN_BETWEEN scenario
#'   #  IF NONE OF THE ABOVE CONDITIONS EXECUTED, then we have an IN_BETWEEN line
#'   if(in_between) {
#' 
#'     # relative position of line
#'     line_position <- "in_between"
#'     # message("line_position: ", line_position)
#' 
#'     # in_between boolean
#'     # message("in_between: ", in_between)
#'     return(line_position)
#'   }
#' 
#'   return(line_position)
#' }
#' 
#' # Extend a transect line outwards by a certain distance from the head and tail directions of the line
#' # starter_line is the original transect line to extend (geos_geoemtry)
#' # head_distance: numeric, distance (meters) to extend from HEAD of the line
#' # tail_distance: numeric, distance (meters) to extend from TAIL of the line
#' # extra_distance: Any extra distance the line should be extended after the original head/tail distances (THIS IS TYPICALLY GOING TO BE cs_width/2)
#' 
#' #' Extend a transect line outwards by a certain distance from the head and tail directions of the line
#' #' Internal function
#' #' @param starter_line geos_geometry, original transect line to extend outwards
#' #' @param head_distance numeric, distance (meters) to extend from "head" of the line
#' #' @param tail_distance numeric, distance (meters) to extend from "tail" of the line
#' #' @param extra_distance numeric, any extra distance (meters) the line should be extended after the original head/tail distances (this is typically going to be the cross section width divded by 2)
#' #'
#' #' @return geos_geometry, extended by specified distance
#' #' @export
#' #'
#' #' @examples
#' geos_extend_transects <- function(
    #'     starter_line,
#'     head_distance  = 0,
#'     tail_distance  = 0,
#'     extra_distance = 0
#' ) {
#' 
#' 
#'   # extra_distance = 100
#'   # head_distance = 5555
#'   # tail_distance = 150
#'   # ifelse(head_distance == 0, 0, extra_distance)
#' 
#'   # set head and tail extra values to the 'extra_distance' argument
#'   head_extra = tail_extra = extra_distance
#' 
#'   # if the HEAD extending distance is 0, also set the 'head_extra' value to 0
#'   if(head_distance == 0) {
#'     head_extra = 0
#'   }
#' 
#'   # if the TAIL extending distance is 0, also set the 'tail_extra' value to 0
#'   if(tail_distance == 0) {
#'     tail_extra = 0
#'   }
#' 
#'   # distance to extend head and tail out by
#'   head_extension <- head_distance + head_extra
#'   tail_extension <- tail_distance + tail_extra
#' 
#'   # head_extension <- head_distance + ifelse(head_distance == 0, 0, extra_distance)
#'   # tail_extension <- tail_distance + ifelse(tail_distance == 0, 0, extra_distance)
#'   # head_extension <- head_distance + (cs_width/2)
#'   # tail_extension <- tail_distance + (cs_width/2)
#' 
#'   # first extend the head outwards
#'   res_geom <- geos_extend_line(
#'     starter_line,
#'     head_extension,
#'     "head"
#'   )
#' 
#'   # then extend the tail from the already head extended line
#'   res_geom <- geos_extend_line(
#'     res_geom,
#'     tail_extension,
#'     "tail"
#'   )
#' 
#'   return(res_geom)
#' 
#' }
#' 
#' # function for extending/updating transect cross section linestrings
#' # Description: Specifically to be used for situations where a river network is braided.
#' # x: transect line to try and extend to cover braided river sections
#' # id: unique identifier (COMID/hy_id) of transect line
#' # geoms_to_cut: (geos_geometry), other lingestrings (flowlines) of network that x should attempt to extend out to, and cut across
#' # geom_ids: vector of unique IDs for each geoms_to_cut
#' # cs_width: numeric, cross section width
#' # bf_width: numeric, bankful width
#' 
#' #' Determine the distances needed to extend a transect linestring geometry across neighboring flowlines
#' #' Internal function that takes a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
#' #' extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided.
#' #' @param cross_section geos_geometry, transect line to try and extend to cover braided river sections. Dataframe row must contain a "cs_width", "bf_width", "hy_id", and a "geometry" column
#' #' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across
#' #' @param geom_ids character, unique identifier (comid/hy_id) of transect line
#' #' @param max_distance numeric, maximum distance (meters) to extend line out by
#' #' @param by numeric, distance to incrementelly extend out transect line.
#' #' @param as_df logical, whether to return data as dataframe or a list of fastmap::fastmap(). Default is TRUE, returns as a dataframe.
#' #' If FALSE, then the return is a list of 2 fastmap::fastmap() containing information on how the head and tail extensions for the transect geometries
#' #' @param carry_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns.
#' #'
#' #' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line.
#' #' @export
#' #'
#' #' @examples
#' geos_augment_transect <- function(cross_section,
#'                                   geoms_to_cut,
#'                                   geom_ids,
#'                                   max_distance = NULL,
#'                                   by = NULL,
#'                                   as_df = TRUE,
#'                                   carry_geom = TRUE
#' ) {
#' 
#'   # max distance from transect of interest and rest of braid flowlines
#'   # TODO (need a better method of determing max possible extension of flowline)
#'   # max_dist <- as.numeric(
#'   #                 max(
#'   #                   sf::st_distance(
#'   #                     geoms_to_cut,
#'   #                     x
#'   #                   )
#'   #                 )
#'   #               )
#' 
#'   # cross_section = xs[i, ]
#'   # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
#'   # geom_ids      = others$comid
#'   # max_distance  = NULL
#'   # by            = 1
#'   # as_df         = FALSE
#'   # carry_geom    = FALSE
#' 
#'   # cross_section = xs[i, ]
#'   # geoms_to_cut  = others
#'   # max_distance  = NULL
#'   # by            = 1
#'   # as_df         = FALSE
#'   # carry_geom    = FALSE
#' 
#'   # cs_line <- geos::as_geos_geometry(cross_section$geometry)
#' 
#'   # extract values from cross_section dataframe
#'   cs_width <- cross_section$cs_widths
#'   bf_width <- cross_section$bf_width
#'   id       <- cross_section$hy_id
#' 
#'   cs_line  <- geos::as_geos_geometry(cross_section$geometry)
#'   # cs_line  <- cross_section$geometry
#' 
#'   # if no "by" argument is given, then the default becomes bf_width/2
#'   if(is.null(max_distance)) {
#'     max_distance <- max(cs_width * 5)
#'   }
#' 
#'   # if no "by" argument is given, then the default becomes bf_width/2
#'   if(is.null(by)) {
#'     by = bf_width/2
#'   }
#' 
#'   # sequence from 0 to the max possible extension distance
#'   dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
#'   # dist_vect <- seq(0, max(c(max_distance, 2000)), multi_transects[i, ]$bf_width)
#' 
#'   # EXTEND OUT lines
#'   # extend transect line out in both directions and find the side that interests with m
#'   # extend line out from HEAD side of line
#'   # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with
#'   head_map <- geos_extend_out(
#'     x             = 1,
#'     line          = cs_line,
#'     distances     = dist_vect,
#'     geoms_to_cut  = geoms_to_cut,
#'     geom_ids      = geom_ids,
#'     ids           = c(id),
#'     dir           = "head",
#'     map           = TRUE
#'   )
#' 
#'   # extend line out from TAIL side of line
#'   # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with
#'   tail_map <- geos_extend_out(
#'     x             = 1,
#'     line          = cs_line,
#'     distances     = dist_vect,
#'     geoms_to_cut  = geoms_to_cut,
#'     geom_ids      = geom_ids,
#'     ids           = c(id),
#'     dir           = "tail",
#'     map           = TRUE
#'   )
#' 
#'   # head_map$as_list()
#'   # tail_map$as_list()
#' 
#'   # # extract the linestringshapes
#'   # tail_ext <- tail_map$get("line")
#'   # head_ext <- head_map$get("line")
#' 
#'   # mapview::mapview(braids,color = "gold") +  mapview::mapview(geoms_to_cut,color = "dodgerblue") +
#'   # mapview::mapview(head_ext,color = "green") +  mapview::mapview(tail_ext,color = "red") +mapview::mapview(cs_line,color = "cyan")
#' 
#'   # get the relative position within the braid of the linestring we are extending our transect out from
#'   position <- check_relative_position(
#'     head_count = head_map$get("count"),
#'     tail_count = tail_map$get("count")
#'   )
#' 
#'   # POSITION VALUES explanation:
#'   # given the count of interesections from the head and tail of a linestring, return whether the line has:
#'   # - NO_INTERSECTION:: (after extending linestring out to max distance)
#'   # - OUTER_SINGLE: extending linestring out in both directions yielded
#'   # zero intersections in one direction AND exactly one intersection in the other direction
#'   # - OUTER_MULTI: extending linestring out in both directions yielded
#'   # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
#'   # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
#'   # INNER scenario intersection count (odd and even cases):
#'   # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
#'   # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
#'   # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
#'   # ----> SKIP THESE (maybe?) !
#'   # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
#' 
#'   # if as_df is FALSE, return the line data hashmaps as a list of length 2,
#'   # first list element is the head extension data and the second is the tail extension data
#'   if(!as_df) {
#' 
#'     # if NOT AN INNER LINE, postpone processesing
#'     if(position != "inner") {
#' 
#'       # set pending values for these geometries
#'       head_map$set("pending", TRUE)
#'       tail_map$set("pending", TRUE)
#' 
#'       # set pending values for these geometries
#'       head_map$set("position", position)
#'       tail_map$set("position", position)
#' 
#'     } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
#' 
#'       # set pending values for these geometries
#'       head_map$set("pending", FALSE)
#'       tail_map$set("pending", FALSE)
#' 
#'       # set pending values for these geometries
#'       head_map$set("position", position)
#'       tail_map$set("position", position)
#' 
#'     }
#' 
#'     # if carry geom is FALSE, remove geometry linestrings from maps before returning
#'     if(!carry_geom) {
#'       head_map$remove("line")
#'       tail_map$remove("line")
#'     }
#' 
#'     return(
#'       list(
#'         head = head_map,
#'         tail = tail_map
#'       )
#'     )
#'     # return(
#'     #   list(
#'     #     head = head_map$as_list(),
#'     #     tail = tail_map$as_list()
#'     #   )
#'     # )
#' 
#'   }
#' 
#' 
#'   # update "relative_position" column in cross_section to reflect the position of the cross section flowline within the braid value
#'   cross_section$relative_position <- position
#' 
#'   # if NOT AN INNER LINE, postpone processesing
#'   if(position != "inner") {
#'     # DON"T UPDATE "pending" value to reflect that this line should be put on hold and processed after the inner flowlines
#' 
#'     # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#'     cross_section$head_distance <- head_map$get("total_distance")
#'     cross_section$tail_distance <- tail_map$get("total_distance")
#' 
#'     # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#'     cross_section$head_cuts <- head_map$get("count")
#'     cross_section$tail_cuts <- tail_map$get("count")
#' 
#'     # if LINE IS A INNER LINE, GET READY TO EXTEND
#'   } else {
#' 
#'     # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#'     cross_section$pending <- FALSE
#' 
#'     # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#'     cross_section$head_distance <- head_map$get("total_distance")
#'     cross_section$tail_distance <- tail_map$get("total_distance")
#' 
#'     # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#'     cross_section$head_cuts <- head_map$get("count")
#'     cross_section$tail_cuts <- tail_map$get("count")
#' 
#'   }
#' 
#'   # res_geom <- extend_transects(
#'   #                   starter_line   = cs_line,
#'   #                   head_distance  = head_map$get("total_distance"),
#'   #                   tail_distance  = tail_map$get("total_distance"),
#'   #                   extra_distance = cs_width/2
#'   #                 )
#' 
#'   return(cross_section)
#' 
#' }
#' 
#' # WHILE LOOP IMPLEMENTATION INSTEAD OF RECURSION
#' 
#' #' Extend a linestring outward and return the minimum linestring (or details of minimum linestring) that crosses all possible other linestrings (geoms_to_cut) for a the given direction
#' #' Internal Function
#' #' @param x start index of distances vector
#' #' @param line transect line that should be extended
#' #' @param distances numeric vector of distance values (meters) in ascending order (sorted)
#' #' @param geoms_to_cut geos_geometry, all other linestrings (all linestrings other than 'line') that should be cut across (typically other linestrings making up a braided section of river)
#' #' @param geom_ids character or numeric vector of unique identifers for each linestring in 'geoms_to_cut'
#' #' @param ids character or numeric vector of unique identifier of the 'line' argument
#' #' @param dir character, either "head" or "tail", indicating which direction to extend 'line' out
#' #' @param map logical, whether to return a fastmap::fastmap() containing details about
#' #'  the extending line (distance, number of intersections, IDs of intersected geometries, etc.)
#' #'  or to just return the extended line. Default is TRUE, which will return a fastmap::fastmap() that can be used
#' #'  later on to extend the line the necessary distance.  If FALSE, a geos_geometry of the extended linestring is returned
#' #'
#' #' @return fastmap::fastmap() with details on line extension, or a geos_geometry of the extended line
#' #' @export
#' #'
#' #' @examples
#' geos_extend_out <- function(
    #'     x,
#'     line,
#'     distances,
#'     geoms_to_cut,
#'     geom_ids,
#'     ids,
#'     dir = "head",
#'     map = TRUE
#' ) {
#' 
#'   # cross_section = xs[i, ]
#'   # geoms_to_cut  = geos::as_geos_geometry(others$geometry)
#'   # geom_ids      = others$comid
#'   # max_distance  = NULL
#'   # by            = 1
#'   # as_df         = FALSE
#'   # carry_geom    = FALSE
#'   # cs_line  <- geos::as_geos_geometry(cross_section$geometry)
#'   #
#'   # x             = 1
#'   # line          = cs_line
#'   # distances     = dist_vect
#'   # # geoms_to_cut  = geoms_to_cut
#'   # geom_ids      = geom_ids
#'   # ids           = c(id)
#'   # dir           = "tail"
#'   # map           = TRUE
#' 
#' 
#'   # # if NOT a geos_geometry class, coerce
#'   # if(!inherits(line, "geos_geometry")) {
#'   #   # convert to geos geometry
#'   #   line <- geos::as_geos_geometry(line)
#'   #   # geoms_to_cut <- geos::as_geos_geometry(others)
#'   # }
#' 
#'   # if NOT a geos_geometry class, coerce
#'   if(!inherits(geoms_to_cut, "geos_geometry")) {
#'     # convert to geos geometry
#'     geoms_to_cut <- geos::as_geos_geometry(geoms_to_cut)
#'     # geoms_to_cut <- geos::as_geos_geometry(others)
#'   }
#' 
#' 
#'   if(map) {
#'     dmap <- fastmap::fastmap()
#'   }
#' 
#'   # count interesections
#'   count <- 0
#'   dcount <- 0
#' 
#'   # while (TRUE) {
#'   while (TRUE) {
#'     # while (x < length(distances)) {
#'     # message("x: ", x)
#'     # message("distances[x]: ", distances[x])
#' 
#'     xx <- geos_bs_distance(
#'       distances    = distances,
#'       line         = line,
#'       geoms_to_cut = geoms_to_cut,
#'       direction    = dir
#'     )
#' 
#'     # count        <- count + 1
#'     # dcount       <- dcount + distances[xx]
#' 
#'     # message("xx: ", xx)
#'     # message("distances[xx]: ", distances[xx])
#'     # message("ids: ", ids)
#' 
#'     if (xx >= length(distances)) {
#'       # message("!!!!!!!!!!!!!!!! ")
#'       # message("!!!!!!!! xx >= distance: ", xx, " >= ", length(distances), " !!!!!!!! ")
#'       # message("!!!!!!!!!!!!!!!! ")
#'       break
#'     }
#' 
#'     # extend line out to midpoint of distances vector
#'     crosser <- geos_extend_line(line, distances[xx], dir = dir)
#' 
#'     # # extend line out to where the binary search first hit a line
#'     # crossersf <- st_extend_line(sf::st_as_sf(line),
#'     #                             distances[xx], end = dir)
#'     # mapview::mapview(sf::st_as_sf(crosser), color = "dodgerblue") + mapview::mapview(crossersf, olor = "red")
#'     # others$geometry <- geos::as_geos_geometry(others$geometry)
#' 
#' 
#'     # Get the 'new_comid' that will be added to "ids" variable and then passed to the next iteration
#'     # - excluding the IDs in 'ids', determine what geometries in 'geoms_to_cut' are intersecting with our extended 'crosser' line
#'     # - then index 'geom_ids' based on the boolean vector returned from geos_intersects(), to then get the newly intersected ID (new_comid)
#'     new_comid <- geom_ids[
#'       geos::geos_intersects(
#'         crosser,
#'         geoms_to_cut[
#'           !geom_ids %in% ids
#'         ]
#'       )
#'     ]
#' 
#'     # # get the comid of the flow line that was intersected,
#'     # # new comid that should be added to "ids" variable and passed to the next iteration
#'     # new_comid <- geoms_to_cut$comid[
#'     #                     unlist(sf::st_intersects(
#'     #                       crosser,
#'     #                       dplyr::filter(geoms_to_cut,
#'     #                                     !comid %in% ids
#'     #                                     )))
#'     #                     ]
#' 
#'     # Update all the variables for next iteration of while loop
#' 
#'     # update 'line'
#'     line         <- crosser
#' 
#' 
#'     # # set the geometries within c(ids, new_comid) to empty (essentially filtering them out)
#'     # geoms_to_cut[geom_ids %in% c(ids, new_comid)] <- geos::geos_empty()
#' 
#'     # # update geom_ids, removing ids and the new_comid
#'     # geom_ids <- geom_ids[!geom_ids %in% c(ids, new_comid)]
#' 
#'     # update 'geoms_to_cut' and drop the newly added 'new_comid'
#'     geoms_to_cut <- geoms_to_cut[
#'       !geom_ids %in% c(ids, new_comid)
#'     ]
#' 
#'     # update 'geom_ids', removing ids and the new_comid
#'     geom_ids <- geom_ids[
#'       !geom_ids %in% c(ids, new_comid)
#'     ]
#' 
#'     # update 'ids' w/ new_comid
#'     ids          <- c(ids, new_comid)
#' 
#'     # update x (index) value
#'     x            <- xx
#' 
#'     # increment count and continue summing distances
#'     count        <- count + 1
#'     dcount       <- dcount + distances[xx]
#' 
#'     # message("FINAL x: ", x)
#'     # message("=======================")
#' 
#'   }
#' 
#'   # # if specified, return distance map of info and line
#'   if(map) {
#' 
#'     # decrement count by 1 if non zero
#'     # count <- ifelse(count == 0, count, count-1)
#' 
#'     dmap$mset(
#'       index           = x,
#'       distance        = distances[x],
#'       total_distance  = dcount,
#'       line            = line,
#'       cut_ids         = ids,
#'       count           = count,
#'       direction       = dir
#'     )
#' 
#'     return(dmap)
#'   }
#' 
#'   # otherwise just return the line
#'   return(line)
#' 
#'   # # if specified, return the distance index of line
#'   # if (index) {
#'   #   return(x)
#'   #   }
#'   #
#'   # return(line)
#' }
#' 
#' # Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
#' # distances: numeric vector sorted in ascending order
#' # line: linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
#' # geoms_to_cut: geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array
#' # direction: character, either "head" or "tail", indicating which end of the line to extend out.
#' 
#' #' Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
#' #'
#' #' @param distances numeric vector sorted in ascending order
#' #' @param line geos_geometry, linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
#' #' @param geoms_to_cut geos_geometry, geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array
#' #' @param direction character, either "head" or "tail", indicating which end of the line to extend out.
#' #'
#' #' @return index of 'distance' vector, representing the minimum extension distance for a line to intersect nearby geometries
#' #' @export
#' #'
#' #' @examples
#' geos_bs_distance <- function(
    #'     distances,
#'     line,
#'     geoms_to_cut,
#'     direction = "head"
#' ) {
#' 
#'   # distances    = distances
#'   # line         = line
#'   # geoms_to_cut = geoms_to_cut
#'   # direction    = dir
#' 
#'   # sftmp <- st_extend_line(xs[i, ], distances[M], end = dir)
#'   # mapview::mapview(geos_tmp, color = "red") +
#'   #   mapview::mapview(xs[i, ], color = "dodgerblue") +
#'   #   mapview::mapview(sftmp, color = "green")
#' 
#'   # distances    = distances
#'   # line         = line
#'   # geoms_to_cut = geoms_to_cut
#'   # direction    = dir
#' 
#' 
#' 
#'   # Left and right pointers (start and end of distances vector)
#'   L = 1
#'   R = length(distances)
#' 
#'   # While left pointer (L) is less than or equal to the right pointer (R), run binary search.
#'   # Each iteration:
#'   # - the midpoint value gets calculated (M)
#'   # - M is the index of the 'distances' vector that we will use as the distance value to extend 'line'
#'   # - if the new extended line ('new_line') intersects with 'geoms_to_cut', then we decrease the distance value (DECREMENT RIGHT POINTER to the MIDPOINT - 1),
#'   # - if NOT we increase the distance value (INCREMENT LEFT POINTER to the MIDPOINT + 1)
#'   while(L <= R) {
#' 
#'     # calculate midpoint between left and right pointers
#'     M = (L + R) %/% 2
#' 
#'     # message("L: ", L)
#'     # message("M: ", M)
#'     # message("R: ", R)
#'     # message("x[L]: ", distances[L])
#'     # message("x[M]: ", distances[M])
#'     # message("x[R]: ", distances[R])
#' 
#'     if(M == 0 | M == length(distances)) {
#'       # message("EARLY STOPPING bc M = ", M)
#'       # message("RETURNING L = ", L)
#'       return(L)
#'     }
#' 
#'     # extend line out to midpoint of distances vector
#'     new_line <- geos_extend_line(line, distances[M], dir = direction)
#'     # new_line_sf <- st_extend_line(sf::st_as_sf(line), distances[M], end = direction)
#' 
#'     # geos::geos_intersects(geoms_to_cut, new_line)
#'     # sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)
#'     # geos::geos_intersects(geoms_to_cut, new_line)
#'     # lengths( sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)) > 0
#'     # any(geos::geos_intersects(geoms_to_cut, new_line))
#'     # any( lengths(sf::st_intersects(  sf::st_as_sf(geoms_to_cut), new_line_sf)) > 0)
#'     # plot(new_line, col = "red", lwd= 5, add = F)
#'     # plot(line, col = "green", lwd= 5, add = T)
#' 
#'     # check if any of the other braid linestrings get intersected by the extended line:
#'     # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
#'     # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
#' 
#'     # if ANY of the geometries in geoms_to_cut are intersected by the new extended line
#'     if(
#'       any(geos::geos_intersects(geoms_to_cut, new_line))
#'     ) {
#' 
#'       # then DECREMENT RIGHT pointer (DECREASE DISTANCE VALUE) to the midpoint - 1
#'       R = M - 1
#' 
#'       # otherwise IF NO intersections occur:
#'     } else {
#' 
#'       # then INCREMENT LEFT pointer (INCREASE DISTANCE VALUE) to the midpoint + 1
#'       L = M + 1
#' 
#'     }
#'     # message("=======================")
#'   }
#' 
#'   # l_line = st_extend_line(ls, x[L])
#'   # return(l_line)
#' 
#'   return(L)
#' }
#' 
#' # mapview::mapview(line, color = "red") +
#' # mapview::mapview(ms_xs, color = "red") +
#' #   # mapview::mapview(crosser, color = "dodgerblue") +
#' #   mapview::mapview(singles, color = "dodgerblue") +
#' #   mapview::mapview(multis, color = "green") +
#' #   transects
#' # mapview::mapview(cross_pt, col.regions = "red") +
#' # start + end  + boi + ms_xs + transects + singles
#' #   dplyr::filter(boi, !comid %in% com)
#' 
#' 
#' #' Extend a linestring out and determine the minimum extension distance to cross all possible other geometries
#' #' Internal function, implements a binary search algorithm to determine the minimum distance the geos_geometry linestring 'line' must be extended to cross all possible geometries in 'geoms_to_cut'
#' #' @param distances numeric vector in ascending order
#' #' @param line geos_geometry linestring
#' #' @param geoms_to_cut geos_geomtry linestrings to try and interesect by extending 'line'
#' #' @param direction character, direction to extend linestring from. Either "head", "tail" or "both". Default is "head".
#' #'
#' #' @return numeric value indicating the index of the value in 'distances' that is the minimum extension distance to intersect all possible geometries in 'geoms_to_cut'
#' binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
#' 
#'   # left and right pointers at the start and end of the 'distances' vector, respectively
#'   L = 1
#'   R = length(distances)
#' 
#'   # while left pointer is less than or equal to the right pointer, run binary search
#'   while(L <= R) {
#' 
#'     # calculate midpoint
#'     M = (L + R) %/% 2
#' 
#'     # if midpoint is at the end or start of the 'distances' vector, return left pointer
#'     if(M == 0 | M == length(distances)) {
#'       # message("EARLY STOPPING bc M = ", M)
#'       # message("RETURNING L = ", L)
#'       return(L)
#'     }
#' 
#'     # extend linestring by distance value at midpoint (M pointer)
#'     new_line <- st_extend_line(line, distances[M], dir = direction)
#' 
#'     # check if any of the other braid linestrings get intersected by the extended line:
#'     # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
#'     # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
#'     if(any(lengths(sf::st_intersects(geoms_to_cut, new_line)) > 0)) {
#'       # message("DECREM RIGHT.--> need smaller value")
#'       # message("R = R - 1 = : ", M - 1)
#' 
#'       # decrement right pointer to middle - 1
#'       R = M - 1
#'     } else {
#'       # message("DECREM RIGHT.--> need smaller value")
#'       # message("L = M + 1 = : ", M + 1)
#' 
#'       # increment left pointer to middle + 1
#'       L = M + 1
#'     }
#'     # message("=======================")
#'   }
#' 
#'   return(L)
#' }
#' 
#' 
#' 
#' #' Find the direction of the endpoints of a linestring
#' #'
#' #' @param line geos_geometry, linestring
#' #'
#' #' @return numeric vector of angle directions of a given linestring
#' #' @export
#' #'
#' #' @examples
#' geos_linestring_dir <- function(line) {
#' 
#'   # if NOT a geos_geometry class, coerce
#'   if(!inherits(line, "geos_geometry")) {
#'     # convert to geos geometry
#'     line <- geos::as_geos_geometry(line)
#'   }
#' 
#'   # convert to WK coords
#'   coords <- wk::wk_coords(line)
#'   coords <- coords[c("x", "y", "feature_id")]
#' 
#'   # dimensions
#'   k <- c(1, - 1)
#'   i <- c(2, nrow(coords) - 1)
#' 
#'   dirs <- mapply(i, k, FUN = function(i, k) {
#'     x1 <- coords[i-k, 1]
#'     y1 <- coords[i-k, 2]
#'     x2 <- coords[i, 1]
#'     y2 <- coords[i, 2]
#'     unname(atan2(y1 - y2, x1 - x2))
#'   })
#' 
#'   return(dirs)
#' 
#' }
#' 
#' #' Extend a geos_geometry linestring from, one or both ends, by a given distance (meters)
#' #'
#' #' @param line sf linestring or geos_geometry linestring to extend
#' #' @param distance numeric value in meters or a vector of length 2 if 'end = "both"' where
#' #       the first value in the vector will extend that tail by that value and the second value extends the head by that value c(tail, head).
#' #       If a single value is given when end = "both", the value is recycled and used to extend both ends
#' #' @param end character, determines whether to extend the linestring from the 'tail', 'head' or 'both' ends
#' #' @param with_crs logical, whether a CRS should be prescribed to extended output geos_geometry linestring
#' #'
#' #' @return geos_geometry linestring extended by 'distance' from either the 'head', 'tail' or 'both' ends of the original linestring
#' #' @export
#' #'
#' #' @examples
#' geos_extend_line <- function(line,
#'                              distance,
#'                              dir = "both", with_crs = TRUE) {
#'   # line <- xs[1, ]
#' 
#'   # if NOT a geos_geometry class, coerce
#'   if(!inherits(line, "geos_geometry")) {
#'     # convert to geos geometry
#'     line <- geos::as_geos_geometry(line)
#'   }
#' 
#'   if(!dir %in% c("head", "tail", "both")) {
#'     stop("Invalid input 'dir' must either be 'head', 'tail', or 'both'")
#'   }
#' 
#'   # crs <- wk::wk_crs(line)
#'   # convert to WK coords
#'   coords <- wk::wk_coords(line)
#'   coords <- as.matrix(coords[c("x", "y")])
#'   # coords <- coords[c("x", "y")]
#' 
#'   # which index to keep
#'   to_keep <- dir != c("tail", "head")
#' 
#'   # dir coords index we want to keep
#'   dirs <- c(1, nrow(coords))[to_keep]
#' 
#'   # get directions of the direction of interest
#'   directions <- geos_linestring_dir(line)[to_keep]
#' 
#'   # if only a single distance, duplicate it, otherwise reverse the first 2 distances
#'   distances <- if (length(distance) == 1) {
#'     rep(distance, 2)
#'   } else {
#'     rev(distance[1:2])
#'   }
#' 
#'   # adjust dir point coordinates
#'   coords[dirs, ]  <- coords[dirs, ] + distances[to_keep] * c(cos(directions), sin(directions))
#' 
#'   # whether to return with a CRS or not
#'   if(with_crs) {
#' 
#'     # # make a new linestring WITH CRS
#'     line <- geos::geos_make_linestring(
#'       x   = coords[, 1],
#'       y   = coords[, 2],
#'       crs = wk::wk_crs(line)
#'     )
#' 
#'     return(line)
#' 
#'   }
#' 
#'   # else {
#'   #   # # make a new linestring WITHOUT CRS
#'   #   line <- geos::geos_make_linestring(x   = coords[, 1], y   = coords[, 2])
#'   #   return(line)
#'   # }
#' 
#'   # line <- sf::st_sfc( sf::st_linestring(coords), crs = sf::st_crs(line))
#'   # mapview::mapview(curr, color = "red") + mapview::mapview(newline, color = "green")
#'   # plot(line,  lwd = 6, add = T)
#'   # plot(curr$geometry,col = "green", lwd = 6, add = T)
#' 
#'   # # make a new linestring WITHOUT CRS
#'   line <- geos::geos_make_linestring(
#'     x = coords[, 1],
#'     y = coords[, 2]
#'   )
#' 
#'   return(line)
#' }
#' 
#' # Apply flowline braid length threshold to braided network dataset
#' # Return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset
#' # x: braided flowlines
#' # originals: not braided flowlines from the same network
#' # threshold: braid_threshold numeric value to remove braids with a total braid flowline length greater than 'threshold'
#' 
#' #' Apply flowline braid length threshold to braided network dataset
#' #' Internal function
#' #' @param x sf object of braided flowlines (output of find_braids())
#' #' @param originals sf object, non braided flowlines from the same network
#' #' @param threshold numeric, remove braids with a total braid flowline length greater than 'threshold'
#' #' @param verbose logical, whether to output progress messages. If TRUE (default), messages are outputted
#' #'
#' #' @return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset, a list of 2 sf objects containing the updated braids with braids removed that are greater than the threshold value, and an sf object containing the original remaining network linestrings
#' #' @export
#' #'
#' #' @examples
#' braid_thresholder <- function(x,
#'                               originals,
#'                               threshold = NULL,
#'                               verbose   = TRUE
#' ) {
#' 
#'   # input check for input 'x'
#'   if(is.null(x)) {
#'     stop("missing 'x' input argument")
#'   }
#' 
#'   # input check for input 'originals'
#'   if(is.null(originals)) {
#'     stop("missing 'originals' input argument")
#'   }
#' 
#'   # input check for input 'threshold'
#'   if(is.null(threshold)) {
#'     stop("missing 'threshold' input argument")
#'   }
#' 
#'   # unpack nested braid_id column0
#'   unpacked <- unnpack_braids(x)
#' 
#'   # calculate total length of all the linestrings in each braid_id
#'   unpacked <-
#'     unpacked %>%
#'     dplyr::group_by(braid_id) %>%
#'     dplyr::mutate(
#'       braid_length = as.numeric(
#'         sum(sf::st_length(geometry), na.rm = T))
#'     ) %>%
#'     dplyr::ungroup()
#'   # dplyr::mutate(braid_length = sf::st_length(geometry)) %>%
#'   # dplyr::mutate(braid_length = as.numeric(sum(braid_length, na.rm = T))) %>%
#' 
#'   # check to make sure some braids are over threshold and can be removed, if NOT then just return original data
#'   if(all(unpacked$braid_length <= threshold)) {
#' 
#'     message("Removing: 0 braids from braided dataset\n",
#'             "Keeping: All braids as all braids have total flowline lengths less than or equal to threshold value: ", threshold)
#' 
#'     return(list(
#'       braids     = x,
#'       not_braids = originals)
#'     )
#' 
#'   }
#' 
#'   # # table of TRUEs and FALSE for braids to keep/remove given 'threshold'
#'   # threshold_tbl <- table(unpacked$braid_length <= threshold)
#'   # if(verbose) { message("Removing: ",  threshold_tbl["FALSE"],
#'   # " braids from braided dataset\nKeeping: ", threshold_tbl["TRUE"],
#'   #           " braids that have total flowline lengths less than or equal to threshold value: ", threshold)}
#' 
#'   # comids to keep (total length of braid linestrings is less than or equal to braid_threshold value)
#'   to_keep <- dplyr::filter(unpacked, braid_length <= threshold)$comid
#' 
#'   # # COMIDs that are too large, add them back to the "not_braids" data
#'   # to_drop <- dplyr::filter(x, !comid %in% to_keep)
#' 
#'   # keep track of keeping and removing count
#'   orig_nrows <- nrow(originals)
#'   x_nrows  <- nrow(x)
#' 
#'   # add the "too big braid COMIDs" back to original "not_braids" data
#'   # and set these comids braid_ids to "no_braid" and is_multibraid = FALSE
#'   originals <- dplyr::bind_rows(
#'     originals,
#'     dplyr::select(
#'       dplyr::mutate(
#'         dplyr::filter(
#'           x, !comid %in% to_keep
#'         ),
#'         braid_id      = "no_braid",
#'         is_multibraid = FALSE
#'       ),
#'       -has_mainstem
#'     )
#'   )
#' 
#'   new_orig_nrows <- nrow(originals)
#' 
#'   # filter out braid_ids/COMIDs that are too big
#'   x <- dplyr::filter(x, comid %in% to_keep)
#' 
#'   # updating count of keeping and removing
#'   new_orig_nrows <- nrow(originals)
#'   new_x_nrows <- nrow(x)
#' 
#'   if(verbose) {
#'     message("Removing: ", new_orig_nrows - orig_nrows,
#'             " braids from braided dataset\nKeeping: ",   new_x_nrows,
#'             " braids that have total flowline lengths less than or equal to threshold value: ", threshold)
#'   }
#' 
#'   return(list(
#'     braids     = x,
#'     not_braids = originals)
#'   )
#' 
#' }