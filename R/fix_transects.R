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
    "make_geoms_to_cut_plot"
  )
)

# *********************************
# ------------- LATEST ------------
# *********************************

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#' @param terminal_id character, column name containing a unique identifier, delineating seperate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
#' @param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
#' lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#' Default is NULL, which will attempt to fix all the braid transects in the data
#' @param version integer, version number of braid algorithm to use, either 1 or 2. Default is 2.
#' @param method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
#' @param precision int, distance in meters to approximate final cross section linestring length. Value you must be greater than 0. Default is 1
#' @param rm_intersects logical, whether to remove transect linestrings that intersect with other parts of the network ('net'). Default is TRUE which will remove intersecting linestrings.
#'
#' @return sf object of transect linestrings
#' @importFrom dplyr filter group_by ungroup left_join select arrange bind_rows mutate
#' @importFrom nhdplusTools rename_geometry
#' @importFrom sf st_crs st_transform st_drop_geometry st_geometry st_as_sf st_intersects
#' @importFrom geos as_geos_geometry geos_intersects_any
#' @importFrom fastmap fastmap
#' @export
fix_braid_transects <- function(
    net, 
    transect_lines,
    terminal_id     = NULL,
    braid_threshold = NULL,
    version         = 2,
    method          = "comid",
    precision       = 1,
    rm_intersects   = TRUE
) {
  
  ### TEST DATA INPUTS
  # net             = net
  # transect_lines  = ll
  # terminal_id     = terminal_id
  # braid_threshold = braid_threshold
  # version         = version
  # method          = braid_method
  # precision       = precision
  # rm_intersects   = rm_self_intersect
  # net = net2
  # transect_lines = trans
  # terminal_id = NULL
  # braid_threshold = NULL
  # # braid_threshold = 25000
  # version = 2
  # method          = "comid"
  # precision       = 1
  # rm_intersects   = TRUE
  ###
  
  # names that transect_lines starts out with to use at the end
  starting_names <- names(transect_lines)
  
  # set geometry name of network to "geometry"
  net <- nhdplusTools::rename_geometry(net, "geometry")
  
  # keep track of the original CRS of the inputs to retransform return 
  start_crs1 <- sf::st_crs(net, parameters = T)$epsg
  start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
  
  message("Start CRS: ", start_crs1)
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs1 != 5070) {
    # message("Transforming CRS to EPSG: 5070")
    net <- sf::st_transform(net, 5070) 
  }
  
  # check if net CRS is 5070, if not, transform it to 5070
  if(start_crs2 != 5070) {
    # message("Transforming CRS to EPSG: 5070")
    transect_lines <- sf::st_transform(transect_lines, 5070) 
  }
  
  message("Identifying braids...")
  
  # add braid_id column to network
  braids <- find_braids(
    network     = net, 
    terminal_id = terminal_id,
    add         = TRUE,
    nested      = TRUE,
    version     = version,
    verbose     = FALSE
  )
  
  # braids <- find_braids(
  #   network   = net,
  #   return_as = "dataframe",
  #   nested    = TRUE,
  #   # nested    = FALSE,
  #   add       = TRUE
  # )
  
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
  
  # trim down network to just the braided parts, and add a comid count to separate out multibraids
  braids <- dplyr::filter(braids, braid_id != "no_braid") 
  
  ###### BRAID LENGTH CHECKING
  # braid_sizes <- braid_lengths(braids, keep_geom = TRUE)
  # hist(braid_sizes$braid_length)

  if (!is.null(braid_threshold)) {
    
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
  
  # add connected component "component_id" column
  braids <- find_connected_components(braids)
  
  # Add braid vector column to use during iteration (braid_vector)
  braids$braid_vector <- strsplit(braids$braid_id, ", ")
  
  # join cross sections w/ braid flowlines
  xs <- 
    transect_lines %>%
    dplyr::filter(hy_id %in% braids$comid) %>%
    dplyr::left_join(
      sf::st_drop_geometry(
        dplyr::select(
          braids, comid, braid_id, is_multibraid, braid_vector
          # braids, comid, braid_id, is_multibraid
        )
      ),
      by = c("hy_id" = "comid")
    ) %>% 
    dplyr::arrange(-totdasqkm)
  
  # keep track of all original crossections
  all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
  
  # column to store the relative position within the braid of the flowline we're on 
  xs$relative_position <- NA
  
  # flag determining whether transect should/has been replaced
  xs$changed <- FALSE
  
  # empty columns to store number of head/tail intersections
  xs$head_cuts     <- NA
  xs$tail_cuts     <- NA
  
  # empty columns to store distance needed to extend from head/tail of line
  xs$head_distance <- NA
  xs$tail_distance <- NA
  
  # check if any transects exist, if not, just return the original transects
  if (nrow(xs) == 0) {
    message("No transect lines intersect with braided flowlines, returning original transect lines")
    return(transect_lines)
    
  } else {
    message( "Fixing ", nrow(xs) , " transect lines intersecting with braided flowlines lines")
  }
  
  # convert sf geometry column into a geos_geometry with the same "geometry" column name
  xs         <- sf_to_geos(xs)
  braids     <- sf_to_geos(braids)
  not_braids <- sf_to_geos(not_braids)
  
  # mapview::mapview(braids, color = "dodgerblue") +
  # mapview::mapview(xs, color = "red") +
  # mapview::mapview(xs[i, ], color = "green")
  
  # Loop through every single cross section and determine:
  # 1. its relative position
  # 2. how far to extend the line
  # 3. in what order should transects be extended, 
  # 4. in what direction to extend the transect
  
  # Loop through all the cross sections and either:
  # - Extend the inner cross sections 
  # OR
  # - Mark them to be processed in a future step
  for(i in 1:nrow(xs)) {
    # message("i: ", i, "/", nrow(xs))
    # i = 1
    
    # 1 = braid2_components
    # 2 = braid_components
    # 3 = braid2_comids
    # 4 = braid2_neighs
    # 5 = braid_comids
    # 6 = braid_neighs
    
    # current cross section
    curr <- xs[i, ]
    
    # Get cross section geos geometry, cross section width and cross section bankful width
    cs_line  <- curr$geometry
    cs_width <- curr$cs_width
    bf_width <- curr$bf_width
    
    # comid of transect line
    com <- curr$hy_id
    
    # braid ID of interest
    braid_of_interest <- curr$braid_id
    
    # get the component ID of current COMID
    comp_id <- braids$component_id[braids$comid == com]
  
    # other geometries to cut across with transects
    others <- get_geoms_to_cut(
                x            = braids,
                id           = com,
                braid_id     = braid_of_interest,
                component    = comp_id,
                method       = method
              )
    
    # get information on extension distance and position of cross section
    extend_maps <- geos_augment_transect(
      cs_line       = cs_line,
      cs_width      = cs_width,
      bf_width      = bf_width,
      id            = com,
      geoms_to_cut  = others$geometry,
      geom_ids      = others$comid,
      max_distance  = NULL, 
      by            = precision, 
      # as_df         = FALSE,
      carry_geom    = FALSE
    )
    
    # extract cross sections position within braids
    position <- extend_maps$head$get("position")
    
    # if a flowline on the inner portion of a braid, make extension and insert
    if(position == "inner") {
      # message("Extending ", i, " and checking if valid replacement...")
      
      # extend line out by total distance key values in head and tail maps
      res_geom <- geos_extend_transects(
        starter_line   = cs_line,
        # starter_line   = geos::as_geos_geometry(xs$geometry[i]),
        head_distance  = extend_maps$head$get("total_distance"),
        tail_distance  = extend_maps$tail$get("total_distance"),
        extra_distance = xs$cs_widths[i]/2
      )
      
      # ONLY UPDATE geometry if it does NOT intersect with 
      # any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
      if(
        !geos::geos_intersects_any(
          res_geom,
          dplyr::filter(xs[-i,], changed)$geometry
          # res_geom,
          # geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
        )
      )  
      # !any(
      #   lengths(
      #     sf::st_intersects(sf::st_as_sf(res_geom),
      #                     dplyr::filter(xs[-i,], changed)
      #                     )
      #   ) > 0)
        {
        
        # update geometry with new, extended cross section
        xs$geometry[i] <- res_geom
        # xs$geometry[i] <- sf::st_geometry(sf::st_as_sf(res_geom))
        
        # flag determining whether transect should be replaced
        xs$changed[i] <- TRUE
        
      }
      
    } 
    
    # update relative position column
    xs$relative_position[i] <- extend_maps$head$get("position")
    
    # update head/tail distances values in dataframe w/ values from head/tail hashmaps
    xs$head_distance[i] <- extend_maps$head$get("total_distance")
    xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
    
    # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
    xs$head_cuts[i] <- extend_maps$head$get("count")
    xs$tail_cuts[i] <- extend_maps$tail$get("count")
    
  }
  
  # mapview::mapview(xs, color = "red") +
  #   mapview::mapview(transect_lines, color = "green") +
  #   mapview::mapview(braids, color = "dodgerblue") + other_xs
  #   mapview::mapview(tmp, color = "green")

  # # keep only the transects that were changed/extended
  # xs <- dplyr::filter(xs, changed)
  
  # check intersection of keeps and NOT BRAID
  # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
  net_intersects <- geos::geos_intersects_any(
                        xs$geometry,
                        not_braids$geometry
                        )
  
  # remove updated cross sections that intersect with the NOT BRAIDED flowlines
  if(any(net_intersects)) {
    
    # message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
    xs <- xs[!net_intersects, ]
    
  }
  
  # select the other cross sections that have NOT been changed yet and are NOT inner 
  # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
  other_xs = dplyr::filter(xs, 
                           !changed,
                           relative_position != "inner")
  # other_xs <- xs[!xs$changed & xs$relative_position != "inner", ]
  
  # inner transects that haven't been changed
  unchanged_inners <- dplyr::filter(xs, 
                                    !changed,
                                    relative_position == "inner")
  # unchanged_inners <- xs[!xs$changed & xs$relative_position == "inner", ]
  
  # remove excess cross sections by keeping ONLY "changed" flowlines
  xs <- dplyr::filter(xs, changed) 
  # xs <- xs[xs$changed, ]
  
  # intersections between updated inner cross sections ("xs") and the remaining inner cross sections that were NOT changed ("unchanged_inners")
  inner_intersects <- geos::geos_intersects_any(
                                  unchanged_inners$geometry,
                                  xs$geometry
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
                                other_xs$geometry,
                                xs$geometry
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
                        # -braid_id, -head_cuts, -tail_cuts
                        -is_multibraid,
                        # -has_mainstem,
                        -changed, 
                        -head_distance, -tail_distance,
                        )
    
    # # bind together final updated transect lines
    # out <- dplyr::select(xs,  -braid_id, 
    #                      # -is_multibraid, -has_mainstem, -changed, -pending, -head_distance, -tail_distance, 
    #                      -head_cuts, -tail_cuts)  
    
  } else {
    
    # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")

    # loop through the remaining transects that were NOT "inner" lines, and do extensions
    for (i in 1:nrow(other_xs)) {
      
      # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
      if (other_xs$relative_position[i] == "no_intersects") {
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
        
        next
      }
      
      # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
      res_geom <- geos_extend_transects(
        starter_line   = other_xs$geometry[i],
        head_distance  = other_xs$head_distance[i],
        tail_distance  = other_xs$tail_distance[i],
        extra_distance = other_xs$cs_widths[i]/2
        # starter_line   = geos::as_geos_geometry(other_xs$geometry[i]),
        # head_distance  = other_xs$head_distance[i],
        # tail_distance  = other_xs$tail_distance[i],
        # extra_distance = other_xs$cs_widths[i]/2
      )
      
      
      # sf::st_intersects(res_geom, xs)
      # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
      # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom, 
      #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
      
      # - Check to make sure that the newly extended res_geom transect line does not intersect with any of the other cross sections in 'xs'
      # - Also check that the new res_geom doesn't intersect with any of the other transects in "other_xs" other than itself
      # ----> If BOTH of these are TRUE, then the new extended transect replaces the original transect in the 'other_xs' geometry column
      if(
        !any(
          geos::geos_intersects_any(
            xs,
            res_geom
            # geos::as_geos_geometry(xs),
            # geos::as_geos_geometry(res_geom)
          )) &
        !any(geos::geos_intersects_any(
            other_xs[-i, ],
            res_geom
            # geos::as_geos_geometry(other_xs[-i, ]),
            # geos::as_geos_geometry(res_geom)
          ))
      ) {
        
        # # # message stating that replacement was made
        # message("----> REPLACING ", i, " transect")
        
        # replace geometry with extended line
        other_xs$geometry[i] <- res_geom
        # other_xs$geometry[i] <- sf::st_geometry(sf::st_as_sf(res_geom))
        
        # flag determining whether transect should be replaced
        other_xs$changed[i] <- TRUE
      }
    }
    
    # keep only the transects that were changed/extended
    other_xs <- dplyr::filter(other_xs, changed)

    # # # keep only the transects that were changed/extended
    # other_drop <- dplyr::filter(other_xs, !changed)
    
    # bind together final updated transect lines
    out <- dplyr::bind_rows(
      dplyr::select(xs, 
                    # -braid_id, 
                    -is_multibraid,
                    # -has_mainstem,
                    -changed, 
                    -head_distance, -tail_distance,
                    # -head_cuts, -tail_cuts
      ),
      dplyr::select(other_xs,
                    # -braid_id, 
                    -is_multibraid,
                    # -has_mainstem,
                    -changed,
                    -head_distance, -tail_distance,
                    # -head_cuts, -tail_cuts
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
                        sf::st_as_sf(out)
                        # out
                      )
  
  # if rm_intersects == TRUE, then remove transects that interesect with other parts of the network
  if(rm_intersects) {
    
    # bind braids and not_braids back together to reform original "net" but with added "braid_id" column
    net <- sf::st_as_sf(
                dplyr::bind_rows(braids, not_braids)
              )
    
    # if final transect_lines has an NA for the braid_id column it means that it was part of the non braided (untouched) transect_lines,
    # set braid_id to "no_braid" in those cases, otherwise keep braid_id as is
    transect_lines$braid_id <- ifelse(
                                  is.na(transect_lines$braid_id), 
                                  "no_braid", 
                                  transect_lines$braid_id
                                  )
    
    # if one of the transect lines interesects MORE than 1 line in net AND it also has a braid_id == "no_braid", then remove it from output
    transect_lines <- transect_lines[!(lengths(sf::st_intersects(transect_lines, net)) > 1 & transect_lines$braid_id == "no_braid"), ]
    
  }
  
  # select and reorder columns back to original starting positions
  transect_lines <- transect_lines[starting_names]

  # mapview::mapview(braids, color = "gold") +
  # mapview::mapview(not_braids, color = "dodgerblue") +
  #  mapview::mapview(transect_lines, color = "gold")
  
  # transform CRS back to input CRS
  if(start_crs2 != 5070) {
    message("Transforming CRS back to EPSG: ", start_crs2)
    transect_lines <- sf::st_transform(transect_lines, start_crs2)
  }
  
  return(transect_lines)
  
}

#### OLD VERSION OF fix_braid_transects (TO DELETE SOON)
# #Fix transects found on braided river sections
# #@param net sf object of NHDplusv2 data
# #@param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
# #@param terminal_id character, column name containing a unique identifier, delineating seperate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network' 
# #@param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline 
# #lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
# #Default is NULL, which will attempt to fix all the braid transects in the data
# #@return sf object of transect linestrings
# #@importFrom dplyr filter group_by ungroup left_join select arrange bind_rows mutate
# #@importFrom nhdplusTools rename_geometry
# #@importFrom sf st_crs st_transform st_drop_geometry st_geometry st_as_sf st_intersects
# #@importFrom geos as_geos_geometry geos_intersects_any
# #@importFrom fastmap fastmap
# fix_braid_transects2 <- function(
#     net, 
#     transect_lines,
#     terminal_id = NULL,
#     braid_threshold = NULL
# ) {
#   
#   # set geometry name of network to "geometry"
#   net <- nhdplusTools::rename_geometry(net, "geometry")
#   
# 
#   # keep track of the original CRS of the inputs to retransform return 
#   start_crs1 <- sf::st_crs(net, parameters = T)$epsg
#   start_crs2 <- sf::st_crs(transect_lines, parameters = T)$epsg
#   
#   message("Start CRS: ", start_crs1)
#   
#   # check if net CRS is 5070, if not, transform it to 5070
#   if(start_crs1 != 5070) {
#     # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
#     message("Transforming CRS to EPSG: 5070")
#     net <- sf::st_transform(net, 5070) 
#   }
#   
#   # check if net CRS is 5070, if not, transform it to 5070
#   if(start_crs2 != 5070) {
#     # if(sf::st_crs(net, parameters = T)$epsg != 5070) {
#     message("Transforming CRS to EPSG: 5070")
#     transect_lines <- sf::st_transform(transect_lines, 5070) 
#   }
#   
#   message("Identifying braids...")
#   
#   # # add braid_id column to network
#   # braids <- find_braids(
#   #   network   = net,
#   #   return_as = "dataframe",
#   #   nested    = TRUE,
#   #   # nested    = FALSE,
#   #   add       = TRUE
#   # )
#   
#   # add braid_id column to network
#   braids <- find_braids(
#                   network     = net, 
#                   terminal_id = terminal_id,
#                   add         = TRUE,
#                   nested      = TRUE,
#                   verbose     = FALSE
#                 )
#   
#   if(all(braids$braid_id == "no_braid")) {
#     
#     message("No braids identified, returning original transects")
#     
#     # transform CRS back to input CRS
#     if(start_crs2 != 5070) {
#       message("Transforming CRS back to EPSG: ", start_crs2)
#       transect_lines <- sf::st_transform(transect_lines, start_crs2)
#     }
#     
#     return(transect_lines)
#   }
#   
#   message("Fixing braid transects...")
#   
#   # not braided flowlines
#   not_braids <-  dplyr::filter(braids, braid_id == "no_braid")
#   # not_braids <- braids[!braids$comid %in% only_braids$comid, ]
#   
#   # trim down network to just the braided parts, and add a comid count to separate out multibraids
#   # only_braids <-
#   braids <-  
#     braids %>% 
#     dplyr::filter(braid_id != "no_braid") %>% 
#     # dplyr::group_by(comid) %>% 
#     # dplyr::mutate(ncomid = n()) %>% 
#     # dplyr::ungroup() %>% 
#     dplyr::group_by(braid_id) %>% 
#     dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
#     dplyr::ungroup()
#   
#   # view data on map
#   # mapview::mapview(not_braids, color = "dodgerblue") +
#   # mapview::mapview(only_braids, color = "red") 
#   
#   if(!is.null(braid_threshold)) {
#     
#     # remove braids that have a total flowline length greater than braid_threshold
#     braids <- braid_thresholder(
#       x         = braids, 
#       originals = not_braids, 
#       threshold = braid_threshold,
#       verbose   = TRUE
#     )
#     
#     # reassign braids and not_braids datasets to the updated values in 'braids' list (REASSIGNMENT ORDER MATTERS HERE)
#     not_braids <- braids$not_braids
#     braids     <- braids$braids
#   }
#   
#   # # unique braid_ids/COMIDs
#   # ubraids <- unique(only_braids$braid_id)
#   # ucoms <- unique(only_braids$comid)
#   
#   # join cross sections w/ braid flowlines
#   xs <- 
#     transect_lines %>%
#     dplyr::filter(hy_id %in% braids$comid) %>%
#     dplyr::left_join(
#       sf::st_drop_geometry(
#         dplyr::select(
#           braids, comid, braid_id, is_multibraid
#         )
#       ),
#       by = c("hy_id" = "comid")
#     ) %>% 
#     # dplyr::filter(divergence == 0)
#     dplyr::group_by(braid_id) %>% 
#     dplyr::mutate(has_mainstem = any(divergence == 0)) %>% 
#     dplyr::ungroup() %>% 
#     dplyr::arrange(-totdasqkm)
#   
#   # keep track of all original crossections
#   all_xs <- paste0(xs$hy_id, "_", xs$cs_id)
#   
#   # column to store the relative position within the braid of the flowline we're on 
#   xs$relative_position <- NA
#   
#   # flag determining whether transect should/has been replaced
#   xs$changed <- FALSE
#   
#   # flag determining whether transect is to be processed in a future step after middle flowlines are processed
#   xs$pending <- TRUE
#   
#   # flag determining whether transect is to be processed in a future step after middle flowlines are processed
#   xs$pending <- TRUE
#   
#   # empty columns to store number of head/tail intersections
#   xs$head_cuts     <- NA
#   xs$tail_cuts     <- NA
#   
#   # empty columns to store distance needed to extend from head/tail of line
#   xs$head_distance <- NA
#   xs$tail_distance <- NA
#   
#   # data.table::data.table(xs)[1, ]
#   
#   # check if any transects exist, if not, just return the original transects
#   if (nrow(xs) == 0) {
#     
#     # message("===== NO 'xs' transect lines =====")
#     # message("===== returning original data =====")
#     message("No transect lines intersect with braided flowlines, returning original transect lines")
#     return(transect_lines)
#     
#   } else {
#     message( "Fixing ", nrow(xs) , " transect lines intersecting with braided flowlines lines")
#     # message("===== ", nrow(xs) , " 'xs' transect lines =====")
#     # message("===== returning original data =====")
#   }
#   
#   # braids %>% 
#   #   geos_make_collection() %>% 
#   #   geos_unary_union() %>% 
#   #   st_as_sfc()
#   # braids %>% 
#   #   dplyr::mutate(
#   #     geometry  =  geos::geos_geometry(.)
#   #   ) %>% 
#   #   dplyr::relocate(geometry2)
#   # geos::as_geos_geometry(braids )
#   
#   # braids$geometry <-  geos::geos_geometry(braids$geometry)
#   # mapview::mapview(braids, color = "dodgerblue") +
#   #   mapview::mapview(xs, color = "red") +
#   # mapview::mapview(xs[i, ], color = "green")
#   
#   # Loop through every single cross section and determine:
#   # 1. its relative position
#   # 2. how far to extend the line
#   # 3. in what order should transects be extended, 
#   # 4. in what direction to extend the transect
#   for(i in 1:nrow(xs)) {
#     # for(i in 1:17) {
#     # message("i: ", i, "/", nrow(xs))
#     # i = 18 
#     # # transect line
#     # tline <- xs[i, ]$geometry
#     # i = 18
#     # curr <- xs[i, ]
#     
#     # comid of transect line
#     com <- xs$hy_id[i]
#     
#     # braid IDs of interest
#     bids <- strsplit(xs[i, ]$braid_id, ", ")[[1]]
#     
#     # get neighboring braid ID for our current braid
#     neighbor_braids <- get_neighbor_braids(x = braids, ids = bids, only_unique = T)
#     
#     # braid flowlines other than self that are within our given braid id or are nearby
#     others <- dplyr::filter(
#       braids,
#       braid_id %in% neighbor_braids,
#       comid != com
#     )
#     
#     # geoms_to_cut  = others
#     extend_maps <- geos_augment_transect(
#       cross_section = xs[i, ],
#       geoms_to_cut  = geos::as_geos_geometry(others$geometry),
#       geom_ids      = others$comid,
#       max_distance  = NULL, 
#       by            = 1, 
#       as_df         = FALSE,
#       carry_geom    = FALSE
#     )
#     
#     # extend_maps$head$as_list()
#     position <- extend_maps$head$get("position")
#     
#     # if a flowline on the inner portion of a braid, make extension and insert
#     if(position == "inner") {
#       # message("Extending ", i, " and checking if valid replacement...")
#       # extend line out by total distance key values in head and tail maps
#       res_geom <- geos_extend_transects(
#         starter_line   = geos::as_geos_geometry(xs$geometry[i]),
#         head_distance  = extend_maps$head$get("total_distance"),
#         tail_distance  = extend_maps$tail$get("total_distance"),
#         extra_distance = xs$cs_widths[i]/2
#       )
#       
#       # mapview::mapview(others) + braids + res_geom + not_braids
#       
#       # ONLY UPDATE geometry if it does NOT intersect with any of the other multibraid transects that have been changed so far (AND LEAVE OUT SELF)
#       if(
#         # !any(
#         #   lengths(
#         #     sf::st_intersects(sf::st_as_sf(res_geom),
#         #                     dplyr::filter(xs[-i,], changed)
#         #                     )
#         #   ) > 0)
#         !geos::geos_intersects_any(
#           res_geom,
#           geos::as_geos_geometry(dplyr::filter(xs[-i,], changed))
#         )
#       ) {
#         
#         # updatem geometry with new, extended cross section
#         xs$geometry[i] <- sf::st_geometry(
#           sf::st_as_sf(res_geom)
#         )
#         
#         # flag determining whether transect should be replaced
#         xs$changed[i] <- TRUE
#         
#         }
#       
#       # update relative position column
#       xs$relative_position[i] <- extend_maps$head$get("position")
#       
#       # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#       xs$pending[i] <- extend_maps$head$get("pending")
#       
#       # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#       xs$head_distance[i] <- extend_maps$head$get("total_distance")
#       xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
#       
#       # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#       xs$head_cuts[i] <- extend_maps$head$get("count")
#       xs$tail_cuts[i] <- extend_maps$tail$get("count")
#       
#       
#     } else {
#       
#       # update relative position column
#       xs$relative_position[i] <- extend_maps$head$get("position")
#       
#       # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
#       xs$pending[i] <- extend_maps$head$get("pending")
#       
#       # update head/tail distances values in dataframe w/ values from head/tail hashmaps
#       xs$head_distance[i] <- extend_maps$head$get("total_distance")
#       xs$tail_distance[i] <- extend_maps$tail$get("total_distance")
#       
#       # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
#       xs$head_cuts[i] <- extend_maps$head$get("count")
#       xs$tail_cuts[i] <- extend_maps$tail$get("count")
#       
#     }
#   }
#   
#   # mapview::mapview(xs, color = "red") +
#   #   mapview::mapview(transect_lines, color = "green") +
#   #   mapview::mapview(braids, color = "dodgerblue") + other_xs
#   #   mapview::mapview(tmp, color = "green")
#   
#   # net_intersects <- sf::st_intersects(not_braids, xs)
# 
#   # # keep only the transects that were changed/extended
#   # to_keep <- dplyr::filter(xs, changed)
#   
#   # # keep only the transects that were changed/extended
#   # xs <- dplyr::filter(xs, changed)
#   # mapview::mapview(xs, color = "red") + braids + not_braids
#   
#   # check intersection of keeps and NOT BRAID
#   # indices of div_xs transects that now intersect with the updated/extended 'xs' transects
#   net_intersects <- geos::geos_intersects_any(
#                             geos::as_geos_geometry(xs),
#                             geos::as_geos_geometry(not_braids)
#                           )
#   # net_intersects <- sf::st_intersects(not_braids, xs)
#   
#   # remove updated cross sections that intersect with the NOT BRAIDED flowlines
#   if(any(net_intersects)) {
#     
#     # message("Removing ", table((unlist(net_intersects)))["TRUE"], " transect lines from 'xs'")
#     xs <- xs[!net_intersects, ]
#     
#   }
#   
#   # select the other cross sections that have NOT been changed yet and are NOT inner 
#   # ---> (not changed "inner" cross sections would intersect with "changed inners", this was checked in the loop above)
#   other_xs = dplyr::filter(xs, 
#                            !changed,
#                            relative_position != "inner"
#                            )
#   
#   # other_xs = dplyr::filter(xs, !changed)
# 
#   # remove excess cross sections by setting "xs" to keep ONLY the cross sections that changed
#   # # keep only the transects that were changed/extended
#   # xs <- dplyr::filter(xs, changed)
#   
#   # inner transects that haven't been changed
#   unchanged_inners <- dplyr::filter(xs, 
#                                     !changed,
#                                     relative_position == "inner")
#   
#   # keep only changed flowlines
#   xs <- dplyr::filter(xs, changed) 
#   
#   # intersections between updated inner cross sections ("xs") and the remaining inner cross sections that were NOT changed ("unchanged_inners")
#   inner_intersects <- geos::geos_intersects_any(
#     geos::as_geos_geometry(unchanged_inners$geometry),
#     geos::as_geos_geometry(xs$geometry)
#   )
#   
#   # add back into "xs" the unchanged inner transects that do NOT intersect with our updated/extended inner transect lines
#   xs <- dplyr::bind_rows(
#     xs,
#     unchanged_inners[!inner_intersects, ]
#   )
#   
#   
#   # # # # keep ALL "inner" transects, both the ones that were extended ("changed" == TRUE) and not changed inners
#   # xs <- dplyr::filter(xs, changed | relative_position == "inner")
#   
#   # check intersection of keeps xs with other_xs
#   
#   # indices of other_xs transects that now intersect with the updated/extended 'xs' transects. 
#   # All the cross section lines in "xs" are now "inner" lines that were extended
#   other_intersects <- geos::geos_intersects_any(
#                             geos::as_geos_geometry(other_xs$geometry),
#                             geos::as_geos_geometry(xs$geometry)
#                           )
#   # other_intersects <- sf::st_intersects(xs, other_xs)
#   # unlist(sf::st_intersects(xs, other_xs))
#   
#   # net_intersects <- sf::st_intersects(not_braids, xs)
#   # lengths(other_intersects)
#   
#   # if there ARE some intersections, remove those intersecting lines from 'div_xs'
#   if(any(other_intersects)) {
#     # message("Removing ", table((unlist(other_intersects)))["TRUE"], " transect lines from 'other_xs'")
#     
#     # drop div_xs transects that are overlapping with 'xs' transects
#     other_xs <- other_xs[!other_intersects, ]
#   }
#   
#   # if there are still other (non "inner") transects, do extension processing
#   if (nrow(other_xs) > 0) {
#     
#     # message("===== ", nrow(other_xs)  ," 'other_xs' transect lines =====")
#     # loop through the remaining transects that were NOT "inner" lines, and do extensions
#     for (i in 1:nrow(other_xs)) {
#       
#       # message("i: ", i, "/", nrow(other_xs))
# 
#       # if we get to a transect that does not intersect the rest of the braid even after extension, than set "changed" to TRUE and skip the iteration
#       if (other_xs$relative_position[i] == "no_intersects") {
#         
#         # flag determining whether transect should be replaced
#         other_xs$changed[i] <- TRUE
#         
#         next
#       }
#       
#       # extend line other_xs[i, ] line out by head_distance/tail_distance and provide the extra_distance of cs_width/2
#       res_geom <- geos_extend_transects(
#                       starter_line   = geos::as_geos_geometry(other_xs$geometry[i]),
#                       head_distance  = other_xs$head_distance[i],
#                       tail_distance  = other_xs$tail_distance[i],
#                       extra_distance = other_xs$cs_widths[i]/2
#                       )
# 
#       
#       # sf::st_intersects(res_geom, xs)
#       # !any(lengths(sf::st_intersects(res_geom, xs)) > 0)
#       # lengths(sf::st_intersects(res_geom, xs)) > 0 | lengths(sf::st_intersects(res_geom, 
#       #                                                                          dplyr::filter(other_xs[-i, ], changed))) > 0
#       
#       if(
#         !any(
#           geos::geos_intersects_any(
#             geos::as_geos_geometry(xs),
#             geos::as_geos_geometry(res_geom)
#           )) &
#         !any(geos::geos_intersects_any(
#           geos::as_geos_geometry(other_xs[-i, ]),
#           geos::as_geos_geometry(res_geom)
#         ))
#       ) {
#         
#         # # # message stating that replacement was made
#         # message("----> REPLACING ", i, " transect")
#         
#         # replace geometry with extended line
#         other_xs$geometry[i] <- sf::st_geometry(sf::st_as_sf(res_geom))
#         
#         # flag determining whether transect should be replaced
#         other_xs$changed[i] <- TRUE
#         
#       }
#       
#     }
#     
#     # # # keep only the transects that were changed/extended
#     # other_drop <- dplyr::filter(other_xs, !changed)
#     
#     # keep only the transects that were changed/extended
#     other_xs <- dplyr::filter(other_xs, changed)
#     
#     # bind together final updated transect lines
#     out <- dplyr::bind_rows(
#                       dplyr::select(xs, 
#                                     -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#                                     -head_distance, -tail_distance, -head_cuts, -tail_cuts
#                                     ),
#                       dplyr::select(other_xs,
#                                     -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#                                     -head_distance, -tail_distance, -head_cuts, -tail_cuts
#                                     )
#                       )
#     
#   } else {
#     
#     # message("===== NO 'other_xs' transect lines =====")
#     
#     # bind together final updated transect lines
#     out <- dplyr::select(xs, 
#                          -braid_id, -is_multibraid, -has_mainstem, -changed, -pending,
#                          -head_distance, -tail_distance, -head_cuts, -tail_cuts
#     )
#     
#   }
#   
#   # drop all of the transects that are on braids, and replace them with the updated/extended transect lines in "out"
#   transect_lines <-  dplyr::bind_rows(
#                           # from original transect_lines, remove all of the cross sections on braids,
#                           dplyr::select(
#                             dplyr::filter(   
#                               dplyr::mutate(transect_lines, 
#                                             tmp_id = paste0(hy_id, "_", cs_id)
#                               ),
#                               !tmp_id %in% all_xs
#                             ),
#                             -tmp_id
#                           ),
#                           # updated braid cross sections
#                           out
#                         )
#   
#   # mapview::mapview(braids, color = "dodgerblue") +
#   # mapview::mapview(not_braids, color = "gold") +
#   # mapview::mapview(transect_lines, color = "green") +
#   # mapview::mapview(transect_lines2, color = "red")
#   
#   # transform CRS back to input CRS
#   if(start_crs2 != 5070) {
#     message("Transforming CRS back to EPSG: ", start_crs2)
#     transect_lines <- sf::st_transform(transect_lines, start_crs2)
#   }
#   
#   return(transect_lines)
#   
# }

#' Get Geometries to Cut/Get the nearby geometries from a COMID in a braided network
#' 
#' Internal function that tries to determine the nearby and/or connected flowlines/COMIDs relative to an origin COMID ('id') in the NHDPlus network dataset ('x'). 
#' The return COMIDs are a subset of the main dataset that will be used to extend transects across because they are eligible as they either: part of the same braid_id(s), neighboring braid_id(s), or within the same connected component of the network.
#' Function requires the NHDPlus sf dataframe and an "id" value and a "method" value.
#' 
#' Geometries can be selected for cutting via 3 methods:
#' 
#' 1. All COMIDs within the current COMIDs braid_id (s) except the origin COMID (method = "comid")
#' 2. COMIDS of the current COMID's braid_id(s) AND the COMIDs of ANY of the braids that neighbor the origin COMIDs braid_id(s) (method = "neighbor")
#' 3. Uses the component ID of the origin COMID to only identify COMIDs that have the same component ID AND are not the origin COMID within the current COMIDs braid_id (s) (method = "component")
#' @param x A data frame containing network data.
#' @param id integer or character, origin COMID or identifier.
#' @param braid_id character, The braid identifier.
#' @param component character or integer, component ID of the origin COMID identifier.
#' @param method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
#'
#' @noRd
#' @keywords internal
#' @return sf dataframe containing the subset of nearby geometries to use for cutting.
#' @importFrom dplyr filter 
get_geoms_to_cut <- function(x, 
                             id          = NULL, 
                             braid_id    = NULL, 
                             component   = NULL,
                             method      = "comid"
                             ) {
  
  # stop the function if an invalid "method" argument is given
  if(!method %in% c("comid", "component", "neighbor")) {
    stop("Invalid 'method' value, must be one of 'comid', 'component', or 'neighbor'")
  }
  
  # stop the function if "id" is missing (NULL)
  if(is.null(id)) {
    stop("Missing 'id' value, provide an origin COMID")
  }
  
  # If missing 'braid_id' argument (NULL) AND there is a "braid_id" column in "x",
  # use the "braid_id" column to get the 'braid_id' value
  if(is.null(braid_id) && "braid_id" %in% names(x)) {
    
    # filter 'x' to comid of interest and get the "component_id"
    braid_id <- x$braid_id[x$comid == id]
    
  }
  
  # if missing 'component' argument (NULL) AND there is a "component_id" column in "x",
  # use the "component_id" column to get the 'component' value
  if(is.null(component) && "component_id" %in% names(x)) {
    
    # filter 'x' to comid of interest and get the "component_id"
    component <- x$component_id[x$comid == id]
    
  }
  
  # use the braid_ids of the origin COMID and get ALL of the COMIDs that are within any of those braid_ids
  # Filter "x" dataset to ONLY:
  # - COMIDs that are in the SAME braid_id OR are in a neighboring braid
  # - COMIDs that are NOT self
  if(method == "comid") {
    
    # braid IDs of interest
    braid_id_list <- strsplit(braid_id, ", ")[[1]]
    
    # get neighboring COMIDs for our current braid
    neighbor_comids <- comids_in_braid_ids(
      x               = x,
      braids_to_match = x$braid_vector, 
      braid_ids       = braid_id_list
    )
    
    # remove self comid
    neighbor_comids <- neighbor_comids[neighbor_comids != id]
    
    # Filter to braid flowlines other than self that are within our given braid id or
    # are a nearby neighboring, flowline of a different (but connected) braid
    others <- x[x$comid %in% neighbor_comids, ]
    
    return(others)
    
  }
  # Uses the component ID of the origin COMID to only identify COMIDs that have the same component ID AND are not the origin COMID within the current COMIDs braid_id (s) (method = "component")
  
  # use the component ID of the origin comid to filter down our braid dataframe down to ONLY:
  # - COMIDs with the same component_id
  # - COMIDs that are NOT self
  if(method == "component") {
    # comp_id <- dplyr::filter(braids, comid == com)$component_id
    
    # if component argument is missing AND there is a "component_id" column in "x", use that
    if(is.null(component)) {
      
      # stop message informing user to give a 'component' value
      stop("Missing 'component' value, provide a 'component' value that distinguises 
               between connected/disconnected components in network 'x'")
    }
    
    # filter 'x' to rows with the same component_id AND that are the origin comid
    others <- x[x$component_id %in% component & x$comid != id, ]
    # others <- dplyr::filter(x, component_id %in% component, comid != id)
    
    return(others)
    
  }
  
  # use the braid IDs of the braids that neighbor our given braid_id(s)
  if(method == "neighbor") {
    
    # braid IDs of interest
    bids <- strsplit(braid_id, ", ")[[1]]
    
    # # get neighboring braid ID for our current braid
    neighbor_braids <- get_neighbor_braids(x = x, ids = bids, only_unique = TRUE)
    
    # braid flowlines other than self that are within our given braid id or are nearby
    others <- x[x$braid_id %in% neighbor_braids & x$comid != id, ]
    # others <- dplyr::filter(x, braid_id %in% neighbor_braids,comid != id)
    
    return(others)
  }
  
  return(others)
}

#' Convert SF geometry columns to geos geometrys and return the original dataframe 
#' Internal function used to switch sf geometry dataframes to geos for processing tasks
#' @param x sf dataframe
#' @noRd
#' @keywords internal
#' @return data.frame, tibble with geometry column containing geos_geometries
sf_to_geos <- function(x) {
  
  x_geoms <- geos::as_geos_geometry(x)
  
  x <- sf::st_drop_geometry(x) 
  
  x$geometry <- x_geoms
  
  # x <- dplyr::mutate(
  #         sf::st_drop_geometry(x),
  #         geometry = geos::as_geos_geometry(x)
  #       )
  
  return(x)
}

#' Determine the relative position of a transect line within a braid given the count of intersecting lines 
#' Determine the relative position of a transect line within a braid, given the count of intersections that occur after extending the transect line in both direction (from the head and tail of the transect line) 
#' @param head_count numeric, count of intersections extending from the "head" end of an extended transect line  
#' @param tail_count numeric, count of intersections extending from the "tail" end of an extended transect line  
#'
#' @noRd
#' @keywords internal
#' @return character ("no_intersects", "outer_single", "outer_multi", "inner" , or "in_between")
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
#' @noRd
#' @keywords internal
#' @return geos_geometry, extended by specified distance
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
#' @param cs_line geos_geometry, transect line to try and extend to cover braided river sections. 
#' @param cs_width numeric, cross section width of cs_line (meters) 
#' @param bf_width numeric, bankful width of cs_line (meters) 
#' @param id integer or character, unique ID of flowline geometry (i.e. COMID)
#' @param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across 
#' @param geom_ids character, unique identifier (comid/hy_id) of transect line 
#' @param max_distance numeric, maximum distance (meters) to extend line out by
#' @param by numeric, distance to incrementelly extend out transect line. 
#' @param carry_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns. 
#' @noRd
#' @keywords internal
#' @return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line. 
#' @importFrom geos as_geos_geometry
#' @importFrom fastmap fastmap
geos_augment_transect <- function(
                                  cs_line,
                                  cs_width,
                                  bf_width,
                                  id,
                                  geoms_to_cut, 
                                  geom_ids,
                                  max_distance = NULL,
                                  by = NULL, 
                                  carry_geom = TRUE
                                  ) {
  
  # max distance from transect of interest and rest of braid flowlines 
  # TODO (need a better method of determing max possible extension of flowline)
  # max_dist <- as.numeric(max(sf::st_distance(geoms_to_cut, x)))

  # # cs_line <- geos::as_geos_geometry(cross_section$geometry)
  # 
  # # extract values from cross_section dataframe
  # cs_width <- cross_section$cs_widths
  # bf_width <- cross_section$bf_width
  # id       <- cross_section$hy_id
  
  # # convert cross_section geometry columns to geos_geometry
  # cs_line  <- geos::as_geos_geometry(cross_section$geometry)
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
  
  # default count of intersections for line extension in a given directionis set to 1, 
  # and then check for the actual total maximum number of possible lines that could be crossed by 
  # the extended version of the cross section geometry. This is done in both directions
  
  head_check = 1
  tail_check = 1
  
  # head_check <- sum( geos::geos_intersects( 
  #                   geos_extend_line(cs_line, max(dist_vect), "head"),
  #                   geoms_to_cut),  na.rm = TRUE)
  # 
  # tail_check <- sum(geos::geos_intersects( 
  #                   geos_extend_line(cs_line, max(dist_vect), "tail"),
  #                   geoms_to_cut),  na.rm = TRUE)
  
  
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
    final_count   = head_check,
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
    final_count   = tail_check,
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
    
    # if NOT AN INNER LINE, postpone processesing
    if(position != "inner") {
      
      # set "position" values for these geometries
      head_map$set("position", position)
      tail_map$set("position", position)
      
    } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
      
      # set "position" values for these geometries
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

  # # update "relative_position" column in cross_section to reflect the position of the cross section flowline within the braid value
  # cross_section$relative_position <- position
  # 
  # # if NOT AN INNER LINE, postpone processesing
  # if(position != "inner") {
  #   # DON"T UPDATE "pending" value to reflect that this line should be put on hold and processed after the inner flowlines
  #   
  #   # update head/tail distances values in dataframe w/ values from head/tail hashmaps
  #   cross_section$head_distance <- head_map$get("total_distance")
  #   cross_section$tail_distance <- tail_map$get("total_distance")
  #   
  #   # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
  #   cross_section$head_cuts <- head_map$get("count")
  #   cross_section$tail_cuts <- tail_map$get("count")
  #   
  #   # if LINE IS A INNER LINE, GET READY TO EXTEND
  # } else {
  #   
  #   # UPDATE "pending" value to reflect that this is a inner flowline and it should be processed at once
  #   cross_section$pending <- FALSE
  #   
  #   # update head/tail distances values in dataframe w/ values from head/tail hashmaps
  #   cross_section$head_distance <- head_map$get("total_distance")
  #   cross_section$tail_distance <- tail_map$get("total_distance")
  #   
  #   # update head_cuts/tail_cuts counts (intersection counts) values dataframe w/ values from head/tail hashmaps
  #   cross_section$head_cuts <- head_map$get("count")
  #   cross_section$tail_cuts <- tail_map$get("count")
  # }
  # # res_geom <- extend_transects(
  # #                   starter_line   = cs_line, 
  # #                   head_distance  = head_map$get("total_distance"),
  # #                   tail_distance  = tail_map$get("total_distance"),
  # #                   extra_distance = cs_width/2
  # #                 )
  # return(cross_section)
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
#' @param final_count integer, final total count of intersections in one direction. Default is 1
#' @param map logical, whether to return a fastmap::fastmap() containing details about
#'  the extending line (distance, number of intersections, IDs of intersected geometries, etc.) 
#'  or to just return the extended line. Default is TRUE, which will return a fastmap::fastmap() that can be used 
#'  later on to extend the line the necessary distance.  If FALSE, a geos_geometry of the extended linestring is returned
#' @noRd
#' @keywords internal
#' @return fastmap::fastmap() with details on line extension, or a geos_geometry of the extended line
#' @importFrom geos as_geos_geometry geos_intersects
#' @importFrom fastmap fastmap
geos_extend_out <- function(
    x,
    line,
    distances,
    geoms_to_cut, 
    geom_ids,
    ids, 
    dir = "head",
    final_count = 1,
    map = TRUE
) {
  
  # initialize hashmap 
  if(map) {
    dmap <- fastmap::fastmap()
  }
  
  # count interesections
  count <- 0
  dcount <- 0
  
  # # if intilial count == 0, return default dataset
  # if(final_count == 0) {
  #   if(map) {
  #     dmap$mset(
  #       index           = x, 
  #       distance        = distances[x], 
  #       total_distance  = dcount,
  #       line            = line,
  #       cut_ids         = ids,
  #       count           = count,
  #       direction       = dir
  #     )
  #     return(dmap) 
  #   } 
  #   return(line)
  # }
  
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
#' @noRd
#' @keywords internal
#' @return index of 'distance' vector, representing the minimum extension distance for a line to intersect nearby geometries
#' @importFrom geos geos_intersects
geos_bs_distance <- function(
    distances, 
    line,
    geoms_to_cut, 
    direction = "head"
) {
  
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

### 
### Old binary search function
### 

# #Extend a linestring out and determine the minimum extension distance to cross all possible other geometries
# #Internal function, implements a binary search algorithm to determine the minimum distance the geos_geometry linestring 'line' must be extended to cross all possible geometries in 'geoms_to_cut'
# #@param distances numeric vector in ascending order
# #@param line geos_geometry linestring
# #@param geoms_to_cut geos_geomtry linestrings to try and interesect by extending 'line'
# #@param direction character, direction to extend linestring from. Either "head", "tail" or "both". Default is "head".
# #@noRd
# #@keywords internal
# #@return numeric value indicating the index of the value in 'distances' that is the minimum extension distance to intersect all possible geometries in 'geoms_to_cut'
# #@importFrom sf st_intersects
# binary_search_distance <- function(distances, line, geoms_to_cut, direction = "head") {
#   
#   # left and right pointers at the start and end of the 'distances' vector, respectively
#   L = 1
#   R = length(distances)
#   
#   # while left pointer is less than or equal to the right pointer, run binary search
#   while(L <= R) {
#   
#     # calculate midpoint
#     M = (L + R) %/% 2
#     
#     # if midpoint is at the end or start of the 'distances' vector, return left pointer
#     if(M == 0 | M == length(distances)) {
#       # message("EARLY STOPPING bc M = ", M)
#       # message("RETURNING L = ", L)
#       return(L)
#     }
#     
#     # extend linestring by distance value at midpoint (M pointer)
#     new_line <- st_extend_line(line, distances[M], dir = direction)
# 
#     # check if any of the other braid linestrings get intersected by the extended line:
#     # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
#     # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
#     if(any(lengths(sf::st_intersects(geoms_to_cut, new_line)) > 0)) {
#       # message("DECREM RIGHT.--> need smaller value")
#       # message("R = R - 1 = : ", M - 1)
#       
#       # decrement right pointer to middle - 1
#       R = M - 1
#     } else {
#       # message("DECREM RIGHT.--> need smaller value")
#       # message("L = M + 1 = : ", M + 1)
#       
#       # increment left pointer to middle + 1
#       L = M + 1
#     }
#     # message("=======================")
#   }
# 
#   return(L)
# }

#' Find the direction of the endpoints of a linestring (v2)
#' Internal function used in geos_extend_line() function to identify the the direction of each of the ends of a linestring
#' @param line geos_geometry, linestring
#' 
#' @noRd
#' @keywords internal
#' @return numeric vector of angle directions of a given linestring
#' @importFrom geos as_geos_geometry
#' @importFrom wk wk_coords
geos_linestring_dir <- function(line) {
  
  # line <- xs$geometry[5]
  
  # convert to WK coords
  coords <- wk::wk_coords(line)
  coords <- as.matrix(coords[c("x", "y", "feature_id")])
  # coords <- coords[c("x", "y", "feature_id")]
  
  x_coords <- unname(coords[, 1])
  y_coords <- unname(coords[, 2])
  
  x1 = x_coords[1]
  y1 = y_coords[1]
  
  x2 = x_coords[2]
  y2 = y_coords[2]
  
  dirs <- c(atan2(y1 - y2, x1 - x2), atan2(y2- y1, x2 - x1))
  
  return(dirs)
  
}

# # Old version of geos_linestring_dir() (v1)
# geos_linestring_dir_old <- function(line) {
#   
#   # if NOT a geos_geometry class, coerce
#   if(!inherits(line, "geos_geometry")) {
#     # convert to geos geometry
#     line <- geos::as_geos_geometry(line)
#   }
#   
#   # convert to WK coords
#   coords <- wk::wk_coords(line)
#   coords <- coords[c("x", "y", "feature_id")]
#   
#   # dimensions
#   k <- c(1, - 1)
#   i <- c(2, nrow(coords) - 1)
#   
#   dirs <- mapply(i, k, FUN = function(i, k) {
#     x1 <- coords[i-k, 1]
#     y1 <- coords[i-k, 2]
#     x2 <- coords[i, 1]
#     y2 <- coords[i, 2]
#     unname(atan2(y1 - y2, x1 - x2))
#   })
#   
#   return(dirs)
#   
# }

#' Extend a geos_geometry linestring from, one or both ends, by a given distance (meters)
#'
#' @param line sf linestring or geos_geometry linestring to extend
#' @param distance numeric value in meters or a vector of length 2 if 'end = "both"' where 
#       the first value in the vector will extend that tail by that value and the second value extends the head by that value c(tail, head).
#       If a single value is given when end = "both", the value is recycled and used to extend both ends
#' @param dir character, determines whether to extend the linestring from the 'tail', 'head' or 'both' ends
#' @param with_crs logical, whether a CRS should be prescribed to extended output geos_geometry linestring
#' 
#' @return geos_geometry linestring extended by 'distance' from either the 'head', 'tail' or 'both' ends of the original linestring
#' @importFrom geos as_geos_geometry geos_make_linestring
#' @importFrom wk wk_coords wk_crs
#' @export
geos_extend_line <- function(line, 
                             distance,
                             dir = "both", 
                             with_crs = TRUE
                             ) {
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
  
  # ###### DETERMINE THE DIRECTION OF EACH COORDINATE
  x_coords <- unname(coords[, 1])
  y_coords <- unname(coords[, 2])
  
  # X/Y coordinate pairs
  x1 = x_coords[1]
  y1 = y_coords[1]
  x2 = x_coords[2]
  y2 = y_coords[2]
  
  directions <- c(atan2(y1 - y2, x1 - x2), atan2(y2 - y1, x2 - x1))
  # ###
  
  # get directions of the direction of interest
  directions <- directions[to_keep]
  # directions <- geos_linestring_dir(line)[to_keep]
  
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
#' @noRd
#' @keywords internal
#' @return a list with 2 sf dataframes, the updated braided dataset and the updated original "not_braided" dataset, a list of 2 sf objects containing the updated braids with braids removed that are greater than the threshold value, and an sf object containing the original remaining network linestrings
#' @importFrom dplyr group_by mutate ungroup filter bind_rows
#' @importFrom sf st_length
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
  
  # # Extract the list of "braid_id" that are getting removed from output braids 
  drop_braids <- unique(dplyr::filter(x, !comid %in% to_keep)$braid_id)
  drop_braids <- unique(unlist(strsplit(drop_braids, ", ") ))
  
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

# 
# braid_thresholder2 <- function(x, 
#                               originals,
#                               threshold = NULL, 
#                               new_braid_ids = "no_braid",
#                               verbose   = TRUE
# ) {
#   # braid_threshold = 15000
#   # x         = braids
#   # originals = not_braids
#   # threshold = braid_threshold
#   # new_braid_ids = "no_braid"
#   # verbose   = TRUE
#   # x         = braids
#   # originals = not_braids
#   # threshold = braid_threshold
#   # new_braid_ids = "no_braid"
#   # verbose   = TRUE
#   
#   # input check for input 'x'
#   if(is.null(x)) {
#     stop("missing 'x' input argument")
#   }
#   
#   # input check for input 'originals'
#   if(is.null(originals)) {
#     stop("missing 'originals' input argument")
#   }
#   
#   # input check for input 'threshold'
#   if(is.null(threshold)) {
#     stop("missing 'threshold' input argument")
#   }
#   
#   xlengths <- 
#     x %>% 
#     dplyr::group_by(braid_id) %>%
#     dplyr::mutate(
#       braid_length = as.numeric(
#         sum(sf::st_length(geometry), na.rm = T))
#     ) %>%
#     dplyr::ungroup()
#   
#   # # check to make sure some braids are over threshold and can be removed, if NOT then just return original data
#   if(all(xlengths$braid_length <= threshold)) {
#     
#     message("Removing: 0 braids from braided dataset\n",
#             "Keeping: All braids as all braids have total flowline lengths less than or equal to threshold value: ", threshold)
#     
#     return(list(
#       braids     = x,
#       not_braids = originals
#       )
#     )
#     
#   }
#   
#   # # comids to keep (total length of braid linestrings is less than or equal to braid_threshold value)
#   # to_keep <- dplyr::filter(unpacked, braid_length <= threshold)$comid
#   # # to_keep <- dplyr::filter(unpacked, braid_length <= threshold)$braid_id
#   # # dplyr::filter(x, grepl(paste0(unique(to_keep), collapse = "|"),braid_id))
#   # 
#   # # # Extract the list of "braid_id" that are getting removed from output braids 
#   # drop_braids <- unique( dplyr::filter(x, !braid_id %in% to_keep)$braid_id)
#   # drop_braids <- unique(unlist(strsplit(drop_braids, ", ") ))
#   
#   # seperate out small and large braid groups
#   smalls <- dplyr::filter(xlengths, braid_length <= threshold)
#   bigs <- dplyr::filter(xlengths, braid_length > threshold)
#   
#   # add the "too big braid COMIDs" back to original "not_braids" data 
#   # and set these comids braid_ids to "no_braid" and is_multibraid = FALSE
#   originals <- dplyr::bind_rows(
#     originals,
#     # dplyr::select(
#     dplyr::mutate(
#       bigs,
#       braid_id      = new_braid_ids,
#       # braid_id      = "no_braid",
#       # braid_id      = "thresholded",
#       is_multibraid = FALSE
#     )
#   )
#   
#   if(verbose) {
#     message("Removing: ", nrow(bigs), 
#             " rows of braids from braided dataset\nKeeping: ",   nrow(smalls),
#             " rows of braids that have total flowline lengths less than or equal to threshold value: ",
#             threshold)
#     }
#   
#   return(list(
#     braids     = smalls,
#     not_braids = originals
#     )
#   )
# }


#' Find the total length of all flowlines of each braid in a NHDPlus dataset
#' 
#' Internal function that gets the lengths of each braid ID in an NHDPlus dataset (output of find_braids() function)
#' 
#' @param x sf object of braided flowlines (output of find_braids()). Requires 'braid_id' and 'comid' column.
#' @param keep_geom logical, whether to keep geometries or not. Default is FALSE to drop geometries
#' @param multibraid logical, whether to give braid lengths for each braid ID our the braid length of all flowlines in a group of braids (i.e. multibraids). If FALSE (default) the braid length is calculated for each unique braid_id. 
#' @noRd
#' @keywords internal
#' @return dataframe with 3 columns, the braid_id, length (meters) of the total flowline length, and comids within the braid, and optionally a sf geometry
#' @importFrom dplyr filter group_by summarize ungroup arrange
#' @importFrom sf st_length st_drop_geometry
braid_lengths <- function(x, 
                          keep_geom = FALSE, 
                          multibraid = FALSE
                          ) {
  
  # input check for input 'x'
  if(is.null(x)) {
    stop("missing 'x' input argument")
  }
  
  # if multibraid == FALSE, then calculate the length of each unique braid_id
  if(!multibraid) {
    # unpack nested braid_id column0
    xlengths <- unnpack_braids(x)
    
    xlengths <-
      xlengths %>% 
      dplyr::group_by(braid_id) %>%
      dplyr::summarize(
        braid_length = as.numeric(
          sum(sf::st_length(geometry), na.rm = T)
        ),
        comids = paste0(c(comid), collapse = ", ")
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::arrange(-braid_length)
    
  } else {

    # lengths of multibraids (groups of braids)
    xlengths <- 
      x %>% 
      dplyr::group_by(braid_id) %>%
      dplyr::mutate(
        braid_length = as.numeric(
          sum(sf::st_length(geometry), na.rm = T))
      ) %>%
      dplyr::ungroup() %>% 
      dplyr::arrange(-braid_length)
  }
  
  # drop geometries if keep_geom is FALSE
  if(!keep_geom) {
    
    xlengths <- sf::st_drop_geometry(xlengths)
  }
  
  return(xlengths)
  
}

#' Retrieve the COMIDs for all connected flowlines in list of "braid_ids"
#' 
#' The 'x' dataframe is the output of putting NHDPlus data through the
#' find_braids() function and then the find_connected_components() function.
#' 
#' @param x must be an dataframe/tibble/sf dataframe containing comid, braid_id, component_id, and braid_vector columns
#' @param braids_to_match list of character vectors to match with braid_ids
#' @param braid_ids character vector of braid_ids (i.e. c("braid_1", "braid_2", "braid_3"))
#'
#' @noRd
#' @keywords internal
#' @return vector containing the unique COMIDs within each braid_id
#' @importFrom dplyr filter
comids_in_braid_ids <- function(x, braids_to_match, braid_ids) {
  
  # braid IDs in. the x dataframe column that have only the braid IDs from braid_ids
  is_subset <- sapply(
    braids_to_match,
    function(vec) {
      all(vec %in% braid_ids)
    }
  )
  
  # Here what ive done is used the find_connected_components on the braids 
  # object and then I've located the connected componment IDs of the braids that 
  # contains ALL and ONLY ALL of the braid_ids in "bids"
  # maybe in addition to using this i can find additional neighbors by looking for 
  # braid_ids that contains my BIDs and ONLY my bids but also extra braid_ids 
  #   (this would only be for cases when the "bid" is a single braid_id? )
  
  # subset x dataset to the rows that contain ALL of the braid_ids and then use the component_id 
  # from these rows to further filter "x" based on the "component_id"
  # # Subset x to rows where braid_ids are a subset of braid_ids
  components <-  x[x$component_id %in% unique(x[is_subset, ]$component_id), ]
  
  # # logical vector to find any braid_ids that have ZERO overlap with the braid_ids in braid_ids
  component_braid_ids <- components$braid_vector
  
  # # logical vector to find any braid_ids that have ZERO overlap with the braid_ids in braid_ids
  no_overlaps <- sapply(
    component_braid_ids,
    function(vec) {
      any(vec %in% braid_ids)
    }
  )
  
  # remove braid_id that have NO overlap with braid_ids
  components <- components[no_overlaps, ]
  
  # return the unique COMIDs 
  return(unique(components$comid))
  
}

# comids_in_braid_ids_old <- function(x, braid_ids) {
# 
#   # split the braid_id column by the ", " delimiter
#   braid_list <- strsplit(x$braid_id, ", ")
#   
#   # braid IDs in. the x dataframe column that have only the braid IDs from braid_ids
#   bid_in_braids <- unlist(lapply(braid_list, function(vec) all(vec %in% braid_ids)))
#   
#   # Here what ive done is used the find_connected_components on the braids 
#   # object and then I've located the connected componment IDs of the braids that 
#   # contains ALL and ONLY ALL of the braid_ids in "bids"
#   # maybe in addition to using this i can find additional neighbors by looking for 
#   # braid_ids that contains my BIDs and ONLY my bids but also extra braid_ids 
#   #   (this would only be for cases when the "bid" is a single braid_id? )
#   
#   # subset x dataset to the rows that contain ALL of the braid_ids and then use the component_id 
#   # from these rows to further filter "x" based on the "component_id"
#   components <- dplyr::filter(
#                       x,
#                       component_id %in% unique(x[bid_in_braids, ]$component_id)
#                       ) 
#   
#   # logical vector to find any braid_ids that have ZERO overlap with the braid_ids in braid_ids
#   no_overlaps <- unlist(
#                     lapply(strsplit(components$braid_id, ", "), function(vec) {
#                       any(vec %in% braid_ids) 
#                       }
#                     )
#                   )
#   
#   # remove braid_id that have NO overlap with braid_ids
#   components <- components[no_overlaps, ]
#   
#   # return the unique COMIDs 
#   return(unique(components$comid))
#   
# }

# *****************************************
# ------------- PLOTTING (WIP) ------------
# *****************************************

### Function for creating a plot of all 3 types of geoms_to_cut methods to see the difference in which 
### "other" geometries are being selected and thus transect lines are attempting to cut across

# make_geoms_to_cut_plot <- function(
#     shp,
#     x,
#     id           = NULL,
#     braid_id     = NULL,
#     component    = NULL,
#     save_path = NULL
# ) {
# 
#   comid_cuts <- get_geoms_to_cut(
#     x            = x,
#     id           = id,
#     braid_id     = braid_id,
#     component    = component,
#     method       = "comid"
#   )
# 
#   comp_cuts <-  get_geoms_to_cut(
#     x            = x,
#     id           = id,
#     braid_id     = braid_id,
#     component    = component,
#     method       = "component"
#   )
# 
#   neigh_cuts <- get_geoms_to_cut(
#     x            = x,
#     id           = id,
#     braid_id     = braid_id,
#     component    = component,
#     method       = "neighbor"
#   )
# 
#   # ggplot2 theme
#   dark_thm <-
#     ggplot2::theme_dark() +
#     ggplot2::theme(
#       plot.title = ggplot2::element_text(size = 14, face = "bold", hjust = 0.5),
#       plot.subtitle = ggplot2::element_text(hjust = 0.5)
#       # axis.text.x = ggplot2::element_blank()
#     )
# 
#   comid_plot <-
#     ggplot2::ggplot() +
#     ggplot2::geom_sf(data = x, color = "cyan",  alpha = 1,lwd = 1) +
#     ggplot2::geom_sf(data = comid_cuts, color = "red", alpha = 1, lwd = 1) +
#     # ggplot2::geom_sf(data = shp, color = "green", lwd = 15) +
#     # ggplot2::geom_sf(data = sf::st_buffer(shp, 750), fill = "green") +
#     ggplot2::geom_sf(data = shp, color = "green", lwd = 2) +
#     ggplot2::geom_sf(data = sf::st_buffer(shp, 60), fill = "green", color = "black") +
#     ggplot2::labs(
#       title = "COMIDS",
#       subtitle = paste0("# geoms to cut = ", nrow(comid_cuts))
#     ) +
#     dark_thm
# 
#   neigh_plot <-
#     ggplot2::ggplot() +
#     ggplot2::geom_sf(data = x, color = "cyan",  alpha = 1, lwd = 1) +
#     ggplot2::geom_sf(data = neigh_cuts, color = "red",  alpha = 1, lwd = 1) +
#     # ggplot2::geom_sf(data = shp, color = "green", lwd = 15) +
#     # ggplot2::geom_sf(data = sf::st_buffer(shp, 750), fill = "green") +
#     ggplot2::geom_sf(data = shp, color = "green", lwd = 2) +
#     ggplot2::geom_sf(data = sf::st_buffer(shp, 60), fill = "green", color = "black") +
#     ggplot2::labs(
#       title = "NEIGHBORS",
#       subtitle = paste0("# geoms to cut = ", nrow(neigh_cuts))
#     ) +
#     dark_thm
# 
#   comp_plot <-
#     ggplot2::ggplot() +
#     ggplot2::geom_sf(data = x, color = "cyan",  alpha = 1, lwd = 1) +
#     ggplot2::geom_sf(data = comp_cuts, color = "red", alpha = 1, lwd = 1) +
#     # ggplot2::geom_sf(data = shp, color = "green", lwd = 15) +
#     # ggplot2::geom_sf(data = sf::st_buffer(shp, 750), fill = "green") +
#     ggplot2::geom_sf(data = shp, color = "green", lwd = 2) +
#     ggplot2::geom_sf(data = sf::st_buffer(shp, 60), fill = "green", color = "black") +
#     ggplot2::labs(
#       title = "COMPONENTS",
#       subtitle = paste0("# geoms to cut = ", nrow(comp_cuts))
#     ) +
#     dark_thm
# 
#   # concatonate all plots together into single plot
#   geoms_to_cuts_plot <- comid_plot + neigh_plot + comp_plot
# 
#   # save_path = "/Users/anguswatters/Desktop/"
#   # save_path = "/Users/anguswatters/Desktop/test_geoms_to_cut_plot.png"
#   if(!is.null(save_path)) {
# 
#     message("Saving geoms_to_cut plot to:\n ", save_path)
# 
#     ggplot2::ggsave(
#       filename = save_path,
#       plot = geoms_to_cuts_plot,
#       # width = 10,
#       # height = 10,
#       scale = 1
#     )
# 
#   }
# 
#   return(geoms_to_cuts_plot)
# 
# }

#Create a plot of all the different transects and there possible groupings
#
#@param net sf object of NHDplusv2 data
#@param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
#@param terminal_id character, column name containing a unique identifier, delineating seperate networks in the 'network' dataset. Default is NULL which will use 'find_connected_components()' and determine the connected components in the graph to try and create a 'component_id' column in 'network'
#@param braid_threshold numeric value, value of the total length of all flowlines in a braid. Only braids with total flowline
#lengths less than or equal to the threshold will be considered by function (i.e. determines that maximum braid size that fix_braid_transects() should operate on).
#Default is NULL, which will attempt to fix all the braid transects in the data
#@param version integer, version number of braid algorithm to use, either 1 or 2. Default is 2.
#@param method The method to determine the geometries to cut. Options are "comid", "component", or "neighbor". Default is "comid"
#@param rm_intersects logical, whether to remove transect linestrings that intersect with other parts of the network ('net'). Default is TRUE which will remove intersecting linestrings.
#@param keep_plots logical, whether to return a list of ggplot2 plots or have function return NULL. Default is FALSE, returns NULL
#@param save_path character, path to a directory to save all ggplot2 plots to.
#@return NULL or a list of ggplot2 plots if keep_plots = TRUE
plot_braid_geoms_to_cut <- function(
    net,
    transect_lines,
    terminal_id     = NULL,
    braid_threshold = NULL,
    version         = 2,
    method          = "comid",
    rm_intersects   = TRUE,
    keep_plots = FALSE,
    save_path = NULL
) {

  ### TEST DATA INPUTS
  # net = net2
  # transect_lines = transects
  # terminal_id = NULL
  # braid_threshold = NULL
  # version = 1
  # rm_intersects = TRUE
  # transect_lines <-  transects_nofix
  # net <- net3
  # braid_threshold = NULL
  # braid_threshold = 25000
  ###

  # names that transect_lines starts out with to use at the end
  starting_names <- names(transect_lines)

  # set geometry name of network to "geometry"
  net <- nhdplusTools::rename_geometry(net, "geometry")

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

  braids <- find_braids(
    network     = net,
    terminal_id = terminal_id,
    add         = TRUE,
    nested      = TRUE,
    version     = version,
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
  braids <-
    braids %>%
    dplyr::filter(braid_id != "no_braid") %>%
    dplyr::group_by(braid_id) %>%
    dplyr::ungroup()


  # temporary braid_threshold variable of "drop_max" will use all of the braids EXCEPT the max length braid (minus 1 meter)
  if(!is.null(braid_threshold) && braid_threshold == "drop_max") {

    braid_threshold <- max(braid_lengths(braids)$braid_length) - 1

  }

  if (!is.null(braid_threshold)) {

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

  # add connected component "component_id" column
  braids <- find_connected_components(braids)

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
  }

  # keep track of seen component_ids
  seen <- fastmap::fastmap()

  # Loop through every single cross section and determine:
  # 1. its relative position
  # 2. how far to extend the line
  # 3. in what order should transects be extended,
  # 4. in what direction to extend the transect

  # list to store plots
  plot_list <- list()

  # system.time({
  for(i in 1:nrow(xs)) {
    # message("i: ", i, "/", nrow(xs))


    # 1 = braid2_components
    # 2 = braid_components
    # 3 = braid2_comids
    # 4 = braid2_neighs
    # 5 = braid_comids
    # 6 = braid_neighs

    # comid of transect line
    com <- xs$hy_id[i]

    # braid ID of interest
    bid <- xs$braid_id[i]

    # get the component ID of current COMID
    comp_id <- braids$component_id[braids$comid == com]

    # make a plot for each unique component ID that comes up
    if(!seen$has(comp_id)) {
      # save_path = "/Users/anguswatters/Desktop/test_geoms_to_cut_plot.png"
      geoms_to_cut_plot <- make_geoms_to_cut_plot(
        shp          = xs[i, ],
        x            = braids,
        id           = com,
        braid_id     = bid,
        component    = comp_id,
        save_path = paste0(save_path, "geoms_to_cut_component_",
                           comp_id,
                           ".png"
        )
        # save_path = paste0("/Users/anguswatters/Desktop/transect_figs/geoms_to_cut/geoms_to_cut_component_",
        #                    comp_id,
        #                    ".png")
      )

      message("--> Adding ", comp_id, " to seen hashmap...")

      # add component ID to hashmap
      seen$set(comp_id, TRUE)

      message("Length of seen hashmap: ", seen$size())
    }

    if(keep_plots) {

      plot_list[[i]] <- geoms_to_cut_plot

    }
  }

  if(keep_plots) {
    plot_list <- Filter(function(x) !is.null(x), plot_list)

    return(plot_list)
  }

}

# *********************************************
# ------------- THRESHOLDING (WIP) ------------
# *********************************************

# #Check if the base line length + extension from head and tail is less than or equal to the threshold distance value
# #Internal function. Used to check if an extended line has gone past its maximum distance allowed by the threshold input.
# #@param base_distance numeric, length (meters) of the line
# #@param head_distance numeric, minimum length (meters) to extend line from HEAD direction to cross all neighboring flowlines
# #@param tail_distance numeric, minimum length (meters) to extend line from TAIL direction to cross all neighboring flowlines
# #@param threshold numeric, maximum length of the final, fully extended line. Default is NULL (which will return TRUE)
# #@noRd
# #@keywords internal
# #@return logical, TRUE if line is within the threshold value, FALSE otherwise
# within_threshold <- function(
#     base_distance,
#     head_distance  = 0,
#     tail_distance  = 0,
#     threshold = NULL
# ) {
#   
#   # # inputs to function
#   # base_distance = cs_width
#   # head_distance  =  head_map$get("total_distance")
#   # tail_distance  =  tail_map$get("total_distance")
#   # threshold = max_distance
#   
#   # if no threshold is given (NULL), return TRUE, the line extensions WILL be within the threshold
#   if(is.null(threshold)) {
#     return(TRUE)
#   }
#   
#   # the base_distance is the length of the line,
#   # so the extra_distance is 1/2 the base_distance (i.e. the distance from the midpoint to either end of the line)
#   extra_distance <- base_distance/2
#   
#   # head/tail distance + the extra distance that the line has to go after the head/tail extension.
#   # Reminder:
#   # the head/tail extension distance will extend the line out the minimum distance needed to
#   # cross over all flowlines in that direction ('head' or 'tail' direction), thus there is still an extra_distance that the line needs
#   # to extend out further in order for the final line to cross the final intersected flowline and still
#   # have a cross section distancce AFTER the final flow line intersection
#   # IF the initial head_distance/tail_distance is 0,
#   # - that means that the line is NOT going to extend out in that direction, thus we can set the extra_head/extra_tail distancce to 0
#   # - otherwise extra_head/extra_tail is the head_distance/tail_distance + 1/2 the initial 'base_distance'
#   extra_head <- ifelse(head_distance > 0, head_distance + extra_distance, 0)
#   extra_tail <- ifelse(tail_distance > 0, tail_distance + extra_distance, 0)
#   
#   # total length of line
#   total_distance <- extra_head + extra_tail + base_distance
#   
#   # boolean of whether total_distance is valid (less than the 'threshold')
#   is_valid_distance <- total_distance < threshold
#   
#   return(is_valid_distance)
#   
# }
# 
# #Reset fastmap containing the extension data for a line
# #Internal function. Used for when the extension is going to be greater than the max transect length threshold
# #@param line_map fastmap with the following keys: index, distance, total_distance, cut_ids, count, and is_thresholded
# #@param id numeric or character ID of the original line to set the "cut_ids" key to. Default is NULL which will use the first id in "cut_ids"
# #@noRd
# #@keywords internal
# #@return 
# reset_line_map <- function(line_map, id = NULL) {
#   
#   # set index to 1
#   line_map$set("index", 1)
#   
#   # set distance and total_distance to 0
#   line_map$set("distance", 0)
#   line_map$set("total_distance", 0)
#   
#   # if id is NULL (not provided in function call), then use the FIRST id in 'cut_ids' key
#   if(is.null(id)) {
#     line_map$set("cut_ids", line_map$get("cut_ids")[1])
#   } else {
#     line_map$set("cut_ids", id)
#   }
#   # set count to 0
#   line_map$set("count", 0)
#   
#   # set is_thresholded to TRUE
#   line_map$set("is_thresholded", TRUE)
#   
#   # return(line_map)
# }

# #Determine the distances needed to extend a transect linestring geometry across neighboring flowlines v2
# #Internal Function, version 2 of geos_augment_transect(). WIP.
# #Function takes in a transect line, and a set of nearby geos_geometries that the transect line should be compared against to see how far the given transect line should be
# #extended in both directions in order to cross over all the avaliable nearby flowline geos_geometries. Specifically to be used for situations where a river network is braided.
# #@param cross_section geos_geometry, transect line to try and extend to cover braided river sections. Dataframe row must contain a "cs_width", "bf_width", "hy_id", and a "geometry" column
# #@param geoms_to_cut geos_geometry, other lingestrings (flowlines) of network that "cross_section" should attempt to extend out to, and to cut across
# #@param geom_ids character, unique identifier (comid/hy_id) of transect line
# #@param max_distance numeric, maximum distance (meters) to extend line out by
# #@param by numeric, distance to incrementelly extend out transect line.
# #@param keep_geom logical, whether to carry geometries and keep them in the functions return. Default is TRUE, which will return the extended geometries with the function returns.
# #@noRd
# #@keywords internal
# #@return dataframe or list of fastmap::fastmap() objects with meta data describing how far to extend a given transect line, in which directions and the relative position of the transect line.
# geos_augment_transect2 <- function(cross_section,
#                                    geoms_to_cut,
#                                    geom_ids,
#                                    max_distance = NULL,
#                                    by = NULL,
#                                    keep_geom = TRUE
# ) {
#   
#   # max distance from transect of interest and rest of braid flowlines
#   # TODO (need a better method of determing max possible extension of flowline)
#   # max_dist <- as.numeric(max(
#   #                   sf::st_distance(
#   #                     geoms_to_cut,
#   #                     x
#   #                   )))
#   
#   # cs_line <- geos::as_geos_geometry(cross_section$geometry)
#   # cross_section$lengthm
#   # cross_section$geometry %>% sf::st_length()
#   # max_distance = 2000
#   # max_distance <- (max_distance - cross_section$lengthm)/2
#   
#   # extract values from cross_section dataframe
#   cs_width <- cross_section$cs_widths
#   bf_width <- cross_section$bf_width
#   id       <- cross_section$hy_id
#   
#   # convert cross section line to geos_geometry
#   cs_line  <- geos::as_geos_geometry(cross_section$geometry)
#   
#   # if no "by" argument is given, then the default becomes bf_width/2
#   if(is.null(by)) {
#     by = bf_width/2
#   }
#   
#   # if no "by" argument is given, then the default becomes bf_width/2
#   if(is.null(max_distance)) {
#     
#     total_distance <- max(cs_width * 5)
#     # max_distance <- max(cs_width * 5)
#     # max_distance <- 10000
#     
#     # sequence from 0 to the max possible extension distance
#     dist_vect <- seq(0, max(c(total_distance, 2000), by = by))
#     
#   } else {
#     
#     # calculate the max distance that a line should possibly be extended from both ends
#     total_distance <- abs((max_distance - cross_section$lengthm)/2)
#     # max_distance <- abs((max_distance - cross_section$lengthm)/2)
#     
#     # sequence from 0 to the max possible extension distance
#     dist_vect <- seq(0, total_distance, by = by)
#     
#   }
#   
#   # # calculate the max distance that a line should possibly be extended from both ends
#   # max_distance <- (max_distance - cross_section$lengthm)/2
#   
#   # # sequence from 0 to the max possible extension distance
#   # dist_vect <- seq(0, total_distance, by = by)
#   # dist_vect <- seq(0, max_distance, by = by)
#   # dist_vect <- seq(0, max(c(max_distance, 2000)), by = by)
#   
#   # dist_vect <- seq(0, max(c(max_distance, 2000)), multi_transects[i, ]$bf_width)
#   
#   # EXTEND OUT lines
#   # extend transect line out in both directions and find the side that interests with m
#   # extend line out from HEAD side of line
#   # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with
#   head_map <- geos_extend_out(
#     x             = 1,
#     line          = cs_line,
#     distances     = dist_vect,
#     geoms_to_cut  = geoms_to_cut,
#     geom_ids      = geom_ids,
#     ids           = c(id),
#     threshold     = max_distance,
#     dir           = "head",
#     map           = TRUE
#   )
#   
#   # head_map$as_list()
#   # head_map$get("is_thresholded")
#   
#   if(head_map$get("is_thresholded")) {
#     message("----> id: ", id, " was thresholded in HEAD direction")
#   }
#   
#   # extend line out from TAIL side of line
#   # the line will extend out until it has gone "max_distance" AND all the possible flowlines have been intersected with
#   tail_map <- geos_extend_out(
#     x             = 1,
#     line          = cs_line,
#     distances     = dist_vect,
#     geoms_to_cut  = geoms_to_cut,
#     geom_ids      = geom_ids,
#     ids           = c(id),
#     threshold     = max_distance,
#     dir           = "tail",
#     map           = TRUE
#   )
#   
#   # tail_map$as_list()
#   # tail_map$get("is_thresholded")
#   
#   if(tail_map$get("is_thresholded")) {
#     message("----> id: ", id, " was thresholded in TAIL direction")
#   }
#   
#   head_map$as_list()
#   tail_map$as_list()
#   
#   # check if the final line is within the maximum transect line distance (threshold)
#   if(!within_threshold(
#     base_distance = cs_width,
#     head_distance = head_map$get("total_distance"),
#     tail_distance = tail_map$get("total_distance"),
#     threshold     = max_distance
#   )
#   ) {
#     
#     # head_map$as_list()
#     # tail_map$as_list()
#     message("==== id: ", id, " ====")
#     message("==== THRESHOLDING LINE ====")
#     # reset the values in the head and tail maps to make it so the line is NOT going to be extended,
#     # because extending the line will make it longer than the maximum transect distance threshold
#     reset_line_map(head_map)
#     reset_line_map(tail_map)
#     
#     # head_map$as_list()
#     # tail_map$as_list()
#     
#   }
#   # head_map$get("total_distance")
#   # tail_map$get("total_distance")
#   
#   
#   # head_map$as_list()
#   # tail_map$as_list()
#   # cross_section$lengthm
#   # 359 + 820
#   # # extract the linestringshapes
#   # tail_ext <- tail_map$get("line")
#   # head_ext <- head_map$get("line")
#   
#   # mapview::mapview(braids,color = "gold") +  mapview::mapview(geoms_to_cut,color = "dodgerblue") +
#   # mapview::mapview(head_ext,color = "green") +  mapview::mapview(tail_ext,color = "red") +mapview::mapview(cs_line,color = "cyan")
#   
#   # get the relative position within the braid of the linestring we are extending our transect out from
#   position <- check_relative_position(
#     head_count = head_map$get("count"),
#     tail_count = tail_map$get("count")
#   )
#   
#   # POSITION VALUES explanation:
#   # given the count of interesections from the head and tail of a linestring, return whether the line has:
#   # - NO_INTERSECTION:: (after extending linestring out to max distance)
#   # - OUTER_SINGLE: extending linestring out in both directions yielded
#   # zero intersections in one direction AND exactly one intersection in the other direction
#   # - OUTER_MULTI: extending linestring out in both directions yielded
#   # zero intersections in one direction AND GREATER THAN ONE intersection in the other direction
#   # - INNER: line is in middle (or one of 2 middle lines if even number of total linestrings to cross over)
#   # INNER scenario intersection count (odd and even cases):
#   # intersection counts are EQUAL OR max(head_count, tail_count) - 1 == min(head_count, tail_count)
#   # ----> EDGE CASE: if intersection counts are (0, 1) or (1, 0), these will count as INNER
#   # - IN_BETWEEN/MIDDLE/: This is the else case when the line is between the outer most line (singles or no intersects) and the middle line(s)
#   # ----> SKIP THESE (maybe?) !
#   # TODO: NEED TO CONFIRM THIS IS WHAT WE WANT) ???
#   
#   # # if the total distance in either direction is greater than the max distance, set the total distance to 0
#   # head_map$set("total_distance",
#   #              ifelse(head_map$get("total_distance") > max_distance,
#   #                                          # max_distance,
#   #                                          0,
#   #                                          head_map$get("total_distance")
#   #                                          )
#   #                                  )
#   # tail_map$set("total_distance",
#   #              ifelse(tail_map$get("total_distance") > max_distance,
#   #                                         # max_distance,
#   #                                         0,
#   #                                         tail_map$get("total_distance")
#   #                                         )
#   #                                  )
#   
#   # set the 'position' and 'pending' values for head_map and tail_map
#   # then drop geometries if carry_geom == FALSE
#   
#   # if NOT AN INNER LINE, postpone processesing
#   if(position != "inner") {
#     
#     # set pending values for these geometries
#     head_map$set("pending", TRUE)
#     tail_map$set("pending", TRUE)
#     
#     # set pending values for these geometries
#     head_map$set("position", position)
#     tail_map$set("position", position)
#     
#     
#   } else {  # if LINE IS A INNER LINE, GET READY TO EXTEND
#     
#     # set pending values for these geometries
#     head_map$set("pending", FALSE)
#     tail_map$set("pending", FALSE)
#     
#     # set pending values for these geometries
#     head_map$set("position", position)
#     tail_map$set("position", position)
#     
#   }
#   
#   # if keep_geom is FALSE, remove geometry linestrings from maps before returning
#   if(!keep_geom) {
#     head_map$remove("line")
#     tail_map$remove("line")
#   }
#   
#   # return list with 2 elements. First is the head extension data and the second is the tail extension data
#   return(
#     list(
#       head = head_map,
#       tail = tail_map
#     )
#   )
# }
# 
# 
# #Extend a linestring outward and return the minimum linestring (or details of minimum linestring) that crosses all possible other linestrings (geoms_to_cut) for a the given direction
# #Internal Function, version 2 of geos_extend_out(). WIP
# #@param x start index of distances vector
# #@param line transect line that should be extended
# #@param distances numeric vector of distance values (meters) in ascending order (sorted)
# #@param geoms_to_cut geos_geometry, all other linestrings (all linestrings other than 'line') that should be cut across (typically other linestrings making up a braided section of river)
# #@param geom_ids character or numeric vector of unique identifers for each linestring in 'geoms_to_cut'
# #@param ids character or numeric vector of unique identifier of the 'line' argument
# #@param dir character, either "head" or "tail", indicating which direction to extend 'line' out
# #@param map logical, whether to return a fastmap::fastmap() containing details about
# # the extending line (distance, number of intersections, IDs of intersected geometries, etc.)
# # or to just return the extended line. Default is TRUE, which will return a fastmap::fastmap() that can be used
# # later on to extend the line the necessary distance.  If FALSE, a geos_geometry of the extended linestring is returned
# #@noRd
# #@keywords internal
# #@return fastmap::fastmap() with details on line extension, or a geos_geometry of the extended line
# geos_extend_out2 <- function(
#     x,
#     line,
#     distances,
#     geoms_to_cut,
#     geom_ids,
#     ids,
#     threshold = NULL,
#     dir = "head",
#     map = TRUE
# ) {
#   
#   # # if NOT a geos_geometry class, coerce
#   # if(!inherits(line, "geos_geometry")) {
#   #   # convert to geos geometry
#   #   line <- geos::as_geos_geometry(line)
#   #   # geoms_to_cut <- geos::as_geos_geometry(others)
#   # }
#   
#   # store the original 'line' in case it needs to be returned
#   stash_line <- line
#   
#   # store the original 'ids' in case it needs to be returned
#   stash_ids <- ids
#   
#   # if NOT a geos_geometry class, coerce
#   if(!inherits(geoms_to_cut, "geos_geometry")) {
#     # convert to geos geometry
#     geoms_to_cut <- geos::as_geos_geometry(geoms_to_cut)
#     # geoms_to_cut <- geos::as_geos_geometry(others)
#   }
#   
#   
#   if(map) {
#     dmap <- fastmap::fastmap()
#   }
#   
#   # count interesections
#   count <- 0
#   dcount <- 0
#   
#   # while (TRUE) {
#   while (TRUE) {
#     # while (x < length(distances)) {
#     # message("x: ", x)
#     # message("distances[x]: ", distances[x])
#     # message("dcount: ", dcount)
#     
#     xx <- geos_bs_distance2(
#       distances    = distances,
#       line         = line,
#       geoms_to_cut = geoms_to_cut,
#       direction    = dir
#     )
#     
#     # count        <- count + 1
#     # dcount       <- dcount + distances[xx]
#     
#     # message("xx: ", xx)
#     # message("distances[xx]: ", distances[xx])
#     # message("ids: ", ids)
#     
#     # if a threshold is given and the total distance count is greater than the threshold, stop iteration
#     if (!is.null(threshold) && dcount > threshold) {
#       # message("!!!!!!!!!!!!!!!! ")
#       # message("!!!!!!!! dcount > threshold ", dcount, " > ", threshold, " !!!!!!!! ")
#       # message("!!!!!!!!!!!!!!!! ")
#       
#       # # if specified, return distance map of info and line
#       if(map) {
#         dmap$mset(
#           index           = 1,
#           distance        = 0,
#           total_distance  = 0,
#           line            = stash_line,
#           cut_ids         = stash_ids,
#           count           = 0,
#           direction       = dir,
#           is_thresholded  = TRUE
#         )
#         
#         return(dmap)
#       }
#       
#       # otherwise just return the orginal line
#       return(stash_line)
#       # break
#     }
#     
#     if (xx >= length(distances)) {
#       # message("!!!!!!!!!!!!!!!! ")
#       # message("!!!!!!!! xx >= distance: ", xx, " >= ", length(distances), " !!!!!!!! ")
#       # message("!!!!!!!!!!!!!!!! ")
#       break
#     }
#     
#     # extend line out to midpoint of distances vector
#     crosser <- geos_extend_line(line, distances[xx], dir = dir)
#     
#     # Get the 'new_comid' that will be added to "ids" variable and then passed to the next iteration
#     # - excluding the IDs in 'ids', determine what geometries in 'geoms_to_cut' are intersecting with our extended 'crosser' line
#     # - then index 'geom_ids' based on the boolean vector returned from geos_intersects(), to then get the newly intersected ID (new_comid)
#     new_comid <- geom_ids[
#       geos::geos_intersects(
#         crosser,
#         geoms_to_cut[
#           !geom_ids %in% ids
#         ]
#       )
#     ]
#     
#     # # get the comid of the flow line that was intersected,
#     # # new comid that should be added to "ids" variable and passed to the next iteration
#     # new_comid <- geoms_to_cut$comid[
#     #                     unlist(sf::st_intersects(
#     #                       crosser,
#     #                       dplyr::filter(geoms_to_cut,
#     #                                     !comid %in% ids
#     #                                     )))
#     #                     ]
#     
#     # Update all the variables for next iteration of while loop
#     
#     # update 'line'
#     line         <- crosser
#     
#     
#     # # set the geometries within c(ids, new_comid) to empty (essentially filtering them out)
#     # geoms_to_cut[geom_ids %in% c(ids, new_comid)] <- geos::geos_empty()
#     
#     # # update geom_ids, removing ids and the new_comid
#     # geom_ids <- geom_ids[!geom_ids %in% c(ids, new_comid)]
#     
#     # update 'geoms_to_cut' and drop the newly added 'new_comid'
#     geoms_to_cut <- geoms_to_cut[
#       !geom_ids %in% c(ids, new_comid)
#     ]
#     
#     # update 'geom_ids', removing ids and the new_comid
#     geom_ids <- geom_ids[
#       !geom_ids %in% c(ids, new_comid)
#     ]
#     
#     # update 'ids' w/ new_comid
#     ids          <- c(ids, new_comid)
#     
#     # update x (index) value
#     x            <- xx
#     
#     # increment count and continue summing distances
#     count        <- count + 1
#     dcount       <- dcount + distances[xx]
#   }
#   
#   # if specified, return distance map of info and line
#   if(map) {
#     
#     dmap$mset(
#       index           = x,
#       distance        = distances[x],
#       total_distance  = dcount,
#       line            = line,
#       cut_ids         = ids,
#       count           = count,
#       direction       = dir,
#       is_thresholded  = FALSE
#     )
#     
#     return(dmap)
#   }
#   
#   # otherwise just return the line
#   return(line)
# }
# 
# #Extend a transect line outwards by a certain distance from the head and tail directions of the line
# #Internal Function, version 2 of geos_extend_transects(). WIP.
# #@param starter_line geos_geometry, original transect line to extend outwards
# #@param head_distance numeric, distance (meters) to extend from "head" of the line
# #@param tail_distance numeric, distance (meters) to extend from "tail" of the line
# #@param extra_distance numeric, any extra distance (meters) the line should be extended after the original head/tail distances (this is typically going to be the cross section width divded by 2)
# #@noRd
# #@keywords internal
# #@return geos_geometry, extended by specified distance
# geos_extend_transects2 <- function(
#     starter_line,
#     head_distance  = 0,
#     tail_distance  = 0,
#     extra_distance = 0
# ) {
#   
#   
#   # extra_distance = 100
#   # head_distance = 5555
#   # tail_distance = 150
#   # ifelse(head_distance == 0, 0, extra_distance)
#   
#   # set head and tail extra values to the 'extra_distance' argument
#   head_extra = tail_extra = extra_distance
#   
#   # if the HEAD extending distance is 0, also set the 'head_extra' value to 0
#   if(head_distance == 0) {
#     head_extra = 0
#   }
#   
#   # if the TAIL extending distance is 0, also set the 'tail_extra' value to 0
#   if(tail_distance == 0) {
#     tail_extra = 0
#   }
#   
#   # distance to extend head and tail out by
#   head_extension <- head_distance + head_extra
#   tail_extension <- tail_distance + tail_extra
#   
#   # head_extension <- head_distance + ifelse(head_distance == 0, 0, extra_distance)
#   # tail_extension <- tail_distance + ifelse(tail_distance == 0, 0, extra_distance)
#   # head_extension <- head_distance + (cs_width/2)
#   # tail_extension <- tail_distance + (cs_width/2)
#   
#   # first extend the head outwards
#   res_geom <- geos_extend_line(
#     starter_line,
#     head_extension,
#     "head"
#   )
#   
#   # then extend the tail from the already head extended line
#   res_geom <- geos_extend_line(
#     res_geom,
#     tail_extension,
#     "tail"
#   )
#   
#   return(res_geom)
#   
# }
# 
# #Perform Binary search on sorted distance vector to determine minimum extension distance for a line to intersect with another geometry
# #Internal Function, version 2 of geos_bs_distance(). WIP.
# #@param distances numeric vector sorted in ascending order
# #@param line geos_geometry, linestring to extend out to the point that it crosses the first geometry in "geoms_to_cut"
# #@param geoms_to_cut geos_geometry, geometries to extend "line" out and cut, when line is extending out and intersects with "geoms_to_cut", algo stops and returns the index of the distance array
# #@param direction character, either "head" or "tail", indicating which end of the line to extend out.
# #@noRd
# #@keywords internal
# #@return index of 'distance' vector, representing the minimum extension distance for a line to intersect nearby geometries
# geos_bs_distance2 <- function(
#     distances,
#     line,
#     geoms_to_cut,
#     direction = "head"
# ) {
#   
#   # distances    = distances
#   # line         = line
#   # geoms_to_cut = geoms_to_cut
#   # direction    = dir
#   
#   # Left and right pointers (start and end of distances vector)
#   L = 1
#   R = length(distances)
#   
#   # While left pointer (L) is less than or equal to the right pointer (R), run binary search.
#   # Each iteration:
#   # - the midpoint value gets calculated (M)
#   # - M is the index of the 'distances' vector that we will use as the distance value to extend 'line'
#   # - if the new extended line ('new_line') intersects with 'geoms_to_cut', then we decrease the distance value (DECREMENT RIGHT POINTER to the MIDPOINT - 1),
#   # - if NOT we increase the distance value (INCREMENT LEFT POINTER to the MIDPOINT + 1)
#   while(L <= R) {
#     
#     # calculate midpoint between left and right pointers
#     M = (L + R) %/% 2
#     
#     # message("L: ", L)
#     # message("M: ", M)
#     # message("R: ", R)
#     # message("x[L]: ", distances[L])
#     # message("x[M]: ", distances[M])
#     # message("x[R]: ", distances[R])
#     
#     if(M == 0 | M == length(distances)) {
#       # message("EARLY STOPPING bc M = ", M)
#       # message("RETURNING L = ", L)
#       return(L)
#     }
#     
#     # extend line out to midpoint of distances vector
#     new_line <- geos_extend_line(line, distances[M], dir = direction)
#     # new_line_sf <- st_extend_line(sf::st_as_sf(line), distances[M], end = direction)
#   
#     # check if any of the other braid linestrings get intersected by the extended line:
#     # IF: Any interesection occurs, DECREMENT right pointer and search for a SMALLER distance value
#     # ELSE: no intersection yet, so INCREMENT left pointer and search for a LARGER distance value
#     
#     # if ANY of the geometries in geoms_to_cut are intersected by the new extended line
#     if(
#       any(geos::geos_intersects(geoms_to_cut, new_line))
#     ) {
#       
#       # then DECREMENT RIGHT pointer (DECREASE DISTANCE VALUE) to the midpoint - 1
#       R = M - 1
#       
#       # otherwise IF NO intersections occur:
#     } else {
#       
#       # then INCREMENT LEFT pointer (INCREASE DISTANCE VALUE) to the midpoint + 1
#       L = M + 1
#       
#     }
#     # message("=======================")
#   }
#   
#   # l_line = st_extend_line(ls, x[L])
#   # return(l_line)
#   
#   return(L)
# }
