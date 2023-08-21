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
# ****************************************
# ------------- LATEST (new braid code) ------------
# ****************************************

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
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
  #   start = 2676154,
  #   return_as = "dataframe",
  #   nested    = TRUE,
  #   # nested    = FALSE,
  #   add       = TRUE,
  #   verbose = T
  # )
  
  # add braid_id column to network
  braids <- find_braids_df(
                    network = net, 
                    add     = TRUE,
                    nested  = TRUE,
                    verbose = verbose
                    )
  
  # get_braid_list(network = net)
  
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
  not_braids <- dplyr::filter(braids, braid_id == "no_braid")
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
        extra_distance = other_xs$cs_widths[i]/2
        # extra_distance = xs$cs_widths[i]/2
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
  # mapview::mapview(transect_lines, color = "green") + mapview::mapview(braids, color = "red") + not_braids
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
# ****************************************
# ------------- LATEST (geos) ------------
# ****************************************

#' Fix transects found on braided river sections
#'
#' @param net sf object of NHDplusv2 data
#' @param transect_lines sf linestring dataframe, containing cross sections of flowlines in 'net' the output of "cut_cross_sections2()" function
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
                      extra_distance = other_xs$cs_widths[i]/2
                      # extra_distance = xs$cs_widths[i]/2
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
