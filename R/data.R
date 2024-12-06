#' Braided Flowlines
#'
#' A dataset containing flowlines representing braided river sections. 
#' These flowlines are used in hydrologic models to simulate complex river networks.
#' @source Generated using hydrofabric3D software.
"braided_flowlines"

#' Flowlines
#'
#' A dataset of primary flowlines for hydrologic and hydraulic modeling.
#' @source NOAA Office of Water Prediction.
"flowlines"

#' Flowlines linestring 
#'
#' A dataset containing flowlines linestrings . 
#' @source Generated using hydrofabric3D software.
"linestring"

#' Flowlines Missing Depth
#'
#' A dataset of flowlines missing depth information, which may require further analysis or imputation.
#' @source Derived from \code{flowlines}.
"flowlines_missing_depth"

#' Invalid Flowlines
#'
#' A dataset of flowlines identified as invalid due to self-intersections or other topological errors.
#' @source Processed using \code{rm_self_intersections}.
"invalid_flowlines"

#' Junction Flowlines
#'
#' A dataset of flowlines representing junctions in the river network, used for hydrodynamic connectivity analysis.
#' @source Derived from \code{flowlines}.
"junction_flowlines"

#' NextGen Braided Flowlines
#'
#' A dataset of braided flowlines compatible with the NextGen hydrologic prediction system.
#' @source Created using hydrofabric3D.
"nextgen_braided_flowlines"

#' Remove Self-Intersections
#'
#' A dataset of flowlines processed to remove self-intersections.
#' @source Generated using \code{rm_self_intersections} function.
"rm_self_intersections"

#' Transects Missing Depth
#'
#' A dataset of transects where depth information is unavailable, potentially impacting hydraulic model accuracy.
#' @source Derived from the transect processing pipeline.
"transects_missing_depth"

utils::globalVariables(
  c(
    ".data",
    "cs_area",
    "cs_pts_lengthm" ,
    "distance_to_extend",
    "extend_transects",
    "extended_length" ,
    "id",
    "initial_length",
    "initial_order",
    "is_complete_cs",
    'is_improved',
    'is_missing_depth',
    'length_check',
    'max_bottom',
    'max_rel_dist',
    'min_bottom',
    'new_cs_length',
    'new_id',
    'new_pt_id',
    'polygon_id',
    'score1',
    'score2',
    'start_crs2'
  )
) 

default_dem <- "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt"