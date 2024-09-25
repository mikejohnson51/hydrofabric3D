library(testthat)
library(dplyr)
library(sf)
# library(hydrofabric3D)
# devtools::load_all()
# -------------------------------------------------------------------
# ---- hydrofabric3D:::validate_cut_cross_section_inputs() ----
# -------------------------------------------------------------------
# create test data (hy_id = "wb-1004970" from nextgen flowlines)
coords <- matrix(c(968520.8, 1381795, 968471.3, 1381851, 968420.6, 1381874, 
                   968418.1, 1381897, 968436.2, 1381961, 968426.9, 1382022, 
                   968412.6, 1382036,  968211.2, 1382114, 968197.2, 1382148, 
                   968172.4, 1382166,  968029.8, 1382217, 967972.7, 1382319, 
                   967936.7, 1382369,  967835.1, 1382461, 967831.7, 1382514, 
                   967836.6, 1382538, 967764.9, 1382589,  967741.8, 1382615, 
                   967695.0, 1382625, 967639.9, 1382619,  967108.0, 1382436, 
                   967072.6, 1382434,  967038.1, 1382448,  966982.6, 1382491, 
                   966947.4, 1382534,  966945.7, 1382549, 966932.3, 1382555, 
                   966886.3, 1382694,  966876.6, 1382781,  966930.3, 1382957, 
                   966926.8, 1382988,  966873.1, 1383015, 966851.8, 1383046, 
                   966807.0, 1383062, 966779.4, 1383172), 
                 ncol = 2, byrow = TRUE)

# create linestring and Sf dataframe
linestring_geom <- sf::st_linestring(as.matrix(coords))
net <- sf::st_as_sf(
  data.frame(hy_id = "wb-1004970", 
             tot_drainage_areasqkm = 3.90825,
             geom = sf::st_geometry(linestring_geom)),
  crs = 5070
)

# Test Cases
testthat::test_that("validate_cut_cross_section_inputs correctly validates inputs", {

   # Test valid inputs
   testthat::expect_equal(validate_cut_cross_section_inputs(net = net, 
                                                              id = "hy_id",
                                                              cs_widths = 100,
                                                              num = 10, 
                                                              smooth = TRUE,
                                                              densify = 2, 
                                                              rm_self_intersect = TRUE, 
                                                              fix_braids = FALSE, 
                                                              # terminal_id =NULL, 
                                                              braid_threshold = NULL, 
                                                              # version = 2, 
                                                              braid_method = "comid", 
                                                              precision = 1, 
                                                              add = FALSE), 
                            NULL)
  
  # Test valid inputs
  testthat::expect_error(alidate_cut_cross_section_inputs(net = net, 
                                                           id = "anotherID",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id =NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE)
                         )
  
  # Test valid inputs with terminal_id
  testthat::expect_equal(validate_cut_cross_section_inputs(net = net, 
                                                           id = "hy_id",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id ="terminal_id", 
                                                           braid_threshold = NULL, 
                                                           # # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE), 
                         NULL)
  
  # Test valid inputs with braid_threshold
  testthat::expect_equal(validate_cut_cross_section_inputs(net = net, 
                                                           id = "hy_id",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id =NULL, 
                                                           braid_threshold = 10, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE), 
                         NULL)
  
  # Test valid inputs with add
  testthat::expect_equal(validate_cut_cross_section_inputs(net = net, 
                                                           id = "hy_id",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id =NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = TRUE), 
                         NULL)
  
   # Test invalid sf object
   testthat::expect_error(validate_cut_cross_section_inputs(net = data.frame(), 
                                                                id = "hy_id",
                                                               cs_widths = 100,
                                                               num = 10, 
                                                               smooth = TRUE,
                                                               densify = 2, 
                                                               rm_self_intersect = TRUE, 
                                                               fix_braids = FALSE, 
                                                               # terminal_id =NULL, 
                                                               braid_threshold = NULL, 
                                                               # version = 2, 
                                                               braid_method = "comid", 
                                                               precision = 1, 
                                                               add = FALSE)
                          )
   
   # Test invalid id type (id is a numeric)
  testthat::expect_error(
    validate_cut_cross_section_inputs(net = net, 
                                       id = 2,
                                       cs_widths = 100,
                                       num = 10, 
                                       smooth = TRUE,
                                       densify = 2, 
                                       rm_self_intersect = TRUE, 
                                       fix_braids = FALSE, 
                                       # terminal_id =NULL, 
                                       braid_threshold = NULL, 
                                       # version = 2, 
                                       braid_method = "comid", 
                                       precision = 1, 
                                       add = FALSE)
                         )
  
  # Test invalid id type (id is NULL)
  testthat::expect_equal(validate_cut_cross_section_inputs(net = net, 
                                                           id = NULL,
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id =NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE),
                         NULL
                         )
  
  # Test invalid id type (id is a numeric)
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = 32324,
                                                            cs_widths = 100,
                                                            num = 10, 
                                                            smooth = TRUE,
                                                            densify = 2, 
                                                            rm_self_intersect = TRUE, 
                                                            fix_braids = FALSE, 
                                                            # terminal_id =NULL, 
                                                            braid_threshold = NULL, 
                                                            # version = 2, 
                                                            braid_method = "comid", 
                                                            precision = 1, 
                                                            add = FALSE)
                         )
  
  # Test invalid num type
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                           cs_widths = 100,
                                                           num = "10", 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id = NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE)
                         )
  
  # Test invalid densify type
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                           id = "hy_id",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = "2", 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id = NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE)
                         )
  
  # Test invalid smooth type
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hy_id",
                                                            cs_widths = 100,
                                                            num = 10, 
                                                            smooth = "453",
                                                            densify = 2, 
                                                            rm_self_intersect = TRUE, 
                                                            fix_braids = FALSE, 
                                                            # terminal_id = NULL, 
                                                            braid_threshold = NULL, 
                                                            # version = 2, 
                                                            braid_method = "comid", 
                                                            precision = 1, 
                                                            add = FALSE)
                         )
  
  # Test invalid rm_self_intersect type 1
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = "TRUE", 
                                                           fix_braids = FALSE, 
                                                           # terminal_id = NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE))
  
  # Test invalid rm_self_intersect type 2
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                            cs_widths = 100,
                                                            num = 10, 
                                                            smooth = TRUE,
                                                            densify = 2, 
                                                            rm_self_intersect = "bad input", 
                                                            fix_braids = FALSE, 
                                                            # terminal_id = NULL, 
                                                            braid_threshold = NULL, 
                                                            # version = 2, 
                                                            braid_method = "comid", 
                                                            precision = 1, 
                                                            add = FALSE))
  
  # Test invalid fix_braids type
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = "FALSE", 
                                                           # terminal_id = NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = FALSE))
  
  # Test invalid terminal ID value (terminal ID is a number)
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                            cs_widths = 100,
                                                            num = 10, 
                                                            smooth = TRUE,
                                                            densify = 2, 
                                                            rm_self_intersect = TRUE, 
                                                            fix_braids = FALSE, 
                                                            # terminal_id = 12, 
                                                            braid_threshold = NULL, 
                                                            # version = 2, 
                                                            braid_method = "comid", 
                                                            precision = 1, 
                                                            add = FALSE)
                         )
  
  # Test invalid terminal ID value (terminal ID is a logical)
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                            cs_widths = 100,
                                                            num = 10, 
                                                            smooth = TRUE,
                                                            densify = 2, 
                                                            rm_self_intersect = TRUE, 
                                                            fix_braids = FALSE, 
                                                            # terminal_id = TRUE, 
                                                            braid_threshold = NULL, 
                                                            # version = 2, 
                                                            braid_method = "comid", 
                                                            precision = 1, 
                                                            add = FALSE)
  )
  
  # Test invalid braid_method value
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id = NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "invalid_method", 
                                                           precision = 1, 
                                                           add = FALSE))
  
  # Test invalid precision value
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id = NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = -1, 
                                                           add = FALSE))
  
  # Test invalid add type
  testthat::expect_error(validate_cut_cross_section_inputs(net = net, 
                                                            id = "hyid",
                                                           cs_widths = 100,
                                                           num = 10, 
                                                           smooth = TRUE,
                                                           densify = 2, 
                                                           rm_self_intersect = TRUE, 
                                                           fix_braids = FALSE, 
                                                           # terminal_id = NULL, 
                                                           braid_threshold = NULL, 
                                                           # version = 2, 
                                                           braid_method = "comid", 
                                                           precision = 1, 
                                                           add = "FALSE"
                                                           ))
   
})
  
