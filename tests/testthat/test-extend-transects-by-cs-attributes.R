library(testthat)
library(dplyr)
library(sf)
# # library(hydrofabric3D)

source("testing_utils.R")
# source("tests/testthat/testing_utils.R")
# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric3D::extend_transects_by_cs_attributes() ----
# -------------------------------------------------------------------
testthat::test_that("extend_transects_by_cs_attributes() transects with 
                    valid_banks=FALSE 
                    AND 
                    has_relief=FALSE 
                    -> extend ALL transects", {

CROSSWALK_ID       <- "id"
LAT        <- 34.41249
LON        <- -119.74095
CRS_OF_PT  <- 4326
EXT_DIST <- 150

flowlines <- 
  create_v_line(lat = LAT, 
                lon = LON, 
                crs = CRS_OF_PT
  ) %>% 
  dplyr::mutate(
    id = 1:dplyr::n()
  ) %>% 
  sf::st_transform(5070)

transects <- cut_cross_sections(
  net = flowlines,
  crosswalk_id  = CROSSWALK_ID,  
  num = 20
) %>% 
  dplyr::filter(
    cs_id %in% c(10, 11, 12)
    
  ) %>%
  dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 

# Add valid_banks and has_relief columns
transects <- 
  transects %>% 
  dplyr::mutate(
    valid_banks = FALSE,
    has_relief  = FALSE
  )

# plot(flowlines$geometry)
# plot(transects$geometry, col = "green", lwd = 2, add = T)

extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
  transects      = transects,
  flowlines      = flowlines,
  crosswalk_id   = CROSSWALK_ID,
  scale          = 0.1,
  keep_lengths   = FALSE,
  reindex_cs_ids = FALSE
)

# plot(flowlines$geometry)
# plot(transects$geometry, col = "green", lwd = 5, add = T)
# plot(extended$geometry, col = "red", lwd = 2, add = T)

# check to make sure cross sections that SHOULD be extended, are in fact extended
cs_check = 
  transects %>% 
  sf::st_drop_geometry() %>% 
  dplyr::mutate(
    should_be_extended = !valid_banks | !has_relief
  ) %>% 
  dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, cs_lengthm, should_be_extended) %>% 
  dplyr::left_join(
    extended %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, new_cs_lengthm = cs_lengthm),
    by = c(CROSSWALK_ID, "cs_id")
  ) %>% 
  dplyr::mutate(
    cs_was_correctly_extended = cs_lengthm != new_cs_lengthm
  )

# make sure all CS were extended if they had invalid CS attributes 
testthat::expect_true(
  cs_check %>% 
    dplyr::filter(should_be_extended) %>% 
    dplyr::pull(cs_was_correctly_extended) %>% 
    all()
)

# there should be ZERO ignored cross sections in this test because they were ALL invalid CS attributes
number_of_not_extended <- 
  cs_check %>% 
  dplyr::filter(!should_be_extended) %>% 
  nrow()
  
testthat::expect_true(number_of_not_extended == 0)

})

testthat::test_that("extend_transects_by_cs_attributes() transects with
                    valid_banks=FALSE 
                    has_relief=TRUE 
                    -> extend all transects", {
  
  CROSSWALK_ID       <- "id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 150
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 20
  ) %>% 
    dplyr::filter(
      cs_id %in% c(10, 11, 12)
      
    ) %>%
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 
  
  # Add valid_banks and has_relief columns
  transects <- 
    transects %>% 
    dplyr::mutate(
      valid_banks = FALSE,
      has_relief  = TRUE
    )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
    transects      = transects,
    flowlines      = flowlines,
    crosswalk_id   = CROSSWALK_ID,
    scale          = 0.1,
    keep_lengths   = FALSE,
    reindex_cs_ids = FALSE
  )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 5, add = T)
  # plot(extended$geometry, col = "red", lwd = 2, add = T)
  
  # check to make sure cross sections that SHOULD be extended, are in fact extended
  cs_check = 
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(
      should_be_extended = !valid_banks | !has_relief
    ) %>% 
    dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, cs_lengthm, should_be_extended) %>% 
    dplyr::left_join(
      extended %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, new_cs_lengthm = cs_lengthm),
      by = c(CROSSWALK_ID, "cs_id")
    ) %>% 
    dplyr::mutate(
      cs_was_correctly_extended = cs_lengthm != new_cs_lengthm
    )
  
  # make sure all CS were extended if they had invalid CS attributes 
  testthat::expect_true(
    cs_check %>% 
      dplyr::filter(should_be_extended) %>% 
      dplyr::pull(cs_was_correctly_extended) %>% 
      all()
  )
  
  # there should be ZERO ignored cross sections in this test because they were ALL invalid CS attributes
  number_of_not_extended <- 
    cs_check %>% 
    dplyr::filter(!should_be_extended) %>% 
    nrow()
  
  testthat::expect_true(number_of_not_extended == 0)
  
})

testthat::test_that("extend_transects_by_cs_attributes() transects with 
                    valid_banks=TRUE 
                    has_relief=FALSE 
                    -> extend all transects", {
  
  CROSSWALK_ID       <- "id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 150
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 20
  ) %>% 
    dplyr::filter(
      cs_id %in% c(10, 11, 12)
      
    ) %>%
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 
  
  # Add valid_banks and has_relief columns
  transects <- 
    transects %>% 
    dplyr::mutate(
      valid_banks = TRUE,
      has_relief  = FALSE
    )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
    transects      = transects,
    flowlines      = flowlines,
    crosswalk_id   = CROSSWALK_ID,
    scale          = 0.1,
    keep_lengths   = FALSE,
    reindex_cs_ids = FALSE
  )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 5, add = T)
  # plot(extended$geometry, col = "red", lwd = 2, add = T)
  
  # check to make sure cross sections that SHOULD be extended, are in fact extended
  cs_check = 
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(
      should_be_extended = !valid_banks | !has_relief
    ) %>% 
    dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, cs_lengthm, should_be_extended) %>% 
    dplyr::left_join(
      extended %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, new_cs_lengthm = cs_lengthm),
      by = c(CROSSWALK_ID, "cs_id")
    ) %>% 
    dplyr::mutate(
      cs_was_correctly_extended = cs_lengthm != new_cs_lengthm
    )
  
  # make sure all CS were extended if they had invalid CS attributes 
  testthat::expect_true(
    cs_check %>% 
      dplyr::filter(should_be_extended) %>% 
      dplyr::pull(cs_was_correctly_extended) %>% 
      all()
  )
  
  # there should be ZERO ignored cross sections in this test because they were ALL invalid CS attributes
  number_of_not_extended <- 
    cs_check %>% 
    dplyr::filter(!should_be_extended) %>% 
    nrow()
  
  testthat::expect_true(number_of_not_extended == 0)
  
})

testthat::test_that("extend_transects_by_cs_attributes() transects with ZERO invalid CS attributes
                    valid_banks=TRUE 
                    AND 
                    has_relief=TRUE
                    -> will do NO extensions on the transects", {
  
  CROSSWALK_ID       <- "id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 150
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 20
  ) %>% 
    dplyr::filter(
      cs_id %in% c(10, 11, 12)
      
    ) %>%
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 
  
  # Add valid_banks and has_relief columns
  transects <- 
    transects %>% 
    dplyr::mutate(
      valid_banks = TRUE,
      has_relief  = TRUE
    )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 2, add = T)
  
  extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
    transects      = transects,
    flowlines      = flowlines,
    crosswalk_id   = CROSSWALK_ID,
    scale          = 0.5,
    keep_lengths   = FALSE,
    reindex_cs_ids = FALSE
  )
  
  # plot(flowlines$geometry)
  # plot(transects$geometry, col = "green", lwd = 5, add = T)
  # plot(extended$geometry, col = "red", lwd = 2, add = T)

  # check to make sure cross sections that SHOULD be extended, are in fact extended
  cs_check = 
    transects %>% 
    sf::st_drop_geometry() %>% 
    dplyr::mutate(
      should_be_extended = !valid_banks | !has_relief
    ) %>% 
    dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, cs_lengthm, should_be_extended) %>% 
    dplyr::left_join(
      extended %>% 
        sf::st_drop_geometry() %>% 
        dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, new_cs_lengthm = cs_lengthm),
      by = c(CROSSWALK_ID, "cs_id")
    )  
    # dplyr::mutate(
    #   cs_was_correctly_extended = dplyr::between(new_cs_lengthm, cs_lengthm + 0.00001, cs_lengthm - 0.00001)
    # )
  
  # cross section lengths should remain nearly identical (i.e. NO EXTENSIONS happened)
  testthat::expect_equal(cs_check$cs_lengthm, cs_check$new_cs_lengthm)
  
})

testthat::test_that("extend_transects_by_cs_attributes() transects with 
                    valid_banks=FALSE 
                    AND 
                    has_relief=FALSE 
                    -> extend ALL transects but does NOT extend those that intersect with another transect line", {
                      
CROSSWALK_ID       <- "id"
LAT        <- 34.41249
LON        <- -119.74095
CRS_OF_PT  <- 4326
EXT_DIST <- 150

flowlines <- 
  create_v_line(lat = LAT, 
                lon = LON, 
                crs = CRS_OF_PT
  ) %>% 
  dplyr::mutate(
    id = 1:dplyr::n()
  ) %>% 
  sf::st_transform(5070)

transects <- cut_cross_sections(
  net = flowlines,
  crosswalk_id  = CROSSWALK_ID,  
  num = 20
) %>% 
  dplyr::filter(
    cs_id %in% c(4, 10, 11, 12)
  ) %>%
  dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) %>% 
  dplyr::mutate(
    cs_id = 1:dplyr::n()
  )


# Add valid_banks and has_relief columns
# first transect is NOT going to get extended, but the other 3 should try to be extended
# the last 2 are able to extend, but not cs_id = 2 because it would hit the cs_id = 1
transects$valid_banks <- c(TRUE, FALSE, FALSE, FALSE)
transects$has_relief  <- c(TRUE, FALSE, FALSE, FALSE)

# plot(flowlines$geometry)
# plot(transects$geometry, col = "green", lwd = 2, add = T)

extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
  transects      = transects,
  flowlines      = flowlines,
  crosswalk_id   = CROSSWALK_ID,
  scale          = 2,
  keep_lengths   = FALSE,
  reindex_cs_ids = FALSE
)

# plot(flowlines$geometry)
# plot(transects$geometry, col = "green", lwd = 5, add = T)
# plot(extended$geometry, col = "red", lwd = 2, add = T)

# check to make sure cross sections that SHOULD be extended, are in fact extended
cs_check = 
  transects %>% 
  sf::st_drop_geometry() %>% 
  dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, cs_lengthm) %>% 
  dplyr::left_join(
    extended %>% 
      sf::st_drop_geometry() %>% 
      dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, new_cs_lengthm = cs_lengthm),
    by = c(CROSSWALK_ID, "cs_id")
  ) 

# cross section lengths should remain nearly identical (i.e. NO EXTENSIONS happened) for the cross section that would interesect another transect if extended
testthat::expect_equal(
  cs_check %>% 
    dplyr::filter(cs_id %in% c(2)) %>% 
    dplyr::pull(cs_lengthm),
  cs_check %>% 
    dplyr::filter(cs_id %in% c(2)) %>% 
    dplyr::pull(new_cs_lengthm)
  )

# make sure the other cross sections eligible for extensions were extended
testthat::expect_true(
  cs_check %>% 
    dplyr::filter(cs_id %in% c(3, 4)) %>% 
    dplyr::mutate(
      cs_was_correctly_extended = cs_lengthm != new_cs_lengthm
    ) %>% 
    dplyr::pull(cs_was_correctly_extended) %>% 
    all()
)

# make sure the NOT eligible for extension cross section is NOT extended
testthat::expect_equal(
  cs_check %>% 
    dplyr::filter(cs_id %in% c(1)) %>% 
    dplyr::pull(cs_lengthm),
  cs_check %>% 
    dplyr::filter(cs_id %in% c(1)) %>% 
    dplyr::pull(new_cs_lengthm)
  )

})

testthat::test_that("extend_transects_by_cs_attributes() error when missing valid_banks or has_relief columns", {

CROSSWALK_ID       <- "id"
LAT        <- 34.41249
LON        <- -119.74095
CRS_OF_PT  <- 4326
EXT_DIST <- 150

flowlines <- 
  create_v_line(lat = LAT, 
                lon = LON, 
                crs = CRS_OF_PT
  ) %>% 
  dplyr::mutate(
    id = 1:dplyr::n()
  ) %>% 
  sf::st_transform(5070)

transects <- cut_cross_sections(
  net = flowlines,
  crosswalk_id  = CROSSWALK_ID,  
  num = 20
) %>% 
  dplyr::filter(
    cs_id %in% c(4, 10, 11, 12)
  ) %>%
  dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) %>% 
  dplyr::mutate(
    cs_id = 1:dplyr::n()
  )


# plot(flowlines$geometry)
# plot(transects$geometry, col = "green", lwd = 2, add = T)

testthat::expect_error(
  hydrofabric3D:::extend_transects_by_cs_attributes(
    transects      = transects,
    flowlines      = flowlines,
    crosswalk_id   = CROSSWALK_ID,
    scale          = 2,
    keep_lengths   = FALSE,
    reindex_cs_ids = FALSE
  )
)


# plot(flowlines$geometry)
# plot(transects$geometry, col = "green", lwd = 5, add = T)
# plot(extended$geometry, col = "red", lwd = 2, add = T)

})


testthat::test_that("extend_transects_by_cs_attributes() keep_lengths = TRUE returns an 'initial_length' column which 
                    stores the initial length before extension", {
  
  CROSSWALK_ID       <- "id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 150
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 5
  )
    
    # Add valid_banks and has_relief columns
  transects <- 
    transects %>% 
    dplyr::mutate(
      valid_banks = F,
      has_relief  = F
    )
  
  
  # stash the original lengths
  starting_lengths <- transects$cs_lengthm
  
  extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
                transects      = transects,
                flowlines      = flowlines,
                crosswalk_id   = CROSSWALK_ID,
                scale          = 0.1,
                keep_lengths   = TRUE,
                reindex_cs_ids = FALSE
              )
  
  # make sure the initial_length column is returned
  testthat::expect_true(
    "initial_length" %in% names(extended)
  )
  
  # make sure the initial_length equals the original starting length values
  testthat::expect_equal(starting_lengths, extended$initial_length)
  
})

testthat::test_that("extend_transects_by_cs_attributes() keep_lengths = TRUE returns an the correct initial length values 
                    of the transects before extensions", {
                      
CROSSWALK_ID       <- "id"
LAT        <- 34.41249
LON        <- -119.74095
CRS_OF_PT  <- 4326
EXT_DIST <- 150

flowlines <- 
  create_v_line(lat = LAT, 
                lon = LON, 
                crs = CRS_OF_PT
  ) %>% 
  dplyr::mutate(
    id = 1:dplyr::n()
  ) %>% 
  sf::st_transform(5070)

transects <- cut_cross_sections(
  net = flowlines,
  crosswalk_id  = CROSSWALK_ID,  
  num = 5
)

# Add valid_banks and has_relief columns
transects <- 
  transects %>% 
  dplyr::mutate(
    valid_banks = FALSE,
    has_relief  = FALSE
  )


# stash the original lengths
starting_lengths <- transects$cs_lengthm

extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
  transects      = transects,
  flowlines      = flowlines,
  crosswalk_id   = CROSSWALK_ID,
  scale          = 0.1,
  keep_lengths   = TRUE,
  reindex_cs_ids = FALSE
)

# make sure the initial_length equals the original starting length values
testthat::expect_equal(starting_lengths, extended$initial_length)

})

testthat::test_that("extend_transects_by_cs_attributes() make sure if an transect gets removed due
to intersections, the cs_ids remain unchanged when reindex_cs_ids = FALSE", {
  
  CROSSWALK_ID       <- "id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 150
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects1 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 5
  ) %>% 
    dplyr::filter(
      cs_id %in% c(3)
    ) %>%
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 
   
  transects2 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 20
  ) %>% 
    dplyr::filter(
      cs_id %in% c(6, 7, 8, 9, 10)
    ) %>%
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 
   
  # plot(flowlines$geometry)
  # plot(transects1$geometry, col = "red", lwd = 5, add = T) 
  # plot(transects2$geometry, col = "green", lwd = 2, add = T) 

  merged_transects <- dplyr::bind_rows(
                          transects1,
                          transects2
                        ) %>% 
                          dplyr::arrange(cs_measure) %>% 
                          dplyr::mutate(
                            cs_id = 1:dplyr::n()
                          )

  # Add valid_banks and has_relief columns
  merged_transects <- 
    merged_transects %>% 
    dplyr::mutate(
      valid_banks = FALSE,
      has_relief  = FALSE
    )
  
  # plot(flowlines$geometry)
  # plot(merged_transects$geometry, col = "green", lwd = 2, add = T)
  
  extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
    transects      = merged_transects,
    flowlines      = flowlines,
    crosswalk_id   = CROSSWALK_ID,
    scale          = 0.1,
    keep_lengths   = FALSE,
    reindex_cs_ids = F
  )
  
  # plot(flowlines$geometry)
  # plot(merged_transects$geometry, col = "green", lwd = 5, add = T)
  # plot(extended$geometry, col = "red", lwd = 2, add = T)
  
  # cs_id = 5 was removed due to intersect
  testthat::expect_equal(
    extended$cs_id,
    c(1, 2, 3, 4, 6)
  )
  
})

testthat::test_that("extend_transects_by_cs_attributes() make sure if an transect gets removed due
to intersections, the cs_ids get correctly renumbered when reindex_cs_ids = TRUE", {
  
  CROSSWALK_ID       <- "id"
  LAT        <- 34.41249
  LON        <- -119.74095
  CRS_OF_PT  <- 4326
  EXT_DIST <- 150
  
  flowlines <- 
    create_v_line(lat = LAT, 
                  lon = LON, 
                  crs = CRS_OF_PT
    ) %>% 
    dplyr::mutate(
      id = 1:dplyr::n()
    ) %>% 
    sf::st_transform(5070)
  
  transects1 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 5
  ) %>% 
    dplyr::filter(
      cs_id %in% c(3)
    ) %>%
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 
  
  transects2 <- cut_cross_sections(
    net = flowlines,
    crosswalk_id  = CROSSWALK_ID,  
    num = 20
  ) %>% 
    dplyr::filter(
      cs_id %in% c(6, 7, 8, 9, 10)
    ) %>%
    dplyr::select(dplyr::any_of(c(CROSSWALK_ID, "cs_id" , "cs_measure", "cs_lengthm"))) 
  
  # plot(flowlines$geometry)
  # plot(transects1$geometry, col = "red", lwd = 5, add = T) 
  # plot(transects2$geometry, col = "green", lwd = 2, add = T) 
  
  merged_transects <- dplyr::bind_rows(
    transects1,
    transects2
  ) %>% 
    dplyr::arrange(cs_measure) %>% 
    dplyr::mutate(
      cs_id = 1:dplyr::n()
    )
  
  # Add valid_banks and has_relief columns
  merged_transects <- 
    merged_transects %>% 
    dplyr::mutate(
      valid_banks = FALSE,
      has_relief  = FALSE
    )
  
  # plot(flowlines$geometry)
  # plot(merged_transects$geometry, col = "green", lwd = 2, add = T)
  
  extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
    transects      = merged_transects,
    flowlines      = flowlines,
    crosswalk_id   = CROSSWALK_ID,
    scale          = 0.1,
    keep_lengths   = FALSE,
    reindex_cs_ids = TRUE
  )
  
  # plot(flowlines$geometry)
  # plot(merged_transects$geometry, col = "green", lwd = 5, add = T)
  # plot(extended$geometry, col = "red", lwd = 2, add = T)
  
  # cs_id = 5 was removed due to intersect but then the cs_ids get correctly renumbered 
  testthat::expect_equal(
    extended$cs_id,
    c(1, 2, 3, 4, 5)
  )
  
})

testthat::test_that("trans -> cs_pts -> extend trans -> new cs_pts -> compare diff -> updated trans + cs_pts", {
  
  # flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  # 
  # # Transects inputs
  # CS_SOURCE            <- 'hydrofabric3D_test'
  # NUM_OF_TRANSECTS     <- 5
  # RM_SELF_INTERSECTS   <- TRUE
  # ID_COL               <- "hy_id"
  # CROSSWALK_ID <- "hy_id"
  # 
  # # Cross section point inputs
  # DEM_PATH       <- testthat::test_path("testdata", "dem_flowlines.tif")
  # POINTS_PER_CS     <- NULL
  # MIN_PTS_PER_CS    <- 10
  # 
  # # NOTE: 0.01 (1%) of cros sections length is the default
  # # percent of cross section length (cs_lengthm) to use as the
  # # threshold depth for classifying whether a cross section has "relief"
  # PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF <- 0.05
  # 
  # EXTENSION_PCT = 0.5
  # 
  # flowlines <-
  #   flowlines %>%
  #   hydrofabric3D::add_powerlaw_bankful_width("tot_drainage_areasqkm", 50) %>% 
  #   dplyr::mutate(
  #     bf_width = bf_width * 2
  #     # bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
  #     # bf_width        = pmax(50, bf_width * 11)
  #   ) %>%
  #   dplyr::select(
  #     hy_id = id,
  #     # tot_drainage_areasqkm,
  #     bf_width,
  #     # input_bf_width,
  #     geometry = geom
  #   )
  # # dplyr::slice(1)
  # 
  # # plot(flowlines$geometry)
  # 
  # transects <- hydrofabric3D::cut_cross_sections(
  #   net               = flowlines,
  #   crosswalk_id      = ID_COL,
  #   cs_widths         = flowlines$bf_width,
  #   num               = 20,
  #   rm_self_intersect = TRUE
  # ) %>%
  #   # dplyr::mutate(
  #   #   cs_source = CS_SOURCE
  #   # ) %>%
  #   dplyr::select(
  #     hy_id,
  #     cs_id,
  #     cs_lengthm,
  #     # cs_source,
  #     cs_measure,
  #     geometry
  #   )
  # 
  # mapview::mapview(transects, color = "green") +
  #   mapview::mapview(flowlines, color = "red")
  # 
  # # ----------------------------------------------------------------------------------------------------------------
  # # ---- Cross section points ----
  # # ----------------------------------------------------------------------------------------------------------------
  # 
  # # ---- STEP 1: Extract cs points from DEM ----
  # 
  # # get cross section point elevations
  # cs_pts <- hydrofabric3D::cross_section_pts(
  #   cs             = transects,
  #   crosswalk_id   = CROSSWALK_ID,
  #   points_per_cs  = POINTS_PER_CS,
  #   min_pts_per_cs = MIN_PTS_PER_CS,
  #   dem            = DEM_PATH
  # )
  # 
  # # ----------------------------------------------------------------------------------------------------------------
  # # ---- STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points ----
  # # ----------------------------------------------------------------------------------------------------------------
  # 
  # # system.time({
  # # NA_Z_COUNT <- sum(is.na(cs_pts$Z))
  # # cs_pts[!is.na(cs_pts$Z), ][[ID_COL]]
  # 
  # # STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points
  # cs_pts <-
  #   cs_pts %>%
  #   hydrofabric3D::drop_incomplete_cs_pts(CROSSWALK_ID) %>%
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = CROSSWALK_ID,
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   ) %>% 
  #   hydrofabric3D::add_tmp_id(CROSSWALK_ID) %>% 
  #   hydrofabric3D:::pts_to_XY() %>% 
  #   dplyr::mutate(
  #     Z_source = "test_source1"
  #   ) 
  # # %>% 
  # # hydrofabric3D:::get_validity_tally()
  # 
  # cs_pts %>%
  #   sf::st_drop_geometry() %>%
  #   dplyr::group_by(hy_id, cs_id) %>%
  #   dplyr::slice(1) %>%
  #   dplyr::ungroup() %>%
  #   dplyr::select(hy_id, cs_id, valid_banks, has_relief)
  # 
  # hydrofabric3D:::get_validity_tally(cs_pts)
  # 
  # transects <- hydrofabric3D:::add_cs_attributes_to_transects(
  #                       transects = transects,
  #                       cs_pts = cs_pts,
  #                       crosswalk_id = CROSSWALK_ID
  #                     )
  # 
  # extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
  #                                                   transects = transects,
  #                                                   flowlines = flowlines,
  #                                                   crosswalk_id = CROSSWALK_ID,
  #                                                   scale = 0.5,
  #                                                   keep_lengths = TRUE,
  #                                                   reindex_cs_ids = FALSE
  #                                                   )
  # 
  # flagged_extended <- 
  #   extended %>% 
  #   flag_transects_for_change(    
  #                     crosswalk_id   = CROSSWALK_ID,
  #                     points_per_cs  = POINTS_PER_CS,
  #                     min_pts_per_cs = MIN_PTS_PER_CS,
  #                     dem            = DEM_PATH,
  #                     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #                   )
  # 
  # 
  # 
  # flag_adjusts             <- adjust_flagged_transects(flagged_extended, crosswalk_id = CROSSWALK_ID, reindex_cs_ids = FALSE)
  # flag_adjusts_reindexed   <- adjust_flagged_transects(flagged_extended, crosswalk_id = CROSSWALK_ID, reindex_cs_ids = TRUE)
  # 
  # mapview::mapview(transects, color = "green") +
  #   mapview::mapview(extended, color = "green") +
  #   # mapview::mapview(extended, color = "gold") +
  #   mapview::mapview(flag_adjusts, color = "red") +
  #   mapview::mapview(flowlines, color = "dodgerblue")
  # 
  # 
  # updated_extensions <- update_cs_attr_based_transects(
  #   x = extended, 
  #   crosswalk_id = CROSSWALK_ID,
  #   points_per_cs  = NULL,
  #   min_pts_per_cs = 10,
  #   dem            = "/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt",
  #   pct_of_length_for_relief = 0.01,
  #   reindex_cs_ids = TRUE
  # )
  # 
  # # final_cs_pts %>% 
  # #   dplyr::filter(hy_id == "wb-1003258",cs_id == 18)
  # 
  # # get cross section point elevations
  # ext_cs_pts <- hydrofabric3D::cross_section_pts(
  #   cs             = extended,
  #   crosswalk_id   = CROSSWALK_ID,
  #   points_per_cs  = POINTS_PER_CS,
  #   min_pts_per_cs = MIN_PTS_PER_CS,
  #   dem            = DEM_PATH
  # ) %>%
  #   hydrofabric3D::drop_incomplete_cs_pts(CROSSWALK_ID) %>%
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = CROSSWALK_ID,
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   ) %>% 
  #   hydrofabric3D::add_tmp_id(CROSSWALK_ID) %>% 
  #   hydrofabric3D:::pts_to_XY() %>% 
  #   dplyr::mutate(
  #     Z_source = "test_source"
  #   )
  # 
  # # get cross section point elevations
  # final_cs_pts <- hydrofabric3D::cross_section_pts(
  #                                 cs             = flag_adjusts,
  #                                 crosswalk_id   = CROSSWALK_ID,
  #                                 points_per_cs  = POINTS_PER_CS,
  #                                 min_pts_per_cs = MIN_PTS_PER_CS,
  #                                 dem            = DEM_PATH
  #                               ) %>%
  #                                 hydrofabric3D::drop_incomplete_cs_pts(CROSSWALK_ID) %>%
  #                                 hydrofabric3D:::classify_points(
  #                                   crosswalk_id             = CROSSWALK_ID,
  #                                   pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #                                 ) %>% 
  #                                 hydrofabric3D::add_tmp_id(CROSSWALK_ID) %>% 
  #                                 hydrofabric3D:::pts_to_XY() %>% 
  #   dplyr::mutate(
  #     Z_source = "test_source"
  #   )
  # 
  # cs_pts %>% 
  #   dplyr::filter(
  #     hy_id == "wb-1003258",
  #     cs_id == 10
  #     ) %>% 
  #   dplyr::slice(1) %>% 
  #   dplyr::relocate(hy_id, cs_id, valid_banks, has_relief)
  # 
  # ext_cs_pts %>% 
  #   dplyr::filter(
  #     hy_id == "wb-1003258",
  #     cs_id == 10
  #   ) %>%  
  #   dplyr::slice(1) %>% 
  #   dplyr::relocate(hy_id, cs_id, valid_banks, has_relief)
  # 
  # final_cs_pts %>% 
  #   dplyr::filter(
  #     hy_id == "wb-1003258",
  #     cs_id == 10
  #   ) %>% 
  #   dplyr::slice(1) %>% 
  #   dplyr::relocate(hy_id, cs_id, valid_banks, has_relief)
  # 
  # 
  # hydrofabric3D:::validate_transects(transects, "hy_id")
  # 
  # hydrofabric3D:::validate_cs_pts(cs_pts, "hy_id")
  # hydrofabric3D:::validate_cs_pts_against_transects(cs_pts, transects, "hy_id")
  # 
  # hydrofabric3D:::validate_cs_pts(final_cs_pts, "hy_id")
  # hydrofabric3D:::validate_cs_pts_against_transects(final_cs_pts, updated_extensions, "hy_id")
  # 
  # hydrofabric3D:::validate_cs_pts_against_transects(final_cs_pts, updated_extensions, "hy_id")
  # 
  # hydrofabric3D:::get_validity_tally(final_cs_pts)
  # hydrofabric3D:::get_validity_tally(cs_pts)
  # 
  # shorten_flagged_transects <- function(transects, crosswalk_id = NULL) {
  #   # x <-
  #   #   extended_transects[is_multi_intersecting, ] %>% 
  #   #   dplyr::left_join(
  #   #     starting_lengths,
  #   #     by = c(crosswalk_id, "cs_id")
  #   #   ) %>% 
  #   #   # dplyr::relocate(initial_length, cs_lengthm)
  #   #   dplyr::mutate(
  #   #     distance_to_shorten = -((cs_lengthm - initial_length) / 2)
  #   #   )
  #   # # 
  #   
  #   # xt <- x
  #   
  #   
  #   
  #   # x$distance_to_shorten
  #   # x <- extended_transects
  #   # stage_trans <- 
  #   #   extended %>% 
  #   #   hydrofabric3D::add_tmp_id(CROSSWALK_ID) %>% 
  #   #   dplyr::left_join(
  #   #     cs_validities %>% 
  #   #       dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, is_improved),
  #   #     by = c(CROSSWALK_ID, "cs_id")
  #   #   ) %>% 
  #   #   dplyr::relocate(is_improved, cs_lengthm, initial_length)
  #   # 
  #   # shortened_trans <-
  #   #   xt %>% 
  #   #   dplyr::filter(flagged)
  #   
  #   suppressWarnings({
  #     
  #     is_valid_df <- validate_df(transects, 
  #                                c(crosswalk_id, "cs_id", "cs_lengthm", "flagged", "extension_distance", "geometry"), 
  #                                "transects")
  #     
  #     # wehich rows are flagged for shortening?  
  #     is_flagged      <- transects$flagged
  #     
  #     has_no_flags    <- !any(is_flagged)
  #     
  #     # return early if NO rows were flagged for shortening 
  #     if (has_no_flags) {
  #       return(transects)
  #     }
  #     
  #     # reduce the length of each transect by extension_distance (from BOTH sides)
  #     shortened_transects  <- extend_by_length(
  #       x             = transects[is_flagged, ], 
  #       crosswalk_id  = crosswalk_id, 
  #       length_vector = -transects[is_flagged, ]$extension_distance, 
  #       length_col    = "cs_lengthm"
  #     ) 
  #     
  #     
  #     # replace the geometries with the shorter transects
  #     sf::st_geometry(transects[is_flagged, ])  <- sf::st_geometry(shortened_transects)
  #     
  #     # update the lengths to align with the above replacement of geometries
  #     transects <- add_length_col(transects, "cs_lengthm") 
  #     
  #     return(transects)
  #     
  #   })
  # }
  # 
  # # get cross section point elevations
  # new_cs_pts <- hydrofabric3D::cross_section_pts(
  #   cs             = extended,
  #   crosswalk_id   = CROSSWALK_ID,
  #   points_per_cs  = POINTS_PER_CS,
  #   min_pts_per_cs = MIN_PTS_PER_CS,
  #   dem            = DEM_PATH
  # ) %>%
  #   hydrofabric3D::drop_incomplete_cs_pts(CROSSWALK_ID) %>%
  #   hydrofabric3D:::classify_points(
  #     crosswalk_id             = CROSSWALK_ID,
  #     pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
  #   ) %>% 
  #   hydrofabric3D::add_tmp_id(CROSSWALK_ID) 
  # 
  # hydrofabric3D:::get_validity_tally(cs_pts, "hy_id")
  # hydrofabric3D:::get_validity_tally(new_cs_pts, "hy_id")
  # 
  # cs_validities <- compare_cs_validity(cs_pts1  = cs_pts, 
  #                                            cs_pts2 = new_cs_pts, 
  #                                            crosswalk_id = CROSSWALK_ID)
  # 
  # crosswalk_id <- "hy_id"
  # cs_validities
  # 
  # # List of unique hy_id/cs_ids (tmp_id) that showed improvement after extension, if valid banks or relief was addded (or both),
  # # then the cross section "showed improvement", and the new values will be put into the output cross section dataset
  # extended_ids_to_keep <-
  #   cs_validities %>% 
  #   dplyr::filter(is_improved) %>% 
  #   get_unique_tmp_ids(x = CROSSWALK_ID)
  # 
  # improved_ids_to_keep <- 
  #   cs_validities %>% 
  #   dplyr::filter(is_improved) %>% 
  #   get_unique_tmp_ids(x = CROSSWALK_ID)
  # 
  # stage_trans <- 
  #   extended %>% 
  #   hydrofabric3D::add_tmp_id(CROSSWALK_ID) %>% 
  #   dplyr::left_join(
  #     cs_validities %>% 
  #       dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, is_improved),
  #     by = c(CROSSWALK_ID, "cs_id")
  #   ) %>% 
  #   dplyr::relocate(is_improved, cs_lengthm, initial_length)
  # 
  # shortened_trans <-
  #   stage_trans %>% 
  #   dplyr::filter(!is_improved, initial_length < cs_lengthm) %>% 
  #   dplyr::mutate(
  #     distance_to_shorten = -((cs_lengthm - initial_length) / 2)
  #   ) %>% 
  #   dplyr::relocate(is_improved, cs_lengthm, initial_length, distance_to_shorten) %>% 
  #   hydrofabric3D::extend_by_length(
  #     crosswalk_id  = CROSSWALK_ID,
  #     length_vector = .$distance_to_shorten,
  #     length_col    = "cs_lengthm"
  #   ) %>% 
  #   dplyr::mutate(
  #     is_extended = FALSE
  #   ) %>% 
  #   dplyr::select(
  #     -distance_to_shorten
  #   )
  # 
  # 
  # shortened_trans
  # 
  # stage_trans %>% 
  #   dplyr::filter(
  #     !tmp_id %in% unique(shortened_trans$tmp_id)
  #   ) %>% 
  #   # dplyr::filter(!is_improved, initial_length >= cs_lengthm) 
  #   dplyr::bind_rows(
  #     shortened_trans
  #   ) %>% 
  #   dplyr::group_by(dplyr::across(dplyr::any_of(c(CROSSWALK_ID, "cs_id")))) %>% 
  #   dplyr::arrange(-cs_measure, .by_group = TRUE) %>% 
  #   dplyr::ungroup()
  #   
  # # dplyr::filter(!is_improved, is_extended) 
  #   # dplyr::filter(
  #   #   !tmp_id %in% improved_ids_to_keep,
  #   #   is_extended
  #   #   )
  # 
  # to_undo_extension
  # 
  # shortened_trans <-
  #   hydrofabric3D::extend_by_length(
  #     x = to_undo_extension,
  #     crosswalk_id = CROSSWALK_ID,
  #     length_vector = to_undo_extension$length_change,
  #     length_col = "cs_lengthm"
  #   )
  #   
  # mapview::mapview(transects, color = "red") +
  #  mapview::mapview(extended, color = "green") +
  #   mapview::mapview(shortened_trans, color = "dodgerblue")
  #   
  #   
  #   
  #  
  # to_undo_extension 
  # 
  # 
  # # ids_to_add_to_good_set <- 
  # #   check_for_improvement %>% 
  # #   dplyr::filter(!improved) %>% 
  # #   get_unique_tmp_ids()
  # 
  # # add a tmp_id for joining and filtering 
  # extended_pts <- add_tmp_id(extended_pts, x = crosswalk_id)
  # 
  # # TODO: Left off here to add back and remove old data 03/05/2024
  # pts_to_keep <- dplyr::filter(extended_pts, 
  #                              tmp_id %in% extended_ids_to_keep)
  # # pts_to_keep <- dplyr::filter(extended_pts2, !tmp_id %in% ids_to_add_to_good_set)
  # # pts_to_move_to_good_set <- dplyr::filter(extended_pts2, tmp_id %in% ids_to_add_to_good_set)
  # 
  # # Reclassify the pts_to_keep so they can be added back to the remaining "good" cross section points from the input
  # pts_to_keep             <- classify_points(
  #   pts_to_keep,
  #   crosswalk_id = crosswalk_id,
  #   pct_of_length_for_relief = pct_of_length_for_relief
  # )
  # # pts_to_keep             <- hydrofabric3D::classify_points(pts_to_keep, pct_of_length_for_relief = pct_of_length_for_relief)
  # 
  # # pts_to_keep %>% 
  # #   dplyr::filter(is_extended)
  # 
  # # add is_extended logical if does not exist
  # if (!"is_extended" %in% names(pts_to_keep)) {
  #   pts_to_keep$is_extended = TRUE
  # }
  # 
  # # remove the IDs of newly updated cross section points from the original data, then 
  # # bind the new version of these points to the rest of the original data
  # final_pts <-
  #   cs_pts %>%  
  #   hydrofabric3D::add_tmp_id(x = crosswalk_id) %>% 
  #   dplyr::filter(
  #     !tmp_id %in% extended_ids_to_keep
  #   ) %>% 
  #   dplyr::mutate(
  #     is_extended = FALSE
  #   ) %>% 
  #   dplyr::bind_rows(
  #     hydrofabric3D::add_tmp_id(pts_to_keep, x = crosswalk_id)
  #   ) %>% 
  #   dplyr::select(-tmp_id) 
  # 
  # 
  # compare_cs_validity
  # 
  # 
  # 
  # 
  # compare_cs_validity <- function(start_cs_pts, 
  #                               end_cs_pts, 
  #                               crosswalk_id = NULL
  #                               ) {
  # 
  #   
  #   start_validity_scores <- 
  #     start_cs_pts %>% 
  #     hydrofabric3D:::calc_validity_scores(crosswalk_id) %>% 
  #     hydrofabric3D::add_tmp_id(crosswalk_id) %>% 
  #     dplyr::rename(start_score = validity_score)
  #   
  #   end_validity_scores <-
  #     end_cs_pts %>% 
  #     hydrofabric3D:::calc_validity_scores(crosswalk_id) %>% 
  #     hydrofabric3D::add_tmp_id(crosswalk_id) %>% 
  #     dplyr::rename(end_score = validity_score)
  #   
  #   # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  #   check_for_improvement <- dplyr::left_join(
  #                               # OLD SCORES
  #                               start_validity_scores %>%
  #                                     dplyr::filter(
  #                                       tmp_id %in% unique(end_validity_scores$tmp_id)
  #                                     ) %>% 
  #                                     dplyr::select(dplyr::any_of(crosswalk_id), cs_id, start_score),
  #                                   
  #                               # NEW SCORES
  #                               end_validity_scores %>% 
  #                                     dplyr::select(dplyr::any_of(crosswalk_id), cs_id, end_score),
  #                                   by = c(crosswalk_id, "cs_id")
  #                             ) %>% 
  #                         dplyr::mutate(
  #                           is_improved = dplyr::case_when(
  #                             end_score > start_score  ~ TRUE,
  #                             TRUE                     ~ FALSE
  #                           )
  #                         ) %>%
  #                         dplyr::select(dplyr::any_of(crosswalk_id), cs_id, 
  #                                       start_score, end_score, 
  #                                       is_improved
  #                                       )
  #   
  #   return(check_for_improvement)
  #   
  # }
  # 
  # hydrofabric3D:::get_validity_tally(cs_pts, "hy_id")
  # hydrofabric3D:::get_validity_tally(new_cs_pts, "hy_id")
  # 
  # old_validity_scores <- 
  #   cs_pts %>% 
  #   hydrofabric3D:::calc_validity_scores(CROSSWALK_ID) %>% 
  #   hydrofabric3D::add_tmp_id(CROSSWALK_ID) 
  # 
  # new_validity_scores <-
  #   new_cs_pts %>% 
  #   hydrofabric3D:::calc_validity_scores(CROSSWALK_ID) %>% 
  #   hydrofabric3D::add_tmp_id(CROSSWALK_ID) 
  # 
  # 
  # # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  # check_for_improvement <- dplyr::left_join(
  #                             # OLD SCORES
  #                             old_validity_scores %>%
  #                               hydrofabric3D::add_tmp_id("hy_id") %>%
  #                               dplyr::filter(
  #                                 tmp_id %in% unique(new_validity_scores$tmp_id)
  #                               ) %>% 
  #                               dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, old_validity_score = validity_score),
  #                             
  #                             # NEW SCORES
  #                             new_validity_scores %>% 
  #                               dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, new_validity_score = validity_score),
  #                             
  #                             by = c(CROSSWALK_ID, "cs_id")
  #                         ) %>% 
  #                         dplyr::mutate(
  #                           is_improved = dplyr::case_when(
  #                             new_validity_score > old_validity_score ~ TRUE,
  #                             TRUE                                    ~ FALSE
  #                           )
  #                         ) %>%
  #                         dplyr::select(dplyr::any_of(CROSSWALK_ID), cs_id, is_improved)
  # 
  # 
  # 
  # # mark as "improved" for any hy_id/cs_ids that increased "validity score" after extending
  # check_for_improvement <- dplyr::left_join(
  #   dplyr::select(
  #     dplyr::filter(old_validity_scores, 
  #                   tmp_id %in% unique(new_validity_scores$tmp_id)),
  #     dplyr::any_of(crosswalk_id), cs_id, old_validity_score
  #   ),
  #   dplyr::select(
  #     new_validity_scores,
  #     dplyr::any_of(crosswalk_id), cs_id, new_validity_score
  #   ),
  #   by = c(crosswalk_id, "cs_id")
  #   # by = c("hy_id", "cs_id")
  # ) %>%
  #   dplyr::mutate(
  #     improved = dplyr::case_when(
  #       new_validity_score > old_validity_score ~ TRUE,
  #       TRUE                                    ~ FALSE
  #     )
  #   ) %>%
  #   dplyr::select(dplyr::any_of(crosswalk_id), cs_id, improved)
  # 
  # # get_validity_tally <- function(x, crosswalk_id = NULL) {
  # #     # x <- classified_pts
  # #     # crosswalk_id = "hy_id"
  # # 
  # #     validity_tally <-
  # #       x %>%
  # #       sf::st_drop_geometry() %>%
  # #       dplyr::select(dplyr::any_of(crosswalk_id), cs_id, valid_banks, has_relief) %>%
  # #       dplyr::group_by(dplyr::across(dplyr::any_of(c(crosswalk_id, "cs_id")))) %>%
  # #       dplyr::slice(1) %>%
  # #       dplyr::ungroup() %>%
  # #       dplyr::count(valid_banks, has_relief)
  # # 
  # #     return(validity_tally)
  # # 
  # # }
  # cs_pts
  # new_cs_pts

  
  })



# testthat::test_that("trans -> cs_pts -> extend trans -> new cs_pts -> compare diff -> updated trans + cs_pts", {
  
#   flowlines <- sf::read_sf(testthat::test_path("testdata", "flowlines.gpkg"))
  
#   # Transects inputs
#   CS_SOURCE            <- 'hydrofabric3D_test'
#   NUM_OF_TRANSECTS     <- 5
#   RM_SELF_INTERSECTS   <- TRUE
#   ID_COL               <- "hy_id"
#   CROSSWALK_ID <- "hy_id"
  
#   # Cross section point inputs
#   DEM_PATH       <- testthat::test_path("testdata", "dem_flowlines.tif")
#   POINTS_PER_CS     <- NULL
#   MIN_PTS_PER_CS    <- 10
  
#   # NOTE: 0.01 (1%) of cros sections length is the default
#   # percent of cross section length (cs_lengthm) to use as the
#   # threshold depth for classifying whether a cross section has "relief"
#   PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF <- 0.01
  
#   EXTENSION_PCT = 0.5
  
#   flowlines <-
#     flowlines %>%
#     hydrofabric3D::add_powerlaw_bankful_width("tot_drainage_areasqkm", 50) %>% 
#     # dplyr::mutate(
#     #   bf_width        = hydrofabric3D::calc_powerlaw_bankful_width(tot_drainage_areasqkm),
#     #   bf_width        = pmax(50, bf_width * 11)
#     # ) %>%
#     dplyr::select(
#       hy_id = id,
#       # tot_drainage_areasqkm,
#       bf_width,
#       # input_bf_width,
#       geometry = geom
#     )
#   # dplyr::slice(1)
  
#   # plot(flowlines$geometry)
  
#   transects <- hydrofabric3D::cut_cross_sections(
#     net               = flowlines,
#     crosswalk_id      = ID_COL,
#     cs_widths         = flowlines$bf_width,
#     num               = NUM_OF_TRANSECTS,
#     rm_self_intersect = TRUE
#   ) %>%
#     # dplyr::mutate(
#     #   cs_source = CS_SOURCE
#     # ) %>%
#     dplyr::select(
#       hy_id,
#       cs_id,
#       cs_lengthm,
#       # cs_source,
#       cs_measure,
#       geometry
#     )
  
#   # mapview::mapview(transects, color = "green") +
#   #   mapview::mapview(flowlines, color = "red")
  
#   # ----------------------------------------------------------------------------------------------------------------
#   # ---- Cross section points ----
#   # ----------------------------------------------------------------------------------------------------------------
  
#   # ---- STEP 1: Extract cs points from DEM ----
  
#   # get cross section point elevations
#   cs_pts <- hydrofabric3D::cross_section_pts(
#     cs             = transects,
#     crosswalk_id   = CROSSWALK_ID,
#     points_per_cs  = POINTS_PER_CS,
#     min_pts_per_cs = MIN_PTS_PER_CS,
#     dem            = DEM_PATH
#   )
  
#   # ----------------------------------------------------------------------------------------------------------------
#   # ---- STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points ----
#   # ----------------------------------------------------------------------------------------------------------------
  
#   # system.time({
#   # NA_Z_COUNT <- sum(is.na(cs_pts$Z))
#   # cs_pts[!is.na(cs_pts$Z), ][[ID_COL]]
  
#   # STEP 2: Remove any cross section that has ANY missing (NA) Z values, and classify the points
#   cs_pts <-
#     cs_pts %>%
#     hydrofabric3D::drop_incomplete_cs_pts(CROSSWALK_ID) %>%
#     hydrofabric3D:::classify_points(
#       crosswalk_id             = CROSSWALK_ID,
#       pct_of_length_for_relief = PCT_LENGTH_OF_CROSS_SECTION_FOR_RELIEF
#     ) %>% 
#     hydrofabric3D::add_tmp_id(CROSSWALK_ID) %>% 
#     hydrofabric3D:::pts_to_XY() %>% 
#     dplyr::mutate(
#       Z_source = "test_source1"
#     )
  
#   cs_pts %>%
#     sf::st_drop_geometry() %>%
#     dplyr::group_by(hy_id, cs_id) %>%
#     dplyr::slice(1) %>%
#     dplyr::ungroup() %>%
#     dplyr::select(hy_id, cs_id, valid_banks, has_relief)
  
#   transects <- hydrofabric3D:::add_cs_attributes_to_transects(
#     transects = transects,
#     cs_pts = cs_pts,
#     crosswalk_id = CROSSWALK_ID
#   )
  
#   extended <- hydrofabric3D:::extend_transects_by_cs_attributes(
#     transects = transects,
#     flowlines = flowlines,
#     crosswalk_id = CROSSWALK_ID,
#     scale = 0.5,
#     keep_lengths = TRUE,
#     reindex_cs_ids = FALSE
#   )
  
  
# })