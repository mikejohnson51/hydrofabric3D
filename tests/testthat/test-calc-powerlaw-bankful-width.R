library(testthat)
library(dplyr)

# devtools::load_all()

# -------------------------------------------------------------------
# ---- hydrofabric::calc_powerlaw_bankful_width() ----
# -------------------------------------------------------------------
testthat::test_that("calculate powerlaw bankful width from valid numeric values", {

  testthat::expect_true(dplyr::between(calc_powerlaw_bankful_width(1), 2, 3))
  testthat::expect_true(dplyr::between(calc_powerlaw_bankful_width(10), 4, 5))
  testthat::expect_true(dplyr::between(calc_powerlaw_bankful_width(100), 10, 11))

})

testthat::test_that("calculate powerlaw bankful width for 0", {

  testthat::expect_true(calc_powerlaw_bankful_width(0) == 0)

})

testthat::test_that("calculate powerlaw bankful width for NA", {
  testthat::expect_error(calc_powerlaw_bankful_width(NA))
})

testthat::test_that("error calculating powerlaw bankful width for string number", {

  testthat::expect_error(calc_powerlaw_bankful_width("10"))

})

testthat::test_that("error calculating powerlaw bankful width for empty dataframe", {

  testthat::expect_error(calc_powerlaw_bankful_width(data_frame()))

})

