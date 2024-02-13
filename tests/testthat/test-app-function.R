test_that("qkmap_app tests works", {
  # Don't run these tests on the CRAN build servers
  testthat::skip_on_cran()
  
  appdir <- system.file(package = "quadkeyr", "app")
  shinytest2::test_app(appdir)
  
  # To record new tests
  # shinytest2::record_test(appdir)
  # Warning! It will save the tests in the installed package directory
})
