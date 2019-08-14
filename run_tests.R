# For recording
# shinytest::recordTest("shinyapp/")
library(testthat)
library(shinytest)
dir.create("tests/test-current", showWarnings = F)

testthat::test_that("Application works", {
  # Use compareImages = FALSE because the expected image screenshots were created
  shinytest::expect_pass(shinytest::testApp(".", testnames = NULL, quiet = F, compareImages = F))
})

