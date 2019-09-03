library(testthat)
library(shinytest)

testthat::context("Shiny AB Test")

# open Shiny app and PhantomJS
app <- shinytest::ShinyDriver$new("../",  loadTimeout = 100000)

testthat::test_that("Click go", {
  
  # btn click
  app$setInputs(btn_go = "click")
  
  # Wait
  Sys.sleep(1)
  
  # Get data
  vals <- app$getAllValues()
  
  # Test
  testthat::expect_identical(vals$output$kable_proportion[[1]], 
                         '<table class=\"table table-striped table-bordered\" style=\"margin-left: auto; margin-right: auto;\">\n <thead>\n  <tr>\n   <th style=\"text-align:right;\"> UU </th>\n   <th style=\"text-align:right;\"> number_of_samples </th>\n   <th style=\"text-align:right;\"> alpha </th>\n   <th style=\"text-align:right;\"> power </th>\n   <th style=\"text-align:right;\"> test_method </th>\n   <th style=\"text-align:right;\"> as_is </th>\n   <th style=\"text-align:right;\"> to_be </th>\n   <th style=\"text-align:right;\"> sample_size_per_group </th>\n   <th style=\"text-align:right;\"> required_sample_size </th>\n   <th style=\"text-align:right;\"> sampling_rate </th>\n  </tr>\n </thead>\n<tbody>\n  <tr>\n   <td style=\"text-align:right;\"> 10,000 </td>\n   <td style=\"text-align:right;\"> 2 </td>\n   <td style=\"text-align:right;\"> 0.05 </td>\n   <td style=\"text-align:right;\"> 0.8 </td>\n   <td style=\"text-align:right;\"> prop.test </td>\n   <td style=\"text-align:right;\"> 0.3 </td>\n   <td style=\"text-align:right;\"> 0.4 </td>\n   <td style=\"text-align:right;\"> <span style=\"     color: blue !important;\">356</span> </td>\n   <td style=\"text-align:right;\"> <span style=\"     color: blue !important;\">712</span> </td>\n   <td style=\"text-align:right;\"> <span style=\"     color: blue !important;\">7.12%</span> </td>\n  </tr>\n</tbody>\n</table>'
  )
  
  # remove click
  app$setInputs(btn_remove = "click")

  # Get data
  vals <- app$getAllValues()
  
  # Test
  testthat::expect_null(vals$output$kable_proportion[[1]])
})

testthat::test_that("Change expected value and Update Lift", {
  
  # input
  app$setInputs(expected_value = 0.5)
  
  # Wait
  Sys.sleep(1)
  
  # Get data
  vals <- app$getAllValues()
  
  # Test 
  testthat::expect_identical(vals$output$ui_lift$html[[1]], 
                             '<div style="font-style: italic;">Lift from As-Is ratio to To-Be ratio :<strong> 66.67% </strong></div>')
})

# stop the Shiny app
app$stop()

