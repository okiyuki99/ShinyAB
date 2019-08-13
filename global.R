# Cleaning ----
detach_all <- function() {
  basic.pkg <- c("package:stats", "package:graphics", "package:grDevices", 
                 "package:utils", "package:datasets", "package:methods", "package:base")
  
  pkg.list <- search()[ifelse(unlist(gregexpr("package:", search())) == 1 ,TRUE, FALSE)]
  
  pkg.list <- setdiff(pkg.list, basic.pkg)
  
  lapply(pkg.list, detach, character.only = TRUE)
}
detach_all()
rm(list = ls())


library(pacman)
pacman::p_load(dplyr, knitr, kableExtra, plotly, testthat)
pacman::p_load(shiny, shinydashboard, tippy, dashboardthemes, shinycssloaders, shinytest)

source("utils/data.R")
source("utils/logo.R")
source("utils/utils.R")

