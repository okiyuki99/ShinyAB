df <- shinyloadtest::load_runs("1 workers" = "shinyloadtest/run1", 
                               "5 workers" = "shinyloadtest/run5", 
                               "10 workers" = "shinyloadtest/run10")
#saveRDS(df, "shinyloadtest/result.rds")
df <- readRDS("shinyloadtest/result.rds")
shinyloadtest::shinyloadtest_report(df, output = "shinyloadtest_report.html")
