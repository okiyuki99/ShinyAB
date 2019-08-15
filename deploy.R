# Ref : https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications
library(rsconnect)
if(!require(dashboardthemes)){
  remotes::install_github("nik01010/dashboardthemes")
}
rsconnect::setAccountInfo(name = Sys.getenv("RSCONNECT_NAME"), token = Sys.getenv("RSCONNECT_TOKEN"), secret = Sys.getenv("RSCONNECT_SECRET"))
rsconnect::deployApp(appDir = ".", appFiles = c("docs/", "global.R", "server.R", "ui.R", "utils/"), 
                     launch.browser = F, forceUpdate = T)