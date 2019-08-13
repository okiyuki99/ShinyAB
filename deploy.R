# Ref : https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications
library(rsconnect)
rsconnect::setAccountInfo(name = Sys.getenv("RS_NAME"), token = Sys.getenv("RS_TOKEN"), secret = Sys.getenv("RS_SECRET"))
deployApp()
