# Ref : https://docs.rstudio.com/shinyapps.io/getting-started.html#deploying-applications
library(rsconnect)
rsconnect::setAccountInfo(name="<ACCOUNT>", token="<TOKEN>", secret="<SECRET>")
deployApp()