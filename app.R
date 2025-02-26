options(shiny.port = 8080)
source("app/global.R")
source("app/ui.R")
source("app/server.R")


auth0::shinyAppAuth0(ui = ui, server = server)
#shiny::shinyApp(ui = ui, server = server)
