source("global.R")
ui <- shiny::fluidPage(
  tags$script(src="css/addhash.js"),
  div(img(src = "img/gwas_logo.png")),
  includeCSS("www/css/custom.css"),
  includeCSS("www/css/footer.css"),
  disconnectMessage(
    text = "Your session timed out, reload the application!",
    refresh = "Reload now",
    background = "#f89f43",
    colour = "white",
    overlayColour = "grey",
    overlayOpacity = 0.75,
    top = 250,
    refreshColour = "brown"
  ),
  navbarPage(
    title = "",
    windowTitle = "BnaGWAS",
    theme = shinytheme("flatly"),
    tabPanel("GWAS",mod_gwas_ui("gwas")),
    tabPanel("Documentation", mod_doc_ui("doc"), icon = icon("file-alt")),
    tabPanel("About",mod_about_ui("about"), icon = icon("info-circle")),
    footer = footerTagList
  )
)

server <- function(input, output, session) {
  callModule(mod_gwas_server,"gwas")
  observeEvent(input$disconnect, {
    session$close()
  })
}

shinyApp(ui, server)
