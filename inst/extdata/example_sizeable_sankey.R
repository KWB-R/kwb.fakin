#
# This script runs a Shiny App in which a Sankey diagram can be resized. 
# The solution was taken from this issue discussion on GitHub:
# https://github.com/rich-iannone/DiagrammeR/issues/93
#
library(shiny)

URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/',
              'master/JSONdata/energy.json')
energy <- jsonlite::fromJSON(URL)

slider_config <- list(
  height = c(min = 100, max = 2000, value = 400, step = 50),
  width = c(min = 100, max = 2000, value = 800, step = 50),
  fontSize = c(min = 4, max = 40, value = 12, step = 1),
  nodeWidth = c(min = 1, max = 40, value = 10, step = 1),
  nodePadding = c(min = 0, max = 20, value = 10, step = 1),
  iterations = c(min = 1, max = 128, value = 32, step = 1)
)

config_to_slider_inputs <- function(config)
{
  lapply(names(config), function(name) {
    do.call(shiny::sliderInput, c(
      list(inputId = name, label = name), config[[name]]
    ))
  })
}

ui <- fluidPage(
  shiny::titlePanel("Test Sankey"),
  do.call(shiny::sidebarPanel, config_to_slider_inputs(slider_config)),
  shiny::mainPanel(
    shiny::uiOutput("sankey")
  )
)

server <- function(input, output, session) {
  
  output$sankey_ <- networkD3::renderSankeyNetwork({
    networkD3::sankeyNetwork(
      Links = energy$links, 
      Nodes = energy$nodes, 
      Source = 'source',
      Target = 'target', 
      Value = 'value', 
      NodeID = 'name',
      units = 'TWh', 
      fontSize = input$fontSize, 
      nodeWidth = input$nodeWidth,
      nodePadding = input$nodePadding,
      iterations = input$iterations,
      zoom = TRUE
    )
  })
  
  output$sankey <- shiny::renderUI({
    networkD3::sankeyNetworkOutput(
      "sankey_", height = input$height, width = input$width
    )
  })
}

shinyApp(ui, server)
