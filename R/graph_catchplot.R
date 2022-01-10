graph_catchplot_plot_ui <-  function(id) {
  shiny::tagList(shiny::fluidPage(shiny::fluidRow(
    shiny::column(3,
    shiny::tagList(
      shiny::selectInput(shiny::NS(id,"type"),
                         "Select Type",
                         choices = c("harvest","yield"),
                         selected = "harvest"),
      shiny::actionButton(shiny::NS(id,"plot_catch"),
                          "Plot Catch Data")
    )
    ),
    shiny::column(9,
                  plotly::plotlyOutput(shiny::NS(id, "catchplot")))
  )))
}

graph_catchplot_server <-
  function(id, data,type) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::observeEvent(input$plot_catch, {
      output$catchplot <-
        plotly::renderPlotly(
          graph_catchPlot(dat = data(),
                          type = input$type,
                          plotly_plot = TRUE))
    })
    })
  }
