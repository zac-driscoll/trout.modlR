graph_catchplot_plot_ui <-  function(id) {
  shiny::tagList(shiny::fluidPage(shiny::fluidRow(
    shiny::column(2,
    shiny::tagList(
      shiny::selectInput(shiny::NS(id,"type"),
                         "Select Type",
                         choices = c("Harvest" = "harvest",
                                     "Yield" = "yield"),
                         selected = "harvest"),
      shiny::selectInput(shiny::NS(id,"title"),
                         "Include Title?",
                         choices = c("Auto" = "Y",
                                     "None" = "N")),
      shiny::actionButton(shiny::NS(id,"plot_catch"),
                          "Plot Catch Data")
    )
    ),
    shiny::column(10,
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
                          title = input$title,
                          plotly_plot = TRUE))
    })
    })
  }
