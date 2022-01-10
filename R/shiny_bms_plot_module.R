shiny_bms_plot_ui <-  function(id) {
  shiny::tagList(shiny::fluidPage(shiny::fluidRow(
    shiny::column(2,
                  shiny::tagList(
                    shiny::selectInput(shiny::NS(id,"bms_title"),
                                       "Include Title?",
                                       choices = c("Auto" = "Y",
                                                   "None" = "N")),
                    shiny::sliderInput(shiny::NS(id,"bms_ymax"),
                                       "Select Y-max",
                                       min = 0, max = 1000, value = 100),
                    shiny::actionButton(shiny::NS(id,"plot_bms"),
                                        "Plot BMS Data")
                  )
    ),
    shiny::column(10,
                  plotly::plotlyOutput(shiny::NS(id, "bms_plot")))
  )))
}

shiny_bms_plot_server <-
  function(id, data,type) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::observeEvent(input$plot_bms, {
        output$catchplot <-
          plotly::renderPlotly(
            make_bmsPlot(dat = data(),
                            ymax = input$bms_ymax,
                            type = input$type,
                            title = input$title,
                            plotly_plot = TRUE))
      })
    })
  }
