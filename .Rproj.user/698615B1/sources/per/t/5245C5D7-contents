model_output_app <- function(){
    ui <- shiny::fluidPage(
      shiny::column(12, align = "center",
                    shiny::h1("Model App")),
      shiny::hr(),
      shiny::fluidRow(
        shiny::br(),
        shiny::column(
          3,
          #inputs
          shiny::fluidRow(shiny::column(
            12,
            offset = 1,
            align = "left",
            shiny::h4("Choose Inputs")
          )),
          shiny::fluidRow(
            shiny::column(
              12,
              offset = 1,
              align = "left",
              shiny::br(),
              shiny::br(),
              select_output_ui("dataset")
            )
          )
        ),
        shiny::column(12,
                      graph_catchplot_plot_ui("catchplot")
        )
      )
    )

    server <- function(input, output, session) {
      model_data <- select_output_server("dataset")
      graph_catchplot_server("catchplot",model_data,type = )
    }

    shiny::runApp(shiny::shinyApp(ui, server),launch.browser = TRUE)
  }
