model_output_app <- function() {
  thematic::thematic_shiny()
  ui <-
    shiny::fluidPage(
      theme = bslib::bs_theme(
        bg = "#271c0b",
        fg =  "#fffe04",
        primary = "#750101",
        secondary = "#4872aa",
      ),
    shiny::column(12, align = "center",
                  shiny::h1("Model App")),
    shiny::hr(),
    shiny::fluidRow(shiny::br(),
                    shiny::column(
                      12,
                      #inputs
                      shiny::tabsetPanel(
                        #replicates
                        shiny::tabPanel("Choose Data",
                                        select_output_ui("dataset")),
                        shiny::tabPanel(
                          "Catchplot",
                          shiny::column(
                            9,
                            offset = 1,
                            align = "left",
                            shiny::br(),
                            shiny::br(),
                            graph_catchplot_plot_ui("catchplot")
                          )
                        ),
                        shiny::tabPanel(
                          "BMS Plot",
                          shiny::column(
                            9,
                            offset = 1,
                            align = "left",
                            shiny::br(),
                            shiny::br(),
                            shiny_bms_plot_ui("BMS_plot")
                          )
                        )
                      )
                    ))
  )
    server <- function(input, output, session) {
      model_data <- select_output_server("dataset")
      graph_catchplot_server("catchplot",model_data)
      shiny_bms_plot_server("BMS_plot",model_data)
    }
    shiny::runApp(shiny::shinyApp(ui, server),launch.browser = TRUE)
  }
