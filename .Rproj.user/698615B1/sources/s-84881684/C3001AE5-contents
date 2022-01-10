select_output_ui <- function(id) {
  shiny::tagList(
    shiny::fileInput(
      shiny::NS(id, "modelData"),
      "Select Model Output"),
    shiny::actionButton(
      shiny::NS(id,"data_button"),
      "Get Data")
  )
}

select_output_server <- function(id) {
    shiny::moduleServer(id, function(input, output, session) {
      shiny::eventReactive(input$data_button, {
      shiny::req(input$modelData)
      dget(input$modelData$datapath)
    })
  })
}
