tableUI <- function (id) {
  ns <- NS(id)
  tagList(br(), DT::dataTableOutput(ns("table")))
}

table <- function(input, output, session, data) {
  output$table <-
    DT::renderDataTable(
      data(),
      options = list(lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')),
                     pageLength = 10),
      selection = 'single',
    )
  return(list(index = reactive({
    input$table_rows_selected
  })))
}