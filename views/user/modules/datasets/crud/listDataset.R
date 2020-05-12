listDatasetUI <- function (id) {
  ns <- NS(id)
  tagList(br(), DT::dataTableOutput(ns("table")))
}

listDataset <- function(input, output, session) {
  observeEvent(input$table_rows_selected, {
    index = input$table_rows_selected
    if (length(index)) {
      name <- datasets$list[[1, index]]
      session$userData$user$dataset <- name
    }
  })
  
  output$table <-
    DT::renderDataTable(
      loadDatasets(session$userData$user$id),
      options = list(lengthMenu = list(c(5, 10,-1), c('5', '10', 'All')),
                     pageLength = 10),
      selection = 'single',
    )
}

loadDatasets <- function(id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select name, description from Dataset where user_id= '%s'",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  })
  dbDisconnect(db)
  return(response)
}