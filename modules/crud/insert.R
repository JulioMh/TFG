insertUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("form"))
}

insert <- function (input, output, session, form_ui, form_server, table, data_to_edit) {
  
  submited_data <- callModule(form_server, "form", data_to_edit)
  output$form <- renderUI(form_ui(session$ns("form")))
  
  observeEvent(submited_data$do_insert, {
    db <-
      dbConnect(
        MySQL(),
        dbname = databaseName,
        host = options()$mysql$host,
        port = options()$mysql$port,
        user = options()$mysql$user,
        password = options()$mysql$password
      )
    res <- "Done!"
    query <- sprintf(
      "INSERT INTO %s (%s) VALUES ('%s')",
      table,
      paste(names(submited_data), collapse = ", "),
      paste(submited_data, collapse = "', '")
    )
    tryCatch({
      dbGetQuery(db, query)
    }, error = function(e) {
      stop(safeError(e))
    })
    dbDisconnect(db)
    return(res)
  })
}
