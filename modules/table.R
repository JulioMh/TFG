tableUI <- function (id) {
  ns <- NS(id)
  DT::dataTableOutput(ns("tablePrev"))
}

table <- function (input, output, session, datapath) {
  output$tablePrev <- DT::renderDataTable({
    req(datapath$path_dataset())
    tryCatch({
      dataset <- read.csv(datapath$path_dataset())
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    return(DT::datatable(
      dataset,
      options = list(lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
                     pageLength = 10)
    ))
  })
  
}