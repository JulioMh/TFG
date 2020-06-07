pickerDatasetUI <- function(id) {
  ns <- NS(id)
  tagList(
    uiOutput(ns("picker")),
    actionBttn(
      inputId = ns("reload"),
      label = NULL,
      style = "material-circle",
      color = "default",
      icon = icon("redo"),
      size = "xs"
    )
  )
}

pickerDataset <- function(input, output, session, datasets) {
  output$picker <- renderUI({
    tagList(
      pickerInput(
        inputId = session$ns("dataset_id"),
        label = tagList(
          icon("file-medical-alt"),
          "Selecciona un conjunto de datos:"
        ),
        choices = getPickerList(datasets()),
        selected = last(datasets()),
        options = list(`live-search` = TRUE)
      )
    )
  })
  
  return(list(id = reactive({
    input$dataset_id
  }),
  reload = reactive({
    input$reload
  })))
}

getPickerList <- function(datasets) {
  list_id <- as.list(datasets$id)
  names(list_id) <- datasets$name
  return(list_id)
}