selectDatasetUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("mode")),
          br(),
          fluidRow(column(
            1,
            prettySwitch(
              inputId = ns("switch"),
              value = FALSE,
              label = ""
            )
          ),
          column(1,
                 actionLink(
                   ns("info"), icon("info-circle")
                 ))))
}

selectDataset <- function(input, output, session) {
  values <-
    reactiveValues(
      dataset = NULL,
      id = NULL,
      target = NULL,
      data_select = getSelectDataFromDB(session$userData$user$id)
    )
  dataset <- callModule(datasetForm, "dataset", NULL)
  
  output$mode <- renderUI({
    if (isTRUE(input$switch)) {
      datasetFormUI(session$ns("dataset"))
    } else{
      tagList(
        pickerInput(
          inputId = session$ns("dataset_id"),
          label = tagList(
            icon("file-medical-alt"),
            "Selecciona un conjunto de datos:"
          ),
          choices = values$data_select,
          options = list(`live-search` = TRUE)
        ),
        actionBttn(
          inputId = session$ns("reload"),
          label = NULL,
          style = "material-circle",
          color = "default",
          icon = icon("redo"),
          size = "xs"
        )
      )
    }
  })

  observeEvent(input$dataset_id, {
    req(input$dataset_id)
    selected_dataset <- loadDataset(input$dataset_id)
    values$dataset <- selected_dataset$dataset
    values$id <- input$dataset_id
  })
  
  observeEvent(input$switch, {
    if (isTRUE(input$switch)) {
      values$dataset <- NULL
      values$id <- NULL
    } else{
      req(input$dataset_id)
      selected_dataset <- loadDataset(input$dataset_id)
      values$dataset <- selected_dataset$dataset
      values$data_select <-
        getSelectDataFromDB(session$userData$user$id)
    }
  })
  
  observeEvent(input$reload, {
    values$data_select <- getSelectDataFromDB(session$userData$user$id)
  })
  
  observeEvent(dataset$path_dataset(), {
    if (dataset$path_dataset() == "reset") {
      updateSwitchInput(session = session,
                        inputId = "switch",
                        value = FALSE)
    } else{
      values$dataset <- read.csv(dataset$path_dataset())
    }
  })
  
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Información",
      text = "Puedes seleccionar un dataset que ya hayas guardado o cargar uno nuevo desde aquí",
      type = "info"
    )
  })
  
  return(list(dataset = reactive({
    values$dataset
  }),
  id = reactive({
    values$id
  })))
}

getSelectDataFromDB <- function(id) {
  list_dataset <- loadDatasets(id)
  list_id <- as.list(list_dataset$id)
  names(list_id) <- list_dataset$name
  return(list_id)
}