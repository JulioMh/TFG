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

selectDataset <- function(input, output, session, datasets) {
  values <-
    reactiveValues(
      dataset = NULL,
      id = NULL,
      target = NULL,
      datasets = NULL,
      reload = NULL
    )
  dataset <- callModule(datasetForm, "dataset", NULL)
  picked <-
    callModule(pickerDataset, "picker_dataset", reactive({
      values$datasets
    }))
  
  observeEvent(datasets(), {
    req(datasets())
    values$datasets = datasets()
  })
  
  output$mode <- renderUI({
    if (isTRUE(input$switch)) {
      datasetFormUI(session$ns("dataset"))
    } else{
      pickerDatasetUI(session$ns("picker_dataset"))
    }
  })
  
  observeEvent(picked$id(), {
    req(picked$id())
    selected_dataset <- loadDataset(picked$id())
    values$dataset <- selected_dataset$dataset
    values$id <- picked$id()
  })
  
  observeEvent(input$switch, {
    if (isTRUE(input$switch)) {
      values$dataset <- NULL
      values$id <- NULL
    } else{
      req(picked$id())
      selected_dataset <- loadDataset(picked$id())
      values$dataset <- selected_dataset$dataset
      values$reload <- TRUE
    }
  })
  
  observeEvent(picked$reload(), {
    req(picked$reload())
    values$reload <- TRUE
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
  
  return(list(
    dataset = reactive({
      values$dataset
    }),
    id = reactive({
      values$id
    }),
    reload = reactive({
      values$reload
    })
  ))
}