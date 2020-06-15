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

selectDataset <- function(input, output, session, preds) {
  ns <- session$ns
  values <-
    reactiveValues(datasets = {
      if (is.null(preds())) {
        loadDatasets(session$userData$user$id)
      } else{
        getAvailableDatasetsToPredict(preds(), session$userData$user$id)
      }
    })
  
  path <- callModule(uploadDatasetServer, "upload")
  
  observeEvent(input$info, {
    sendSweetAlert(
      session = session,
      title = "Información",
      text = "Puedes seleccionar un dataset que ya hayas guardado o cargar uno nuevo desde aquí",
      type = "info"
    )
  })
  
  observeEvent(input$reload, {
    if (is.null(preds())) {
      values$datasets <- loadDatasets(session$userData$user$id)
    } else{
      values$datasets <-
        getAvailableDatasetsToPredict(preds(), session$userData$user$id)
    }
  })
  
  output$mode <- renderUI({
    if (isTRUE(input$switch)) {
      uploadDatasetUI(ns("upload"))
    } else{
      values$datasets <- loadDatasets(session$userData$user$id)
      tagList(
        pickerInput(
          inputId = ns("dataset_id"),
          label = tagList(
            icon("file-medical-alt"),
            "Selecciona un conjunto de datos:"
          ),
          choices = getPickerList(values$datasets),
          selected = last(values$datasets),
          options = list(`live-search` = TRUE)
        ),
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
  })
  
  return(list(id = reactive(input$dataset_id),
              dataset = reactive({
                if (isTRUE(input$switch)) {
                  if (is.null(path())) {
                    NULL
                  } else {
                    read.csv(path())
                  }
                } else {
                  req(input$dataset_id)
                  getDataset(input$dataset_id)
                }
              })))
}
