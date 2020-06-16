uploadDatasetUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(
      ns("csv"),
      tagList(icon("file-medical-alt"), "Sube el conjunto de datos:"),
      multiple = FALSE,
      accept = c(
        "text/csv",
        "text/comma-separated-values,text/plain",
        ".csv",
        ".data"
      )
    ),
    basicFormUI(ns("form")),
    uiOutput(ns("submit"))
  )
}

uploadDatasetServer <- function(input, output, session, show) {
  values <- reactiveValues()
  
  attributes <-
    callModule(basicForm, "form", NULL, reactive(values$done))

  output$submit <- renderUI({
    validate(
      need(attributes$name() != "", "Introduce un nombre"),
      need(!is.null(values$path), "Carga un conjunto de datos")
    )
    
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("submit"),
        label = "Guardar",
        style = "minimal",
        color = "success"
      )
    ))
  })
  
  observeEvent(input$csv$datapath, {
    values$path <- input$csv$datapath
  })
  
  observeEvent(show(), {
    if(isFALSE(show())){
      values$path <- NULL
    }
  })
  
  observeEvent(values$done, {
    reset("csv")
    values$path <- NULL
  })
  
  observeEvent(input$submit, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm",
      type = "warning",
      title = "¿Seguro que quieres continuar?",
    )
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      tryCatch({
        uploadDataset(
          attributes$name(),
          attributes$description(),
          input$csv$datapath,
          session$userData$user$id
        )
        values$done <- TRUE
        sendSweetAlert(
          session = session,
          title = "Listo",
          text = "Dataset guardado",
          type = "success"
        )
      },
      error = function(message) {
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "El dataset no se ha podido acualizar",
          type = "error"
        )
      })
    }
  })
  
  return(reactive(values$path))
  
}