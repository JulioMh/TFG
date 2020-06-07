datasetFormUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("form"))
}

datasetForm <- function (input, output, session, data) {
  values <-
    reactiveValues(reload = FALSE,
                   upload_state = NULL)
  fields <-
    callModule(basicForm, "basic", data, reactive({
      values$reload
    }))
  
  file_input <- reactive({
    if (is.null(values$upload_state)) {
      return(NULL)
    } else if (values$upload_state == 'uploaded') {
      return(input$dataset$datapath)
    } else if (values$upload_state == 'reset') {
      return("reset")
    }
  })
  
  output$form <- renderUI({
    file = ""
    if (is.null(data)) {
      file <- fileInput(
        session$ns("dataset"),
        tagList(icon("file-medical-alt"), "Sube el conjunto de datos:"),
        multiple = FALSE,
        accept = c(
          "text/csv",
          "text/comma-separated-values,text/plain",
          ".csv"
        )
      )
    }
    tagList(useSweetAlert(),
            file,
            basicFormUI(session$ns("basic")),
            uiOutput(session$ns("submit")),)
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
      submitedData <- list(
        "name" = fields$name(),
        "id" = ifelse(is.null(data), "", data()$id),
        "description" = fields$description(),
        "dataset" = input$dataset$datapath,
        "user_id" = session$userData$user$id
      )
      
      res <- saveDataset(submitedData)
      
      if (res) {
        sendSweetAlert(
          session = session,
          title = "Listo!!",
          text = "Dataset guardado",
          type = "success"
        )
        if (is.null(data)) {
          values$upload_state <- "reset"
          values$reload <- TRUE
          reset("dataset")
        }
        
      } else{
        sendSweetAlert(
          session = session,
          title = "Error...",
          text = "El dataset no se ha podido almacenar",
          type = "error"
        )
      }
    }
  })
  
  output$submit <- renderUI({
    if (is.null(data)) {
      validate(
        need(!is.null(file_input()) &&
               file_input() != "reset", label = "File"),
        need(fields$name() != "", label = "Name")
      )
    } else{
      validate(need(fields$name() != "", label = "Name"))
    }
    return(div(style = "text-align: center;",
               actionBttn(
                 inputId = session$ns("submit"),
                 label = "Guardar",
                 style = "minimal",
                 color = "success"
               )))
  })
  
  observeEvent(input$dataset, {
    values$upload_state <- 'uploaded'
  })
  
  return(list(path_dataset = file_input))
}