editDatasetUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionBttn(
      inputId = ns("cancel"),
      icon = icon("arrow-circle-left"),
      style = "pill",
      color = "warning",
      size = "xs"
    ),
    br(),
    br(),
    sidebarPanel(
      basicFormUI(ns("form")),
      uiOutput(ns("submit")),
      br(),
      actionBttn(
        inputId = ns("delete"),
        label = "Eliminar dataset",
        style = "fill",
        color = "danger",
        size = "xs"
      ),
    ),
    mainPanel(datasetUI(ns("dataset")))
  )
}

editDatasetServer <- function(input, output, session, dataset_id) {
  values <- reactiveValues(cancel = 0)
  
  attributes <-
    callModule(basicForm, "form", reactive({
      req(dataset_id())
      getDatasetAttributes(dataset_id())
    }),
    reactive(NULL))
  callModule(dataset, "dataset", reactive({
    req(dataset_id())
    getDataset(dataset_id())
  }))
  
  output$submit <- renderUI({
    validate(need(attributes$name() != "", "Introduce un nombre"))
    
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
  
  observeEvent(input$submit, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm",
      type = "warning",
      title = "¿Seguro que quieres continuar?",
    )
  })
  
  observeEvent(input$delete, {
    if (isRelated(dataset_id())) {
      sendSweetAlert(
        session = session,
        title = "Este datset tiene modelos asociados",
        text = "Para eliminarlo debes eliminar primero los modelos que lo usan.",
        type = "info"
      )
    } else{
      confirmSweetAlert(
        session = session,
        inputId = "confirm_delete",
        type = "warning",
        title = "¿Estas seguro?",
        text = "Esta acción es irreversible"
      )
    }
  })
  
  observeEvent(input$confirm_delete, {
    if (isTRUE(input$confirm_delete)) {
      tryCatch({
        deleteDataset(dataset_id())
        values$cancel <- values$cancel + 1
        sendSweetAlert(session = session,
                       title = "Listo",
                       type = "success")
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "Se ha encontrado un problema...",
          text = cond,
          type = "error"
        )
      })
    }
  })
  
  observeEvent(input$cancel, {
    values$cancel <- input$cancel
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      tryCatch({
        editDataset(dataset_id(),
                    attributes$name(),
                    attributes$description())
        sendSweetAlert(
          session = session,
          title = "Listo!!",
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
  return(reactive(values$cancel))
}