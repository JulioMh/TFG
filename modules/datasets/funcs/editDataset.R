editDatasetUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarPanel(
    basicFormUI(ns("form")),
    uiOutput(ns("submit")),
    actionBttn(
      inputId = ns("cancel"),
      icon = icon("arrow-circle-left"),
      style = "pill",
      color = "warning",
      size = "xs"
    ),
  ),
  mainPanel(datasetUI(ns("dataset"))))
}

editDatasetServer <- function(input, output, session, dataset_id) {
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
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      tryCatch({
        editDataset(dataset_id(), attributes$name(), attributes$description())
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
  return(reactive(input$cancel))
}