modelSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarPanel(basicFormUI(ns("edit")),
                       uiOutput(ns("submit"))),
          mainPanel(verbatimTextOutput(ns("compare"))))
}

modelSummary <- function(input, output, session, data, id) {
  new_attributes <-
    callModule(basicForm, "edit", reactive({
      data()$attributes
    }),
    reactive({NULL}))
  
  output$submit <- renderUI({
    validate(need(new_attributes$name() != "", "Debes introducir un nombre"))
    
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("submit"),
        label = "Editar",
        style = "minimal",
        color = "warning"
      )
    ))
  })
  
  observeEvent(input$submit, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm",
      type = "warning",
      title = "¿Estas seguro?",
      text = "Este proceso tardará varios minutos"
    )
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      attributes <-
        list(name = new_attributes$name(),
             description = new_attributes$description())
      tryCatch({
        editModel(attributes, id())
        sendSweetAlert(
          session = session,
          title = "Listo !!",
          text = "Los datos han sido guardados",
          type = "success"
        )
      },
      error = function(cond) {
        sendSweetAlert(
          session = session,
          title = "No se han podido guardar los cambios...",
          text = cond,
          type = "error"
        )
      })
    }
  })
  
  output$compare <-
    renderPrint(summary(resamples(data()$models)))
}