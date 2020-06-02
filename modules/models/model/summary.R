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
    }))
  
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
      res <- editModel(attributes, id())
      if (res) {
        sendSweetAlert(
          session = session,
          title = "Success !!",
          text = "Dataset saved",
          type = "success"
        )
      }
    }
  })
  
  output$compare <-
    renderPrint(summary(resamples(
      list(
        "RandomForest" = data()$models$rf,
        "SMV" = data()$models$svm,
        "XGBDART" = data()$models$xgbdart
      )
    )))
}