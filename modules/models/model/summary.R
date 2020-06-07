modelSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarPanel(basicFormUI(ns("edit")),
                       uiOutput(ns("submit"))),
          mainPanel(uiOutput(ns("show_btton")),
                    tabsetPanel(
                      tabsetPanel("Detalles",
                                  verbatimTextOutput(ns("compare")))
                    )))
}

modelSummary <- function(input, output, session, data, id, reset) {
  values <- reactiveValues(show_btton = TRUE)
  
  new_attributes <-
    callModule(basicForm, "edit", reactive({
      data()$attributes
    }),
    reactive({
      NULL
    }))
  
  observeEvent(input$show_tabs, {
    values$show_btton <- FALSE
  })
  
  observeEvent(reset(), {
    req(reset())
    values$show_btton <- TRUE
  })
  
  output$show_btton <- renderUI({
    if (values$show_btton) {
      tagList(
        actionBttn(
          inputId = session$ns("show_tabs"),
          label = "Ver en detalle...",
          style = "jelly",
          color = "success",
          size = "xs"
        ),
        br(),
      )
    } else{
      NULL
    }
  })
  
  output$submit <- renderUI({
    validate(need(new_attributes$name() != "", "Debes introducir un nombre"))
    
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("submit"),
        label = "Editar",
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
    renderPrint({
      if (length(data()$models) > 1) {
        summary(resamples(data()$models))
      } else{
        first(data()$models)
      }
    })
  
  return(reactive({
    input$show_tabs
  }))
}