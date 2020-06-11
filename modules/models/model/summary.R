modelSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(sidebarPanel(basicFormUI(ns("edit")),
                       uiOutput(ns("submit"))),
          mainPanel(
            tabsetPanel(
              id = ns("tabs"),
              tabPanel(
                "Resumen",
                br(),
                verbatimTextOutput(ns("summary")),
                verbatimTextOutput(ns("compare")),
                placeholder = TRUE
              ),
              tabPanel("Conjunto de entrenamiento",
                       br(),
                       tableUI(ns("train")))
            ),
            br(),
            uiOutput(ns("show_btton"))
          ))
}

modelSummary <- function(input, output, session, id) {
  values <- reactiveValues(show_btton = TRUE, models = NULL)
  callModule(table, "train", reactive({
    values$datasets$train
  }))
  
  observeEvent(id, {
    values$attributes <- getModelsAttributes(id)
    values$models <- getModels(id)
    values$target <- getModelTarget(id)
  })
  
  observeEvent(input$tabs, {
    if (input$tabs == "Conjunto de entrenamiento") {
      if (is.null(values$datasets)) {
        values$datasets <- getModelDataset(id)
      }
    }
  })
  
  new_attributes <-
    callModule(basicForm, "edit", reactive({
      values$attributes
    }),
    reactive({
      NULL
    }))
  
  observeEvent(input$show_tabs, {
    values$show_btton <- !values$show_btton
  })
  
  output$compare <- renderPrint({
    if (length(values$models) > 1) {
      summary(resamples(values$models))
    }
  })
  
  observeEvent(input$show_tabs, {
    lapply(values$models, function(model) {
      tab <- tabPanel(title = model$modelInfo$label,
                      br(),
                      detailsUI(session$ns(model$method)))
      appendTab("tabs",
                tab,
                select = ifelse(model$method == first(values$models)$method,
                                TRUE,
                                FALSE))
      callModule(details, model$method, model)
    })
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
      text = "Puedes volver a cambiar estos datos mas adelante"
    )
  })
  
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      attributes <-
        list(name = new_attributes$name(),
             description = new_attributes$description())
      tryCatch({
        editModel(attributes, id)
        sendSweetAlert(
          session = session,
          title = "Listo",
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
  
  output$summary <-
    renderText({
      req(values$models)
      paste0(
        "Objetivo: ",
        values$target,
        "\n",
        "Tipo: ",
        first(values$models)$modelType,
        "\n",
        "Métodos: ",
        paste(lapply(values$models, function(model) {
          if (is.list(model)) {
            model$modelInfo$label
          }
        }), collapse = ", ")
      )
    })
}
