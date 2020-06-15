modelSummaryUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("mode"))
}

modelSummary <- function(input, output, session, model_id) {
  ns <- session$ns
  values <-
    reactiveValues(
      show_btton = TRUE,
      models = NULL,
      attributes = getModelsAttributes(model_id)
    )
  
  new_attributes <-
    callModule(modelForm, "edit", reactive({
      values$attributes
    }),
    reactive({
      NULL
    }))
  
  output$mode <- renderUI({
    if (session$userData$user$id == getModelOwner(model_id)) {
      tagList(
        br(),
        sidebarPanel(modelFormUI(ns("edit")),
                     uiOutput(ns("submit"))),
        mainPanel(
          verbatimTextOutput(ns("summary")),
          verbatimTextOutput(ns("compare")),
          br(),
          uiOutput(ns("show_btton"))
        )
      )
    } else{
      if (length(values$models) > 1) {
        tagList(
          br(),
          fluidRow(
            column(4,
                   verbatimTextOutput(ns("attributes")),
                   verbatimTextOutput(ns("summary"))),
            column(8, verbatimTextOutput(ns("compare")))
          ),
          br(),
          uiOutput(ns("show_btton"))
        )  
      }else{
        tagList(
          br(),
          verbatimTextOutput(ns("attributes")),
          verbatimTextOutput(ns("summary")),
          br(),
          uiOutput(ns("show_btton"))
        ) 
      }
    }
  })
  
  output$submit <- renderUI({
    validate(need(new_attributes$name() != "", "Debes introducir un nombre"))
    
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = ns("submit"),
        label = "Editar",
        style = "minimal",
        color = "success"
      )
    ))
  })
  
  output$compare <- renderPrint({
    if (length(values$models) > 1) {
      summary(resamples(values$models))
    }
  })
  
  output$show_btton <- renderUI({
    if (values$show_btton) {
      tagList(
        actionBttn(
          inputId = ns("show_tabs"),
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
  
  output$attributes <-
    renderText({
      req(values$models)
      paste0(
        "Nombre: ",
        values$attributes$name,
        "\n",
        "Description: ",
        values$attributes$description
      )
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
  
  observeEvent(input$show_tabs, {
    values$show_btton <- !values$show_btton
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
  
  observeEvent(model_id, {
    values$models <- getModels(model_id)
    values$target <- getModelTarget(model_id)
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      attributes <-
        list(name = new_attributes$name(),
             description = new_attributes$description())
      isPublic <- ifelse(isTRUE(new_attributes$isPublic()), 1, 0)
      tryCatch({
        editModel(attributes, isPublic, model_id)
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
  
  return(list(
    doTabs = reactive(input$show_tabs),
    models = reactive(values$models)
  ))
}
