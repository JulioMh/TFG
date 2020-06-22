modelSummaryUI <- function(id) {
  ns <- NS(id)
  tagList(uiOutput(ns("show_btton")),
          uiOutput(ns("mode")))
}

modelSummary <- function(input, output, session, model_id) {
  ns <- session$ns
  values <-
    reactiveValues(
      show_btton = TRUE,
      models = NULL,
      attributes = getModelsAttributes(model_id),
      reload_follow_btt = FALSE
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
        sidebarPanel(
          modelFormUI(ns("edit")),
          uiOutput(ns("submit")),
          br(),
          br(),
          actionBttn(
            inputId = ns("delete"),
            label = "Eliminar modelo",
            style = "fill",
            color = "danger",
            size = "xs"
          )
        ),
        
        mainPanel(verbatimTextOutput(ns("summary")),
                  verbatimTextOutput(ns("compare")), )
      )
    } else{
      if (length(values$models) > 1) {
        tagList(br(),
                fluidRow(
                  column(4,
                         verbatimTextOutput(ns(
                           "attributes"
                         )),
                         verbatimTextOutput(ns("summary"))),
                  column(8, verbatimTextOutput(ns("compare")))
                ),
                br(),
                uiOutput(ns("follow_bttn")))
      } else{
        tagList(
          br(),
          verbatimTextOutput(ns("attributes")),
          verbatimTextOutput(ns("summary")),
          br(),
          uiOutput(ns("follow_bttn"))
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
        br(),
        actionBttn(
          inputId = ns("show_tabs"),
          label = "Ver en detalle...",
          style = "jelly",
          color = "primary",
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
  
  observeEvent(input$delete, {
    confirmSweetAlert(
      session = session,
      inputId = "confirm_delete",
      type = "warning",
      title = "¿Estas seguro?",
      text = "Esta acción es irreversible"
    )
  })
  
  observeEvent(input$confirm_delete, {
    if (isTRUE(input$confirm_delete)) {
      tryCatch({
        deleteModel(model_id)
        session$userData$user$deleted_model <- TRUE
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
  
  output$follow_bttn <- renderUI({
    values$reload_follow_btt
    if (isFollower(model_id, session$userData$user$id)) {
      actionBttn(
        inputId = ns("unfollow"),
        label = "Dejar de seguir",
        style = "minimal",
        color = "danger",
        size = "xs"
      )
    } else{
      actionBttn(
        inputId = ns("follow"),
        label = "Guardar modelo",
        style = "minimal",
        color = "success",
        size = "xs"
      )
    }
  })
  
  observeEvent(input$unfollow, {
    tryCatch({
      unfollowModel(model_id, session$userData$user$id)
      values$reload_follow_btt <- !values$reload_follow_btt
    },
    error = function(cond) {
      sendSweetAlert(
        session = session,
        title = "Se ha encontrado un problema...",
        text = cond,
        type = "error"
      )
    })
  })
  
  observeEvent(input$follow, {
    tryCatch({
      followModel(model_id, session$userData$user$id)
      values$reload_follow_btt <- !values$reload_follow_btt
    },
    error = function(cond) {
      sendSweetAlert(
        session = session,
        title = "Se ha encontrado un problema...",
        text = cond,
        type = "error"
      )
    })
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
