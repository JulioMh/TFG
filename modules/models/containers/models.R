modelsUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("mode"))
}

models <- function (input, output, session, load, train_id) {
  values <-
    reactiveValues(show_details = FALSE,
                   reload = FALSE,
                   count = 0)
  
  model_id <-
    callModule(listServer, "list", load, reactive({
      values$reload
    }))
  
  observeEvent(input$cancel, {
    values$show_details <- !values$show_details
    values$reload <- TRUE
  })
  
  observeEvent(session$userData$user$deleted_model, {
    if (isTRUE(session$userData$user$deleted_model)) {
      values$show_details <- !values$show_details
      values$reload <- TRUE
    }
  })
  
  observeEvent(model_id(), {
    req(model_id())
    values$count <- values$count + 1
    values$id <- model_id()
    values$show_details <- !values$show_details
    values$reload <- FALSE
    callModule(model,
               paste0("model-", values$id, "-", values$count),
               values$id)
  })
  
  observeEvent(train_id(), {
    values$count <- values$count + 1
    values$id <- train_id()
    values$reload <- FALSE
    values$show_details <- TRUE
    callModule(model,
               paste0("model-", values$id, "-", values$count),
               values$id)
  })
  
  output$mode <- renderUI({
    if (values$show_details) {
      tagList(
        actionBttn(
          inputId = session$ns("cancel"),
          icon = icon("arrow-circle-left"),
          style = "pill",
          color = "warning",
          size = "xs"
        ),
        br(),
        br(),
        modelUI(session$ns(
          paste0("model-", values$id, "-", values$count)
        ))
      )
    } else{
      listUI(session$ns("list"))
    }
  })
}