basicFormUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("form"))
}

basicForm <- function (input, output, session, data, reload_data) {
  values <-
    reactiveValues(name = "",
                   description = "",
                   reload = FALSE)
  
  output$form <- renderUI({
    tagList(
      textInput(
        session$ns("name"),
        tagList(icon("signature"), "Nombre:"),
        value = ifelse(is.null(data), "", data()$name)
      ),
      textAreaInput(
        session$ns("description"),
        tagList(icon("signature"), "Descripcion:"),
        value = ifelse(is.null(data), "", data()$description)
      )
    )
  })
  
  
  observeEvent(reload_data(), {
    req(reload_data())
    if (reload_data()) {
      reset("name")
      reset("description")
    }
  })
  
  return(list(name = reactive({
    input$name
  }),
  description = reactive({
    input$description
  })))
}