basicFormUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("form"))
}

basicForm <- function (input, output, session, data, reload) {
  values <- reactiveValues()
  
  observeEvent(reload(), {
    values$reload <- reload()
  })
  
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
  
  observeEvent(reload(),{
    reset("name")
    reset("description")
    values$reload <- FALSE
  })
  
  return(list(name = reactive({
    input$name
  }),
  description = reactive({
    input$description
  })))
}