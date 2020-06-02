modelFormUI <- function (id) {
  ns <- NS(id)
  uiOutput(ns("form"))
}

modelForm <- function (input, output, session, data) {
  values <-
    reactiveValues(name = "",
                   description = "")
  
  output$form <- renderUI({
    tagList(
      textInput(
        session$ns("name"),
        tagList(icon("signature"), "Nombre:"),
        value = if (is.null(data))
          ""
        else
          data$name
      ),
      textAreaInput(
        session$ns("description"),
        tagList(icon("signature"), "Descripcion:"),
        value = if (is.null(data))
          ""
        else
          data$description
      )
    )
  })
  
  return(list(name = reactive({
    input$name
  }),
  description = reactive({
    input$description
  })))
}