modelFormUI <- function(id){
  ns <- NS(id)
  tagList(
    basicFormUI(ns("basic")),
    uiOutput(ns("privacy_container"))
  )
}

modelForm <- function(input, output, session, data, reload){
  ns <- session$ns
  
  attributes <-
    callModule(basicForm, "basic", data, reload)
  
  output$privacy_container <- renderUI({
    print(data()$isPublic)
    switchInput(
      inputId = ns("privacy"),
      size = "mini",
      onLabel = "Publico",
      offLabel = "Privado",
      value = isTRUE(data()$isPublic > 0)
    )
  })
  
  return(list(
    name = attributes$name ,
    description = attributes$description,
    isPublic = reactive(input$privacy)
  ))
}