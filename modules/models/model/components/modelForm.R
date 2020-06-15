modelFormUI <- function(id){
  ns <- NS(id)
  tagList(
    basicFormUI(ns("basic")),
    switchInput(
      inputId = ns("privacy"),
      size = "mini",
      onLabel = "Publico",
      offLabel = "Privado"
    )
  )
}

modelForm <- function(input, output, session, data, reload){
  attributes <-
    callModule(basicForm, "basic", data, reload)
  
  return(list(
    name = attributes$name ,
    description = attributes$description,
    isPublic = reactive(input$privacy)
  ))
}