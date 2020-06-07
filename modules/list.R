listUI <- function (id) {
  ns <- NS(id)
  tagList(
    actionBttn(
      inputId = ns("reload"),
      label = NULL,
      style = "material-circle",
      color = "default",
      icon = icon("redo"),
      size = "xs"
    ),
    br(),
    tableUI(ns("table"))
  )
}

listServer <- function (input, output, session, load, reload) {
  values <-
    reactiveValues(list = load(session$userData$user$id), id = NULL)
  selected <- callModule(table, "table", reactive({
    values$list[3:ncol(values$list)]
  }))

  observeEvent(reload(),{
    if(reload()){
      values$list <- load(session$userData$user$id)
      values$id <- NULL
    }
  })
  
  observeEvent(input$reload, {
    values$list <- load(session$userData$user$id)
    values$id <- NULL
  })
  
  observeEvent(selected$index(), {
    req(selected$index())
    values$id <- values$list[selected$index(), ]$id
  })
  
  return(reactive({values$id}))
}