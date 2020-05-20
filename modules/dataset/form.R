formDatasetUI <- function (id) {
  ns <- NS(id)
  tagList(uiOutput(ns("form")),
          br(),
          textOutput(ns("err")))
}

formDataset <- function (input, output, session, data) {
  result <- reactiveValues()
  output$form <- renderUI({
    tagList(
      fileInput(
        session$ns("dataset"),
        tagList(icon("file-medical-alt"), "Sube el conjunto de datos:"),
        multiple = FALSE,
        accept = c(".json",
                   ".csv")
      ),
      textInput(
        session$ns("name"),
        tagList(icon("signature"), "Nombre:"),
        value = data$name
      ),
      textAreaInput(
        session$ns("description"),
        tagList(icon("signature"), "Descripcion:"),
        value = data$description
      ),
      uiOutput(session$ns("submit"))
    )
  })
  
  observeEvent(input$submit, {
    submitedData <- list(
      "name" = input$name,
      "description" = input$description,
      "dataset" = input$dataset$datapath,
      "user_id" = session$userData$user$id
    )
    res <- saveData(submitedData, "Dataset")
    if(res){
      output$err <- renderText(res)  
    }
  })
  
  output$submit <- renderUI({
    validate(need(input$dataset != "", label = "File"),
             need(input$name != "", label = "Name"))
    return(div(style = "text-align: center;",
               actionButton(session$ns("submit"), "Save")))
  })
  
  return(list(
    path_dataset = reactive({input$dataset$datapath})
  ))
}