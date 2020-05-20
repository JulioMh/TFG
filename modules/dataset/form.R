formDatasetUI <- function (id) {
  ns <- NS(id)
  tagList(br(),
          sidebarPanel(uiOutput(ns("form")),
                       br(),
                       textOutput(ns("err"))),
          mainPanel(DT::dataTableOutput(ns("tablePrev"))))
}

formDataset <- function (input, output, session, data) {
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
  
  output$submit <- renderUI({
    validate(need(input$dataset != "", label = "File"),
             need(input$name != "", label = "Name"))
    return(div(style = "text-align: center;",
               actionButton(session$ns("submit"), "Save")))
  })
  
  output$tablePrev <- DT::renderDataTable({
    req(input$dataset$datapath)
    tryCatch({
      dataset <- reactive(read.csv(input$dataset$datapath))
    },
    error = function(e) {
      stop(safeError(e))
    })
    
    return(DT::datatable(dataset(),
                         options = list(
                           lengthMenu = list(c(5, 10, -1), c('5', '10', 'All')),
                           pageLength = 10
                         )))
  })

  return(
    list(
      "name" = reactive({input$name}),
      "description" = reactive({input$description}),
      "dataset" = dataset(),
      "user_id" = session$userData$user$id,
      "do_insert" = reactive({
        input$submit
      })
    )
  )
}