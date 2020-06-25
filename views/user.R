userUI <- function(id) {
  ns <- NS(id)
  tagList(
    useShinyjs(),
    useSweetAlert(),
    
    navbarPage(
      title = "Modelando",
      id = ns("user_nav"),
      theme = shinytheme("flatly"),
      tabPanel(
        "Mis modelos",
        tabsetPanel(
          id = ns("my_models"),
          tabPanel("Modelos propios",
                   br(),
                   modelsUI(ns("own_models"))),
          tabPanel("Modelos guardados",
                   br(),
                   modelsUI(ns("saved_models"))),
          tabPanel("Entrena un modelo",
                   br(),
                   trainUI(ns("train")))
        )
      ),
      tabPanel("Mis datasets",
               datasetsUI(ns("datasets"))),
      tabPanel("Comunidad",
               modelsUI(ns("community"))),
      tabPanel("Cerrar sesión")
    )
  )
}

user <- function(input, output, session) {
  callModule(datasets, "datasets")
  callModule(models, "community", loadNotOwnModels, reactive(NULL))
  callModule(models, "saved_models", loadSavedModels, reactive(NULL))
  train_id <- callModule(trainServer, "train")
  callModule(models, "own_models", loadOwnModels, train_id)
  
  observeEvent(train_id(), {
    updateTabsetPanel(session = session,
                      inputId = "my_models",
                      selected = "Modelos propios")
  })
  
  
  observeEvent(input$user_nav, {
    if (input$user_nav == "Cerrar sesión") {
      confirmSweetAlert(
        session = session,
        inputId = "confirm",
        type = "warning",
        title = "¿Estas seguro?"
      )
    }
  })
  
  observeEvent(input$confirm, {
    if (isTRUE(input$confirm)) {
      session$userData$user$id <- 0
    } else{
      updateNavbarPage(session = session,
                       inputId = "user_nav",
                       selected = "Mis modelos")
    }
  })
}    