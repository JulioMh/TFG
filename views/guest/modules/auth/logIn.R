logInUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "login",
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        h2("LOG IN", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
        textInput(
          ns("userName"),
          placeholder = "Username",
          label = tagList(icon("user"), "Username")
        ),
        passwordInput(
          ns("passwd"),
          placeholder = "Password",
          label = tagList(icon("unlock-alt"), "Password")
        ),
        br(),
        uiOutput(ns("submit")),
        br(),
        br(),
        textOutput(ns("res")),
        br()
      )
    )
  )
}

logIn <- function(input, output, session, USER) {
  output$submit <- renderUI({
    validate(
      need(input$userName != '', label = "User name"),
      need(input$passwd != '', label = "Password")
    )
    return(div(
      style = "text-align: center;",
      actionButton(
        session$ns("login"),
        "LOG IN",
        style = "color: white;
                                 padding: 10px 15px; width: 150px; cursor: pointer;
                                 font-size: 18px; font-weight: 600;"
      ),
      br(),
    ))
  })
  observeEvent(input$login, {
    res <-
      performanceLogIn(isolate(input$userName), isolate(input$passwd))
    if (is.null(nrow(res))) {
      output$res <- renderText(res)
    }else{
      USER$logged <- TRUE
      USER$id <- res$id
    }
  })
}

performanceLogIn <- function(userName, password) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select * from User where userName= '%s' and password = '%s'",
            userName,
            password)
  response <- dbGetQuery(db, query)
  if (nrow(response) == 0) {
    res <- "Wrong user or password"
  } else{
    res <- response
  }
  dbDisconnect(db)
  return(res)
}