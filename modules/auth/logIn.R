logInUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "login",
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        h2("Iniciar sesión", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
        textInput(
          ns("email"),
          placeholder = "Email",
          label = tagList(icon("envelope"), "Email")
        ),
        passwordInput(
          ns("passwd"),
          placeholder = "Contraseña",
          label = tagList(icon("unlock-alt"), "Contraseña")
        ),
        br(),
        uiOutput(ns("submit"))
      )
    )
  )
}

logIn <- function(input, output, session) {
  output$submit <- renderUI({
    validate(
      need(input$email != '', message = "Debes introducir un correo electrónico"),
      need(input$passwd != '', message = "Debes introducir una contraseña")
    )
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("login"),
        label = "Iniciar sesión",
        style = "stretch",
        color = "success"
      ),
      br(),
    ))
  })
  observeEvent(input$login, {
    res <-
      performanceLogIn(input$email)
    if (isTRUE(res$id > 0) && sha256(input$passwd) == res$password) {
      session$userData$user$id <- res$id
    } else{
      sendSweetAlert(session = session,
                     title = "Email o contraseña incorrecta",
                     type = "error")
    }
  })
}

performanceLogIn <- function(email) {
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
    sprintf("select id, password from User where email= '%s'",
            email)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))
  return(response)
}