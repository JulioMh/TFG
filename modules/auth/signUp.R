signUpUI <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      id = "signup",
      style = "width: 500px; max-width: 100%; margin: 0 auto; padding: 20px;",
      wellPanel(
        h2("Registro", class = "text-center", style = "padding-top: 0;color:#333; font-weight:600;"),
        textInput(
          ns("userName"),
          placeholder = "Nombre de usuario",
          label = tagList(icon("user"), "Nombre de usuario")
        ),
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
        passwordInput(
          ns("confPasswd"),
          placeholder = "Confirmar contraseña",
          label = tagList(icon("unlock-alt"), "Confirmar contraseña")
        ),
        br(),
        uiOutput(ns("submit"))
      )
    )
  )
}

signUp <- function(input, output, session) {
  emailRejex <-
    "^[a-zA-Z0-9.!#$%&’*+/=?^_`{|}~-]+@[a-zA-Z0-9-]+(?:\\.[a-zA-Z0-9-]+)*$"
  passRejex <- "^(?=.*[a-z])(?=.*[A-Z])(?=.*\\d)[a-zA-Z\\d]{6,}$"
  
  output$submit <- renderUI({
    validate(
      need(input$userName != "", message = "Debes introducir un nombre de usuario"),
      need(str_detect(input$email, regex(emailRejex)), message = "El formato del email no es correcto "),
      need(str_detect(input$passwd, regex(passRejex)), message = "La contraseña debe tener al menos 6 caracteres, un número, una mayuscula y una minuscula"),
      need(input$passwd == input$confPasswd, message = "Las contraseñas deben coincidir")
    )
    return(div(
      style = "text-align: center;",
      actionBttn(
        inputId = session$ns("signup"),
        label = "Registrarse", 
        style = "stretch",
        color = "success"
      ),
      br(),
    ))
  })
  
  observeEvent(input$signup, {
    data <- list(
      "username" = input$userName,
      "password" = hashpw(input$passwd),
      "email" = input$email
    )
    tryCatch({
      session$userData$user$id <- performanceSignUp(data)      
    }, error = function(cond){
      sendSweetAlert(
        session = session,
        title = "Este correo electrónico ya ha sido registrado.",
        type = "error"
      )
    })

  })
}

performanceSignUp <- function(data) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <- sprintf(
    "INSERT INTO %s (%s) VALUES ('%s')",
    "User",
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  tryCatch({
    rs <- dbSendQuery(db, query)
    dbClearResult(rs)
    id <- dbGetQuery(db, "select last_insert_id();")[1,1]
  }, error = function(e) {
    stop(safeError(e))
  },finally = dbDisconnect(db))
  return(id)
  
}