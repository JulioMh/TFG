guestUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "Modelando",
      theme = shinytheme("flatly"),
      tabPanel(
        "Inicio",
        br(),
        br(),
        HTML(
          "<h1><center>BIENVENIDO A <b>MODELANDO</b>...</center></h1>"
        ),
        br(),
        hr(),
        br(),
        br(),
        br(),
        column(width = 12,
               br(), br(), br(), br(),
               wellPanel(
                 HTML("<h1><b>Modelando</b></h1>"),
                 HTML(
                   "<h4><b>Modelando</b> es un proyecto que anima a todo aquel que trabaje en el campo del medicina a
                                    que le de una oportunidad al machine learning y entrene por él mismo un modelo
                                    con el que poder entender mejor como se relacionan los datos de los que disponga y 
                                    conseguir así mejorar sus análisis
                               .</h4>"
                 )
               ))
      ),
      tabPanel("Iniciar sesión",
               logInUI(ns("login"))),
      tabPanel("Registrarse",
               signUpUI(ns("signup")))
    )
  )
}

guest <- function(input, output, session){
  callModule(logIn, "login")
  callModule(signUp, "signup")
}