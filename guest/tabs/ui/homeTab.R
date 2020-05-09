homeTab <- tabPanel(
  "Home",
  br(), br(),
  HTML(
    "<h1><center>WELCOME TO THE <b>BEST</b> APP EVER MADE...</center></h1>"
  ),
  br(),
  hr(),
  br(),
  br(),
  br(),
  column(width = 12,
         br(), br(), br(), br(),
         wellPanel(
           HTML("<h1><b>BestApp</b></h1>"),
           HTML("<h4><b>BestApp</b> es un proyecto que anima a todo aquel que trabaje en el campo del medicina a 
                                    que le de una oportunidad al machine learning y entrene por él mismo un modelo
                                    con el que poder apoyarse a la hora de sacar conclusiones
                               .</h4>")
         )
  )
)