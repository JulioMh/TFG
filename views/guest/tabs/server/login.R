output$missing <- renderText(
  validate(
    need(input$userName != '', label= "User name"),
    need(input$passwd != '', label= "Password")
  )
)
output$user <- renderText(session$userData$Logged)
observeEvent(input$login, isolate({
  req(input$userName)
  req(input$passwd)
  res <- performanceLogIn(input$userName, input$passwd, session)
  output$res <- renderText(res)
  session$userData$Logged = reactive({res == "Welcome!"})
}))

performanceLogIn <- function(userName, password, session){
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
    "select * from User where userName= '%s' and password = '%s'",
    userName,
    password
  )
  response <- dbGetQuery(db,query)
  if(nrow(response)==0){
    res <- "Wrong user or password"
  }else{
    res <- "Welcome!"
  }
  dbDisconnect(db)
  return(res)
}