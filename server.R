shinyServer(function(input, output, session){
  logged = FALSE
  USER <- reactiveValues(logged = logged, id=0)
  output$mode <- renderUI({
    if(USER$logged){
      callModule(newDataset, "fromDataset", USER)
      callModule(myDatasets, "mydatasets", USER)
      userUI("user")
    }else{
      callModule(signUp, "signup", USER)
      callModule(logIn, "login", USER)
      guestUI("guest")
    }
  })
})