shinyServer(function(input, output, session){
  logged = FALSE
  USER <- reactiveValues(logged = logged)
  output$mode <- renderUI({
    if(USER$logged){
      userUI("user")
    }else{
      callModule(signUp, "signup")
      callModule(logIn, "login", USER)
      guestUI("guest")
    }
  })
  
  
  
  #callModule(user, "user")
})