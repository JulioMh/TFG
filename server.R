shinyServer(function(input, output, session){
  session$userData$user <- reactiveValues(id = -1, deleted_model = FALSE)
  
  output$mode <- renderUI({
    if(session$userData$user$id > 0){
      callModule(user, "user")
      userUI("user")
    }else{
      callModule(guest, "guest")
      guestUI("guest")
    }
  })
})