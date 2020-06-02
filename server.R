shinyServer(function(input, output, session){
  session$userData$user <- reactiveValues(id = -1, dataset="")
  observeEvent(session$userData$user$id, {
    if(session$userData$user$id > 0){
      output$mode <- renderUI(userUI("user"))    
      callModule(user, "user")
    }else{
      output$mode <- renderUI(guestUI("guest"))    
      callModule(guest, "guest")
    }
  })
})