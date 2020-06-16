shinyServer(function(input, output, session) {
  values <- reactiveValues(aux = 0)
  
  session$userData$user <-
    reactiveValues(id = -1,
                   deleted_model = FALSE,
                   deleted_dataset = 0)
  
  observeEvent(session$userData$user$id, {
    values$aux <- values$aux + 1
  })
  
  output$mode <- renderUI({
    if (session$userData$user$id > 0) {
      callModule(user, paste0("user-", values$aux))
      userUI(session$ns(paste0("user-", values$aux)))
    } else{
      callModule(guest, paste0("guest-", values$aux))
      guestUI(session$ns(paste0("guest-", values$aux)))
    }
  })
})