guestUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "BestAppEver",
      theme = shinytheme("simplex"),
      homeTab,
      logInTab,
      signUpTab
  ))
}
