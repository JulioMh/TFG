guestUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "BestAppEver",
      theme = shinytheme("cosmo"),
      homeTab,
      logInTab,
      signUpTab
  ))
}
