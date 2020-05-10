userUI <- function(id) {
  ns <- NS(id)
  tagList(
    navbarPage(
      title = "BestAppEver",
      theme = shinytheme("cosmo"),
      myModelsTab,
      myDatasetsTab,
      communityTab,
      perfilTab,
      logOutTab
    ))
}
