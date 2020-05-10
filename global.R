################################
# LIBRARY
################################
library(shiny)
library(shinyjs)
library(shinythemes)
library(DBI)
library(RMySQL)
library(stringr)

################################
# MODULES
################################
source("views/guest/guest.R")
source("views/user/user.R")
source("views/guest/modules/auth/signUp.R")
source("views/guest/modules/auth/logIn.R")
source("views/guest/tabs/ui/signUpTab.R")
source("views/guest/tabs/ui/logInTab.R")
source("views/guest/tabs/ui/homeTab.R")
source("views/user/tabs/ui/myModelsTab.R")
source("views/user/tabs/ui/myDatasetsTab.R")
source("views/user/tabs/ui/communityTab.R")
source("views/user/tabs/ui/perfilTab.R")
source("views/user/tabs/ui/logOutTab.R")
source("views/user/modules/datasets/newDataset.R")
source("views/user/modules/datasets/myDatasets.R")

################################
# DATABASE
################################
options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3308,
  "user" = "root",
  "password" = "root"
))
databaseName <- "db"