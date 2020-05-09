################################
# LIBRARY
################################
library(shiny)
library(shinythemes)
library(DBI)
library(RMySQL)
library(stringr)

################################
# MODULES
################################
source("guest/guest.R")
source("user/user.R")
source("guest/modules/auth/signUp.R")
source("guest/modules/auth/logIn.R")
source("guest/tabs/ui/signUpTab.R")
source("guest/tabs/ui/logInTab.R")
source("guest/tabs/ui/homeTab.R")

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