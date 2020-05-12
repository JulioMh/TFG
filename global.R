################################
# LIBRARY
################################
library(shiny)
library(shinyjs)
library(shinythemes)
library(DBI)
library(DT)
library(RMySQL)
library(stringr)

################################
# MODULES
################################
source("views/guest/guest.R")
source("views/user/user.R")
source("views/guest/modules/auth/signUp.R")
source("views/guest/modules/auth/logIn.R")
source("views/user/modules/datasets/crud/newDataset.R")
source("views/user/modules/datasets/crud/listDataset.R")
source("views/user/modules/datasets/crud/editDataset.R")
source("views/user/modules/datasets/crud/formDataset.R")
source("views/user/modules/datasets/datasets.R")

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
