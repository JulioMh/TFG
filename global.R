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

source("database/crud/post.R")
source("database/crud/get.R")

source("modules/dataset/form.R")
source("modules/dataset/dataset.R")
source("modules/table.R")

source("views/user/modules/datasets/crud/listDataset.R")
source("views/user/modules/datasets/crud/editDataset.R")

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
