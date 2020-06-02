################################
# LIBRARY
################################
library(shiny)
library(shinyjs)
library(shinythemes)
library(shinyWidgets)
library(DBI)
library(RMySQL)
library(stringr)
library(tidyverse)
library(caret)
library(doSNOW)


################################
# MODULES
################################
source("modules/models/caret/train.R")
source("modules/models/caret/prepareData.R")
source("modules/models/caret/preProcess.R")
source("modules/models/caret/predict.R")

source("views/guest/modules/auth/signUp.R")
source("views/guest/modules/auth/logIn.R")

source("views/guest/guest.R")

source("modules/datasets/form.R")
source("modules/datasets/datasets.R")
source("modules/models/train/train.R")
source("modules/models/train/select_dataset.R")
source("modules/models/train/advance_mode.R")
source("modules/models/train/form.R")
source("modules/models/models.R")
source("modules/models/model/model.R")
source("modules/models/model/summary.R")
source("modules/models/model/compareDatasets.R")
source("modules/models/model/details.R")
source("modules/list.R")
source("modules/table.R")
source("modules/basicForm.R")

source("views/user.R")

source("database/crud/dataset/save.R")
source("database/crud/dataset/load.R")
source("database/crud/model/save.R")
source("database/crud/model/load.R")

################################
# DATABASE
################################
options(mysql = list(
  "host" = "127.0.0.1",
  "port" = 3308,
  "user" = "root",
  "password" = "root"
),
encoding = 'UTF-8')
databaseName <- "db"

titanic <- read.csv("database/files/1/datasets/0.388745460659266/0.csv")
rock <- read.csv("database/files/1/datasets/0.330122414045036/0.csv")
insurance <- read.csv("database/files/1/datasets/0.816763020586222/0.csv")
