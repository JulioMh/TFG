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
library(rlist)
library(bcrypt)

################################
# MODULES
################################
source("modules/models/caret/train.R")
source("modules/models/caret/prepareData.R")
source("modules/models/caret/preProcess.R")
source("modules/models/caret/predict.R")

source("modules/auth/signUp.R")
source("modules/auth/logIn.R")

source("views/guest.R")

source("modules/datasets/components/dataset.R")

source("modules/datasets/funcs/editDataset.R")
source("modules/datasets/funcs/uploadDataset.R")

source("modules/datasets/containers/datasets.R")
source("modules/datasets/containers/selectDataset.R")

source("modules/models/train/train.R")
source("modules/models/train/setUpTrain.R")
source("modules/models/train/advanceMode.R")

source("modules/models/containers/models.R")

source("modules/models/model/model.R")
source("modules/models/model/use.R")
source("modules/models/model/components/modelForm.R")
source("modules/models/model/components/summary.R")
source("modules/models/model/components/details.R")

source("modules/basic/list.R")
source("modules/basic/table.R")
source("modules/basic/form.R")

source("views/user.R")

source("helper/getPickerList.R")
source("helper/getPreds.R")

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

################################
# LIST
################################
rf <- list.filter(getModelInfo(), "Random Forest" %in% tags)
nnt <- list.filter(getModelInfo(), "Neural Network" %in% tags)
tbm <- list.filter(getModelInfo(), "Tree-Based Model" %in% tags)
raw.methods <- c(rf, nnt, tbm)


