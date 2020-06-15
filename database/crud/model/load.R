loadOwnModels <- function(id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf(
      "select
              m.id,
              m.user_id,
              m.name,
              m.description,
              d.name as dataset,
              m.target
            from Model m join Dataset d on m.dataset_id = d.id
            where
              m.user_id = %s
            order by id DESC;",
      id
    )
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(response)
}

loadNotOwnModels <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf(
      "select
              m.id,
              m.user_id,
              m.name,
              m.description,
              d.name as dataset,
              m.target
            from Model m join Dataset d on m.dataset_id = d.id
            where
              m.user_id != %s
            order by id DESC;",
      id
    )
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(response)
}

getModels <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select models, target from Model where id= %s",
            id)
  models <- list()
  
  tryCatch({
    response <- dbGetQuery(db, query)
    
    models_dirs <- response$models
    models_dirs <- unlist(strsplit(models_dirs, split = ", "))
    
    for(dir in models_dirs){
      model <- readRDS(dir)
      models[[model$method]] <- model
    }
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(models)
}

getModelsAttributes <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select name, description from Model where id= %s",
            id)
  attributes <- list()
  
  tryCatch({
    response <- dbGetQuery(db, query)
    
    attributes$name <- response$name
    attributes$description <- response$description
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(attributes)
}

getModelDataset <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select dataset_id, trainRowNumbers, cols, target from Model where id= %s",
            id)
  datasets <- list()
  
  tryCatch({
    response <- dbGetQuery(db, query)
    
    dataset_data <- loadDataset(response$dataset_id)
    
    trainRowNumbers <-
      convertStringToMatrix(response$trainRowNumbers)
    
    datasets$train <- dataset_data$dataset[trainRowNumbers, ]
    datasets$test <- dataset_data$dataset[-trainRowNumbers, ]
    
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(datasets)
}

getModelPreds <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select cols from Model where id= %s",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
    cols <- unlist(strsplit(response$cols, split = ", "))
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(cols)
}

getModelOwner <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select user_id from Model where id= %s",
            id)
  
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(response$user_id)
}

getModelTarget <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select target from Model where id= %s",
            id)
  
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(response$target)
}

getPreModels <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <-
    sprintf("select dummy_model, center_model, impute_model, target from Model where id= %s",
            id)
  preProcess <- list()
  
  tryCatch({
    response <- dbGetQuery(db, query)
    
    preProcess$target <- response$target
    
    preProcess$dummy_model <- readRDS(response$dummy_model)
    
    if(response$center_model == ""){
      preProcess$center_model <- ""
    }else{
      preProcess$center_model <- readRDS(response$center_model)
    }
    
    if(response$impute_model == ""){
      preProcess$impute_model <- ""
    }else{
      preProcess$impute_model <- readRDS(response$impute_model)
    }
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(preProcess)
}

convertStringToMatrix <- function(string) {
  rows <- unlist(strsplit(string, split = ", "))
  return(as.matrix(as.numeric(rows)))
}