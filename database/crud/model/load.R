loadModels <- function(id) {
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

loadModel <- function(id) {
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
    sprintf("select * from Model where id= %s",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
    response <- processResponse(response)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(response)
}

processResponse  <- function(response) {
  attributes <- list()
  
  attributes$name <- response$name
  attributes$description <- response$description
  
  models <- list()
    
  models_dirs <- response$models
  models_dirs <- unlist(strsplit(models_dirs, split = ", "))
  
  for(dir in models_dirs){
    model <- readRDS(dir)
    models[[model$method]] <- model
  }
  
  preProcess <- list()
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
  
  datasets <- list()
  dataset_data <- loadDataset(response$dataset_id)
  
  trainRowNumbers <-
    convertStringToMatrix(response$trainRowNumbers)
  
  datasets$train <- dataset_data$dataset[trainRowNumbers, ]
  datasets$test <- dataset_data$dataset[-trainRowNumbers, ]
  datasets$processed <- datasets$test

  datasets$processed <- prepareDataToPredict(
    dataset = datasets$processed,
    impute_model = preProcess$impute_model,
    dummy_model = preProcess$dummy_model,
    center_model = preProcess$center_model,
    target = preProcess$target
  )
  
  datasets$predictions <-
    doPrediction(models = models, datasets$processed)
  
  return(
    list(
      attributes = attributes,
      cols = unlist(strsplit(response$cols, split = ", ")),
      models = models,
      preProcess = preProcess,
      datasets = datasets
    )
  )
}


convertStringToMatrix <- function(string) {
  rows <- unlist(strsplit(string, split = ", "))
  return(as.matrix(as.numeric(rows)))
}