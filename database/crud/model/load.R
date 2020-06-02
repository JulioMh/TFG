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
  
  models$rf <- readRDS(response$rf)
  models$svm <- readRDS(response$svm)
  models$xgbdart <- readRDS(response$xgbdart)
  
  preProcess <- list()
  preProcess$dummy_model <- readRDS(response$dummy_model)
  preProcess$target <- response$target
  preProcess$center_model <-
    ifelse(
      is.na(response$center_model),
      response$center_model,
      readRDS(response$center_model)
    )
  preProcess$impute_model <-
    ifelse(
      is.na(response$impute_model),
      response$impute_model,
      readRDS(response$impute_model)
    )
  
  datasets <- list()
  dataset_data <- loadDataset(response$dataset_id)
  
  trainRowNumbers <-
    convertStringToMatrix(response$trainRowNumbers)
  
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