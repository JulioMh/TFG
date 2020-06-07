loadDatasets <- function(id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  tryCatch({
    query <-
      sprintf("select id, dataset, name, description from Dataset where user_id= '%s' order by id DESC",
              id)
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(response)
}

loadDataset <- function(id) {
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
    sprintf("select * from Dataset where id= %s",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  if (nrow(response) == 0) {
    data <- FALSE
  } else{
    data <- list(
      "name" = response$name,
      "description" = response$description,
      "id" = response$id,
      "dataset" = read.csv(response$dataset)
    )
  }
  return(data)
}


getAvailableDatasetsToPredict <- function(preds, user_id) {
  response <- loadDatasets(user_id)
  matched <- lapply(response$dataset, function(path) {
    dataset <- read.csv(path)
    matched <- availableToPredict(preds, dataset)
  })
  response$matched <- matched
  datasets <- response[response$matched != FALSE,]
  return(datasets)
}

availableToPredict <- function(preds, dataset){
  return(!anyNA(match(preds, colnames(dataset))))
}