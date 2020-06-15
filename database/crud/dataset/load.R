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

getDataset <- function(id){
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
    sprintf("select dataset from Dataset where id= %s",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =  dbDisconnect(db))
  
  dataset <- read.csv(response$dataset)
  return(dataset)
}

isRelated <- function(id){
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
            m.id
            from Model m
            where m.dataset_id = %s
            order by id DESC;",
      id
    )
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(isTRUE(response$id > 0))
}

getDatasetRootPath <- function(id){
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
    sprintf("select rootPath from Dataset where id= %s",
            id)
  
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =   dbDisconnect(db))
  return(response$rootPath)
}


getDatasetAttributes <- function(id){
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
    sprintf("select name, description from Dataset where id= %s",
            id)
  tryCatch({
    response <- dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally =  dbDisconnect(db))

  return(list(
    name = response$name,
    description = response$description
  ))
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