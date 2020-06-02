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
  query <-
    sprintf("select * from Dataset where user_id= '%s' order by id DESC",
            id)
  tryCatch({
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
