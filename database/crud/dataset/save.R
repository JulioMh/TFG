uploadDataset <- function(name, description, path, user_id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  dataset <- saveDatasetFile(path, user_id)
  query <- sprintf(
    "INSERT INTO dataset (name, description, dataset, user_id, rootPath) VALUES ('%s', '%s', '%s', %s, '%s')",
    name,
    description,
    dataset$dataset_dir,
    user_id,
    dataset$root
  )
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    res <- safeError(e)
    stop(safeError(e))
  }, finally = dbDisconnect(db))
}

deleteDataset <- function(id){
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <- sprintf(
    "DELETE FROM dataset where id = %s",
    id
  )
  
  rootPath <- getDatasetRootPath(id)
  
  tryCatch({
    dbGetQuery(db, query)
    
    unlink(rootPath, recursive = TRUE)
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db)) 
}

editDataset <- function(id, name, description) {
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
    sprintf("UPDATE dataset SET name = '%s', description = '%s' where id = %s",
            name,
            description,
            id)
  
  
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    res <- safeError(e)
    stop(safeError(e))
  }, finally = dbDisconnect(db))
}

saveDatasetFile <- function(file_path, user_id) {
  root <- normalizePath(".")
  
  user_dir <- file.path('database', 'files', user_id)
  dir.create(user_dir, showWarnings = FALSE)
  datasets_dir <-
    file.path('database', 'files', user_id, "datasets")
  dir.create(datasets_dir, showWarnings = FALSE)
  
  repeat {
    file_id <- runif(1)
    dataset_dir <-
      file.path(datasets_dir, file_id)
    if (dir.create(dataset_dir, showWarnings = FALSE)) {
      break
    }
  }
  root <- dataset_dir
  if (file.copy(file_path, normalizePath(dataset_dir), copy.mode = TRUE)) {
    dataset_dir <- paste0(dataset_dir, "/", basename(file_path))
  } else{
    dataset_dir <- FALSE
  }
  
  return(list(dataset_dir = dataset_dir,
              root = root))
}
