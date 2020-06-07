saveDataset <- function(data) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  res <- TRUE
  if(data$id==""){
    data$id <- NULL
    data$dataset <- saveDatasetFile(data$dataset, data$user_id)
    query <- sprintf(
      "INSERT INTO Dataset (%s) VALUES ('%s')",
      paste(names(data), collapse = ", "),
      paste(data, collapse = "', '")
    )
  }else{
    query <- sprintf(
      "UPDATE Dataset SET name = '%s', description = '%s' where id = %s",
      data$name, data$description, data$id
    )
  }
  
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    res <- safeError(e)
    stop(safeError(e))
  }, finally = dbDisconnect(db)) 
  
  return(res)
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
  if (file.copy(file_path, normalizePath(dataset_dir), copy.mode = TRUE)) {
    dataset_dir <- paste0(dataset_dir, "/", basename(file_path))
  } else{
    dataset_dir <- FALSE
  }
  
  return(dataset_dir)
}
