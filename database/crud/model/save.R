saveModel <- function(models,
                      target,
                      name,
                      description,
                      user_id,
                      dataset_id,
                      indexs,
                      cols) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  
  dirs <- saveModelFiles(models, user_id)

  query <- sprintf(
    "INSERT INTO Model (name, description, target, user_id, models, %s,dataset_id, trainRowNumbers, cols)
    VALUES ('%s', '%s', '%s', %s, '%s','%s', %s, '%s', '%s')",
    paste(names(tail(dirs, n = 3)), collapse = ", "),
    name,
    description,
    target,
    user_id,
    paste(head(dirs, n = length(dirs) - 3), collapse = ", "),
    paste(tail(dirs, n = 3), collapse = "', '"),
    dataset_id,
    indexs, 
    paste(cols, collapse =", ")
  )

  tryCatch({
    rs <- dbSendQuery(db, query)
    dbClearResult(rs)
    id <- dbGetQuery(db, "select last_insert_id();")[1,1]
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))  
  
  return(id)
}

editModel <- function(new_attributes, id) {
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
  query <- sprintf(
    "UPDATE Model set %s
    WHERE id = %s",
    paste0(names(new_attributes), " = '", new_attributes, "'" , collapse = ", "),
    id
  )
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))
}

saveModelFiles <-
  function(models,
           user_id) {
    user_dir <- file.path('database', 'files', user_id)
    dir.create(user_dir, showWarnings = FALSE)
    models_dir <-
      file.path('database', 'files', user_id, "model")
    dir.create(models_dir, showWarnings = FALSE)
    
    repeat {
      file_id <- runif(1)
      model_dir <-
        file.path(models_dir, file_id)
      if (dir.create(model_dir, showWarnings = FALSE)) {
        break
      }
    }
    
    dirs <- list()
    index = 1
    while (index <= length(models)) {
      name <- names(models[index])
      model <- models[[index]]
      if (model != "") {
        dir <- file.path(model_dir, name)
        dir.create(dir, showWarnings = FALSE)
        file_path <- paste0(dir, "/", name, ".rds")
        saveRDS(model, file_path)
        dirs[name] <- file_path
      } else{
        dirs[name] <- ""
      }
      index = index + 1
    }
    
    return(dirs)
  }
