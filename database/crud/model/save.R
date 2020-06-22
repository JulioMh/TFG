saveModel <- function(models,
                      target,
                      name,
                      description,
                      user_id,
                      dataset_id,
                      indexs,
                      cols,
                      isPublic) {
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
  rootPath <- dirs$root
  dirs$root <- NULL
  query <- sprintf(
    "INSERT INTO Model (name, description, target, user_id, models, %s,dataset_id, trainRowNumbers, cols, isPublic, rootPath)
    VALUES ('%s', '%s', '%s', %s, '%s','%s', %s, '%s', '%s', %s, '%s')",
    paste(names(tail(dirs, n = 3)), collapse = ", "),
    name,
    description,
    target,
    user_id,
    paste(head(dirs, n = length(dirs) - 3), collapse = ", "),
    paste(tail(dirs, n = 3), collapse = "', '"),
    dataset_id,
    indexs,
    paste(cols, collapse = ", "),
    ifelse(isTRUE(isPublic), 1, 0),
    rootPath
  )
  
  tryCatch({
    rs <- dbSendQuery(db, query)
    dbClearResult(rs)
    id <- dbGetQuery(db, "select last_insert_id();")[1, 1]
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))
  
  return(id)
}

followModel <- function(model_id, user_id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <- sprintf("INSERT INTO Model_User (model_id, user_id) values (%s, %s)",
                   model_id,
                   user_id)
  
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))
}

unfollowModel <- function(model_id, user_id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  query <- sprintf("DELETE FROM Model_User where user_id = %s and model_id = %s",
                   user_id,
                   model_id)
  
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))
}

deleteModel <- function(model_id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  delete_model <- sprintf("DELETE FROM Model where id = %s",
                          model_id)
  
  
  rootPath <- getModelRootPath(model_id)
  
  tryCatch({
    removeFollowers(model_id)
    dbGetQuery(db, delete_model)
    
    unlink(rootPath, recursive = TRUE)
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))
}

removeFollowers <- function(model_id) {
  db <-
    dbConnect(
      MySQL(),
      dbname = databaseName,
      host = options()$mysql$host,
      port = options()$mysql$port,
      user = options()$mysql$user,
      password = options()$mysql$password
    )
  delete_relations <- sprintf("DELETE FROM Model_User where model_id = %s",
                              model_id)
  tryCatch({
    dbGetQuery(db, delete_relations)
  }, error = function(e) {
    stop(safeError(e))
  }, finally = dbDisconnect(db))
}

editModel <- function(new_attributes, isPublic, id) {
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
    "UPDATE Model set %s, isPublic = %s
    WHERE id = %s",
    paste0(names(new_attributes), " = '", new_attributes, "'" , collapse = ", "),
    isPublic,
    id
  )
  tryCatch({
    if (isPublic == 0) {
      removeFollowers(id)
    }
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
    dirs$root <- model_dir
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
