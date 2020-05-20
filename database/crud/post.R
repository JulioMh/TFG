saveData <- function(data, table){
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
    "INSERT INTO %s (%s) VALUES ('%s')",
    table,
    paste(names(data), collapse = ", "),
    paste(data, collapse = "', '")
  )
  tryCatch({
    dbGetQuery(db, query)
  }, error = function(e) {
    res <- FALSE
    stop(safeError(e))
  })
  dbDisconnect(db)
  return(res)
}