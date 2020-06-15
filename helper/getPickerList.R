getListForPickerMethod = function(tag) {
  raw_methods <- list.filter(getModelInfo(), tag %in% tags)
  names <- names(raw_methods)
  methods <- list()
  for (name in names) {
    methods[[raw_methods[[name]]$label]] <- name
  }
  return(methods)
}

getPickerList <- function(datasets) {
  list_id <- as.list(datasets$id)
  names(list_id) <- datasets$name
  return(list_id)
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