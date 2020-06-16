getListForPickerMethod = function(tag, types) {
  methods <- list.filter(raw.methods, tag %in% tags)
  availableMethods <- list.filter(methods, types %in% type)
  names <- names(availableMethods)
  methods <- list()
  for (name in names) {
    methods[[availableMethods[[name]]$label]] <- name
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