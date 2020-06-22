trainDummyModel <- function(dataset, target) {
  model <- dummyVars(paste(target, "~ ."), data = dataset)
  
  return(model)
}

predictDummyModel <- function(dataset, target, model) {
  y <- dataset[target]
  trainData_mat <- predict(model, newdata = dataset)
  dataset <- data.frame(trainData_mat)
  dataset[target] <- y
  
  return(dataset)
}

trainImputeModel <- function(dataset) {
  model <-
    preProcess(dataset, method = 'bagImpute')
  
  return(model)
}

predictImputeModel <- function(dataset, model) {
  dataset <-
    predict(model, newdata = dataset)
  
  return(dataset)
}

trainCenterModel <- function(dataset, preds_to_center) {
  cols <- dataset %>% select(all_of(preds_to_center))
  model <-
    preProcess(cols, method = c("center", "scale"))
  return(model)
}

predictCenterModel <- function(dataset, model) {
  dataset <- predict(model, dataset)
  
  return(dataset)
}
