doPrediction <- function(models, dataset) {
  predictions <- list()
  for(model in models){
    predictions[[model$method]] <- predict(model, newdata = dataset)
  }
  
  return(predictions)
}
