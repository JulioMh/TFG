doPrediction <- function(models, dataset) {
  return(list(
    predict_RF = predict(models$rf, newdata = dataset),
    predict_XGBDART = predict(models$xgbdart, newdata =  dataset),
    predict_SVM = predict(models$svm, newdata = dataset)
  ))
}
