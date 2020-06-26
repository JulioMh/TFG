prepareDataForTraining <-
  function(dataset,
           preds,
           preds_to_center,
           do_pred_na,
           target) {
    dataset <-
      dataset %>% select(all_of(preds), all_of(target))
    
    if('?' %in% dataset[[target]]){
      dataset <- dataset[!dataset[[target]] %in% c("?"), ]
      
      dataset[[target]] <- factor(dataset[[target]]) 
    }
    
    trainRowNumbers <-
      createDataPartition(dataset[[target]],
                          times = 1,
                          p = 0.8,
                          list = FALSE)
    
    trainData <- dataset[trainRowNumbers, ]
    
    testData <- dataset[-trainRowNumbers, ]
    
    if (!is.null(do_pred_na) && do_pred_na) {
      impute_model <- trainImputeModel(trainData)
      trainData <- predictImputeModel(trainData, impute_model)
    } else{
      impute_model <- ""
    }
    dummy_model <- trainDummyModel(trainData, target)
    trainData <- predictDummyModel(trainData, target, dummy_model)
    
    if (!is.null(preds_to_center)) {
      center_model <- trainCenterModel(trainData, preds_to_center)
      trainData <- predictCenterModel(trainData, center_model)
    } else{
      center_model <- ""
    }
    
    return(
      list(
        trainData = trainData,
        index = trainRowNumbers,
        impute_model = impute_model,
        dummy_model = dummy_model,
        center_model = center_model
      )
    )
  }

prepareDataToPredict <-
  function(dataset,
           impute_model,
           dummy_model,
           center_model,
           target) {
    
    if (impute_model != "") {
      dataset <- predictImputeModel(dataset, impute_model)
    }
    
    dataset <- predictDummyModel(dataset, target, dummy_model)
    
    if (center_model != "") {
      dataset <- predictCenterModel(dataset, center_model)
    }
    
    return(dataset)
  }
