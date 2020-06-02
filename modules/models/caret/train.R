doTrain <-
  function(dataset,
           target) {
    
    fitControl <- trainControl(
      method = "cv",
      number = 5,
      search = "grid"
    )
    c1 <- makeCluster(4, type = 'SOCK')
    registerDoSNOW(c1)
  
    models <- list()
    for(method in l){
      models[method] <- train(
        as.formula(paste(target, '~ .')),
        data = dataset,
        method = method,
        trControl = fitControl
      )
    }
    
    stopCluster(c1)
    
    return(models)
  }



models <- list()
for(method in l){
  models[method] <- train(
    charges ~ .,
    data = insurance,
    method = method,
    trControl = fitControl
  )
}
