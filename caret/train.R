doTrain <-
  function(methods,
           dataset,
           target) {
    
    fitControl <- trainControl(
      method = "cv",
      number = 5,
      search = "grid",
      allowParallel = T
    )
    
    c1 <- makeCluster(4, type = 'SOCK')
    registerDoSNOW(c1)
    
    models <- list()
    for(method in methods){
      models[[method]] <- train(
        as.formula(paste(target, '~ .')),
        data = dataset,
        method = method,
        trControl = fitControl
      )
    }

    stopCluster(c1)
    
    return(
      models
    )
  }
