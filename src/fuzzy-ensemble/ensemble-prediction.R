ensemble.classifier<-function(classifiers, best, test){
  classification.results<-c()
  testTask  <- mlr::makeClassifTask(data = test, target = colnames(test)[ncol(test)])
  n.class<-seq(1,length(classifiers)-1,by=2)
  resp<-matrix(nrow=length(best), ncol=length(n.class))
  
  for(i in 1:length(n.class))
    resp[, i]<-predict(classifiers[[n.class[i]]], testTask)$data$response

  for(i in 1:length(best))
    classification.results[i]<-resp[i, best[i]]
  
  return(classification.results)
}

ensemble.predict<-function(classifiers, test){
  classification.results<-c()
  class.model<-list()
  
  label<-colnames(test)[ncol(test)]
  
  load(file=paste(sep="", "results/fuzzy-ens/temp-single-classif/model.svm"))
  class.model[[1]] <- final.model
  
  load(file=paste(sep="", "results/fuzzy-ens/temp-single-classif/model.svm-rad"))
  class.model[[2]] <- final.model
  
  load(file=paste(sep="", "results/fuzzy-ens/temp-single-classif/model.rf"))
  class.model[[3]] <- final.model
  
  for(i in 1:length(classifiers)){
    #cat("Classifying instance: ", i, "\n")
    
    
    suppressWarnings(testTask  <- mlr::makeClassifTask(data = test[i,], target = label))
    
    if(classifiers[i]==1){ 
      ##########SVM-POL##########
      #cat("***SVM-pol\n")
      predict.value <- predict(class.model[[1]], testTask)
      classification.results[i]<-predict.value$data$response %>% as.numeric()-1
      
    }else if(classifiers[i]==2){
      ##########SVM-RAD##########
      #cat("***SVM-rad\n")
      predict.value <- predict(class.model[[2]], testTask)
      classification.results[i]<-predict.value$data$response %>% as.numeric()-1
      
    }else if(classifiers[i]==3){
      ##########RF##########
      #cat("***RF\n")
      predict.value <- predict(class.model[[3]], testTask)
      classification.results[i]<-predict.value$data$response %>% as.numeric()-1
    }
  }
  
  return(classification.results)
}