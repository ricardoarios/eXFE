######
library(mlr)
library(dplyr)
library(KernelKnn)
######

basic.models<-function(label, trainTask, classes){

  inst.correct.classif<-list()
  index.ensemble<-1
  acc.mean<-c()
  multiclass.ens<-c()
  
  ##########SVM-POL##########
  cat("***SVM-POL\n")
  load(file=paste(sep="", "results/svm-polyn/model-", label, ".svm"))
  load(file=paste(sep="", "results/svm-polyn/best-acc-", label, ".svm"))
  
  final.model <- mlr::train(model, trainTask)
  
  save(final.model, file=paste(sep="", "results/fuzzy-ens/temp-single-classif/model.svm"))
  multiclass.ens<-cbind(multiclass.ens, (as.numeric(final.model$learner.model$fitted)-1))
  # we only consider models with kappa greater than 0
  cat("Training SVM-POL", my.accuracy((as.numeric(final.model$learner.model$fitted)-1),classes)$overall[1], "\n")
  if(my.accuracy((as.numeric(final.model$learner.model$fitted)-1),classes)$overall[2] > 0){
    inst.correct.classif[[index.ensemble]]<-final.model
    index.ensemble<-index.ensemble+1
    inst.correct.classif[[index.ensemble]]<-which((as.numeric(final.model$learner.model$fitted)-1)==classes)
    index.ensemble<-index.ensemble+1
  }else{
    inst.correct.classif[[index.ensemble]]<-final.model
    index.ensemble<-index.ensemble+2
  }
  
  acc.mean<-c(acc.mean, round(my.accuracy((as.numeric(final.model$learner.model$fitted)-1),classes)$overall[1], 2))
  
  ##########SVM-RAD##########
  cat("***SVM-RAD\n")
  load(file=paste(sep="", "results/svm/model-", label, ".svm"))
  load(file=paste(sep="", "results/svm/best-acc-", label, ".svm"))
  
  final.model <- mlr::train(model, trainTask)
  
  save(final.model, file=paste(sep="", "results/fuzzy-ens/temp-single-classif/model.svm-rad"))
  multiclass.ens<-cbind(multiclass.ens, (as.numeric(final.model$learner.model$fitted)-1))
  # we only consider models with kappa greater than 0
  #if(my.accuracy((as.numeric(final.model$learner.model$fitted)-1),classes)$overall[2] > 0){
  cat("Training SVM-RAD", my.accuracy((as.numeric(final.model$learner.model$fitted)-1),classes)$overall[1], "\n")
  if(my.accuracy((as.numeric(final.model$learner.model$fitted)-1),classes)$overall[2] > 0){    
    inst.correct.classif[[index.ensemble]]<-final.model
    index.ensemble<-index.ensemble+1
    inst.correct.classif[[index.ensemble]]<-which((as.numeric(final.model$learner.model$fitted)-1)==classes)
    index.ensemble<-index.ensemble+1
  }else{
    inst.correct.classif[[index.ensemble]]<-final.model
    index.ensemble<-index.ensemble+2
  }
  
  acc.mean<-c(acc.mean, round(my.accuracy((as.numeric(final.model$learner.model$fitted)-1),classes)$overall[1], 2))
  
  ##########RF##########
  cat("***RF\n")
  load(file=paste(sep="", "results/rforest/model-", label, ".RF"))
  load(file=paste(sep="", "results/rforest/best-acc-", label, ".RF"))
  
  final.model <- mlr::train(model, trainTask)
  
  save(final.model, file=paste(sep="", "results/fuzzy-ens/temp-single-classif/model.rf"))
  multiclass.ens<-cbind(multiclass.ens, (as.numeric(final.model$learner.model$predicted)-1)) 
  cat("Training RF", my.accuracy((as.numeric(final.model$learner.model$predicted)-1),classes)$overall[1], "\n")
  # we only consider models with kappa greater than 0
  if(my.accuracy((as.numeric(final.model$learner.model$predicted)-1),classes)$overall[2] > 0){
    inst.correct.classif[[index.ensemble]]<-final.model
    index.ensemble<-index.ensemble+1
    inst.correct.classif[[index.ensemble]]<-which((as.numeric(final.model$learner.model$predicted)-1)==classes)
    index.ensemble<-index.ensemble+1
  }else{
    inst.correct.classif[[index.ensemble]]<-final.model
    index.ensemble<-index.ensemble+2
  }
  
  acc.mean<-c(acc.mean, round(my.accuracy((as.numeric(final.model$learner.model$predicted)-1),classes)$overall[1], 2))
  #print(multiclass.ens)
  #save(multiclass.ens, file="/tmp/teste.out")
  cat("multiclass: ", round(my.accuracy(as.numeric(apply(multiclass.ens, 1, function(x){names(sort(table(x),decreasing=TRUE))[1]})),classes)$overall[1], 2), "\n")
  
  inst.correct.classif[[index.ensemble]]<-acc.mean
  
  inst.correct.classif
}

