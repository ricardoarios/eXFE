library(caret) 

#calculating confusion matrix
my.accuracy<-function(pred, truth){
  dif.pt<-setdiff(unique(truth),unique(pred))
  truth<-c(as.character(truth),dif.pt)
  pred<-c(as.character(pred), dif.pt)
  xtab <- table(pred, truth)
  xtab[dif.pt, dif.pt]<-0
  mcc.f1(confusionMatrix(xtab))
}

mcc.f1<-function(confusion){
  
  conf.matrix<-confusion$table
  
  s<-(conf.matrix[1,1]*conf.matrix[2,2] - conf.matrix[1,2]*conf.matrix[2,1])
  l<-sqrt((conf.matrix[1,1]+conf.matrix[1,2])*(conf.matrix[1,1]+conf.matrix[2,1])*
            (conf.matrix[2,2]+conf.matrix[1,2])*(conf.matrix[2,2]+conf.matrix[2,1]))
  mmc.value<-s/l
  
  if(is.nan(mmc.value)){
    mmc.value<-NA
  }else{
    mmc.value<-round(mmc.value, 2)
  }
  
  s<-2*conf.matrix[1,1]
  l<-(2*conf.matrix[1,1] + conf.matrix[1,2] + conf.matrix[2,1])
  
  f1m<-s/l
  
  if(is.nan(f1m)){
    f1m<-NA
  }else{
    f1m<-round(f1m, 2)
  }
  
  confmat.names<-names(confusion$overall)
  confusion$overall<-c(confusion$overall, mmc.value, f1m)
  names(confusion$overall)<-c(confmat.names, "MCC", "F1m")
  confusion
}