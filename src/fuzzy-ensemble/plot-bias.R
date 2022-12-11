library(ggfortify)


plot.bias<-function(dados, correct.instances){
  resp<-NULL
  for(i in seq(2, length(correct.instances), 2)){
    if(!is.null(inst.correct.classif[[i]])){
      if(is.null(resp)){
        resp<-cbind(dados[correct.instances[[i]], ], i/2)
      }else{
        resp<-rbind(resp, cbind(dados[correct.instances[[i]], ], i/2))
      }
    }
  }
  resp[, ncol(resp)]<-as.character(resp[, ncol(resp)])
  colnames(resp)<-c(colnames(dados), "Classifier")
  df<-as.data.frame(resp[, -ncol(resp)])
  
  autoplot(prcomp(df), data = resp, colour = 'Classifier', frame = TRUE, frame.type = 'norm')
  
  #invisible(resp)
}