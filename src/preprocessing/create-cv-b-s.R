create.cv.estrat<-function(dataset, label.index=1, cv=10){
  
  n.inst<-ceiling((nrow(dataset))*(1/cv))
  cv.folds<-list()
  actual.inst<-(1:nrow(dataset))
  temp.classes<-classes[actual.inst,label.index]
  balance<-table(temp.classes)  
  n.inst.c0<-round(n.inst*round(balance[1]/sum(balance), 2), 0) %>% as.numeric()
  
  for(cv.i in 1:(cv-1)){
    tmp<-which(classes[actual.inst,label.index]==(names(balance)[1]))
    c1<-actual.inst[sample(tmp, n.inst.c0)]
    tmp<-which(classes[actual.inst,label.index]==(names(balance)[2]))
    c2<-actual.inst[sample(tmp, n.inst-length(c1))]
    cv.folds[[cv.i]]<-sample(c(c1,c2)) #shuffle our cv.values
    actual.inst<-subset(actual.inst, !actual.inst%in%c(c1,c2))
  }
  
  cv.folds[[cv]]<-actual.inst
  
  invisible(cv.folds)
}
