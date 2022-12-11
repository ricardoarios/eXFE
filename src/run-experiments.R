# initial setup...
rm(list=ls())

if(!exists("get.wd"))
  get.wd<-getwd()

########### basic codes ###########
### loading the data...
source(paste(sep="", get.wd, "/src/preprocessing/load-data.R"))
### metrics: MCC and F1
source(paste(sep="", get.wd, "/src/preprocessing/confMat.R"))
### 10-fold cross validation
source(paste(sep="", get.wd, "/src/preprocessing/create-cv-b-s.R"))
###################################

##### codes necessary to run our proposal #####
source(paste(sep="", get.wd, "/src/fuzzy-ensemble/select-truePos.R"))
source(paste(sep="", get.wd, "/src/fuzzy-ensemble/create-rules.R"))
source(paste(sep="", get.wd, "/src/fuzzy-ensemble/select-classifier.R"))
source(paste(sep="", get.wd, "/src/fuzzy-ensemble/ensemble-prediction.R"))
###############################################

# function used to "see" the learning bias
source(paste(sep="", get.wd, "/src/fuzzy-ensemble/plot-bias.R"))

#--------------------------------------------------------------------------------------
#loading dataset and defining the test label(rotulo)
#c(8,9,11,12,13,14)
set.seed(4561) #-8
#set.seed(4561254) #-9 11 12 
#set.seed(9561254) #-13
#set.seed(9561254) #-14
rotulo=14
k=1
#--------------------------------------------------------------------------------------

############################# 
### defining fuzzy parameters
############################# 
.N.SETS = 5
.FUZ.FUNC = "TRIANGLE"
.T.NORM = "MIN"
.S.NORM = "MAX"
.FUZ.TYPE = "FRBCS.CHI"
############################# 

########################################
### variables to store the final results
########################################
class.final.result<-c()
final.index<-1

max.class.index<-c()
diff.inst.correct<-c()
numb.inst.by.class<-c()
numb.rules.by.class<-matrix(NA, ncol=3, nrow=10)

train.validation<-c()
########################################

for(k in 1:10){
#for(k in 1:1){  
  #load(paste(sep="", get.wd, "/results/index-test.data"))
  cv.10<-create.cv.estrat(all.att, rotulo, cv=10)
  
  cat("Calculating fold: ", k, "\n")
  #load(paste(sep="", get.wd, "/results/index-test.data"))
  cv.test<-sort(cv.10[[k]])
  cv.train<-sort(unlist(cv.10))
  cv.train<-cv.train[which(!(cv.train %in% cv.test))]
  #index.test<-c(index.test, cv.test)
  
  train<-cbind(all.att[cv.train, meteo.vars], classes[cv.train,rotulo])
  colnames(train)<-c(colnames(all.att[cv.train, meteo.vars]), colnames(classes)[rotulo])
  trainTask <- mlr::makeClassifTask(data = train, target = colnames(classes)[rotulo])
  
  
  ### getting information based on individual models
  inst.correct.classif<-basic.models(colnames(classes)[k], trainTask, classes[cv.train, rotulo])
  
  
  ### creating the fuzzy rules
  resp.f<-create.rules.models(inst.correct.classif, all.att, meteo.vars, 
                              label=rotulo, n.sets = .N.SETS, fuz.func = .FUZ.FUNC, 
                              type.tnorm = .T.NORM, plot=F, type.snorm=.S.NORM, fuz.type = .FUZ.TYPE)
  
  #### running fuzzy engine
  resposta <- FRBCS.ishi.eng(resp.f, train[, -c(ncol(train))])
  best.classifiers<-as.vector(resposta$res)
  new.labels<-ensemble.predict(best.classifiers, train)     
  
  ours<-round(my.accuracy(new.labels, as.numeric(train[,ncol(train)])-1)$overall[1], 2)    
  cat("Accuracy ours - train:", ours, "\n")
  ####
  
  ### using test fold
  test<-cbind(all.att[cv.test, meteo.vars], classes[cv.test, rotulo])
  colnames(test)<-c(colnames(all.att[cv.test, meteo.vars]), colnames(classes)[rotulo])
  testTask <- mlr::makeClassifTask(data = test, target = colnames(classes)[rotulo])
  
  models<-seq(1, length(inst.correct.classif)-1, 2)
  
  for(i in models){
    if(!is.null(inst.correct.classif[[i+1]])){
      predict.value<-predict(inst.correct.classif[[i]], testTask)
      new.labels<-predict.value$data$response %>% as.numeric()-1
      temp<-round(my.accuracy(new.labels, as.numeric(test[,ncol(test)])-1)$overall[1], 2)    
      cat("Accuracy others:", temp, "\n")
      
      class.final.result[final.index]<-temp
      
    }else{
      class.final.result[final.index]<-NA
    }
    final.index<-(final.index+1)
  }
  
  train.validation<-c(train.validation, inst.correct.classif[[7]])
  load(file="/tmp/resp-13538.output")
  numb.inst.by.class<-c(numb.inst.by.class, table(diff.correct.class) %>% as.vector())
  cat("#rules ", k, " - ", resp.f$rule[,ncol(resp.f$rule)] %>% table(), "\n")
  numb.rules.by.class[k, c(resp.f$rule[,ncol(resp.f$rule)] %>% table() %>% names() %>% as.numeric())]<-resp.f$rule[,ncol(resp.f$rule)] %>% table() %>% as.vector()
  ####
  resposta <- FRBCS.ishi.eng(resp.f, test[, -c(ncol(test))])
  best.classifiers<-as.vector(resposta$res)
  new.labels<-ensemble.predict(best.classifiers, test)    
  
  max.class.index[k]<-inst.correct.classif[[7]] %>% which.max()
  diff.inst.correct<-c(diff.inst.correct, 
                       c(inst.correct.classif[[2]] %>% length(), 
                         inst.correct.classif[[4]] %>% length(), 
                         inst.correct.classif[[6]] %>% length()))
  
  ours<-round(my.accuracy(new.labels, as.numeric(test[,ncol(test)])-1)$overall[1], 2)    
  cat("Accuracy ours:", ours, "\n")
  cat("Acc: ", round(my.accuracy(new.labels, as.numeric(test[,ncol(test)])-1)$overall[1], 2), "\n")
  cat("MCC: ", round(my.accuracy(new.labels, as.numeric(test[,ncol(test)])-1)$overall[2], 2), "\n")
  cat("F1: ", round(my.accuracy(new.labels, as.numeric(test[,ncol(test)])-1)$overall[3], 2), "\n")
  class.final.result[final.index]<-ours
  final.index<-(final.index+1)
  ####
}

cbind(matrix(train.validation, ncol=3, byrow = T), 
      matrix(numb.inst.by.class, ncol=3, byrow = T),
      numb.rules.by.class,
      matrix(class.final.result, ncol=4, byrow = T), 
      max.class.index) %>% xtable::xtable()
ballance<-classes[,rotulo] %>% table()



