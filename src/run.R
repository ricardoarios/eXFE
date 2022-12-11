##################################################################################
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.
#
# This code is released as part of the manuscript
# “eXplainable Ensemble Strategy using distinct and restrict learning biases: 
#      A Case Study on the Brazilian Forest”, by Tatiane Rios, Ricardo Rios, 
#      and Rodrigo Mello (Applied Soft Computing, 2022).
#
# Code used to run examples that support our hypothesis.
#
# Date: December, 2020
#
# Developers:   Ricardo Rios, 
#               Tatiane Nogueira, 
#               Rodrigo Mello
#
##################################################################################
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
# loading dataset and defining the test label(rotulo)
# c(8,9,11,12,13,14)
# making sure we're gonna get the same results (reproducibility)
set.seed(4561)
rotulo=8
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

output<- cbind(matrix(train.validation, ncol=3, byrow = T), 
	       matrix(numb.inst.by.class, ncol=3, byrow = T),
	       numb.rules.by.class,
	       matrix(class.final.result, ncol=4, byrow = T), 
	       max.class.index)


colnames(output)<-c('C1(train)', 'C2(train)', 'C3(train)', 
		    'C1(Inst)', 'C2(Inst)', 'C3(Inst)', 'C1(Rules)', 
		    'C2(Rules)', 'C3(Rules)', 'C1(Test)','C2(Test)', 
		    'C3(Test)', 'eXFE', 'Best (Train)')

# To print results as latex table:
#output %>% xtable::xtable()

print(output)

