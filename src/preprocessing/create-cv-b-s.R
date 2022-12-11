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
# Code implemented to run a stratified coss-validation strategy.
#
# Date: December, 2020
#
# Developers:   Ricardo Rios, 
#               Tatiane Nogueira, 
#               Rodrigo Mello
#
##################################################################################

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
