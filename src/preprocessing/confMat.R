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
# Code implemented to create a confusion matrix.
#
# Date: December, 2020
#
# Developers:   Ricardo Rios, 
#               Tatiane Nogueira, 
#               Rodrigo Mello
#
##################################################################################

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
