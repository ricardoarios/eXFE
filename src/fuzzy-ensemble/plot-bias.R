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
# Code implemented to plot the different learner performances.
#
# Date: December, 2020
#
# Developers:   Ricardo Rios, 
#               Tatiane Nogueira, 
#               Rodrigo Mello
#
##################################################################################

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
