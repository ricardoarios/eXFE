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
# Code implemented to load the basic data.
#
# Date: December, 2020
#
# Developers:   Ricardo Rios, 
#               Tatiane Nogueira, 
#               Rodrigo Mello
#
##################################################################################

#get.wd<-"/home/rios/fapar/fapar/"
load(paste(sep="", get.wd, "/dataset/dataset.RData"))

####
#meteo.vars<-4:11
meteo.vars<-c(4,5,7:11)
classes.indexes<-12:31

### normalizing all attributes
normalize<-function(x){
  (x - min(x))/(max(x)-min(x))
}

for(i in meteo.vars){
  D[,i]<-normalize(D[,i])
}

### transforming all classes as factor
for(i in classes.indexes){
  D[,i]<-as.factor(as.character(D[,i]))  
}

classes<-D[,classes.indexes]
all.att<-D[,-classes.indexes]

rm(D)
gc()
