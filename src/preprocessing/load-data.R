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
