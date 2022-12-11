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
# Code implemented to create the fuzzy rules.
#
# Date: December, 2020
#
# Developers:   Ricardo Rios, 
#               Tatiane Nogueira, 
#               Rodrigo Mello
#
##################################################################################

library(frbs)

create.fuzzy.function<-function(dataset, label, m.type = "FRBCS.W", n.labels = 7, memb.func = "TRAPEZOID", t.norm = "MIN", s.norm = "MAX", plot=F){
  range.data.input <- apply(dataset, 2, range)
  label<-unclass(as.factor(label))
  object.frbcs.w <- frbs.learn(cbind(dataset, as.numeric(label)), range.data.input,
                               method.type = m.type, 
                               control = list(num.labels = n.labels, type.mf = memb.func, type.tnorm=t.norm, type.snorm=s.norm))
  
  if(plot)
    plotMF(object.frbcs.w)
  
  err.label<-as.numeric(object.frbcs.w$rule[,ncol(object.frbcs.w$rule)])
  object.frbcs.w$rule[,ncol(object.frbcs.w$rule)]<-levels(label)[err.label]
  object.frbcs.w
}

create.all.rules<-function(inst.correct.classif, models, all.att, meteo.vars, label){
  resp<-matrix()
  for(i in models){
    out<-create.fuzzy.function(round(as.vector(all.att[inst.correct.classif[[i]], meteo.vars]),2), 
                               as.numeric(classes[inst.correct.classif[[i]], label]))$rule
    
    
    #out<-cbind(out, rep(inst.correct.classif[[i-1]]$learner$short.name, nrow(out)))
    out<-cbind(out, rep(i, nrow(out)))
    
    if(ncol(resp) == 1){
      resp<-out
    }else{
      resp<-rbind(resp, out)
    }
  }
  
  resp
}

create.all.models<-function(inst.correct.classif, models, all.att, meteo.vars, label, fuz.type = "FRBCS.W", n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN"){
  resp<-list()
  for(i in models){
    if(!is.null(inst.correct.classif[[i]])){
      resp[[i]]<-create.fuzzy.function(round(as.vector(all.att[inst.correct.classif[[i]], meteo.vars]),2), 
                                       as.numeric(classes[inst.correct.classif[[i]], label]),
                                       m.type = fuz.type, n.labels = n.sets, memb.func = fuz.func, t.norm = type.tnorm)
    }
  }
  
  resp
}

create.weight.rules.models<-function(inst.correct.classif, models, all.att, meteo.vars, label, fuz.type = "FRBCS.W", n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN"){
  resp<-matrix(0, nrow(all.att), ncol=length(models))
  
  for(i in models){
    if(!is.null(inst.correct.classif[[i]])){
      resp[inst.correct.classif[[i]], i/2]<-inst.correct.classif[[length(inst.correct.classif)]][i/2]
    }
  }
  
  resp.index<-which(apply(resp, 1, sum)==0)
  resp.label<-apply(resp, 1, which.max)
  
  
  return(create.fuzzy.function(all.att[-resp.index,meteo.vars], 
                               resp.label[-resp.index],
                               m.type = fuz.type, n.labels = n.sets, memb.func = fuz.func, t.norm = type.tnorm))
}


#create.rules.models<-function(inst.correct.classif, models, all.att, meteo.vars, label, fuz.type = "FRBCS.CHI", n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN"){
create.rules.models<-function(inst.correct.classif, all.att, meteo.vars, label, fuz.type = "FRBCS.W", 
                              n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN", plot = T, type.snorm="MAX"){
  resp<-NULL
  models<-seq(2, length(inst.correct.classif), 2)
  
  class.order<-c()
  for(i in models){
    class.order<-c(class.order,length(inst.correct.classif[[i]]))
  }
  models<-models[sort.list(class.order)]
  
  for(i in 1:length(models)){
    if(!is.null(inst.correct.classif[[models[i]]])){
      data.temp<-unlist(inst.correct.classif[models[-c(i)]])
      diff.temp<-(unlist(inst.correct.classif[models[c(i)]]) %in% data.temp)
      index.temp<-unlist(inst.correct.classif[models[c(i)]])[!diff.temp]
      if(is.null(resp)){
        resp<-cbind(all.att[index.temp,meteo.vars], i)
      }else{
        if(length(index.temp) > 0)
          resp<-rbind(resp, cbind(all.att[index.temp,meteo.vars], i))
      }
    }
  }
  
  #remove next line...
  #it's is just a debug to better understand the results...
  diff.correct.class<-resp$i
  save(diff.correct.class, file="/tmp/resp-13538.output")
  
  if(length(table(resp[,ncol(resp)])) == 1) 
    return (resp[,ncol(resp)][1])
  else
    return(create.fuzzy.function(resp[,-ncol(resp)], 
                                 resp[,ncol(resp)],
                                 m.type = fuz.type, n.labels = n.sets, memb.func = fuz.func, t.norm = type.tnorm, plot=plot, s.norm=type.snorm))
}

distinct.create.rules.models<-function(inst.correct.classif, all.att, meteo.vars, label, fuz.type = "FRBCS.W", 
                                       n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN"){
  resp<-NULL
  models<-seq(2, length(inst.correct.classif), 2)
  
  class.order<-c()
  for(i in models){
    class.order<-c(class.order,length(inst.correct.classif[[i]]))
  }
  models<-models[sort.list(class.order)]
  selecting.classif<-rep(NA, nrow(all.att))
  
  for(i in models){
    if(!is.null(inst.correct.classif[[i]])){
      selecting.classif[inst.correct.classif[[i]]]<-i/2
    }
  }
  
  index.valid<-which(!is.na(selecting.classif))
  
  resp<-cbind(round(as.vector(all.att[index.valid, meteo.vars]),2), selecting.classif[index.valid])
  
  #### REMOVER  
  save(resp, file="/tmp/test.Rdata")
  ####
  
  if(length(table(resp[,ncol(resp)])) == 1) 
    return (resp[,ncol(resp)][1])
  else
    return(create.fuzzy.function(resp[,-ncol(resp)], 
                                 resp[,ncol(resp)],
                                 m.type = fuz.type, n.labels = n.sets, memb.func = fuz.func, t.norm = type.tnorm))
}

#create.rules.models<-function(inst.correct.classif, models, all.att, meteo.vars, label, fuz.type = "FRBCS.CHI", n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN"){
all.create.rules.models<-function(inst.correct.classif, all.att, meteo.vars, label, fuz.type = "FRBCS.W", n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN"){
  resp<-NULL
  models<-seq(2, length(inst.correct.classif), 2)
  for(i in models){
    if(!is.null(inst.correct.classif[[i]])){
      temp<-cbind(round(as.vector(all.att[inst.correct.classif[[i]], meteo.vars]),2), i/2)
      if(is.null(resp))
        resp<-temp
      else
        resp<-rbind(resp, temp)
    }
  }
  
  if(length(table(resp[,ncol(resp)])) == 1) 
    return (resp[,ncol(resp)][1])
  else
    return(create.fuzzy.function(resp[,-ncol(resp)], 
                                 resp[,ncol(resp)],
                                 m.type = fuz.type, n.labels = n.sets, memb.func = fuz.func, t.norm = type.tnorm))
}

create.rules.chi.models<-function(inst.correct.classif, models, all.att, meteo.vars, label, fuz.type = "FRBCS.CHI", n.sets = 7, fuz.func = "TRAPEZOID", type.tnorm = "MIN"){
  resp<-NULL
  for(i in models){
    if(!is.null(inst.correct.classif[[i]])){
      temp<-cbind(round(as.vector(all.att[inst.correct.classif[[i]], meteo.vars]),2), i/2)
      if(is.null(resp))
        resp<-temp
      else
        resp<-rbind(resp, temp)
    }
  }
  
  if(length(table(resp[,ncol(resp)])) == 1) 
    return (resp[,ncol(resp)][1])
  else
    return(create.fuzzy.function(resp[,-ncol(resp)], 
                                 resp[,ncol(resp)],
                                 m.type = fuz.type, n.labels = n.sets, memb.func = fuz.func, t.norm = type.tnorm))
}

