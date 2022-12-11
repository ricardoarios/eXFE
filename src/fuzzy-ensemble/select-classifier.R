FRBCS.ishi.eng <- function(object, newdata){
  varinp.mf <- object$varinp.mf
  num.varinput <- ncol(object$num.labels) - 1
  num.labels.input <- object$num.labels[, -ncol(object$num.labels), drop = FALSE]
  num.fvaloutput <- object$num.labels[, ncol(object$num.labels), drop = FALSE]
  
  ## Change the linguistic values
  seq.names <- rep(1:num.varinput, num.labels.input)
  names.fvalinput <- paste(seq.names, matrix(colnames(object$varinp.mf), nrow = 1), sep=".")
  names(varinp.mf) <- names.fvalinput
  
  rule <- object$rule
  if (rule[1, 1] == "IF"){
    k <- 1
    new.rule <- matrix(NA, nrow = nrow(rule), ncol = (2 * num.varinput + 1))
    for (i in 1 : num.varinput) {
      new.rule[, k] <- paste(i, rule[, (4 * i), drop = FALSE],sep=".")
      #new.rule[, k + 1] <- "and"
      new.rule[, k + 1] <- rule[, ((4 * i) + 1)] 
      k <- k + 2
    }
    new.rule[, (ncol(new.rule) - 1)] <- "->"
    new.rule[, ncol(new.rule)] <- rule[, ncol(rule), drop = FALSE]
    rule <- new.rule
  }
  
  classes <- as.matrix(as.numeric(rule[, ncol(rule), drop = FALSE]))
  
  if (!is.null(object$grade.cert)) {
    grade.certainty <- object$grade.cert
  } else { 
    grade.certainty <- cbind(as.numeric(rule[, ncol(rule), drop = FALSE]), 1)
  }
  
  type.tnorm <- object$type.tnorm
  type.snorm <- object$type.snorm
  type.model <- object$type.model
  
  ##################
  ### I. Rule Based Module
  ### In this function, Checking of the rule given by user will be done.
  ### There are two kinds of model used which are Mamdani and TSK rule model.
  ##################
  rule <- rulebase(type.model, rule, classes)
  
  ###################
  ### II. Fuzzification Module
  ### In this function, we convert crisp value into linguistic value based on the data and parameter of membership function.
  ### There are several membership function can be used such as triangular, trapezoid, gaussian and logistic/sigmoid.
  ###################
  MF <- fuzzifier(newdata, num.varinput, num.labels.input, varinp.mf)
  
  ###################
  ### III. Inference Module
  ### In this function, we will calculate the confidence factor on antecedent for each rule. We use AND, OR, NOT operator. 
  ################### 	
  miu.rule <- inference(MF, rule, names.fvalinput, type.tnorm, type.snorm)
  
  alpha.class.all <- miu.rule
  
  for (i in 1 : nrow(miu.rule)){
    #Different weight for each rule:
    #miu.rule[i,] #degree of fire
    #grade.certainty[, 1] #weight of the rule
    
    #alpha.class.all[i, ] <- t(miu.rule[i, ] * grade.certainty[, 1])
    
    #All rules have the same weight:
    alpha.class.all[i, ] <- t(miu.rule[i, ] * rep(1, length(grade.certainty[, 1])))
  }
  indx.max <- matrix()
  max.degree<-matrix()
  for (i in 1 : nrow(miu.rule)){
    indx.max[i] <- which.max(alpha.class.all[i, ])
    max.degree[i]<- max(alpha.class.all[i, ])
  }	
  result <- matrix()
  
  for (i in 1 : length(indx.max)){
    result[i] <- object$class[indx.max[i], 1]
  }	
  
  real.label<-names(table(as.numeric(object$rule[,ncol(object$rule)])))
  result<-as.numeric(real.label[result])
  res <- matrix(result)
  
  my.output<-list()
  my.output$miu<-miu.rule
  my.output$alpha<-alpha.class.all
  my.output$grade.certainty<-grade.certainty[, 1]
  my.output$indmax<-indx.max
  my.output$res<-res
  my.output$degree<-max.degree
  my.output$MF<-MF
  
  return(my.output)	
}

remove.null.degrees<-function(degrees){
  summed.values<-apply(degrees, 1, sum)
  return(which(summed.values==0))
}

select.classifier<-function(degrees){
  apply(degrees, 1, which.max)
}

