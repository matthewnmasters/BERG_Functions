
flattenCorrMatrix <- function(cormat, pmat, nmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  = (cormat)[ut],
    p = pmat[ut],
    n = nmat[ut] #added by Matt to make getting n's easier
  )
}

#########################################################################################
#function to get mean and sd together easily                                            #
#########################################################################################

meansd<- function(datset, vec1){
  
  if(is.vector(vec1)==FALSE){
    stop("second argument must be a vector")
  }
  
  if(is.character(vec1)==FALSE){
    stop("vector needs to be strings of column names in the data")
  }
  
  outData <- data.frame(var1=vec1, mn="")
  
  for(i in 1:length(vec1)){
    meanresult <- sapply(datset[,vec1[i]], mean, na.rm=TRUE) #apparently mean and sd no longer work on data frames
    sdresult <- sapply(datset[,vec1[i]], sd, na.rm=TRUE)   #so we have to use sapply()
    outData$mn[i] <- paste0(round(meanresult, digits=4), " (", round(sdresult, digits=2), ")")

  }
  
  return(outData)
  
}


#########################################################################################
#Function for paired t-tests from a dataset and vector of matching variable names       #
#########################################################################################

test.ttest <- function(datset, firstvars, secondvars){
  
  if(length(firstvars) != length(secondvars)){
    stop("vectors of variables aren't the same length")
  }
  
  if(is.vector(firstvars)==FALSE | is.vector(secondvars)==FALSE){
    stop("one of your vector arguments isn't a vector")
  }
  
  if(is.character(firstvars)==FALSE | is.character(secondvars)==FALSE){
    stop("vectors need to be strings of column names in the data")
  }
  
  datset <- as.data.frame(datset) #if it comes in as a tibble it won't work
  
  outData <- data.frame(var1=firstvars, var2=secondvars, mdiff=0, confl=0, confu=0,  pval=0)
  
  for(i in 1:length(firstvars)){
    modelresult <- t.test(datset[,firstvars[i]], datset[,secondvars[i]], paired=TRUE)
    outData$mdiff[i] <- round(modelresult$estimate, digits=4)
    outData$confl[i] <- round(modelresult$conf.int[1], digits=4)
    outData$confu[i] <- round(modelresult$conf.int[2], digits=4)
    outData$pval[i] <- round(modelresult$p.value, digits=4)
  }
  
  return(outData)
  
}


#########################################################################################
#Function for linear weighted kappa from a dataset and vector of matching variable names#
#########################################################################################

kappafunc <- function(datset, firstvars, secondvars){
  
  if(length(firstvars) != length(secondvars)){
    stop("vectors of variables aren't the same length")
  }
  
  if(is.vector(firstvars)==FALSE | is.vector(secondvars)==FALSE){
    stop("one of your vector arguments isn't a vector")
  }
  
  if(is.character(firstvars)==FALSE | is.character(secondvars)==FALSE){
    stop("vectors need to be strings of column names in the data")
  }
  
  datset <- as.data.frame(datset) #if it comes in as a tibble it won't work
  
  outData <- data.frame(var1=firstvars, var2=secondvars, kappa=0, pval=0)
  
  for(i in 1:length(firstvars)){
    modelresult <- irr::kappa2(data.frame(datset[,firstvars[i]], datset[,secondvars[i]]), weight="equal")
    outData$kappa[i] <- round(modelresult$value, digits=4)
    outData$pval[i] <- round(modelresult$p.value, digits=4)
  }
  
  return(outData)
  
}

