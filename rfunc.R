#######################################################################################################
#Function for pullin t test statistics from a list of variables found in a data set                   #
#######################################################################################################

vec1<- c("disp", "mpg", "cyl")
vec2<- c("drat", "gear", "hp")

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
  
  outData <- data.frame(var1=firstvars, var2=secondvars, mdiff=0, confl=0, confu=0,  pval=0)

  for(i in 1:length(firstvars)){
    modelresult <- t.test(datset[,firstvars[i]], datset[,secondvars[i]], paired=TRUE)
    outData$mdiff[i] <- modelresult$estimate
    outData$confl[i] <- modelresult$conf.int[1]
    outData$confu[i] <- modelresult$conf.int[2]
    outData$pval[i] <- modelresult$p.value
  }
    
  return(outData)
  
}

x<- test.ttest(mtcars,vec1,vec2)

