
# qeFair*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    sensNames:  sensitive variables to be excluded from the ML analysis
#    possible algorithm-specific options
#    holdout:  size of holdout set, if any

# value:

#    see individual functions below

# predict() arguments:

#    object:  output from q*()
#    newx:  data frame of points to be predicted
#    possible options
 
# value:  R list with components as follows:
 
#    classification case:

#       ypreds:  R factor instance of predicted class labels, one element f
#          for each row of newx 
#       conditprobs:  vector/matrix of class probabilities; in the 2-class
#          case, a vector, the probabilities of Y = 1
 
#    regression case:

#       vector of predicted values

#########################  qeFairKNN()  #################################

# selectProbs: 
 
qeFairKNN <- function(data,yName,
   k=25,expandVars=NULL,expandVals=NULL,sensNames=NULL,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)

   nonSensNames <- setdiff(names(data),sensNames)
   data1 <- data[nonSensNames]

   classif <- is.factor(yName)
   if (classif) classNames <- levels(yName)

   knnout <- qeKNN(data1,yName,
      maxk=k,expandVars=expandVars,expandVals=expandVals,
      holdout=holdout)

   srout <- list(knnout=knnout)
   srout$classif <- TRUE
   srout$deweightNames <- deweightNames
   srout$deweightVal <- deweightVal
   srout$sensNames <- sensNames
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeFairKNN')
   srout$holdIdxs <- knnout$holdIdxs
   srout$holdoutPreds <- knnout$holdoutPreds
   srout$testAcc <- knnout$testAcc
   srout$baseAcc <- knnout$baseAcc
   srout$confusion <- knnout$confusion

   if (!is.null(sensNames)) 
      srout$corrs <- corrsens(data,yName,srout,sensNames)
   srout
}

predict.qeFairKNN <- function(object,newx)
{
   knnout <- object$knnout
   predict(knnout,newx)
}
 

