
# qe*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
#    possible algorithm-specific options
#    allDefaults:  if TRUE, take all the defaults of the wrapped
#       function, e.g. e1071::svm()
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

#########################  qeSemiRidge()  #################################

# arguments:  see above, plus

#     k: number of nearest neighbors
#     scaleX: if TRUE, features will be centered and scaled; note that
#        this means the features must be numeric
#     smoothingFtn: as in kNN(); 'mean' or 'loclin'

# value:  see above

require(qeML)
 
qeSemiRidge <- function(data,yName,lambdas,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   trainRow1 <- getRow1(data,yName)
   classif <- is.factor(data[[yName]])
   if (!is.null(holdout)) splitData(holdout,data)
   xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=classif,
      makeYdumms=TRUE)
   x <- xyc$x
   colnamesX <- colnames(x)
   xm <- as.matrix(x)
   xm <- cbind(1,xm)

   factorsInfo <- xyc$factorsInfo
   if (!is.null(factorsInfo)) attr(xm,'factorsInfo') <- factorsInfo
   y <- xyc$y

   if (!is.numeric(y)) stop('Y must be numeric')
   lambdaVars <- names(lambdas)
   d <- rep(0,ncol(xm))
   names(d) <- c('const',colnames(x))
   d[lambdaVars] <- unlist(lambdas)
   xpx <- t(xm) %*% xm
   xpy <- t(xm) %*% y
   bhat <- solve(xpx + diag(d)) %*% xpy
   bhat <- as.vector(bhat)

   srout <- list(bhat=bhat)
   srout$classif <- classif
   srout$factorsInfo <- factorsInfo
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeSemiRidge')
   if (!is.null(holdout)) {
      predictHoldout(srout)
      srout$holdIdxs <- holdIdxs
   } else srout$holdIdxs <- NULL
   srout
}

predict.qeSemiRidge <- function(object,newx)
{
   if (!regtools::allNumeric(newx)) 
      newx <- qeML:::setTrainFactors(object,newx)
   classif <- object$classif
   xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo,makeYdumms=TRUE)
   if (is.vector(newx)) {
      nr <- 1
   } else{
      nr <- nrow(newx)
   } 
   newx <- matrix(xyc$x,nrow=nr)
   newx <- cbind(1,newx)

   preds <- newx %*% object$bhat
   if (!object$classif) return(preds)
   if (is.vector(preds)) preds <- matrix(preds,nrow=1)
   collectForReturn(object,preds)
}

