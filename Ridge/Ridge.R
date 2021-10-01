
# qeFair*() arguments:

#    data:  dataframe, training set; class labels col is a factor; other
#       columns may be factors
#    yName:  column name for outcome variable; vector indicates
#       regression, factor classification 
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

#########################  qeFairRidge()  #################################
 
qeFairRidgeLin <- function(data,yName,lambdas,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)

   if (length(setdiff(names(lambdas),names(data))) > 0)
      stop('invalid feature name')

   # standard qe*-series code for ML methods needing numeric X
   trainRow1 <- getRow1(data,yName)
   if (!is.null(holdout)) splitData(holdout,data)
   xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=FALSE,
      makeYdumms=TRUE)
   x <- xyc$x
   colnamesX <- colnames(x)
   xm <- as.matrix(x)

   # need to update lambdas re X dummies
   yCol <- which(names(data) == yName)
   isf <- sapply(1:length(names(data[,-yCol])),
      function(col) is.factor(data[[col]]))
   isf <- which(isf)
   newLambdas <- lambdas[-isf]  # the nonfactor sensitive variables
   # now for the factor sensitive variables
   for (i in isf) {
      lvls <- levels(data[,i])
      lvls <- lvls[-length(lvls)]
      newLambdas[lvls] <- lambdas[[colnamesX[i]]]
   }
   lambdas <- newLambdas

   # scale X data
   xm <- scale(xm)

   factorsInfo <- xyc$factorsInfo
   if (!is.null(factorsInfo)) attr(xm,'factorsInfo') <- factorsInfo
   y <- xyc$y
   if (!is.numeric(y)) stop('Y must be numeric')

   # center Y
   ybar <- mean(y)
   y <- y - ybar

   xpx <- t(xm) %*% xm
   xpx <- xpx / nrow(xm)  # now all diags are 1.0
   xpy <- t(xm) %*% y  
   xpy <- xpy / nrow(xm)  # retain scale

   # compute diag perturbation
   lambdaVars <- names(lambdas)
   d <- rep(0,ncol(xm))
   names(d) <- colnames(x)
   d[lambdaVars] <- unlist(lambdas)

   # solve for beta-hats
   bhat <- solve(xpx + diag(d)) %*% xpy
   bhat <- as.vector(bhat)

   srout <- list(bhat=bhat,
      ctr=attr(xm,'scaled:center'),scl=attr(xm,'scaled:scale'),ybar=ybar)
   srout$classif <- FALSE
   srout$factorsInfo <- factorsInfo
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeFairRidgeLin')
   if (!is.null(holdout)) {
      predictHoldout(srout)
      srout$holdIdxs <- holdIdxs
   } else srout$holdIdxs <- NULL
   srout
}

predict.qeFairRidgeLin <- function(object,newx)
{
   if (!regtools::allNumeric(newx)) 
      newx <- qeML:::setTrainFactors(object,newx)
   xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo,makeYdumms=TRUE)
   if (is.vector(newx)) {
      nr <- 1
   } else{
      nr <- nrow(newx)
   } 
   newx <- matrix(xyc$x,nrow=nr)
   newx <- scale(newx,center=object$ctr,scale=object$scl)

   preds <- newx %*% object$bhat + object$ybar
   preds
}
 
qeFairRidgeLog <- function(data,yName,lambdas,start=NULL,nIters=10,
   holdout=floor(min(1000,0.1*nrow(data))))
{
   require(qeML)

   if (length(setdiff(names(lambdas),names(data))) > 0)
      stop('invalid feature name')

   # standard qe*-series code for ML methods needing numeric X; here we
   # have a classification problem, so getXY() will also create a dummy
   # for each level of the factor Y
   trainRow1 <- getRow1(data,yName)
   classif <- is.factor(data[[yName]])
   if (!is.null(holdout)) splitData(holdout,data)
   xyc <- getXY(data,yName,xMustNumeric=TRUE,classif=classif,
      makeYdumms=TRUE)
   xy <- xyc$xy
   x <- xyc$x
   colnamesX <- colnames(x)
   xm <- as.matrix(x)
   xm <- scale(xm)

   yDumms <- xyc$yDumms  # dummies version of Y; xy is X+this
   y <- xyc$y  # original R factor version of Y
   classNames <- xyc$classNames
   nClass <- length(classNames)
   ncxy <- ncol(xy)
   nx <- ncol(x)
   nydumms <- ncxy - nx  # redundant, same as nClass
   empirClassProbs <- colMeans(yDumms)

   # need to update lambdas re X dummies
   yCol <- which(names(data) == yName)
   dataX <- data[,-yCol]
   isf <- sapply(1:length(names(dataX)),
      function(col) is.factor(dataX[[col]]))
   isf <- which(isf)
   if (length(isf) > 0) {
      newLambdas <- lambdas[-isf]  # the nonfactor sensitive variables
      # now for the factor sensitive variables
      for (i in isf) {
         if (names(dataX)[i] %in% names(lambdas)) {
            lvls <- levels(data[,i])
            lvls <- lvls[-length(lvls)]
            newLambdas[lvls] <- lambdas[[colnamesX[i]]]
         }
      }
      lambdas <- newLambdas
   }

   factorsInfo <- xyc$factorsInfo
   if (!is.null(factorsInfo)) attr(xm,'factorsInfo') <- factorsInfo
   y <- xyc$y

   doGlmFairRidge <- function(colI) {
      tmpDF <- cbind(x, yDumms[, colI])
      names(tmpDF)[nx + 1] <- "yDumm"
      bhat <- glmFitLambda(xm,yDumms[,colI],start=start,family=binomial(),
         lambdas,nIters) 
      bhat     
   }

   bhats <- sapply(1:nydumms, doGlmFairRidge)

   srout <- list(bhats=bhats,classNames=levels(y),
      ctr=attr(xm,'scaled:center'),scl=attr(xm,'scaled:scale'))
   srout$classif <- classif
   srout$yName <- yName
   srout$factorsInfo <- factorsInfo
   srout$trainRow1 <- trainRow1
   class(srout) <- c('qeFairRidgeLog')
   if (!is.null(holdout)) { 
      predictHoldout(srout)
      srout$holdIdxs <- holdIdxs
   } else srout$holdIdxs <- NULL
   srout
}

predict.qeFairRidgeLog <- function(object,newx)
{
   if (!regtools::allNumeric(newx)) 
      newx <- qeML:::setTrainFactors(object,newx)
   classif <- TRUE
   xyc <- getXY(newx,NULL,TRUE,FALSE,object$factorsInfo,makeYdumms=TRUE)
   if (is.vector(newx)) {
      nr <- 1
   } else{
      nr <- nrow(newx)
   } 
   newx <- matrix(xyc$x,nrow=nr)
   newx <- scale(newx,center=object$ctr,scale=object$scl)
   newx <- cbind(1,newx)

   preds <- newx %*% object$bhats 
   preds <- 1 / (1 + exp(-preds))
   rs <- rowSums(preds)
   preds <- (1/rs) * preds
   qeML:::collectForReturn(object,preds)
}

# call to glm.fit() with lambdas

# arguments:

#    x,y,start,family: as in glm.fit()
#    lambdas: as in qeFairRidgeLin() above
#    nIters: number of iterations

#    glm.fit() being fit first, without lambdas, with the resulting
#    beta-hats then being used for initial values for our algorithm here

glmFitLambda <- function(x,y,start=NULL,family=binomial(),lambdas,nIters) 
{
   # x should already be scaled
   if (is.null(attr(x,'scaled:center')))
      stop('x must already be scaled')
   xm <- cbind(1,x)
   colnames(xm)[1] <- 'const'
   xm <- as.matrix(xm)


   z <- glm.fit(x=xm,y=y,family=family)  
   preds <- xm %*% coef(z)
   preds <- as.vector(1 / (1 + exp(-preds)))
   wts <- 1 / (preds * (1-preds))

   lambdaVars <- names(lambdas)
   d <- rep(0,ncol(xm))
   names(d) <- colnames(xm)
   d[lambdaVars] <- unlist(lambdas)
   for (i in 1:nIters) {
      xw <- sqrt(wts) * xm
      xpx <- t(xw) %*% xw
      # xpx <- t(xm) %*% diag(wts) %*% xm
      xpy <- t(xm) %*% (wts * y)
      scaleToMax1 <- max(diag(xpx))
      xpx <- xpx / scaleToMax1
      xpy <- xpy / scaleToMax1
      bhat <- solve(xpx + diag(d)) %*% xpy
      bhat <- as.vector(bhat)
      if (i < nIters) {
         preds <- as.vector(xm %*% bhat)
         preds <- 1 / (1 + exp(-preds))
         wts <- 1 / (preds * (1-preds))
      }

   }

   bhat

}
