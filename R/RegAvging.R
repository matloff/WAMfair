
# companion code for the Walk a Mile method of assessing fairness in ML

# counterfactual, How would Group A fare if subjected the policies for
# Group B? 

# arguments:
# 
#    data: a data frame
#    yName: name of Y column
#    qeFtn: a ftn from the qeML pkg
#    grpName: name of the grouping column
#    yYes: for binary Y, name of the positive factor level
#    grpIntervals: for continuous grouping variable, break into
#       this many intervals
#    naRM:  TRUE means na.rm = TRUE in mean() calls
#    stdErr:  compute standard errors

# value: matrix of the counterfactual means

# 'data' will be grouped according to 'grpName'; then 
# within each group 'qeFtn' will be applied with 'yName' as Y, resulting
# in 'qeObj'; then each 'qeObj' will be used to predict in all the other
# groups, with the resulting average Y values returned

# for now, default values will be used for qeFtn()

regAvg <- function(data,yName,qeFtn,grpName,
   yYes=NULL,grpIntervals=NULL,naRM=TRUE,stdErr=FALSE) 
{
   if(is.factor(data[[yName]])) {
      if (length(levels(data[[yName]])) > 2)
         stop('Y must be binary or continuous')
      classif <- TRUE
   } else classif <- FALSE

   # does qeFtn have a yesYVal arg?
   needsYesYVal <- classif && 'yesYVal' %in% names(formals(qeFtn))

   grpvar <- data[[grpName]]
   if (!is.factor(grpvar)) 
      data[[grpName]] <- cut(grpvar,grpIntervals)

   grps <- split(data,data[[grpName]])
   # get XY data by removing grouping variable
   grpsXY <- lapply(grps,function(grp) {grp[[grpName]] <- NULL; grp})
   # get X data by removing Y variable
   grpsX <- lapply(grpsXY,function(grp) {grp[[yName]] <- NULL; grp})
   # do the model fits
   qeObjs <- 
      if (!needsYesYVal) 
         lapply(grpsXY,function(grp) qeFtn(grp,yName,holdout=NULL))
      else
         lapply(grpsXY,function(grp) qeFtn(grp,yName,yesYVal=yYes,holdout=NULL))
   nGrps <- length(grps)
   avgs <- matrix(nrow=nGrps,ncol=nGrps)
   rownames(avgs) <- levels(data[[grpName]])
   colnames(avgs) <- levels(data[[grpName]])

   for (i in 1:nGrps)
      for (j in 1:nGrps) {
         if (i == j) {
            # EY = E[E(Y|X)]
            tmp <- grps[[i]][[yName]]
            if (classif) tmp <- as.numeric(tmp) - 1
            avgs[i,i] <- mean(tmp,na.rm=naRM)
         } else {
            grpsxi <- grpsX[[i]]
            tmp <- predict(qeObjs[[j]],grpsxi)

            if (stdErr) {
               # for now, just print out; later make it optional, part
               # of an R list return value; and this code should be a
               # separate function
               ai <- colMeans(grpsxi)
               cvbj <- vcov(qeObjs[[j]])
               cvbj <- cvbj[-1,-1]
               bj <- coef(qeObjs[[j]])
               bj <- bj[-1]
               cvxi <- cov(grpsxi)
               ni <- nrow(grpsxi)
               term1 <- t(ai) %*% cvbj %*% ai
               term2 <- t(bj) %*% cvxi %*% bj / ni
               cat(i,j,sqrt(term1+term2),'\n')
            }

           if (classif) {
               prbsY <- 
                  if (ncol(tmp$probs) == 2)  tmp$probs[,yYes]
                  else tmp$probs[,1]
                  tmp <- prbsY
            }
            avgs[i,j] <- mean(tmp,na.rm=naRM)
         }
      }
   
   avgs
}

# similar to regAvg(), but done separatewly so as to avoid code clutter
# and confusion

regAvgFPos <- function() 
{
   x <- 0
#                 num <- mean( (tmp >= 0.5) * (1 - tmp) )
#                 den <- mean(tmp >= 0.5)
#                 avgs[i,j] <- num/den
}

rA <- regAvg

