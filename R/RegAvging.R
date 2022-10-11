
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

# value: matrix of the counterfactual means

# 'data' will be grouped according to 'grpName'; then 
# within each group 'qeFtn' will be applied with 'yName' as Y, resulting
# in 'qeObj'; then each 'qeObj' will be used to predict in all the other
# groups, with the resulting average Y values returned

# for now, default values will be used for qeFtn()

regAvg <- function(data,yName,qeFtn,grpName,
   yYes=NULL,grpIntervals=NULL,naRM=TRUE,fPos=FALSE) 
{

   if (!is.function(qeFtn)) 
      stop('arg 2 must be a function, not a function name')

   yCol <- which(names(data) == yName)
   grpCol <- which(names(data) == grpName)

   if(is.factor(data[[yName]])) {
      if (length(levels(data[[yName]])) > 2)
         stop('Y must be binary or continuous')
      classif <- TRUE
   } else classif <- FALSE

   if (fPos && !classif) 
      stop('FPR only makes sense for classification problems')

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

   avgs <- matrix(nrow=nGrps,ncol=nGrps)  # our ultimate output
   rownames(avgs) <- levels(data[[grpName]])
   colnames(avgs) <- levels(data[[grpName]])

   # what would happen if group i were subjected to the environment 
   # of group j?
   for (i in 1:nGrps)
      for (j in 1:nGrps) {
         if (! fPos) {
            if (i == j) {
               # EY = E[E(Y|X)]
               tmp <- grps[[i]][[yName]]
               if (classif) tmp <- as.numeric(tmp) - 1
               avgs[i,i] <- mean(tmp,na.rm=naRM)
            } else {
               grpsxi <- grpsX[[i]]
               tmp <- predict(qeObjs[[j]],grpsxi)
   
              if (classif) {
                  prbsY <- 
                     if (ncol(tmp$probs) == 2)  tmp$probs[,yYes]
                     else tmp$probs[1,]
                     tmp <- prbsY
               }
               avgs[i,j] <- mean(tmp,na.rm=naRM)
            }
         } else {  
            # compute false positives; some duplicated code, but
            # hopefully clearer that way
            grpsxi <- grpsX[[i]]
            tmp <- predict(qeObjs[[j]],grpsxi)
            prbsY <- 
               if (ncol(tmp$probs) == 2)  tmp$probs[,yYes]
               else tmp$probs[1,]
            tmp <- prbsY
            num <- mean( (tmp >= 0.5) * (1 - tmp) )
            den <- mean(tmp >= 0.5)
            avgs[i,j] <- num/den
         }
      }
   
   avgs
}

rA <- regAvg

### regAvgFPos <- function() 
### {
###    num <- mean( (tmp >= 0.5) * (1 - tmp) )
###    den <- mean(tmp >= 0.5)
###    avgs[i,j] <- num/den
### }

