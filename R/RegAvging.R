
# 'data' will be grouped according to 'grpName'; then 
# within each group 'qeFtn' will be applied with 'yName' as Y, resulting
# in 'qeObj'; then each 'qeObj' will be used to predict in all the other
# groups, with the resulting average Y values returned

# for now, default values will be used for qeFtn()

regAvg <- function(data,yName,qeFtn,grpName,yYes=NULL) 
{
   if(is.factor(data[[yName]])) {
      if (length(levels(data[[yName]])) > 2)
         stop('Y must be binary or continuous')
      if (is.null(yYes)) stop('null yYes, binary Y')
      classif <- TRUE
   } else classif <- FALSE

   grps <- split(data,data[[grpName]])
   # get XY data by removing grouping variable
   grpsXY <- lapply(grps,function(grp) {grp[[grpName]] <- NULL; grp})
   # get X data by removing Y variable
   grpsX <- lapply(grpsXY,function(grp) {grp[[yName]] <- NULL; grp})
   # do the model fits
   qeObjs <- lapply(grpsXY,
      function(grp) qeFtn(grp,yName,holdout=NULL))

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
            avgs[i,i] <- mean(tmp)
         } else {
            grpsxj <- grpsX[[i]]
            tmp <- predict(qeObjs[[j]],grpsxj)
            if (classif) tmp <- tmp$probs[,yYes]
            avgs[i,j] <- mean(tmp)
         }
      }
   
   avgs
}

