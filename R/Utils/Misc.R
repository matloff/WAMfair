
# modified version of qeML::predictHoldout()

# globals:  trn, tst, yName, preds

predictHoldoutFair <- defmacro(res,
   expr={
      # ycol <- which(names(data) == yName);
      ycol <- which(names(tst) == yName);
      ycolData <- which(names(data) == yName);
      tstx <- tst[,-ycol,drop=FALSE];
      preds <- predict(res,tstx);
      res$holdoutPreds <- preds;
      if (res$classif) {
         if (is.numeric(preds)) preds <- list(predClasses=preds)
         res$testAcc <- mean(preds$predClasses != tst[,ycol])
         res$baseAcc <- 1 - max(table(data[,ycolData])) / nrow(data)
         res$confusion <- regtools::confusion(tst[,ycol],preds$predClasses)
         doOneConfMatrix <- function(sensName) 
         {
            tmp <- sensName
            sens <- data[[tmp]][idxs]
            table(tst[,ycol],preds$predClasses,sens)
         }
         res$sensConfusion <- lapply(sensNames,doOneConfMatrix)
      } else {
         res$testAcc <- mean(abs(preds - tst[,ycol]))
         res$baseAcc <-  mean(abs(tst[,ycol] - mean(data[,ycolData])))
      }
   }
)

# calculate the confusion matrix for each factor in sensNames (assumed
# to all be factors)

calcSensConfusion <- function(data,dataNoSens,yName,idxs,preds,sensNames) 
{
   ycol <- which(names(dataNoSens) == yName)
   tst <- dataNoSens[idxs,]

   doOneConfMatrix <- function(sensName) 
   {
      tmp <- sensName
      sens <- data[[tmp]][idxs]
      table(tst[,ycol],preds$predClasses,sens)
   }

   lapply(sensNames,doOneConfMatrix)
         
}

# calculate conditional probabilities in a 2x2 table output by
# calcSensConfusion(); say table is rbind(c(u,v),c(w,x)); then output
# c(x/(w+x),x/(v+x)); tableNum is the third coordinate of the 3-dim
# array output by calcSensConfusion()

nonoyesyes <- function(cSCout,tableNum) 
{
   ary <- cSCout[[1]]
   tbl <- ary[,,tableNum]
   x <- tbl[2,2]; w <- tbl[2,1]; v <- tbl[1,2]; u <- tbl[1,1]
   l <- list(
      NoPredNoActual=u/(u+w),
      NoActualNoPred=u/(u+v),
      YesPredYesActual=x/(v+x),
      YesActualYesPred=x/(x+w))     
   c(l[[1]],l[[2]],l[[3]],l[[4]])
}

# main privacy criterion, based on avoiding disparate treatment (not
# disparate impact); ideally, E(Y | X,S) should be independent of S, and
# we need a measure of how close we meet that ideal 

# our measure is based on the fact the disparate treatment means that
# individuals of similar X should have similar predicted Y

# for now, we require that Y (coded 0,1) be binary or scalar numeric,
# and that S be categorical; the procedure is this:

# at each X_i, we find the k-nearest neighbors of X_i (including X_i);
# in each neighborhood, we partition the points according to their
# values of S; let m denote the number of levels of S; then the
# partitioning gives us m groups (some possibly empty); then calculate
# mean predicted Y for each group; then compute the ratio of mean in
# the various groups, relative to a reference level, for now levels(S)[1];
# finally compute the mean ratios across all neighborhoods; the closer
# these are to 1.0, the fairer the analysis

dispTreat <- function(data,yName,sName,classif,qeFairOut,k,nSam) 
{
   ycol <- which(names(data) == yName)
   scol <- which(names(data) == sName)
   data <- data[sample(1:nrow(data),nSam),]
   dataX <- data[,-c(ycol,scol)]
   dataXdumms <- factorsToDummies(dataX)
   S <- data[[sName]]
   if (is.numeric(S)) S <- as.factor(S)
   sLevels <- levels(S)
   nSlevels <- length(sLevels)
   n <- nrow(data)

   predictGroup <- function(grp)
   {
      grpIdxs <- bySlevel[[grp]]
      preds <- predict(qeFairOut,dataX[grpIdxs,])
      if (classif) preds <- preds$probs[,2]
      preds
   }

   findNbhd<- function(xrow) 
   {
      require(FNN)
      xrow <- matrix(xrow,nrow=1)
      tmp <- FNN::get.knnx(data=dataXdumms,query=xrow,k=k)
      tmp$nn.index[,1:k]
   }

   ratioMeans <- matrix(nrow=n,ncol=nSlevels-1)
   for (i in 1:nrow(data)) {
      nearIdxs <- findNbhd(dataXdumms[i,])  # indices of the neighborhood
      bySlevel <- 
         split(nearIdxs,S[nearIdxs])  # one set of indices for each S value
      checkEmpty <- sapply(bySlevel,length)
      if (any(checkEmpty == 0)) next
      preds <- lapply(1:length(bySlevel),predictGroup)  
print(preds[[2]][1])
      for (g in 2:nSlevels) {
         tmp <- mean(preds[[1]]) / mean(preds[[g]])
         ratioMeans[i,g-1] <- tmp
      }
   }

   colMeans(ratioMeans,na.rm=TRUE)

}

