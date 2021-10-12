
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

