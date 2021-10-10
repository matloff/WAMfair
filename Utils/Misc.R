
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
         tmp <- sensNames[1]
         sens <- data[[tmp]][idxs]
         res$sensConfusion <- table(tst[,ycol],preds$predClasses,sens)
      } else {
         res$testAcc <- mean(abs(preds - tst[,ycol]))
         res$baseAcc <-  mean(abs(tst[,ycol] - mean(data[,ycolData])))
      }
   }
)
         
