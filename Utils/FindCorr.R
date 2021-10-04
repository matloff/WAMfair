
# finds correlation between predicted value (a probability in the
# classification case) and the sensitive variable; if the latter is
# Bernoulli, replace the variable by its conditional probability given
# the de-weighted variables (estimated by logit, for convenience)

# it is assumed that variables that are directly sensitive have been
# excluded from the ML analysis that returned fittedObject; only
# "proxyorting" variables have been used in the analysis; e.g. we are
# worred about Age so it is excluded but YearsOfWorkExperience has not

# for now, the classification case only allows binary Y, still required
# to be an R factor but only 2 levels

# arguments:

#    data: as in qeFair*(); the full dataset, not e.g. a training set
#    yName: as in qeFair*()
#    sensNames: names in 'data' of sensitive cols, if any, 
#       excluded in ML analysis
#    proxyNames: column names in 'data' playing a "proxyorting role" in
#       'sensNames', i.e. related to 'sensNames' but not excluded
#    fittedObject: return value from qeFair*(); latter needs to have
#       been called with non-NULL holdout

corrsens <- function(data,yName,fittedObject,proxyNames,sensNames=NULL) 
{
   classif <- fittedObject$classif
   if (is.null(classif)) stop('"classif" missing in fittedObject')

   preds <- if(classif) fittedObject$holdoutPreds$probs
            else fittedObject$holdoutPreds

   xNames <- setdiff(names(data),c(yName,proxyNames))
   
   holdIdxs <- fittedObject$holdIdxs
   corrs <- NULL  # eventual output
   nCorrs <- 0
   for (sensNm in sensNames) {

      sens <- data[[sensNm]][holdIdxs]
      if (is.factor(sens)) {
         
         lvls <- levels(sens)
         tmp <- qeLogit(data[xNames],sensNm,family=binomial())
         nCols <- if (length(lvls) == 2) 1 else ncol(preds)
         for (i in 1:nCols) {
            sens <- preds[,i]
            nm <- paste0(sensNm,'.',colnames(preds)[i]
            corrs[nm] <- cor(preds[,1],sens)
         }
      } else {
         corrs <- c(corrs,cor(preds,sens)
         nCorrs <- nCorrs + 
         names(corrs[nCorrs] <- sensNm
         
         sensNm] <- cor(preds[,1],sens)
      }

   }

   corrs

}

