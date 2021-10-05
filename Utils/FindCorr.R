
# finds correlation between predicted value (a probability in the
# classification case) and the sensitive variable; if the latter is
# Bernoulli, replace the variable by its conditional probability given
# the de-weighted variables (estimated by logit, for convenience)

# it is assumed that variables that are directly sensitive have been
# excluded from the ML analysis that returned fittedObject; only
# "proxyorting" variables have been used in the analysis; e.g. we are
# worred about Age so it is excluded but YearsOfWorkExperience has not

# for now, the classification case only allows binary Y, still required
# to be an R factor but only 2 levels; we allow the sensitive variable
# to be categorical, though

# arguments:

#    data: as in qeFair*(); the full dataset, not e.g. a training set
#    yName: as in qeFair*()
#    sensNames: names in 'data' of sensitive cols, if any, 
#       excluded in ML analysis; if no data on this, corrsens() won't be
#       called
#    fittedObject: return value from qeFair*(); latter needs to have
#       been called with non-NULL holdout

corrsens <- function(data,yName,fittedObject,sensNames=NULL) 
{
   classif <- fittedObject$classif
   if (is.null(classif)) stop('"classif" missing in fittedObject')

   preds <- if(classif) fittedObject$holdoutPreds$probs[,1]
            else fittedObject$holdoutPreds
   xNames <- setdiff(names(data),c(yName,sensNames))
   xSensNames <- setdiff(names(data),c(yName))
   holdIdxs <- fittedObject$holdIdxs

   corrs <- NULL  # eventual output
   nCorrs <- 0
   for (sensNm in sensNames) {
      sens <- data[[sensNm]][holdIdxs]
      # the case of factor sens is more complex, for 2 reasons:
      # 1. we must change the 1 or more dummies to probabilities, so
      # that cor() makes sense, and 2. if this is a categorical
      # variable, not Bernoulli, then we must call cor() on each of the
      # resulting dummies
      if (is.factor(sens)) {
         # item 1 above
         tmp <- qeLogit(data[xSensNames],sensNm,family=binomial())
         sensProbs <- tmp$preClasses$probs
         # item 2; want 1 col of probs for Bernoulli sens, or 1 col for
         # each dummy in the categorical case
         lvls <- levels(sens)
         nCols <- if (length(lvls) == 2) 1 else ncol(sensProbs)
         for (i in 1:nCols) {
            sens <- sensProbs[,i]
            corrs <- c(corrs,cor(preds[,i],sens))
            nCorrs <- nCorrs + 1
            nm <- paste0(sensNm,'.',colnames(preds)[i])
            names(corrs)[nCorrs] <- nm
         }
      } else {
         corrs <- c(corrs,cor(preds,sens))
         nCorrs <- nCorrs + 1
         names(corrs)[nCorrs] <- sensNm
      }
   }

   corrs

}

