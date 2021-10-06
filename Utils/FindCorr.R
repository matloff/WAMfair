
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
               browser()
      sens <- data[sensNm][holdIdxs,]
      # the case of factor sens is more complex, for 2 reasons:
      # 1. we must change the 1 or more dummies to probabilities, so
      # that cor() makes sense, and 2. if this is a categorical
      # variable, not Bernoulli, then we must call cor() on each of the
      # resulting dummies
      if (is.factor(sens)) {
         # item 1 above
         lvls <- levels(sens)
         sens <- as.numeric(sens)
         nLvls <- length(lvls)
         if (nLvls == 2) {
            frml <- paste0(sensNm,' ~ .')
            frml <- as.formula(frml)
            tmp <- glm(frml,data[xSensNames],family=binomial())
            sensProbs <- tmp$fitted.values[holdIdxs]
            corrs <- c(corrs,cor(preds,sensProbs)^2)
            nCorrs <- nCorrs + 1
            names(corrs)[nCorrs] <- sensNm
         } else {
            # item 2; want 1 col of probs for Bernoulli sens, or 1 col 
            # for each dummy in the categorical case
            tmp <- qeLogit(data[xSensNames],sensNm,holdout=NULL)
            for (i in 1:length(tmp$glmOuts)) {
               glmout <- tmp$glmOuts[[i]]
               sens <- glmout$fitted.values[holdIdxs]
               corrs <- c(corrs,cor(preds,sens)^2)
               nCorrs <- nCorrs + 1
               nm <- paste0(sensNm,'.',lvls[i])
               names(corrs)[nCorrs] <- nm
            }
         }
      } else {
         corrs <- c(corrs,cor(preds,sens)^2)
         nCorrs <- nCorrs + 1
         names(corrs)[nCorrs] <- sensNm
      }
   }

   corrs

}

