
# finds correlation between predicted value (a probability in the
# classification case) and the sensitive variable; if the latter is
# Bernoulli, replace the variable by its conditional probability given
# the de-weighted variables (estimated by logit, for convenience)

# arguments:

#    data: as in qeFair*(); the full dataset, not e.g. a training set
#    yName: as in qeFair*()
#    fittedObject: return value from qeFair*(); latter needs to have
#       been called with non-NULL holdout
#    sens variables (just 1 for now), length 

corsens <- function(data,yName,fittedObject,sens) 
{
   classif <- fittedObject$classif
   if (is.null(classif)) stop('"classif" missing in fittedObject')

   preds <- if(classif) fittedObject$holdoutPreds$probs
            else fittedObject$holdoutPreds

   if (is.factor(sens)) {
      if (length(levels(sens)) != 2) stop('Y must have 2 levels')
      deWeightNames <- fittedObject$deWeightNames
      tmp <- glm(sens ~ data[deWeightNames],family=binomial())
      sens <- tmp$fitted.values
   }

}

