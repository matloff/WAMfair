
# finds correlation between predicted value (a probability in the
# classification case) and the sensitive variable; if the latter is
# Bernoulli, replace the variable by its conditional probability given
# the de-weighted variables (estimated by logit, for convenience)

# it is assumed that variables that are directly sensitive have been
# excluded from the ML analysis that returned fittedObject; only
# "supporting" variables have been used in the analysis; e.g. we are
# worred about Age so it is excluded but YearsOfWorkExperience has not

# for now, the classification case only allows binary Y, still required
# to be an R factor but only 2 levels

# arguments:

#    data: as in qeFair*(); the full dataset, not e.g. a training set
#    yName: as in qeFair*()
#    sensNames: names in 'data' of sensitive cols, excluded in ML analysis
#    suppNames: column names in 'data' playing a "supporting role" in
#       'sensNames', i.e. related to 'sensNames' but not excluded
#    fittedObject: return value from qeFair*(); latter needs to have
#       been called with non-NULL holdout

corrsens <- function(data,yName,fittedObject,suppNames) 
{
   classif <- fittedObject$classif
   if (is.null(classif)) stop('"classif" missing in fittedObject')

   preds <- if(classif) fittedObject$holdoutPreds$probs
            else fittedObject$holdoutPreds

   xNames <- setdiff(names(data),c(yName,suppNames))
   
   holdIdxs <- fittedObject$holdIdxs
   corrs <- rep(0,length(suppNames))  # eventual output
   names(corrs) <- suppNames
   for (suppNm in suppNames) {

      supp <- data[[suppNm]][holdIdxs]
      if (is.factor(supp)) {
         if (length(levels(supp)) != 2) stop('factor Y must have 2 levels')
         tmp <- glm(supp ~ .,data=data[xNames],family=binomial())
         supp <- tmp$fitted.values
      }
      corrs[suppNm] <- cor(preds[,1],supp)

   }

   corrs

}

