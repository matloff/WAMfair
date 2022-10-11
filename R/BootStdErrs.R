
# for each (i,j), u != j, find a bootstrapped standard errors 
# for estimated nu_ii, nu_ij and nu_ii - nu_ij

bootSEs <- function(data,yName,qeFtn,grpName,yYes,nResamps,fPos=FALSE) 
{
   nRows <- nrow(data)
   nGrps <- length(levels(data[[grpName]]))

   z <- replicate(nResamps,
         {rows <- sample(1:nRows,nRows,replace=TRUE);
          regAvg(data[rows,],yName,qeFtn,grpName,yYes,fPos=fPos)})

   nDF <- nGrps * (nGrps-1)

   res <- data.frame(i=rep(nDF,'0'),j=rep(nDF,'0'),
      myGrpEst=rep(nDF,0),
      myGrpSE=rep(nDF,0),
      theirGrpEst=rep(nDF,0),
      theirGrpSE=rep(nDF,0),
      bias=rep(nDF,0),
      biasSE=rep(nDF,0))

   grpLvls <- levels(data[[grpName]])
   dfRow <- 1  
   for (i in 1:nGrps) 
      for (j in 1:nGrps) 
         if (i != j) {
            myBootVals <- z[i,i,] 
            theirBootVals <- z[i,j,]
            myGrpEst <- mean(myBootVals)
            myGrpSE <- sd(myBootVals)
            theirGrpEst <- mean(theirBootVals)
            theirGrpSE <- sd(theirBootVals)
            bias <- myGrpEst - theirGrpEst
            biasSE <- sd(myBootVals - theirBootVals)
            res[dfRow,1] <- grpLvls[i]
            res[dfRow,2] <- grpLvls[j]
            res[dfRow,3] <- myGrpEst
            res[dfRow,4] <- myGrpSE
            res[dfRow,5] <- theirGrpEst
            res[dfRow,6] <- theirGrpSE
            res[dfRow,7] <- bias
            res[dfRow,8] <- biasSE
            dfRow <- dfRow + 1
         }

   res
}

# u <- z1[1,1,1:100] - z1[1,2,1:100]
# sd(u)


