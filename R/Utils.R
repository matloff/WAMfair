
# to set up stdErr with qeLin, make all variables numeric except Y and
# the group variable

makeAllXNumeric <- function(data,yName,grpName)
{
   yCol <- which(names(data) == yName)
   grpCol <- which(names(data) == grpName)

   X <- data[,-c(yCol,grpCol)]
   xd <- factorsToDummies(X,omitLast=TRUE)
   res <- cbind(xd,data[,grpCol],data[,yCol])

   nColRes <- ncol(res)
   colnames(res)[(nColRes-1):nColRes] <- c(grpName,yName)

   xd <- as.data.frame(xd)

   res
}

