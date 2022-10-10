
# make all variables numeric except Y and the group variable

# args:

#    data: data frame, usual qeML form
#    yName: name of Y, usual qeML form; must be an R factor for
#       classification case
#    grpName: must be an R factor

makeAllXNumeric <- function(data,yName,grpName)
{
   yCol <- which(names(data) == yName)
   grpCol <- which(names(data) == grpName)

   X <- data[,-c(yCol,grpCol)]
   xd <- factorsToDummies(X,omitLast=TRUE)
   res <- cbind(xd,data[,grpCol],data[,yCol])

   nColRes <- ncol(res)
   colnames(res)[(nColRes-1):nColRes] <- c(grpName,yName)

   res <- as.data.frame(res)

   if (is.factor(data[[yName]]))
      res[,nColRes] <- as.factor(res[,nColRes])
   res[,nColRes-1] <- as.factor(res[,nColRes-1])

   res

}

mAXN <- makeAllXNumeric

Num01ToFactor01 <- function(x)   # x a vector of 0s, 1s
{
   xChar <- ifelse(x == 1,'1','0')
   as.factor(xChar)
}

