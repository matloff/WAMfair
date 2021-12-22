
# 'data' will be grouped according to 'grpName'; then 
# within each group 'qeFtn' will be applied with 'yName' as Y, resulting
# in 'qeObj'; then each 'qeObj' will be used to predict in all the other
# groups, with the resulting average Y values returned

regAvg <- function(data,yName,qeFtn,grpName) 
{
   predict(qeObj,newxDF) / nrow(newxDF))
}

