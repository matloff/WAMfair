
# 'data' will be grouped according to 'grpName'; then 
# within each group 'qeFtn' will be applied with 'yName' as Y, resulting
# in 'qeObj'; then each 'qeObj' will be used to predict in all the other
# groups, with the resulting average Y values returned

# for now, default values will be used for qeFtn()

regAvg <- function(data,yName,qeFtn,grpName) 
{
   grps <- split(data,data[['grpName']]
   qeObjs <- lapply(grps,
      function(grp) qeFtn(grp$data,yName)


   predict(qeObj,newxDF) / nrow(newxDF))
}

