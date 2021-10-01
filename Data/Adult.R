
# famous Adult dataset, some preprocessing

adult <- read.csv('~/Datasets/Adult/adult.data',stringsAsFactors=T)
names(adult) <- c('age', 'workclass', 'fnlwgt', 'education', 'education-num', 'marital-status', 'occupation', 'relationship', 'race', 'sex', 'capital-gain', 'capital-loss', 'hours-per-week', 'native-country')
adult$fnlwgt <- NULL
adult$'education-num' <- NULL

