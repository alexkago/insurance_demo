getData <- function() {
  # read in data
  ticdata2000 <- read.table(sep='\t',file='data/ticdata2000.txt')
  ticeval2000 <- read.table(sep='\t',file='data/ticeval2000.txt')
  tictgts2000 <- read.table(sep='\t',file='data/tictgts2000.txt')
  
  
  # read in dictionary for column names
  dict <- read.table(sep=' ',file='data/dictionary.txt',fill=T)
  datacolnames <- as.character(dict[,2])
  datacolnames <- datacolnames[datacolnames != '-' & datacolnames != '' 
                               & datacolnames != 'Name' & datacolnames != '1']
  
  colnames(ticdata2000) <- datacolnames
  
  
  # create test dataset from train and targets
  tictest2000 <- cbind(ticeval2000,tictgts2000)
  colnames(tictest2000) <- datacolnames
  
  
  # combine datasets
  ticcompl <- rbind(ticdata2000,tictest2000)
  
  return(ticcompl)
}

assignLevels <- function(ticcompl) {
  L0_dict <- c('High Income, expensive child',
               'Very Important Provincials',
               'High status seniors',
               'Affluent senior apartments',
               'Mixed seniors',
               'Career and childcare',
               'Dinki''s (double income no kids)',
               'Middle class families',
               'Modern, complete families',
               'Stable family',
               'Family starters',
               'Affluent young families',
               'Young all american family',
               'Junior cosmopolitan',
               'Senior cosmopolitans',
               'Students in apartments',
               'Fresh masters in the city',
               'Single youth',
               'Suburban youth',
               'Etnically diverse',
               'Young urban have-nots',
               'Mixed apartment dwellers',
               'Young and rising',
               'Young, low educated', 
               'Young seniors in the city',
               'Own home elderly',
               'Seniors in apartments',
               'Residential elderly',
               'Porchless seniors: no front yard',
               'Religious elderly singles',
               'Low income catholics',
               'Mixed seniors',
               'Lower class large families',
               'Large family, employed child',
               'Village families',
               'Couples with teens ''Married with children''',
               'Mixed small town dwellers',
               'Traditional families',
               'Large religous families',
               'Large family farms',
               'Mixed rurals')
}