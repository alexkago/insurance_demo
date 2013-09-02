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

createFactor <- function(column,col_dict) {
  indxs <- match(column,col_dict$Values)
  repl_col <- col_dict$Labels[indxs]
  return(repl_col)
}

assignLevels <- function(ticcompl) {
  # data store
  col_dict <- c('Customer Subtype see L0',
                'Number of houses 1 - 10',
                'Avg size household 1 - 6',
                'Avg age see L1',
                'Customer main type see L2',
                'Roman catholic see L3',
                'Protestant',
                'Other religion',
                'No religion',
                'Married',
                'Living together',
                'Other relation',
                'Singles',
                'Household without children',
                'Household with children',
                'High level education',
                'Medium level education',
                'Lower level education',
                'High status',
                'Entrepreneur',
                'Farmer',
                'Middle management',
                'Skilled labourers',
                'Unskilled labourers',
                'Social class A',
                'Social class B1',
                'Social class B2',
                'Social class C',
                'Social class D',
                'Rented house',
                'Home owners',
                '1 car',
                '2 cars',
                'No car',
                'National Health Service',
                'Private health insurance',
                'Income < 30.000',
                'Income 30-45.000',
                'Income 45-75.000',
                'Income 75-122.000',
                'Income >123.000',
                'Average income',
                'Purchasing power class',
                'Contribution private third party insurance',
                'Contribution third party insurance (firms)',
                'Contribution third party insurane (agric.)',
                'Contribution car policies',
                'Contribution delivery van policies',
                'Contribution motorcycle/scooter policies',
                'Contribution lorry policies',
                'Contribution trailer policies',
                'Contribution tractor policies',
                'Contribution agricultural machines policies ',
                'Contribution moped policies',
                'Contribution life insurances',
                'Contribution private accident insurance policies',
                'Contribution family accidents insurance policies',
                'Contribution disability insurance policies',
                'Contribution fire policies',
                'Contribution surfboard policies',
                'Contribution boat policies',
                'Contribution bicycle policies',
                'Contribution property insurance policies',
                'Contribution social security insurance policies',
                'Number of private third party insurance 1 - 12',
                'Number of third party insurance (firms) ...',
                'Number of third party insurane (agriculture)',
                'Number of car policies',
                'Number of delivery van policies',
                'Number of motorcycle/scooter policies',
                'Number of lorry policies',
                'Number of trailer policies',
                'Number of tractor policies',
                'Number of agricultural machines policies',
                'Number of moped policies',
                'Number of life insurances',
                'Number of private accident insurance policies',
                'Number of family accidents insurance policies',
                'Number of disability insurance policies',
                'Number of fire policies',
                'Number of surfboard policies',
                'Number of boat policies',
                'Number of bicycle policies',
                'Number of property insurance policies',
                'Number of social security insurance policies',
                'Number of mobile home policies 0 - 1')
  
  L0_dict <- data.frame(
    Values=1:41,
    Labels=c('High Income, expensive child',
             'Very Important Provincials',
             'High status seniors',
             'Affluent senior apartments',
             'Mixed seniors',
             'Career and childcare',
             'Dinki\'s (double income no kids)',
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
             'Couples with teens \'Married with children\'',
             'Mixed small town dwellers',
             'Traditional families',
             'Large religous families',
             'Large family farms',
             'Mixed rurals'))
  
  L1_dict <- data.frame(
    Values=1:6,
    Labels=c('20-30 years',
             '30-40 years',
             '40-50 years',
             '50-60 years',
             '60-70 years',
             '70-80 years'))
  
  L2_dict <- data.frame(
    Values=1:10,
    Labels=c('Successful hedonists',
             'Driven Growers',
             'Average Family',
             'Career Loners',
             'Living well',
             'Cruising Seniors',
             'Retired and Religeous',
             'Family with grown ups',
             'Conservative families',
             'Farmers'))
  
  L3_dict <- data.frame(
    Values=0:9,
    Labels=c('0%',
             '1 - 10%',
             '11 - 23%',
             '24 - 36%',
             '37 - 49%',
             '50 - 62%',
             '63 - 75%',
             '76 - 88%',
             '89 - 99%',
             '100%'))
  
  L4_dict <- data.frame(
    Values=0:9,
    Labels=c('0',
             '1 - 49',
             '50 - 99',
             '100 - 199',
             '200 - 499',
             '500 - 999',
             '1000 - 4999',
             '5000 - 9999',
             '10.000 - 19.999',
             '20.000 - ?'))
  
  ticcompl$MOSTYPE <- createFactor(ticcompl$MOSTYPE,L0_dict)
  ticcompl$MGEMLEEF <- createFactor(ticcompl$MGEMLEEF,L1_dict)
  ticcompl$MOSHOOFD <- createFactor(ticcompl$MOSHOOFD,L2_dict)
    
  for (colname in names(ticcompl)[6:43]) {
    print(colname)
    ticcompl[[colname]] <- createFactor(ticcompl[[colname]],L3_dict)
  }
    
  for (colname in names(ticcompl)[44:64]) {
    ticcompl[[colname]] <- createFactor(ticcompl[[colname]],L4_dict)
  }
  
  return(ticcompl)
}