library('shiny')
library('ggplot2')

source('getData.R')

ticcompl <- assignLevels(getData())

runApp('hitandmiss')