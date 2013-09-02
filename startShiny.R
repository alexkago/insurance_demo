source('getData.R')

ticcompl <- assignLevels(getData())

runApp('shiny')