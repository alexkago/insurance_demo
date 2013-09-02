library(shiny)
library(ggplot2)

dataset <- ticcompl

value_names <- names(dataset)
names(value_names) <- col_dict

shinyUI(pageWithSidebar(
  
  headerPanel("Insurance Dataset Explorer"),
  
  sidebarPanel(
    
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=500, round=0),
    
    selectInput('x', 'X', value_names),
    selectInput('y', 'Y', value_names, value_names[[2]]),
    selectInput('color', 'Color', c('None', value_names)),
    
    checkboxInput('jitter', 'Jitter'),
    sliderInput('jitterStrength', 'Jitter Strength', min=0, max=5, value=1, step=0.1, round=F),
    checkboxInput('smooth', 'Smooth'),
    
    selectInput('facet_row', 'Facet Row', c(None='.', value_names)),
    selectInput('facet_col', 'Facet Column', c(None='.', value_names))
  ),
  
  mainPanel(
    plotOutput('plot')
  )
))