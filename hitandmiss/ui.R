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
    
    conditionalPanel(condition = "input.panelchoice == 'marketing'",
                     uiOutput('selectXcontrols'),
                     uiOutput('selectYcontrols')
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("All Values", 
               h2(textOutput("All the potential customers")),
               plotOutput("plotall"),
               value="all"
      ), 
      tabPanel("Marketing selection", 
               h2(textOutput("Customers selected by marketing")),
               plotOutput("plotmarketing"),
               value="marketing"
      ), 
      tabPanel("Model selection", 
               h2(textOutput("Customers selected by data science")),
               plotOutput("plotdatascience"),
               value="datascience"
      ),
      id="panelchoice")
  )
  
))