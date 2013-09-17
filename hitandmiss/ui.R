library(shiny)
library(ggplot2)

dataset <- ticcompl

value_names <- names(dataset)
names(value_names) <- col_dict

shinyUI(pageWithSidebar(
  
  headerPanel(img(src='https://www.google.com/a/gopivotal.com/images/logo.gif?alpha=1&service=google_white',
                  align='bottom',
                  " - Insurance Data Demo")),
  
  sidebarPanel(
    
    sliderInput('sampleSize', 'Sample Size', min=1, max=nrow(dataset),
                value=min(1000, nrow(dataset)), step=500, round=0),
    
    selectInput('x', 'X', value_names, selected=names(value_names)[5]),
    selectInput('y', 'Y', value_names, selected=names(value_names)[4]),
    conditionalPanel(
      condition = "input.panelchoice == 'all'",
      selectInput('color', 'Color', c('None', value_names))
    ),
    
    sliderInput('alphaStrength', 'Alpha Strength', min=0, max=1, value=1, step=0.05, round=F),
    
    sliderInput('jitterStrength', 'Jitter Strength', min=0, max=5, value=1, step=0.1, round=F),
    
    conditionalPanel(
      condition = "input.panelchoice == 'marketing'",
      checkboxInput('showSelectX','Select X Variables'),
      conditionalPanel(
        condition = "input.showSelectX",
        uiOutput('selectXcontrols')
      ),
      checkboxInput('showSelectY','Select Y Variables'),
      conditionalPanel(
        condition = "input.showSelectY",
        uiOutput('selectYcontrols')
      )
    ),
    conditionalPanel(
      condition = "input.panelchoice == 'datascience'",
      selectInput('predictFcnName', 'Choose Predictive Algorithm', c("Random Forest" = "predictRandomForest", 
                                                                     "Logistic Regression" = "logisticRegression")),
      sliderInput('classCutoff', 'Classification Cutoff', min=0, max=0.3, value=0.05, step=0.001, round=F)
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("All Values", 
               p("This tab is for general exploration of the dataset."),
               plotOutput("plotall"),
               value="all"
      ), 
      tabPanel("Marketing selection", 
               p("On this tab, a selection of customers can be done manually and the results of this method are displayed on the bottom."),
               p("Groups of customers can be selected/deselected on the left. Initially, all customers are selected, that means you think
                 all customers will buy caravan insurance. Deselect groups that you think will not buy caravan insurance."),
               plotOutput("plotmarketing"),
               p("Displaying the results of the selection as a table."),
               tableOutput("mktconfusionMatrix"),
               value="marketing"
      ), 
      tabPanel("Model selection", 
               h3("Please wait a few seconds for the model to predict!"),
               p("On this tab, the outcome of the prediction with a data science model is illustrated."),
               p("Model output, a probability between 0 and 1 for a customer to buy the caravan insurance."),
               plotOutput("plotdatascience"),
               p("Model decision based on the classification cutoff, which can be tuned by the slider on the left."),
               plotOutput("plotdatascienceclassification"),
               p("Table representation of the model decision outcome."),
               tableOutput("dsconfusionMatrix"),
               p("Plot of the F-score for different classifation cutoffs."),
               plotOutput("plotperfcurve"),
               value="datascience"
      ),
      id="panelchoice")
  )
  
))