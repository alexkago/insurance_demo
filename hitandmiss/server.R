library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
  
  dataset <- reactive({
    subset <- ticcompl[sample(nrow(ticcompl), input$sampleSize),]
    subset$xrange <- subset[[input$x]] %in% input$selectedX
    subset$yrange <- subset[[input$y]] %in% input$selectedY
    
    subset$comb_range <- subset$xrange & subset$yrange
        
    return(subset)
  })
  
  output$plot <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + geom_point()
    
    p <- p + xlab(col_dict[names(ticcompl) == input$x]) + ylab(col_dict[names(ticcompl) == input$y])
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    if (input$jitter) {
      width_strength <- ifelse(is.factor(ticcompl[[input$x]]),
                               nlevels(ticcompl[[input$x]]),
                               diff(range(ticcompl[[input$x]]))) * 0.01 * input$jitterStrength
      height_strength <- ifelse(is.factor(ticcompl[[input$y]]),
                               nlevels(ticcompl[[input$y]]),
                               diff(range(ticcompl[[input$y]]))) * 0.01 * input$jitterStrength
      p <- p + geom_jitter(position=position_jitter(width=width_strength,height=height_strength))
    }
    
    p <- p + aes_string(color='comb_range') + scale_colour_manual(values = c("grey","black"))

    print(p)
    
  }, height=550)
  
  output$selectXcontrols <- renderUI({
    if (is.factor(dataset()[[input$x]]))
      choices <- levels(dataset()[[input$x]])
    else
      choices <- sort(unique(dataset()[[input$x]]))
    
    checkboxGroupInput("selectedX", "Select X Values", choices, selected = choices)
  })
  
  output$selectYcontrols <- renderUI({
    if (is.factor(dataset()[[input$y]]))
      choices <- levels(dataset()[[input$y]])
    else
      choices <- sort(unique(dataset()[[input$y]]))
    
    checkboxGroupInput("selectedY", "Select Y Values", choices, selected = choices)
  })
  
})