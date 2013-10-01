library(shiny)
library(ggplot2)

shinyServer(function(input, output) {

  dataset <- reactive({
    subset <- ticcompl[sample(nrow(ticcompl), input$sampleSize),]
  })
  
  traindataset <- reactive({
    subset <- ticcompl[sample(nrow(ticcompl), input$sampleSize),]
  })
  
  output$plotall <- renderPlot({
    
    p <- ggplot(dataset(), aes_string(x=input$x, y=input$y)) + theme(axis.text.x = element_text(angle = 40,hjust=1))

    p <- p + xlab(col_dict[names(ticcompl) == input$x]) + ylab(col_dict[names(ticcompl) == input$y])
    
    if (input$color != 'None') {
      p <- p + aes_string(color=input$color)
    }
    
    if (input$jitterStrength > 0) {
      width_strength <- ifelse(is.factor(ticcompl[[input$x]]),
                               nlevels(ticcompl[[input$x]]),
                               diff(range(ticcompl[[input$x]]))) * 0.01 * input$jitterStrength
      height_strength <- ifelse(is.factor(ticcompl[[input$y]]),
                               nlevels(ticcompl[[input$y]]),
                               diff(range(ticcompl[[input$y]]))) * 0.01 * input$jitterStrength
      p <- p + geom_jitter(position=position_jitter(width=width_strength,height=height_strength),alpha=input$alphaStrength)
    } else {
      p <- p + geom_point(alpha=input$alphaStrength)
    }
    
    print(p)
  })
  
  output$plotmarketing <- renderPlot({
    plotData <- dataset()
    
    plotData$xrange <- plotData[[input$x]] %in% input$selectedX
    plotData$yrange <- plotData[[input$y]] %in% input$selectedY
    plotData$comb_range <- plotData$xrange & plotData$yrange
    plotData$comb_range <- ifelse(plotData$comb_range,'black','Predict No Buy')
    plotData$comb_range <- ifelse(plotData$comb_range == 'black',
                                  ifelse(plotData$CARAVAN == 1,'Correctly Predicted Buy','Incorrectly Predicted Buy'),
                                  plotData$comb_range)
    
    plotData$comb_range <- factor(plotData$comb_range)
        
    p <- ggplot(plotData, aes_string(x=input$x, y=input$y)) + theme(axis.text.x = element_text(angle = 40, hjust = 1))
    
    p <- p + xlab(col_dict[names(ticcompl) == input$x]) + ylab(col_dict[names(ticcompl) == input$y])

    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    if (input$jitterStrength > 0) {
      width_strength <- ifelse(is.factor(ticcompl[[input$x]]),
                               nlevels(ticcompl[[input$x]]),
                               diff(range(ticcompl[[input$x]]))) * 0.01 * input$jitterStrength
      height_strength <- ifelse(is.factor(ticcompl[[input$y]]),
                                nlevels(ticcompl[[input$y]]),
                                diff(range(ticcompl[[input$y]]))) * 0.01 * input$jitterStrength
      p <- p + geom_jitter(position=position_jitter(width=width_strength,height=height_strength),alpha=input$alphaStrength)
    } else {
      p <- p + geom_point(alpha=input$alphaStrength)
    }
    #browser()
    colorpalette <- c("Correctly Predicted Buy" = "green","Predict No Buy" = "grey","Incorrectly Predicted Buy" = "grey30")
    p <- p + aes_string(color='comb_range') + scale_colour_manual(values = colorpalette)
    #p <- p + theme(legend.position="none")
    
    print(p)
    
  })
  
  output$mktconfusionMatrix <- renderTable({
    plotData <- dataset()
        
    plotData$xrange <- plotData[[input$x]] %in% input$selectedX
    plotData$yrange <- plotData[[input$y]] %in% input$selectedY
    plotData$comb_range <- plotData$xrange & plotData$yrange
    
    plotData$comb_range <- ifelse(plotData$comb_range,"Predict Buy","Predict No Buy")
    plotData$CARAVAN <- ifelse(plotData$CARAVAN == 0,"Actual No Buy","Actual Buy")
        
    table(plotData$CARAVAN,plotData$comb_range)
  })
  
  output$mktCostAnalysis <- renderTable({
    plotData <- dataset()
    
    plotData$xrange <- plotData[[input$x]] %in% input$selectedX
    plotData$yrange <- plotData[[input$y]] %in% input$selectedY
    plotData$comb_range <- plotData$xrange & plotData$yrange
    
    descriptions <- c("Budget spent on Marketing:",
                      "Budget spent successfully:",
                      "Successful Spending:",
                      "Potential Return on Marketing:",
                      "Return on Marketing:",
                      "Realized Potential:",
                      "ROI:")
    
    succSpend <- length(which(plotData$comb_range & plotData$CARAVAN == 1))/length(which(plotData$comb_range))*100
    realPot <- length(which(plotData$comb_range & plotData$CARAVAN == 1))/length(which(plotData$CARAVAN == 1))*100
    roi <- ((length(which(plotData$comb_range & plotData$CARAVAN == 1))*as.numeric(input$mktReturn))/
      (length(which(plotData$comb_range))*as.numeric(input$mktCost)))*100
    
    values <- c(length(which(plotData$comb_range))*as.numeric(input$mktCost),
                length(which(plotData$comb_range & plotData$CARAVAN == 1))*as.numeric(input$mktCost),
                paste(round(succSpend,digits=2),'%'),
                length(which(plotData$CARAVAN == 1))*as.numeric(input$mktReturn),
                length(which(plotData$comb_range & plotData$CARAVAN == 1))*as.numeric(input$mktReturn),
                paste(round(realPot,digits=2),'%'),
                paste(round(roi,digits=2),'%'))
    data.frame(row.names=descriptions,Traditional.Targeting=values)
  })
  
#   output$plotdatascience <- renderPlot({
#     plotData <- dataset()
# 
#     plotData$predictions <- eval(call(input$predictFcnName))
# 
#     p <- ggplot(plotData, aes_string(x=input$x, y=input$y))
#     
#     p <- p + xlab(col_dict[names(ticcompl) == input$x]) + ylab(col_dict[names(ticcompl) == input$y])
#     
#     if (input$color != 'None')
#       p <- p + aes_string(color=input$color)
#     
#     if (input$jitterStrength > 0) {
#       width_strength <- ifelse(is.factor(ticcompl[[input$x]]),
#                                nlevels(ticcompl[[input$x]]),
#                                diff(range(ticcompl[[input$x]]))) * 0.01 * input$jitterStrength
#       height_strength <- ifelse(is.factor(ticcompl[[input$y]]),
#                                 nlevels(ticcompl[[input$y]]),
#                                 diff(range(ticcompl[[input$y]]))) * 0.01 * input$jitterStrength
#       p <- p + geom_jitter(position=position_jitter(width=width_strength,height=height_strength),alpha=input$alphaStrength)
#     } else {
#       p <- p + geom_point(alpha=input$alphaStrength)
#     }
#     
#     p <- p + aes_string(color='predictions')
#         
#     print(p)
#   })
  
  output$plotdatascienceclassification <- renderPlot({
    plotData <- dataset()
    
    plotData$predictions <- eval(call(input$predictFcnName))[[1]]
    plotData$predictions <- ifelse(plotData$predictions < input$classCutoff^2,FALSE,TRUE)
    
    plotData$predictions <- ifelse(plotData$predictions,'black','Predict No Buy')
    plotData$predictions <- ifelse(plotData$predictions == 'black',
                                   ifelse(plotData$CARAVAN == 1,'Correctly Predicted Buy','Incorrectly Predicted Buy'),
                                   plotData$predictions)
    
    p <- ggplot(plotData, aes_string(x=input$x, y=input$y)) + theme(axis.text.x = element_text(angle = 40, hjust = 1))
    
    p <- p + xlab(col_dict[names(ticcompl) == input$x]) + ylab(col_dict[names(ticcompl) == input$y])
    
    if (input$color != 'None')
      p <- p + aes_string(color=input$color)
    
    if (input$jitterStrength > 0) {
      width_strength <- ifelse(is.factor(ticcompl[[input$x]]),
                               nlevels(ticcompl[[input$x]]),
                               diff(range(ticcompl[[input$x]]))) * 0.01 * input$jitterStrength
      height_strength <- ifelse(is.factor(ticcompl[[input$y]]),
                                nlevels(ticcompl[[input$y]]),
                                diff(range(ticcompl[[input$y]]))) * 0.01 * input$jitterStrength
      p <- p + geom_jitter(position=position_jitter(width=width_strength,height=height_strength),alpha=input$alphaStrength)
    } else {
      p <- p + geom_point(alpha=input$alphaStrength)
    }
    
    colorpalette <- c("Correctly Predicted Buy" = "green","Predict No Buy" = "grey","Incorrectly Predicted Buy" = "grey30")
    p <- p + aes_string(color='predictions') + scale_colour_manual(values = colorpalette)
    
    print(p)
  })
  
#   output$plotperfcurve <- renderPlot({
#     plotData <- dataset()
#     
#     i <- 1
#     fscore <- c()
#     xval <- seq(0,0.3,0.005)
#     for (cutoff in xval) {
#       plotData$predictions <- eval(call(input$predictFcnName))
#       plotData$predictions <- ifelse(plotData$predictions < cutoff,FALSE,TRUE)
#       
#       true_pos <- length(which(plotData$predictions & plotData$CARAVAN == 1))
#       true_neg <- length(which(!plotData$predictions & plotData$CARAVAN == 0))
#       false_pos <- length(which(plotData$predictions & plotData$CARAVAN == 0))
#       false_neg <- length(which(!plotData$predictions & plotData$CARAVAN == 1))
#       
#       prec <- true_pos/(true_pos+false_pos)
#       recall <- true_pos/(true_pos+false_neg)
#       
#       fscore[i] <- 2 * (prec*recall)/(prec+recall)
#       i <- i + 1
#     }
#     perfData <- data.frame(fscore=fscore,cutoff=xval)
#     
#     p <- ggplot(perfData, aes_string(x='cutoff', y='fscore')) + geom_line()
# 
#     print(p)
#   })

  output$dsconfusionMatrix <- renderTable({
    plotData <- dataset()
    
    plotData$predictions <- eval(call(input$predictFcnName))[[1]]
    plotData$predictions <- ifelse(plotData$predictions < input$classCutoff^2,FALSE,TRUE)
    
    plotData$predictions <- ifelse(plotData$predictions,"Predict Buy","Predict No Buy")
    plotData$CARAVAN <- ifelse(plotData$CARAVAN == 0,"Actual No Buy","Actual Buy")
    
    table(plotData$CARAVAN,plotData$predictions)
  })
  
  output$dsCostAnalysis <- renderTable({
    plotData <- dataset()
    
    plotData$predictions <- eval(call(input$predictFcnName))[[1]]
    plotData$predictions <- ifelse(plotData$predictions < input$classCutoff^2,FALSE,TRUE)
    
    descriptions <- c("Budget spent on Marketing:",
                      "Budget spent successfully:",
                      "Successful Spending:",
                      "Potential Return on Marketing:",
                      "Return on Marketing:",
                      "Realized Potential:",
                      "ROI:")
    
    succSpend <- length(which(plotData$predictions & plotData$CARAVAN == 1))/length(which(plotData$predictions))*100
    realPot <- length(which(plotData$predictions & plotData$CARAVAN == 1))/length(which(plotData$CARAVAN == 1))*100
    roi <- ((length(which(plotData$predictions & plotData$CARAVAN == 1))*as.numeric(input$mktReturn))/
      (length(which(plotData$predictions))*as.numeric(input$mktCost)))*100
    
    values <- c(length(which(plotData$predictions))*as.numeric(input$mktCost),
                length(which(plotData$predictions & plotData$CARAVAN == 1))*as.numeric(input$mktCost),
                paste(round(succSpend,digits=2),'%'),
                length(which(plotData$CARAVAN == 1))*as.numeric(input$mktReturn),
                length(which(plotData$predictions & plotData$CARAVAN == 1))*as.numeric(input$mktReturn),
                paste(round(realPot,digits=2),'%'),
                paste(round(roi,digits=2),'%'))
    
    # Traditional Targeting
    plotData$xrange <- plotData[[input$x]] %in% input$selectedX
    plotData$yrange <- plotData[[input$y]] %in% input$selectedY
    plotData$comb_range <- plotData$xrange & plotData$yrange
    
    succSpendTrad <- length(which(plotData$comb_range & plotData$CARAVAN == 1))/length(which(plotData$comb_range))*100
    realPotTrad <- length(which(plotData$comb_range & plotData$CARAVAN == 1))/length(which(plotData$CARAVAN == 1))*100
    roiTrad <- ((length(which(plotData$comb_range & plotData$CARAVAN == 1))*as.numeric(input$mktReturn))/
                  (length(which(plotData$comb_range))*as.numeric(input$mktCost)))*100
    
    valuesTrad <- c(length(which(plotData$comb_range))*as.numeric(input$mktCost),
                    length(which(plotData$comb_range & plotData$CARAVAN == 1))*as.numeric(input$mktCost),
                    paste(round(succSpendTrad,digits=2),'%'),
                    length(which(plotData$CARAVAN == 1))*as.numeric(input$mktReturn),
                    length(which(plotData$comb_range & plotData$CARAVAN == 1))*as.numeric(input$mktReturn),
                    paste(round(realPotTrad,digits=2),'%'),
                    paste(round(roiTrad,digits=2),'%'))

    ratio <- c(round(as.numeric(values[1])/as.numeric(valuesTrad[1]),digits=2),
               round(as.numeric(values[2])/as.numeric(valuesTrad[2]),digits=2),
               round(succSpend/succSpendTrad,digits=2),
               round(as.numeric(values[4])/as.numeric(valuesTrad[4]),digits=2),
               round(as.numeric(values[5])/as.numeric(valuesTrad[5]),digits=2),
               round(realPot/realPotTrad,digits=2),
               round(roi/roiTrad,digits=2))
    
    data.frame(row.names=descriptions,Model.Targeting=values,Traditional.Targeting=valuesTrad, Ratio=ratio)
  })
  
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
  
  predictRandomForest <- reactive({
    library("randomForest")
    trainData <- traindataset()
    #trainData[,86] <- factor(trainData[,86])
    
    posPrior <- length(which(trainData$CARAVAN == 1))/length(trainData$CARAVAN)
    negPrior <- length(which(trainData$CARAVAN == 0))/length(trainData$CARAVAN)
        
    rf <- randomForest(trainData[,2:85],trainData[,86],classwt=c(negPrior,posPrior))
    print(rf)
    
    predictions <- predict(rf,dataset()[2:85],type='prob')[,2]
        
    return(list(predictions,rf))
  })
  
  output$randomForestVarImp <- renderTable({
    rf <- eval(call(input$predictFcnName))[[2]]
    impVars <- importance(rf)[order(importance(rf),decreasing=T)[1:10],]
    
    impVarNames <- sapply(names(impVars),function(name) col_dict[which(names(dataset()) == name)])
    
    data.frame(row.names=impVarNames,Importance=impVars)
  })
  
})
