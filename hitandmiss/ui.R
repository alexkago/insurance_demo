library(shiny)
library(ggplot2)

dataset <- ticcompl

descriptions <- c("Budget spent on Marketing:",
                  "Budget spent successfully:",
                  "Successful Spend Percent:",
                  "Potential Return:",
                  "Realized Return:",
                  "Realized Potential Percent:",
                  "ROI:")

colorpalette <- c("Correctly Predicted Buy" = "green","Predict No Buy" = "grey","Incorrectly Predicted Buy" = "grey30")

explanationsList <- p(HTML("
                <ul>
                  <li><strong>Budget spent on Marketing</strong>: Number of targeted customers * the marketing cost specified on the left.</li>
                  <li><strong>Budget spent successfully</strong>: Budget spent on customers that are responding to the offer.</li>
                  <li><strong>Successful Spend Percent</strong>: Ratio of 'Budget spent successfully' and 'Budget spent on Marketing'.</li>
                  <li><strong>Potential Return</strong>: Possible profit if every interested customer gets an offer. Based off the 'Return on 
                                                          new customer' value specified on the left.</li>
                  <li><strong>Realized Return</strong>: Realized profit, calculated by interested customers that were actually reached *
                                                        the 'Return on new customer' value specified on the left.</li>
                  <li><strong>Realized Potential Percent</strong>: Ratio of Realized Return and Potential Return</li>
                  <li><strong>ROI</strong>: Ratio of 'Realized Return' and 'Budget spent on Marketing', expressed as percentage. 100% means the
                                            marketing cost was covered.</li>
                </ul>"))

value_names <- names(dataset)
names(value_names) <- col_dict

shinyUI(pageWithSidebar(
  
  headerPanel("Insurance Marketing Demo",
              windowTitle='Insurance Marketing Demo'),
  
  sidebarPanel(
    img(src='https://www.google.com/a/gopivotal.com/images/logo.gif?alpha=1&service=google_white'),
    conditionalPanel(
      condition = "input.panelchoice != 'introduction' && input.panelchoice != 'nextsteps' &&
      input.panelchoice != 'modelexplanation'",
      h4("Select plot parameters:"),
      
      sliderInput('sampleSize', 'Sample Size', min=2500, max=nrow(dataset),
                  value=min(4000, nrow(dataset)), step=500, round=0),
      
      selectInput('x', 'X', value_names, selected=names(value_names)[5]),
      selectInput('y', 'Y', value_names, selected=names(value_names)[4]),
      conditionalPanel(
        condition = "input.panelchoice == 'all'",
        selectInput('color', 'Color', c('None', value_names))
      ),
      
      sliderInput('alphaStrength', 'Transparency', min=0, max=1, value=1, step=0.05, round=F),
      
      sliderInput('jitterStrength', 'Jitter', min=0, max=5, value=2, step=0.1, round=F),
      
      conditionalPanel(
        condition = "input.panelchoice == 'marketing'",
        h4("Select and deselect customers groups:"),
        selectInput('dim1', 'Variable 1', c('',value_names)),
        conditionalPanel(
          condition = "input.dim1 != ''",
          uiOutput('selectDim1controls')
        ),
        selectInput('dim2', 'Variable 2', c('',value_names)),
        conditionalPanel(
          condition = "input.dim2 != ''",
          uiOutput('selectDim2controls')
        ),
        selectInput('dim3', 'Variable 3', c('',value_names)),
        conditionalPanel(
          condition = "input.dim3 != ''",
          uiOutput('selectDim3controls')
        ),
        selectInput('dim4', 'Variable 4', c('',value_names)),
        conditionalPanel(
          condition = "input.dim4 != ''",
          uiOutput('selectDim4controls')
        ),
        selectInput('dim5', 'Variable 5', c('',value_names)),
        conditionalPanel(
          condition = "input.dim5 != ''",
          uiOutput('selectDim5controls')
        )
#         selectInput('dim6', 'Variable 6', c('',value_names)),
#         conditionalPanel(
#           condition = "input.dim6 != ''",
#           uiOutput('selectDim6controls')
#         ),
#         selectInput('dim7', 'Variable 7', c('',value_names)),
#         conditionalPanel(
#           condition = "input.dim7 != ''",
#           uiOutput('selectDim7controls')
#         ),
#         selectInput('dim8', 'Variable 8', c('',value_names)),
#         conditionalPanel(
#           condition = "input.dim8 != ''",
#           uiOutput('selectDim8controls')
#         ),
#         selectInput('dim9', 'Variable 9', c('',value_names)),
#         conditionalPanel(
#           condition = "input.dim9 != ''",
#           uiOutput('selectDim9controls')
#         ),
#         selectInput('dim10', 'Variable 10', c('',value_names)),
#         conditionalPanel(
#           condition = "input.dim10 != ''",
#           uiOutput('selectDim10controls')
#         )
      ),
      conditionalPanel(
        condition = "input.panelchoice == 'datascience'",
        h4("Select predictive algorithm:"),
        selectInput('predictFcnName', 'Choose Predictive Algorithm', c("Random Forest" = "predictRandomForest", 
                                                                       "Logistic Regression" = "predictLogisticRegression")),
        conditionalPanel(
          condition = "input.predictFcnName == 'predictRandomForest'",
          sliderInput('classCutoff', 'Classification Cutoff', min=0, max=0.5, value=0.1, step=0.001, round=F)
        ),
        conditionalPanel(
          condition = "input.predictFcnName == 'predictLogisticRegression'",
          sliderInput('classCutoffLR', 'Classification Cutoff', min=0, max=1, value=0.5, step=0.01, round=F)
        )
      ),
      conditionalPanel(
        condition = "input.panelchoice == 'datascience' || input.panelchoice == 'marketing'",
        h4("Cost and Return Values:"),
        textInput("mktCost","Marketing cost per customer",value="1"),
        textInput("mktReturn","Return on new customer",value="100")
      )
    )
  ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("Introduction", 
               h5("This is an interactive Data Science demo by the Pivotal Data Science team (",a(href="http://www.gopivotal.com",'www.gopivotal.com'),")."),
               h4("Scenario"),
               p("An insurance company creates a new product, which is a mobile home/caravan
                 insurance policy. It already has data on their customers and what types of policies they already own, together with
                 external data that contains demographic information on the customers. They want to determine from this which 
                 customers are likely to buy the new mobile home insurance policy. Therefore, the company sends out offers to a number
                 of customers and collects data on responding customers."),
               p("After this, a team is given the task to determine from the collected data which customers want to buy the new
                 mobile home insurance. They can create hypotheses on this in a traditional way and determine customer groups that are likely
                 to buy insurance simply by looking through the data. Alternatively, they could also go with a data-driven approach
                 and use a predictive model to determine the target group."),
               h4("Concept"),
               p("This demo shows how a data-driven approach to select customers for marketing has advantages over a traditional approach. 
                 It allows you to go through the manual process first and select the best customers to market the new
                 insurance policy to in a traditional way. You can then assign a
                 cost to your marketing campaign so you can see how effectively this budget is spent."),
               p("After this, compare your manual selection to a selection generated by a predictive model based on Machine Learning.
                 It will use the same cost you assigned earlier and allows you to compare the difference between both approaches. 
                 Hopefully, you will find reduced cost and an increasing return rate on a model based marketing campaign."),
               h4("Instructions"),
               p(HTML("
                  <ul>
                    <li>You navigate through this demo by clicking on the individual tabs on the top. You can always come back to this 
                        panel to review the instructions.</li>
                    <li>To get a general understanding of the dataset and what it looks like, you can explore the dataset on the 
                        'Data Explorer' Tab.</li>
                    <li>Since this demo is intended to provide an interactive comparison, on the 'Targeting by Hand' tab you can try to 
                        specify a target group for marketing insurance by hand.</li>
                    <li>In contrast, the 'Targeting with model support' tab will walk you through a process in 
                        which a model is fitted to the dataset that predicts which customers are likely to buy an insurance.</li>
                  </ul>")),
               value="introduction"
      ), 
      tabPanel("Data Explorer", 
               h4("Instructions"),
               h5("This tab is for general exploration of the dataset."),
               p(HTML("
                  On the left hand side you can see the controls for this plot. The plot will update automatically after you change
                  a setting.
                  <ul>
                    <li>The <strong>'Sample Size'</strong> slider controls how many points of
                        the original dataset you want to see in the plot. It's a good idea to select a sample of only a few thousand points,
                        otherwise the plot will get overcrowded.</li>
                    <li>You can also select both <strong>X and Y axes</strong> of the plot below on the left.</li>
                    <li>You can display a third variable by changing the <strong>'Color'</strong> option on the left , which will color the points
                        in the plot according to this variable.</li>
                    <li>The <strong>'Transparency'</strong> allows you to change the transparency of points, to make overlapping points visible.</li>
                    <li>With <strong>'Jitter'</strong> you can introduce some random displacement into the plot, also to avoid overlapping points.</li>
                  </ul>")),
               p(strong("Tip:"),"You could already start looking for interesting customer
                 groups which might be interested in a new insurance. Set the 'Color' dropdown list to 'No. of mobile home pol.',
                 and you will see customers that bought mobile home insurance in a different color on the plot."),
               plotOutput("plotall"),
               value="all"
      ), 
      tabPanel("Traditional Targeting", 
               h4("Background"),
               h5("This tab allows you to select a group for insurance marketing manually."),
               p("The purpose of this tab is to give you the ability to select a target group for insurance marketing yourself. While doing
                 this, you can continually refine your hypothesis by looking at the dataset again and further select/deselect user groups. 
                 The result of your targeting group is shown graphically in the plot below as well as in the success numbers below the plot."),
               h4("Instructions"),
               p(HTML("
                  <ul>
                    <li>On the left hand side you can control the <strong>plot parameters</strong> like in the previous tab.</li>
                    <li>Below the plot parameters, it is possible to <strong>select and deselect customer groups for your marketing target group</strong>.
                        You can restrict the target group in 5 variables.</li>
                    <li>To assign a concrete dollar value to your marketing success, you can assign a <strong>cost to your marketing</strong> 
                        campaign as well as a <strong>return for every new customer</strong> for the new caravan insurance. The results based off 
                        these numbers are shown below the plot. This will influence the financial result shown in the table below the plot.</li>
                  </ul>")),
               p(strong("Tip:"),"You should try to find relevant variables for the plot first, which show a clear separability of buyers and non-buyers of
                 the caravan insurance. After you found these variables, you can select the customer groups for marketing. ",strong("Initially, all 
                 customers are selected,"),"which means we will market caravan insurance to everyone. Deselect groups that you think will 
                 not buy caravan insurance."),
               h4("Results"),
               plotOutput("plotmarketing"),
               p("These are the financial results of the chosen target group selection based off the cost and return values supplied on the 
                 left."),
               #tableOutput("mktconfusionMatrix"),
               tableOutput("mktCostAnalysis"),
               explanationsList,
               value="marketing"
      ), 
      tabPanel("Model Explanations", 
               p("In this demo, you can choose two different Machine Learning models for predicting new insurance customers. While there 
                 are certainly a lot more possibilities for other predictive models, we chose these two models because they are very 
                 popular in the Machine Learning community and relatively easy to understand, compared to other model choices."),
               h4("Random Forest"),
               p("The Random Forest algorithm has a name that is derived from its structure: It is built up by a large group of 
                 decision trees. A decision tree is a model with a tree-like structure that branches out at every node. On each of these
                 nodes, a decision is made about a particular customer's information, e.g. his age or his marital status. Going through
                 the tree essentially means answering a few question about a particular customer. After you went through these questions,
                 you end up at a leaf node, which gives you a probability of what kind of classification you should make. The following
                 picture illustrates this explanation:"),
               img(src="decisiontree.png"),
               p("What gives the Random Forest algorithm its predictive power is the combination of many decision trees. One decision tree
                 by itself is a very rough separation of data points into two groups. However, if you have an ensemble of decision trees,
                 it is possible
                 to simulate a probabilistic separation, by taking the fraction of the classifications produced by these decision trees."),
               img(src="randomforest.jpg"),
               p("Random Forests were the winning algorithm in a lot of Kaggle competitions. For further information regarding random
                 forests, please look at",a(href="http://en.wikipedia.org/wiki/Random_forest","the very good Wikipedia article.")),
               h4("Logistic Regression"),
               p("The Logistic Regression model is a lot simpler than a Random Forest, since it is linear in its parameters. A Random Forest
                 can fit non-linear relationships within the data easily, but a Logistic Regression model can only have a good fit on much
                 simpler relationships within the data. However, it is always worthwile to try this kind of model because linear
                 relationships are predominant in a lot of datasets."),
               p("Logistic Regression is conceptually very similar to Linear Regression. It forms a linear hypothesis based on the
                 parameters. The difference in logistic regression is, the linear hypothesis becomes transformed by the logistic function:"),
               img(src="LogReg.png"),
               p("The Logistic Function projects the output of the linear hypotheses onto the [0,1] range. The goal of a logistic regression
                 is usually to classify data points into two classes, where 0 would be one class and 1 the other. If a data point is being 
                 projected onto the [0,1] range, this can be interpreted as a probability for the data point belonging to class 1."),
               p("For more information about Logistic Regression, please consider the",
                 a(href="http://en.wikipedia.org/wiki/Decision_tree_learning","Wikipedia article"),"."),
               value="modelexplanation"
      ),
      tabPanel("Targeting with model support", 
               conditionalPanel(condition="$('div#plotdatascienceclassification').contents().length==0 || 
                                           $('div#plotdatascienceclassification').hasClass('recalculating')",
                                h3("Please wait a few seconds for the model to finish training...")),
               h4("Background"),
               p("On this tab, a Machine Learning model is being trained that predicts which customers are likely to buy caravan insurance.
                 After the model has finished training, you can see an output of the model decision in the plot below. Keep in mind, that
                 with this approach, you're not only limited to five variables, instead the model looks at all variables simultaneously. Below
                 the plot you have the additional information what the financial performance of the model is with your chosen cost and return
                 values and furthermore, which variables are of high importance to the trained model"),
               p(strong("Note:"),"Remember that the data we have on the customers does not come from the insurance company only (which is only his 
                 number of certain policies and his contributions to them). On every customer
                 we have ",strong("external data"),", which in this demo is information about the ",strong("demographics in his zip code"),".
                 It could also be
                 data from social networks, for example. This external data is what allows the model to make good predictions, since
                 this data contains a lot more information about the customer than the insurance company's data alone."),
               h4("Instructions"),
               p(HTML("
                  <ul>
                    <li>On the left hand side you can control the <strong>plot parameters</strong> like in previous tabs.</li>
                    <li>Below this, you can select what kind of <strong>predictive algorithm</strong> you want to train.</li>
                    <li>After the Machine Learning model is trained, you have the possibility to change the <strong>'Classification Cutoff'</strong> 
                        of the model, to adjust how sensitive the trained model is in predicting insurance customers: If you set a 
                        <strong>low cutoff</strong>, you will see <strong>more predicted buyers</strong>, with a <strong>high cutoff</strong> the 
                        model will predict that <strong>less people are buyers</strong>.</li>
                    <li>Below that you can set the <strong>cost and return</strong> values for marketing. This will influence the financial result 
                        in the table below the plot.</li>
                  </ul>")),
               h4("Results"),
               #p("Model output, a probability between 0 and 1 for a customer to buy the caravan insurance."),
               #plotOutput("plotdatascience"),
               p("The below plot shows the classification of the customer base, based on the trained model and specified classification cutoff:"),
               conditionalPanel(condition="$('div#plotdatascienceclassification').contents().length==0 || 
                                           $('div#plotdatascienceclassification').hasClass('recalculating')",
                                div(align="center",style="height: 400px",img(src="loading.gif"))),
               conditionalPanel(condition="!($('div#plotdatascienceclassification').contents().length==0 || 
                                           $('div#plotdatascienceclassification').hasClass('recalculating'))",
                                plotOutput("plotdatascienceclassification")),
               #conditionalPanel(condition="!($('div#baseline').text()=='' || $('html').hasClass('shiny-busy'))", br()),
               #plotOutput("plotdatascienceclassification"),
               p("In the table below are the financial results of the chosen target group selection based off the cost and return values 
                 supplied on the left. ",br(),strong("Tip:"),"After the model has finished training, you should try to set the cutoff to a 
                 value such that the realized return on Model Targeting and Traditional Targeting is approximately the same. This allows you 
                 to compare both approaches."),
               #tableOutput("dsconfusionMatrix"),
               conditionalPanel(condition="$('div#plotdatascienceclassification').contents().length==0 || 
                                           $('div#plotdatascienceclassification').hasClass('recalculating')",
                                div(align="center",img(src="loading.gif"))),
               conditionalPanel(condition="!($('div#plotdatascienceclassification').contents().length==0 || 
                                           $('div#plotdatascienceclassification').hasClass('recalculating'))",
                                tableOutput("dsCostAnalysis")),
               p(strong("You can see that
                 the data-driven approach has a significantly higher ROI. You can also choose to be very selective (set a high cutoff) and 
                 you will see the ROI increase very fast. This could give you information about high-value customers in your customer base.")),
               explanationsList,
               #p("Plot of the F-score for different classifation cutoffs."),
               #plotOutput("plotperfcurve"),
               conditionalPanel(condition="input.predictFcnName == 'predictRandomForest'",
                                h4("Model Details"),
                                p("The table below should give you some intuition about what the trained model is doing. It shows how important each of the
                 shown variables is to the final model decision and shows the variables in decreasing order of importance."),
                                conditionalPanel(condition="$('div#plotdatascienceclassification').contents().length==0 || 
                                           $('div#plotdatascienceclassification').hasClass('recalculating')",
                                                 div(align="center",img(src="loading.gif"))),
                                conditionalPanel(condition="!($('div#plotdatascienceclassification').contents().length==0 || 
                                           $('div#plotdatascienceclassification').hasClass('recalculating'))",
                                                 tableOutput("randomForestVarImp"))),
#                conditionalPanel(condition="input.predictFcnName == 'predictLogisticRegression'",
#                                 h4("Model Details"),
#                                 p("The table below should give you some intuition about what the trained model is doing. It shows how the coefficients
#                                   of the linear model learned by linear regression."),
#                                 conditionalPanel(condition="$('div#plotdatascienceclassification').contents().length==0 || 
#                                            $('div#plotdatascienceclassification').hasClass('recalculating')",
#                                                  div(align="center",img(src="loading.gif"))),
#                                 conditionalPanel(condition="!($('div#plotdatascienceclassification').contents().length==0 || 
#                                            $('div#plotdatascienceclassification').hasClass('recalculating'))",
#                                                  tableOutput("logisticRegressionCoeff"))),
               value="datascience"
      ),
      tabPanel("Next Steps", 
               h4("Future Development"),
               p("There are several possibilities how this demo could be extended:"),
               p(HTML("
                  <ul>
                    <li>Add Social Media Data as an external data source.</li>
                    <li>Take Open Data by government data into account.</li>
                    <li>Use a Pivotal platform in the backend to accelerate the model training and scoring, even on Tera- and Petabyte scales of data.</li>
                    <li>Use more advanced Machine Learning models.</li>
                    <li>Collect temporal data on insurance policies and see shifts and trends in customers' interests.</li>
                  </ul>")),
               h4("Operationalization Scenario"),
               p("Machine Learning models for marketing could be operationalized in multiple ways:"),
               p(HTML("
                  <ul>
                    <li>Use this model to choose an optimal target group for marketing.</li>
                    <li>Find customers which have a high probability to be interested.</li>
                    <li>Differentiate marketing campaigns by the predicted level of interest.</li>
                    <li>Learn about important factors that determine a customer's interest. This could influence product development itself.</li>
                  </ul>")),
               h4("Further Information"),
               p("For further information regarding this demo and Data Science, please contact:"),
               p("Alexander Kagoshima -", a(href="mailto:akagoshima@gopivotal.com",'akagoshima@gopivotal.com')),
#                p("For further info regarding this demo aData Science, please contact:"),
#                p("Michael Natusch (mnatusch@gopivotal.com) - Data Science EMEA"),
#                p("Greg Whalen (gwhalen@gopivotal.com) - Data Science APJ"),
#                p("Alex Luttschyn (aluttschyn@gopivotal.com) - Data Science Americas"),
               value="nextsteps"
      ),
      id="panelchoice")
  )
  
))