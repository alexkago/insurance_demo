library('shiny')
library('ggplot2')

source('getData.R')

ticcompl <- assignLevels(getData())

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

runApp('hitandmiss',port=Sys.getenv('VCAP_APP_PORT'),launch.browser=F)
