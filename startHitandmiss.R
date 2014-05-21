# source files and assign data
library('shiny')
library('ggplot2')
library('randomForest')

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



if (Sys.getenv('VCAP_APP_PORT') == "") {
  # In case we're on a local system, run this:
  print('running locally')
  runApp('hitandmiss',port=8000,launch.browser=F)
  
} else {
  # In case we're on Cloudfoundry, run this:
  print('running on CF')
  
  # Starting Rook server during CF startup phase - after 60 seconds start the actual Shiny server
  library(Rook)
  myPort <- as.numeric(Sys.getenv('VCAP_APP_PORT'))
  myInterface <- Sys.getenv('VCAP_APP_HOST')
  status <- -1
  
  # R 2.15.1 uses .Internal, but the next release of R will use a .Call.
  # Either way it starts the web server.
  if (as.integer(R.version[["svn rev"]]) > 59600) {
    status <- .Call(tools:::startHTTPD, myInterface, myPort)
  } else {
    status <- .Internal(startHTTPD(myInterface, myPort))
  }
  
  if (status == 0) {
    unlockBinding("httpdPort", environment(tools:::startDynamicHelp))
    assign("httpdPort", myPort, environment(tools:::startDynamicHelp))
    
    s <- Rhttpd$new()
    s$listenAddr <- myInterface
    s$listenPort <- myPort
    
    s$print()
    Sys.sleep(60)
    s$stop()
  }
  
  
  # run shiny server
  sink(stderr())
  options(bitmapType='cairo')
  getOption("bitmapType")
  print("test")
  write("prints to stderr", stderr())
  write("prints to stdout", stdout())
  runApp('hitandmiss',port=myPort,host="0.0.0.0",launch.browser=F)
}
