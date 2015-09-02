# run app locally in default browser

### INSTALL PACKAGE DEPENDENCIES

install.packages('shiny')
install.packages('gplots')
install.packages('ggplot2')
install.packages('data.table')
install.packages('Hmisc')
install.packages('DT')
install.packages('RMINC')

### RUN THE APP

library(shiny)

# Linux
setwd('//micehome//jbruce//Projects//')
runApp(appDir='MICeAutomaticReportTool', launch.browser=FALSE, display.mode='normal')


### DEPLOY THE APP

# deploy app on shinyapps.io server
library(shinyapps)

setwd('//micehome//jbruce//Projects//')
shinyapps::deployApp('MICeAutomaticReportTool')


### USE A DIFFERENT THEME

install.packages('shinythemes')


### DEBUGGING TIPS

# Can use this somehow to automatically spit out shiny app errors to the browser
# when it crashes
options(shiny.error=browser)

# Immediately enter the browser when an error occurs
options(error = browser)

# Call the recover function when an error occurs
# allows you to browser at any point in the call stack
options(error = recover)
