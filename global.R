# global.R

# Data is loaded here so it can be used in ui.R.

require(gplots)
require(ggplot2)
require(data.table)
# should load plyr before dplyr apparently
require(plyr)
require(dplyr)
require(reshape2)
require(shiny)
require(stats)
require(DT)  # interactive HTML data table widget
require(RMINC)  # needed for FDR calculations

# Data and helper functions.
source('helpers.R')
# source('plotting_helpers.R')
# source('obi_griffith_heatmap3.R')

LoadData()

absoluteIndividualData = IndividualData(datadefs, 'absolute')
relativeIndividualData = IndividualData(datadefs, 'relative')