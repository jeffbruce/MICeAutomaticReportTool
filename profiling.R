# Useful resource: http://adv-r.had.co.nz/Profiling.html

library(lineprof)
library(shiny)

# You have to use source to match the profiler with srcrefs.
source('helpers.R')

l = lineprof(IndividualData(datadefs, 'absolute'))
l
shine(l)  # A graphical way of viewing the profiling results.