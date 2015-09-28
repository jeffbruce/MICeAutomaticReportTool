# Useful resource: http://adv-r.had.co.nz/Profiling.html
# Useful resource about data.tables: http://www.milanor.net/blog/?p=286

library(lineprof)
library(shiny)

# You have to use source to match the profiler with srcrefs.
source('helpers.R')

l = lineprof(IndividualData(datadefs, 'absolute'))
l
shine(l)  # A graphical way of viewing the profiling results.