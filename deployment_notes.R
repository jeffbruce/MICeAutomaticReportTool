# Need to use packrat for this project, because of the following:
# - interactive data tables from the DT package require shiny v 0.12.0 or greater, which would require updating shiny on the cluster here

# Basically followed these instructions:
# https://rstudio.github.io/packrat/walkthrough.html
# Need to ensure to start an R session from within the project dir.

# Start by initializing packrat.
packrat::init('~/Projects/MICeAutomaticReportTool')

# If the initialization screws up for whatever reason, to start afresh, do the following:
# packrat::disable(project=<project_dir>, restart=FALSE)
# if the project dir is the working dir, can leave project=NULL
# delete the packrat directory