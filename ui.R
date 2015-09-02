# ui.R

# This file is run once.  It generates HTML which is cached and sent to each browser that connects.

shinyUI(
  navbarPage(title='Collaborative Mouse Neuroanatomy App', 
             position='fixed-top', 
             inverse=TRUE,
             collapsible=TRUE,
             windowTitle='Collaborative Mouse Neuroanatomy App',
             
# TAB PANEL ---------------------------------------------------------------
             
    tabPanel(strong('Abstract'),
      
      br(),
      br(),
      br(),
      
      a(name='Abstract'),
      titlePanel('Abstract'),
      
      hr(),
      
      p('Text that would normally go in Abstract goes in this section instead.'),
      
      a(href='#Abstract', 'Back to top')
      
    ),


# TAB PANEL ---------------------------------------------------------------

    tabPanel(strong('Interactive Table and Plots'),
             
      fluidPage(

        br(),
        br(),
        br(),
    
        titlePanel('Interactive Table and Plots'),
        
        hr(),
        
        p('This page gives you the freedom to explore your data.  Here is the suggested workflow:'),

        tags$ul(
          tags$li('Select whether you want to work with absolute or relative volumes.'),
          tags$li('Select two groups you want to compare; each group can be an arbitrary combination of Strains, Genotypes, and Treatments.'), 
          tags$li('The metadata levels for Age, Sex, and Background are automatically populated based on the two groups you selected.  Filter by whatever levels of metadata you want to plot by.  If Age, Sex, and Background are not visible then this means the data files do not all have data for these fields.'),
          tags$li('Click on a field name in the interactive table to sort the table by that field.'),
          tags$li('Click on rows in the interactive table to plot them.')
        ),
       
        hr(),

        sidebarPanel(

          fluidRow(
            column(6, selectInput(inputId = 'selectStrains1', 
                                  label = h4('Strain Group 1:'), 
                                  choices = datadefs$name,
                                  selected = NULL,
                                  multiple = TRUE)),
            column(6, selectInput(inputId = 'selectStrains2', 
                                  label = h4('Strain Group 2:'), 
                                  choices = datadefs$name,
                                  selected = NULL,
                                  multiple = TRUE))
          ),

          fluidRow(
            uiOutput('selectGenotypes1'),
            uiOutput('selectGenotypes2')
          ),

          fluidRow(
            uiOutput('selectTreatments1'),
            uiOutput('selectTreatments2')
          ),

          fluidRow(
            uiOutput('ageGroups'),
            uiOutput('sexGroups'),
            uiOutput('backgroundGroups')
          ),

          fluidRow(
            column(4, radioButtons(inputId='volumeType',
                                   label=h4('Volume:'),
                                   choices=list('Absolute', 
                                                'Relative'),
                                   selected='Absolute')),
            column(4, radioButtons(inputId='fdrLevel',
                                   label=h4('FDR:'),
                                   choices=list('1%', 
                                                '5%',
                                                '10%'),
                                   selected='5%'))
          ),

          # only show up if strain is populated
          # fluidRow(genotypes1, genotypes2)

          # only show up if genotype is populated
          # fluidRow(treatment1, treatment2)

          hr()

          # filter by metadata
          # Age, RawAge, Sex, Background

          # table/plot options
          # Relative/Absolute, FDR options

          
        ),

        mainPanel(

          # requires shiny >= 0.12.0, need to use packrat package for this project
          fluidRow(
            column(12, dataTableOutput(outputId='interactiveTable'))
          ),

          fluidRow(
            column(12, plotOutput(outputId='meansPlot', height='600px'))
          )
        )

          # hr(),

          # fluidRow(column(12, plotOutput(outputId='interactiveTablePlot')))
      
      )
    )
    

# TAB PANEL ---------------------------------------------------------------

    # tabPanel(strong('Hierarchical Clustering'),
      
    #   br(),
    #   br(),
    #   br(),
             
    #   titlePanel('Hierarchical Clustering'),
             
    #   hr(),
      
    #   p('Using the data you filtered and reclustered in the ', tags$mark('Filter and Recluster'), ' tab, here you can zone in on particular mouse strains and brain regions of interest to plot means and effect sizes, using your preferred plot type.'),
      
    #   hr(),
      
    #   fluidRow(
    #     column(4, radioButtons(inputId='statisticType',
    #                            label=h4('Statistic to Plot:'),
    #                            choices=list('Effect Sizes'=1, 
    #                                         'Means'=2),
    #                            selected=1))
    #   ),
      
    #   hr(),
      
    #   conditionalPanel(
    #     condition="input.statisticType=='1'",
    #     fluidRow(
    #       column(4, radioButtons(inputId='plotBy',
    #                              label=h4('Plot By:'),
    #                              choices=list('Strain'=1, 
    #                                           'Region'=2),
    #                              selected=1)),
    #       column(4, uiOutput('selectBoxStrainRegion'))
    #     ),
    #     hr(),
    #     p(strong(em('Effect sizes have been capped at -3.0 and +3.0 for increased readability.'))),
    #     fluidRow(
    #       column(12, plotOutput(outputId='effectSizePlot',
    #                             height='800px'))
    #     )
    #   ),
      
    #   conditionalPanel(
    #     condition="input.statisticType=='2'",
    #     fluidRow(
    #       column(4, uiOutput('selectInputStrains')),
    #       column(4, uiOutput('selectInputRegions')),
    #       column(4, radioButtons(inputId='plotType',
    #                              label=h4('Plot Type:'),
    #                              choices=list('Box'=2,
    #                                           'Bar'=1,
    #                                           'Dot'=4,
    #                                           'Violin'=3),
    #                              selected=2))
    #     ),
    #     hr(),
    #     p(strong(em('Error bars are 95% confidence intervals centred about the mean.'))),
    #     fluidRow(
    #       column(12, plotOutput(outputId='meansPlot',
    #                             height='800px'))
    #     )
    #   ),
      
    #   hr(),
      
    #   fluidRow(
    #     column(12, plotOutput(outputId='heatmap2', 
    #                           height='1000px'))
    #   )
    # )

  )
)