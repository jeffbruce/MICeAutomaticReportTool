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
          tags$li('Select two groups you want to compare; each group can be an arbitrary combination of Strains, Genotypes, and Treatments.'), 
          tags$li('The metadata options (Age, Sex, and Background) are dynamically populated for each group.  Filter by whatever levels of metadata you want to plot by for each group.  Age, Sex, and Background will only appear for a selected group if these fields are defined in all gf files that are being used in that group.'),
          tags$li('Click on a field name in the interactive table to sort the table by that field.'),
          tags$li('Select regions and factors to plot by.')
        ),
       
        hr(),

        sidebarPanel(

          h3(tags$u('Groups to Compare')),

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

          h3(tags$u('Group 1 Metadata')),

          fluidRow(
            uiOutput('ageGroups1'),
            uiOutput('sexGroups1'),
            uiOutput('backgroundGroups1')
          ),

          br(),

          h3(tags$u('Group 2 Metadata')),

          fluidRow(
            uiOutput('ageGroups2'),
            uiOutput('sexGroups2'),
            uiOutput('backgroundGroups2')
          ),

          br(),

          h3(tags$u('Plotting Options')),

          fluidRow(
            column(4, radioButtons(inputId='volumeType',
                                   label=h4('Volume:'),
                                   choices=list('Absolute', 
                                                'Relative'),
                                   selected='Absolute')),
            # column(4, radioButtons(inputId='fdrLevel',
            #                        label=h4('FDR:'),
            #                        choices=list('1%', 
            #                                     '5%',
            #                                     '10%'),
            #                        selected='5%'))
            column(4, radioButtons(inputId='plotType',
                                   label=h4('Plot Type:'),
                                   choices=list('Box'=2,
                                                'Bar'=1,
                                                'Dot'=4,
                                                'Violin'=3),
                                   selected=2)),
            uiOutput('regionsToPlot')
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

  )
)