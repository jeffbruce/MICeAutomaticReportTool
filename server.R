# server.R

# Shared data/libraries are declared in global.R instead of here because ui.R needs access to them.

shinyServer(
  function(input, output, session) {

# Code in shinyServer runs for each user, every time they refresh their browser.

    activeData = reactive({
        # This is either absolute or relative data depending on user selection

        # TODO: Might not need "tempData" here
        if (input$volumeType == 'Absolute') {
            tempData = absoluteIndividualData
        } else {
            tempData = relativeIndividualData
        }

        tempData
    })

    strainDataSource1 = reactive({
        strainDataSource1 = activeData()

        if (!is.null(input$selectStrains1)) {
            strainDataSource1 = subset(strainDataSource1, Strain %in% input$selectStrains1)
        }

        strainDataSource1
    })
    strainDataSource2 = reactive({
        strainDataSource2 = activeData()

        if (!is.null(input$selectStrains2)) {
            strainDataSource2 = subset(strainDataSource2, Strain %in% input$selectStrains2)
        }

        strainDataSource2
    })

    genotypeDataSource1 = reactive({
        genotypeDataSource1 = strainDataSource1()

        if (!is.null(input$selectGenotypes1)) {
            genotypeDataSource1 = subset(genotypeDataSource1, Genotype %in% input$selectGenotypes1)
        }
        
        genotypeDataSource1
    })
    genotypeDataSource2 = reactive({
        genotypeDataSource2 = strainDataSource2()

        if (!is.null(input$selectGenotypes2)) {
            genotypeDataSource2 = subset(genotypeDataSource2, Genotype %in% input$selectGenotypes2)
        }
        
        genotypeDataSource2
    })

    treatmentDataSource1 = reactive({
        treatmentDataSource1 = genotypeDataSource1()

        if (!is.null(input$selectTreatments1)) {
            treatmentDataSource1 = subset(treatmentDataSource1, Treatment %in% input$selectTreatments1)
        }
        
        treatmentDataSource1
    })
    treatmentDataSource2 = reactive({
        treatmentDataSource2 = genotypeDataSource2()

        if (!is.null(input$selectTreatments2)) {
            treatmentDataSource2 = subset(treatmentDataSource2, Treatment %in% input$selectTreatments2)
        }
        
        treatmentDataSource2
    })

    summaryTable = reactive({

        if (!is.null(finalFilteredDataSource1()) && !is.null(finalFilteredDataSource2())) {
            summaryTable = StatsSummaryTable(finalFilteredDataSource1(), finalFilteredDataSource2())
        }

        summaryTable
    })

    finalDataSource1 = reactive({

        data = activeData()
        if (!is.null(input$selectStrains1)) {
            # data = subset(data, Strain %in% input$selectStrains1)
            data = strainDataSource1()
            if (!is.null(input$selectGenotypes1)) {
                # data = subset(data, Genotype %in% input$selectGenotypes1)
                data = genotypeDataSource1()
                if (!is.null(input$selectTreatments1)) {
                    # data = subset(data, Treatment %in% input$selectTreatments1)
                    data = treatmentDataSource1()
                }
            }
        }

        data$Group = 'Group 1'
        data
    })
    finalDataSource2 = reactive({
        data = activeData()
        if (!is.null(input$selectStrains2)) {
            # data = subset(data, Strain %in% input$selectStrains2)
            data = strainDataSource2()
            if (!is.null(input$selectGenotypes2)) {
                # data = subset(data, Genotype %in% input$selectGenotypes2)
                data = genotypeDataSource2()
                if (!is.null(input$selectTreatments2)) {
                    # data = subset(data, Treatment %in% input$selectTreatments2)
                    data = treatmentDataSource2()
                }
            }
        }

        data$Group = 'Group 2'
        data
    })

    finalFilteredDataSource1 = reactive({
        # Filter finalDataSource1 by available metadata
        data = finalDataSource1()

        # if (!is.null(input$ageGroups1)) {
        #     data = filter(data, FactorAge %in% input$ageGroups1)
        # }
        if (!is.null(input$ageGroups1)) {
            data = filter(data, RawAge > input$ageGroups1[1], RawAge < input$ageGroups1[2])
        }
        if (!is.null(input$sexGroups1)) {
            data = filter(data, Sex %in% input$sexGroups1)
        }
        if (!is.null(input$backgroundGroups1)) {
            data = filter(data, Background %in% input$backgroundGroups1)
        }

        data
    })
    finalFilteredDataSource2 = reactive({
        # Filter finalDataSource2 by available metadata
        data = finalDataSource2()

        # if (!is.null(input$ageGroups2)) {
        #     data = filter(data, FactorAge %in% input$ageGroups2)
        # }
        if (!is.null(input$ageGroups2)) {
            data = filter(data, RawAge > input$ageGroups2[1], RawAge < input$ageGroups2[2])
        }
        if (!is.null(input$sexGroups2)) {
            data = filter(data, Sex %in% input$sexGroups2)
        }
        if (!is.null(input$backgroundGroups2)) {
            data = filter(data, Background %in% input$backgroundGroups2)
        }

        data
    })

    output$interactiveTable = renderDataTable({
        datatable(summaryTable(), options=list(pageLength=10))
    })

    output$selectGenotypes1 = renderUI({

        input$selectStrains1

        column(6, 
            conditionalPanel(
                condition="input.selectStrains1 != null",
                selectInput(inputId = 'selectGenotypes1', 
                            label = h4('Genotype Group 1:'), 
                            choices = as.character(unique(isolate(strainDataSource1())$Genotype)),
                            selected = NULL,
                            multiple = TRUE)
            )
        )
    })
    output$selectGenotypes2 = renderUI({

        input$selectStrains2
        
        column(6,
            conditionalPanel(
                condition="input.selectStrains2 != null",
                selectInput(inputId = 'selectGenotypes2', 
                            label = h4('Genotype Group 2:'), 
                            choices = as.character(unique(isolate(strainDataSource2())$Genotype)),
                            selected = NULL,
                            multiple = TRUE)
            )
        )
    })

    output$selectTreatments1 = renderUI({

        input$selectGenotypes1

        if ("Treatment" %in% colnames(isolate(activeData()))) {
            column(6,
                conditionalPanel(
                    condition="input.selectGenotypes1 != null",
                    selectInput(inputId = 'selectTreatments1', 
                                label = h4('Treatment Group 1:'), 
                                choices = as.character(unique(isolate(genotypeDataSource1())$Treatment)),
                                selected = NULL,
                                multiple = TRUE)
                )
            )
        }
    })
    output$selectTreatments2 = renderUI({

        input$selectGenotypes2

        if ("Treatment" %in% colnames(isolate(activeData()))) {
            column(6,
                conditionalPanel(
                    condition="input.selectGenotypes2 != null",
                    selectInput(inputId = 'selectTreatments2', 
                                label = h4('Treatment Group 2:'), 
                                choices = as.character(unique(isolate(genotypeDataSource2())$Treatment)),
                                selected = NULL,
                                multiple = TRUE)
                )
            )
        }
    })

    # output$ageGroups1 = renderUI({

    #     data = finalDataSource1()

    #     if ("FactorAge" %in% colnames(data)) {
    #         column(4,
    #             checkboxGroupInput(inputId = 'ageGroups1', 
    #                                label = h4('Ages'), 
    #                                choices = as.character(unique(data$FactorAge)),
    #                                selected = as.character(unique(data$FactorAge)))
    #         )
    #     }
    # })
    # output$ageGroups2 = renderUI({

    #     data = finalDataSource2()

    #     if ("FactorAge" %in% colnames(data)) {
    #         column(4,
    #             checkboxGroupInput(inputId = 'ageGroups2', 
    #                                label = h4('Ages'), 
    #                                choices = as.character(unique(data$FactorAge)),
    #                                selected = as.character(unique(data$FactorAge)))
    #         )
    #     }
    # })
    output$ageGroups1 = renderUI({

        data = finalDataSource1()
        min = min(data$RawAge)
        max = max(data$RawAge)

        sliderInput(inputId="ageGroups1", 
                    label=h4("Age Range (Days):"), 
                    min=min, 
                    max=max, 
                    value=c(min, max),
                    round=TRUE,
                    step=1)
    })
    output$ageGroups2 = renderUI({

        data = finalDataSource2()
        min = min(data$RawAge)
        max = max(data$RawAge)

        sliderInput(inputId="ageGroups2", 
                    label=h4("Age Range (Days):"), 
                    min=min, 
                    max=max, 
                    value=c(min, max),
                    round=TRUE,
                    step=1)
    })

    output$sexGroups1 = renderUI({

        data = finalDataSource1()

        if ("Sex" %in% colnames(data)) {
            column(4,
                checkboxGroupInput(inputId = 'sexGroups1', 
                                   label = h4('Sex:'), 
                                   choices = as.character(unique(data$Sex)),
                                   selected = as.character(unique(data$Sex)))
            )
        }
    })
    output$sexGroups2 = renderUI({

        data = finalDataSource2()

        if ("Sex" %in% colnames(data)) {
            column(4,
                checkboxGroupInput(inputId = 'sexGroups2', 
                                   label = h4('Sex:'), 
                                   choices = as.character(unique(data$Sex)),
                                   selected = as.character(unique(data$Sex)))
            )
        }
    })

    output$backgroundGroups1 = renderUI({

        data = finalDataSource1()

        if ("Background" %in% colnames(data)) {
            column(4,
                checkboxGroupInput(inputId = 'backgroundGroups1', 
                                   label = h4('Backgrounds:'), 
                                   choices = as.character(unique(data$Background)),
                                   selected = as.character(unique(data$Background)))
            )
        }
    })
    output$backgroundGroups2 = renderUI({

        data = finalDataSource2()

        if ("Background" %in% colnames(data)) {
            column(4,
                checkboxGroupInput(inputId = 'backgroundGroups2', 
                                   label = h4('Backgrounds:'), 
                                   choices = as.character(unique(data$Background)),
                                   selected = as.character(unique(data$Background)))
            )
        }
    })

    output$regionsToPlot = renderUI({

        data = isolate(finalDataSource1())

        column(4, selectInput(inputId='regionsToPlot',
                              label=h4('Regions to Plot:'),
                              choices=as.character(unique(data$Region)),
                              selected = NULL,
                              multiple = TRUE))
    })

    output$meansPlot = renderPlot({

        # selectedRegions = summaryTable()[input$interactiveTable_rows_selected,]$Region
        selectedRegions = input$regionsToPlot

        if (length(selectedRegions) != 0) {
            fullData = rbind.fill(finalFilteredDataSource1(), finalFilteredDataSource2())
            fullData = filter(fullData, Region %in% selectedRegions)

    #         meansPlot = ggplot(data=meansData, aes(x=name, y=volume, fill=genotype, colour=genotype))
            meansPlot = ggplot(data=fullData, aes(x=Region, y=Volume, fill=Group, colour=Group))

            if (input$plotType == 1) {
                dodge = position_dodge(width=0.9)
                meansPlot = (meansPlot
                            + stat_summary(fun.y=mean, position=position_dodge(width=1), geom='bar')
                            + stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1), geom='errorbar', color='black', size=0.5, width=0.5))
    #                        + scale_y_continuous(limits=c(min(meansData$volume), max(meansData$volume)))
            } else if (input$plotType == 2) {
                meansPlot = (meansPlot
                            + geom_point(position=position_jitterdodge(dodge.width=0.9))
                            + geom_boxplot(fill='white', position=position_dodge(width=0.9), alpha=0.5, outlier.size=0)
                            + stat_summary(fun.y=mean, position=position_dodge(width=0.9), shape=3, col='red', geom='point'))
            } else if (input$plotType == 3) {
                meansPlot = (meansPlot
                            + geom_point(position=position_jitterdodge(dodge.width=0.9))
                            + geom_violin(fill='white', position=position_dodge(width=0.9), alpha=0.5))
            } else if (input$plotType == 4) {
                # means = tapply(meansData$volume, meansData$genotype, mean)
                # sds = tapply(meansData$volume, meansData$genotype, sd)
                meansPlot = (meansPlot
                            + geom_point(position=position_jitterdodge(dodge=1.0))
                            + stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1.0), geom='errorbar', color='black', size=0.5, width=0.5))
    #                        + stat_summary(fun.y=mean, position=position_dodge(width=1.0), shape=1, col='red', geom='point'))
            }

    #         meansPlot = (meansPlot
    #                     + geom_point(position=position_jitterdodge(dodge.width=0.9))
    #                     + geom_boxplot(fill='white', position=position_dodge(width=0.9), alpha=0.5, outlier.size=0)
    #                     + stat_summary(fun.y=mean, position=position_dodge(width=0.9), shape=3, col='red', geom='point'))

    #         #         if (tolower(input$volumeType) == 'absolute') {  # absolute volumes
    # #           meansPlot = meansPlot + labs(x='strain', y=bquote(Volume~(mm^{3})))
    # #         } else {  # relative volumes
    # #           meansPlot = meansPlot + labs(x='strain', y='Relative Volume (%)')
    # #         }

            # customize theme aspects of the plot
            meansPlot = (meansPlot
                        + facet_wrap( ~ Region, scales='free')
                         # + theme(plot.title = element_text(color='#000000', face='bold', family='Trebuchet MS', size=24))
                         # + theme(axis.title = element_text(color='#000000', face='bold', family='Trebuchet MS', size=16))
                        + theme(axis.title.y = element_text(color='#000000', family='Trebuchet MS', size=16, angle=90))
                        + theme(axis.text.y = element_text(color='#000000', family='Trebuchet MS', size=14))
                        + theme(axis.title.x = element_blank())
                         # + theme(axis.text.x = element_text(color='#000000', family='Trebuchet MS', size=16))
                        + theme(axis.text.x = element_blank())

                        + theme(strip.text = element_text(size=16))
                         # + theme(strip.text = element_blank())
                        + theme(legend.title = element_blank())
                        + theme(legend.text = element_text(size=14)))
            
            meansPlot
        }
    })
  }
)