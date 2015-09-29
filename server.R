# server.R

# Shared data/libraries are declared in global.R instead of here because ui.R needs access to them.

shinyServer(function(input, output, session) {

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

    ### FactorAge Stuff
    
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

        if ("RawAge" %in% colnames(data)) {

            min = min(data$RawAge)
            max = max(data$RawAge)

            sliderInput(inputId="ageGroups1", 
                        label=h4("Age Range (Days):"), 
                        min=min, 
                        max=max, 
                        value=c(min, max),
                        round=TRUE,
                        step=1)
        }
    })
    output$ageGroups2 = renderUI({

        data = finalDataSource2()

        if ("RawAge" %in% colnames(data)) {

            min = min(data$RawAge)
            max = max(data$RawAge)

            sliderInput(inputId="ageGroups2", 
                        label=h4("Age Range (Days):"), 
                        min=min, 
                        max=max, 
                        value=c(min, max),
                        round=TRUE,
                        step=1)
        }
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

        data = isolate(activeData())

        column(4, selectInput(inputId='regionsToPlot',
                              label=h4('Regions to Plot:'),
                              choices=as.character(unique(data$Region)),
                              selected = NULL,
                              multiple = TRUE))
    })

    output$strainsToPlot = renderUI({

        data = isolate(activeData())

        column(4, selectInput(inputId='strainsToPlot',
                              label=h4('Strains to Plot:'),
                              choices=as.character(unique(data$Strain)),
                              selected = NULL,
                              multiple = TRUE))
    })

    # selectedMetadataLevels = reactive({

    #     gfMetadata = GfMetadata(datadefs$gf)
    #     gfMetadataTrue = which(gfMetadata)

    #     names(data) = gfMetadataTrue

    #     if (!(input$ageGroups1 == NULL)) {
    #         gf
    #     }

    # })

    # Couldn't get downloadHandler to work using this method.
    # output$downloadPlot <- downloadHandler(
    #     # filename = function() {
    #     #     # strains = paste(isolate(input$strainsToPlot), collapse='_')
    #     #     # regions = paste(isolate(input$regionsToPlot), collapse='_')
    #     #     # name = paste(strains, regions, sep=' ')
    #     #     # name = paste(name, isolate(input$imageType), sep='.')
    #     #     # name
    #     #     'plot.png'
    #     # },
    #     filename = "Shinyplot.pdf",
    #     # filename = "Shinyplot.png",
    #     content = function(file) {
    #         # if (input$imageType == 'pdf') {
    #         #     pdf(file)
    #         # } else {
    #         #     png(file)
    #         # }
    #         # pdf(file)
    #         CairoPDF(file)  # Prints out a white canvas.
    #         # png(file)
    #         # CairoPNG(file)  # Prints out a black square.
    #         # y = makePlot()  # This just prints out code and doesn't help.
    #         # print(y)
    #         makePlot()
    #         dev.off()
    #     }
    #     # This doesn't help anything.
    #     # },
    #     # contentType = 'application/pdf'
    # )

    # Refactor this at some point to save the sorted interactive table.
    output$downloadTable = downloadHandler(
        filename = 'table.csv',
        content = function(file) {
            write.csv(summaryTable(), file)
        }
    )

    # Works!
    output$downloadPlot = downloadHandler(
        filename = function() {
            regions = paste(input$regionsToPlot, collapse=' & ')
            strains = paste(input$strainsToPlot, collapse=' & ')
            name = paste(strains, regions, sep=' - ')
            name = paste(name, input$imageType, sep='.')
            name
        },
        content = function(file) {
            # Can't do this because makePlot() is not a ggplot, but is a renderPlot.
            # ggsave(file, plot = makePlot(), device=pdf)
            # Not allowed to do this because you can't read from Shiny output objects.
            # ggsave(file, plot = output$meansPlot, device=pdf)
            
            # This works!  thePlot() is a reactive expression and yet it still works.
            # The file name cannot exist in the base folder of the app otherwise it will copy the wrong plot.
            regions = paste(input$regionsToPlot, collapse=' & ')
            strains = paste(input$strainsToPlot, collapse=' & ')
            name = paste(strains, regions, sep=' - ')
            name = paste(name, input$imageType, sep='.')
            ggsave(file, plot=thePlot(), height=length(input$regionsToPlot)*4, width=length(input$strainsToPlot)*4.5)
            file.copy(paste(name), file, overwrite=TRUE)
        }
    )

    makePlot = function() {
        renderPlot({
            thePlot()
        }, height=exprToFunction(length(input$regionsToPlot)*350))
    }

    thePlot = reactive({

        # selectedRegions = summaryTable()[input$interactiveTable_rows_selected,]$Region
        selectedRegions = input$regionsToPlot
        selectedStrains = input$strainsToPlot
        fullData = activeData()

        if ((length(selectedRegions) != 0) && (length(selectedStrains) != 0)) {
            # fullData = rbind.fill(finalFilteredDataSource1(), finalFilteredDataSource2())
            fullData = filter(fullData, Region %in% selectedRegions)
            fullData = filter(fullData, Strain %in% selectedStrains)

            if ('Treatment' %in% colnames(fullData)) {
                meansPlot = ggplot(data=fullData, aes(x=Genotype, y=Volume, fill=Treatment, colour=Treatment))
            } else {
                meansPlot = ggplot(data=fullData, aes(x=Genotype, y=Volume, fill=Genotype, colour=Genotype))
            }

            if (input$plotType == 1) {
                dodge = position_dodge(width=0.9)
                meansPlot = (meansPlot
                            + stat_summary(fun.y=mean, position=position_dodge(width=1), geom='bar')
                            + stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1), geom='errorbar', color='black', size=0.5, width=0.5))
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
                meansPlot = (meansPlot
                            + geom_point(position=position_jitterdodge(dodge=1.0))
                            + stat_summary(fun.data=mean_cl_normal, position=position_dodge(width=1.0), geom='errorbar', color='black', size=0.5, width=0.5))
            }

            # Change y axis label to correct label for absolute or relative volume.
            if (tolower(input$volumeType) == 'absolute') {
                meansPlot = meansPlot + labs(x='Genotype', y=bquote(Volume~(mm^{3})))
            } else {
                meansPlot = meansPlot + labs(x='Genotype', y='Relative Volume (%)')
            }

            # customize theme aspects of the plot
            meansPlot = (meansPlot
                        + facet_grid(Region ~ Strain, scales='free')
                        # + facet_wrap( ~ Region, scales='free')
                        # + theme(plot.title = element_text(color='#000000', face='bold', family='Trebuchet MS', size=24))
                        # + theme(axis.title = element_text(color='#000000', face='bold', family='Trebuchet MS', size=16))
                        + theme(axis.title.y = element_text(color='#000000', face='bold', size=16, angle=90))
                        + theme(axis.text.y = element_text(color='#000000', size=14))
                        + theme(axis.title.x = element_text(color='#000000', face='bold', size=16))
                        + theme(axis.text.x = element_text(color='#000000', size=14))
                        + theme(strip.text.x = element_text(color='#000000', face='bold', size=14))
                        + theme(strip.text.y = element_text(color='#000000', face='bold', size=14, angle=90))
                        # + theme(axis.title.x = element_blank())
                        # + theme(axis.text.x = element_text(color='#000000', family='Trebuchet MS', size=16))
                        # + theme(axis.text.x = element_blank())
                        + theme(strip.text = element_text(size=16))
                        # + theme(strip.text = element_blank())
                        # + theme(legend.title = element_blank())
                        + theme(legend.title = element_text(size=14, face='bold'))
                        + theme(legend.text = element_text(size=14)))
                
            # ggplot_build I believe is only needed for ggsave if you are passing in a grobs object and not a ggplot. 
            # g = ggplot_build(meansPlot)
            # This in combination with the second output$downloadHandler method works but then it doesn't plot the actual plot.
            # ggsave(filename='plot.pdf', plot=meansPlot, device=pdf)
            # dev.off()

            meansPlot
        }
    })
    
    # This must be defined after makePlot() is defined!
    output$meansPlot = makePlot()

})