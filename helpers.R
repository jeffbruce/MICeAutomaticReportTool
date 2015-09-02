# Data Specification ---------------------------------------

# Only defined for relative volumes but "relative" is replaced with "absolute" when actually loading the data, but only for the combined_vols files, not the gf files.
datadefs <- rbind(c(name="TSC1", gf="gf_Tsai_40um_factorage", data="combined_vols_Tsai_40um",term="Genotype", G1="WT", G2="KO", group="TSC1"))

datadefs <- as.data.frame(datadefs, stringsAsFactors=FALSE)


# Loading Data ------------------------------------------------------------

GfMetadata <- function(gfFiles) {
  # Summary:
  #   Returns a named vector of the optional fields that users will be able to explore in the app.
  
  # Determine which optional metadata columns being used in the gf files.
  gfMetadata = c("Treatment"=FALSE, "Sex"=FALSE, "Background"=FALSE, "FactorAge"=FALSE, "RawAge"=FALSE)
  for (file in gfFiles) {
    gfFile = paste(getwd(), "/data/", file, ".txt", sep="")
    tempDf = read.table(gfFile)
    tempDfCols = colnames(tempDf)
    for (item in names(gfMetadata)) {
      if (item %in% tempDfCols) {
        gfMetadata[[item]] = TRUE
      }
    }
  }
  
  return(gfMetadata)
}


LoadData <- function() {
  # Summary:
  #   Loads the gf files and both the relative and absolute combined vols data files if they exist.
  
  dataFiles = datadefs$data
  gfFiles = datadefs$gf

  # load gf files
  for (file in gfFiles) {
    gfFile = paste(getwd(), "/data/", file, ".txt", sep="")
    assign(x=file, value=read.table(file=gfFile, header=TRUE, sep=" "), envir=parent.frame())
  }
  
  # load relative volume files
  for (file in dataFiles) {
    relativeFile = paste(getwd(), "/data/", file, "_relative.txt", sep="")
    if (file.exists(relativeFile)) {
      assign(x=paste(file, "_relative", sep=""), value=read.table(file=relativeFile, header=TRUE, sep=" "), envir=parent.frame())
    }
  }
  
  # load absolute volume files
  for (file in dataFiles) {
    absoluteFile = paste(getwd(), "/data/", file, "_absolute.txt", sep="")
    if (file.exists(absoluteFile)) {
      assign(x=paste(file, "_absolute", sep=""), value=read.table(file=absoluteFile, header=TRUE, sep=" "), envir=parent.frame())
    }
  }
}


IndividualData <- function(datadefs, volumeType) {
  # Summary:
  #   Creates a data table containing volume data for each individual mouse for each brain region.
  # Args:
  #   datadefs: A specification of the data files and the labels used to represent different strains.
  #   volumeType: One of 'relative' or 'absolute'.
  # Returns:
  #   A data table containing volume data for each individual mouse for each brain region.
  
  # Find optional fields that might be contained in the gf files that users could plot by.
  gfMetadata = GfMetadata(datadefs$gf)
  
  # <app dir>/data/ should is the base path for data
  basePath = paste(getwd(), '/data/', sep='')
  
  # Get an example combined vols file and save to a data frame.  Assumes that the first combined vols data file specified in datadefs is in the /data/ subdirectory.
  combinedVolsFile = paste(basePath, datadefs$data[1], '_absolute.txt', sep='')
  if (file.exists(combinedVolsFile)) {
    combinedVolsDf = read.table(combinedVolsFile)
  } else {
    combinedVolsFile = paste(basePath, datadefs$data[1], '_relative.txt', sep='')
    combinedVolsDf = read.table(combinedVolsFile)
  }
  
  # Returns the fields in gfMetadata that are marked TRUE.
  gfMetadataTrue = which(gfMetadata)

  # Calculate how many optional columns were used in the gf files.
  count = length(gfMetadataTrue)
  
  # number of columns = Strain + Genotype + count (optional columns like Treatment, Sex, Age, etc.) + Num Regions
  individualData = data.table(matrix(ncol = 2 + count + dim(combinedVolsDf)[2]))
  individualData = individualData[-1, ]  # remove NAs from initialization
  
  # Set appropriate column names.
  individualDataColLabels = c(regionNameDictionary, "Strain", "Genotype", names(gfMetadataTrue))
  setnames(individualData, individualDataColLabels)
  
  # Loop through the gf files that need to be processed and extract the data.
  for (i in 1:nrow(datadefs)) {

    # Get data for a single strain as a data.table.
    row = datadefs[i, ]
    filename = paste(row$data, '_', volumeType, sep='')

    if (file.exists(paste(basePath, filename, '.txt', sep=''))) {

      tempData = get(filename)
      tempData = as.data.table(tempData)

      # If RawAge is in the file, it will read "5" in as an integer, even when written in quotes.  It probably also reads 5 in as an integer, though I haven't tested this.
      gfFile = get(row$gf)
       
      # Set proper column names.  Look up names from a dictionary (named vector).
      setnames(tempData, regionNameDictionary)
      tempData$Strain = row$name
      tempData$Genotype = gfFile$Genotype
      
      # Get optional column names (Treatment, RawAge, FactorAge, Sex, Background).
      for (i in 1:length(gfMetadataTrue)) {
        tempData[, names(gfMetadataTrue)[i]] = get(names(gfMetadataTrue[i]), gfFile)
      }

      # Append data to total dataset.
      individualData = rbind(individualData, tempData)
    }
  }
  
  # Make long format and set appropriate column names.
  individualData = reshape2:::melt(individualData, id=c('Strain', 'Genotype', names(gfMetadataTrue)))
  setnames(individualData, c('Strain', 'Genotype', names(gfMetadataTrue), 'Region', 'Volume'))
  individualData$Genotype = as.factor(individualData$Genotype)
  individualData$Region = as.factor(individualData$Region)
  individualData$Strain = as.factor(individualData$Strain)
  
  return(individualData)
}


# Quality Assurance -------------------------------------------------------

CheckCombinedVolsFiles <- function() {
  # Summary:
  #   Loop through all combined vols files, ensure all column names are same and in same order.  Called at outset of app to prevent user from running app if files do not have same column schema.
  # Returns:
  #   TRUE if all combined vols files have same column schema.
  #   FALSE if some combined vols files have different column schema.
  
  dataFiles = datadefs$data
  gfFiles = datadefs$gf
  
  # get region names
  volumeFile = paste(getwd(), "/data/", dataFiles[1], "_relative.txt", sep="")
  if (file.exists(volumeFile)) {
    volumeDf = read.table(volumeFile)
    regionNames = colnames(volumeDf)
  } else {
    volumeFile = paste(getwd(), "/data/", dataFiles[1], "_absolute.txt", sep="")
    volumeDf = read.table(volumeFile)
    regionNames = colnames(volumeDf)
  }
  
  # load relative volume files
  for (file in dataFiles) {
    relativeFile = paste(getwd(), "/data/", file, "_relative.txt", sep="")
    if (file.exists(relativeFile)) {
      volumeDf = read.table(relativeFile)
      otherRegionNames = colnames(volumeDf)
      if (!all(otherRegionNames == regionNames)) {
        return(FALSE)
      }
    }
  }
  
  # load absolute volume files
  for (file in dataFiles) {
    absoluteFile = paste(getwd(), "/data/", file, "_absolute.txt", sep="")
    if (file.exists(absoluteFile)) {
      volumeDf = read.table(absoluteFile)
      otherRegionNames = colnames(volumeDf)
      if (!all(otherRegionNames == regionNames)) {
        return(FALSE)
      }
    }
  }
  
  return(TRUE)
}


StatsSummaryTable <- function(df1, df2) {
  # Summary:
  #   Takes two data frames, each representing a group of mice.  Computes descriptive stats (Mean, SD) for each group along with statistical tests comparing the groups (t-test, Cohen's d, etc.).

  means1 = with(df1, tapply(Volume, Region, mean))
  sds1 = with(df1, tapply(Volume, Region, sd))
  means2 = with(df2, tapply(Volume, Region, mean))
  sds2 = with(df2, tapply(Volume, Region, sd))
  d = abs(means1 - means2)/sds1

  # Round for display in interactive table.
  r_means1 = round(means1, 2)
  r_sds1 = round(sds1, 2)
  r_means2 = round(means2, 2)
  r_sds2 = round(sds2, 2)
  r_d = round(d, 2)

  pvals = CalculateStats(df1, df2)

  summaryTable = data.frame("Region"=rownames(r_means1), "G1Mean"=r_means1, "G1Stdev"=r_sds1, "G2Mean"=r_means2, "G2Stdev"=r_sds2, "Effect"=r_d)
  summaryTable = merge(summaryTable, pvals, by='Region')
  summaryTable$P_value = round(summaryTable$P_value, 3)
  summaryTable$FDR_P_value = round(summaryTable$FDR_P_value, 3)
  rownames(summaryTable) = NULL

  return(summaryTable)
}


CalculateStats <- function(df1, df2) {
  data = rbind.fill(df1, df2)
  pvals = ddply(data, .(Region), ttesthelper)
  setnames(pvals, c('Region', 'P_value'))
  pvals$FDR_P_value = p.adjust(p=pvals$P_value, method='fdr')
  setnames(pvals, c('Region', 'P_value', 'FDR_P_value'))

  pvals
}


ttesthelper <- function(x) {
  t.test(x[,'Volume'] ~ x[,'Group'])$p.value
}


# Proper Column Names ----------------------------------------------------------

# Doesn't need to be a named list!  Can simply be a named vector.  Lists are only needed if the elements are of different types.
regionNameDictionary <- c("amygdala"="Amygdala", 
                             "anterior.commissure..pars.anterior"="Anterior Commissure - Pars Anterior",
                             "anterior.commissure..pars.posterior"="Anterior Commissure - Pars Posterior",
                             "basal.forebrain"="Basal Forebrain",
                             "bed.nucleus.of.stria.terminalis"="Bed Nucleus of Stria Terminalis",
                             "cerebellar.peduncle..inferior"="Cerebellar Peduncle - Inferior",
                             "cerebellar.peduncle..middle"="Cerebellar Peduncle - Middle",
                             "cerebellar.peduncle..superior"="Cerebellar Peduncle - Superior",
                             "cerebral.aqueduct"="Cerebral Aqueduct",
                             "cerebral.cortex..entorhinal.cortex"="Entorhinal Cortex",
                             "cerebral.cortex..frontal.lobe"="Frontal Lobe",
                             "cerebral.cortex..occipital.lobe"="Occipital Lobe",
                             "cerebral.cortex..parieto.temporal.lobe"="Parieto-Temporal Lobe",
                             "cerebral.peduncle"="Cerebral Peduncle",
                             "colliculus..inferior"="Inferior Colliculus",
                             "colliculus..superior"="Superior Colliculus",
                             "corpus.callosum"="Corpus Callosum",
                             "corticospinal.tract.pyramids"="Corticospinal Tract",
                             "cuneate.nucleus"="Cuneate Nucleus",
                             "dentate.gyrus.of.hippocampus"="Dentate Gyrus",
                             "facial.nerve..cranial.nerve.7."="Cranial Nerve 7",
                             "fasciculus.retroflexus"="Fasciculus Retroflexus",
                             "fimbria"="Fimbria",
                             "fornix"="Fornix",
                             "fourth.ventricle"="Fourth Ventricle",
                             "fundus.of.striatum"="Fundus of Striatum",
                             "globus.pallidus"="Globus Pallidus",
                             "habenular.commissure"="Habenular Commissure",
                             "hippocampus"="Hippocampus",
                             "hypothalamus"="Hypothalamus",
                             "inferior.olivary.complex"="Inferior Olivary Complex",
                             "internal.capsule"="Internal Capsule",
                             "interpedunclar.nucleus"="Interpeduncular Nucleus",
                             "lateral.olfactory.tract"="Lateral Olfactory Tract",
                             "lateral.septum"="Lateral Septum",
                             "lateral.ventricle"="Lateral Ventricle",
                             "mammillary.bodies"="Mammillary Bodies",
                             "mammilothalamic.tract"="Mammillothalamic Tract",
                             "medial.lemniscus.medial.longitudinal.fasciculus"="Medial Longitudinal Fasciculus",
                             "medial.septum"="Medial Septum",
                             "medulla"="Medulla",
                             "midbrain"="Midbrain",
                             "nucleus.accumbens"="Nucleus Accumbens",
                             "olfactory.bulbs"="Olfactory Bulbs",
                             "olfactory.tubercle"="Olfactory Tubercle",
                             "optic.tract"="Optic Tract",
                             "periaqueductal.grey"="Periaqueductal Grey",
                             "pons"="Pons",
                             "pontine.nucleus"="Pontine Nucleus",
                             "posterior.commissure"="Posterior Commissure",
                             "pre.para.subiculum"="Pre-Para Subiculum",
                             "stratum.granulosum.of.hippocampus"="Stratum Granulosum",
                             "stria.medullaris"="Stria Medullaris",
                             "stria.terminalis"="Stria Terminalis",
                             "striatum"="Striatum",
                             "subependymale.zone...rhinocele"="Rhinocele",
                             "superior.olivary.complex"="Superior Olivary Complex",
                             "thalamus"="Thalamus",
                             "third.ventricle"="Third Ventricle",
                             "ventral.tegmental.decussation"="Ventral Tegmental Decussation",
                             "lobules.1.2..lingula.and.central.lobule..ventral."="Lobules 1-2 - Lingula and Central Lobule (Ventral)",
                             "lobule.3..central.lobule..dorsal."="Lobule 3 - Central Lobule (Dorsal)",
                             "lobules.4.5..culmen..ventral.and.dorsal."="Lobules 4-5 - Culmen (Ventral and Dorsal)",
                             "lobule.6..declive"="Lobule 6 - Declive",
                             "lobule.7..tuber..or.folium."="Lobule 7 - Tuber (Or Folium)",
                             "lobule.8..pyramis"="Lobule 8 - Pyramis",
                             "lobule.9..uvula"="Lobule 9 - Uvula",
                             "lobule.10..nodulus"="Lobule 10 - Nodulus",
                             "anterior.lobule...lobules.4.5."="Anterior Lobule (Lobules 4-5)",
                             "simple.lobule..lobule.6."="Simple Lobule (Lobule 6)",
                             "crus.1..ansiform.lobule..lobule.6."="Crus 1 - Ansiform Lobule (Lobule 6)",
                             "crus.2..ansiform.lobule..lobule.7."="Crus 2 - Ansiform Lobule (Lobule 7)",
                             "paramedian.lobule..lobule.7."="Paramedian Lobule (Lobule 7)",
                             "copula..pyramis..lobule.8."="Copula - Pyramis (Lobule 8)",
                             "flocculus..FL."="Flocculus (FL)",
                             "paraflocculus..PFL."="Paraflocculus (PFL)",
                             "trunk.of.arbor.vita"="Trunk of Arbor Vita",
                             "lobule.1.2.white.matter"="Lobules 1-2 - White Matter",
                             "lobule.3.white.matter"="Lobule 3 - White Matter",
                             "trunk.of.lobules.1.3.white.matter"="Trunk of Lobules 1-3 - White Matter",
                             "lobules.4.5.white.matter"="Lobules 4-5 - White Matter",
                             "lobules.6.7.white.matter"="Lobules 6-7 - White Matter",
                             "lobule.8.white.matter"="Lobule 8 - White Matter",
                             "trunk.of.lobules.6.8.white.matter"="Trunk of Lobules 6-8 - White Matter",
                             "lobule.9.white.matter"="Lobule 9 - White Matter",
                             "lobule.10.white.matter"="Lobule 10 - White Matter",
                             "anterior.lobule.white.matter"="Anterior Lobule - White Matter",
                             "simple.lobule.white.matter"="Simple Lobule - White Matter",
                             "crus.1.white.matter"="Crus 1 - White Matter",
                             "trunk.of.simple.and.crus.1.white.matter"="Trunk of Simple and Crus 1 - White Matter",
                             "crus.2.white.matter"="Crus 2 - White Matter",
                             "paramedian.lobule"="Paramedian Lobule",
                             "trunk.of.crus.2.and.paramedian.white.matter"="Trunk of Crus 2 and Paramedian - White Matter",
                             "copula.white.matter"="Copula - White Matter",
                             "paraflocculus.white.matter"="Paraflocculus - White Matter",
                             "flocculus.white.matter"="Flocculus - White Matter",
                             "dentate.nucleus"="Dentate Nucleus",
                             "nucleus.interpositus"="Nucleus Interpositus",
                             "fastigial.nucleus"="Fastigial Nucleus")