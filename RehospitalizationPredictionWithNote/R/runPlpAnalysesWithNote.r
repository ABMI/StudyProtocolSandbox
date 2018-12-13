runPlpAnalysesWithNote <- function(connectionDetails,
                          cdmDatabaseSchema,
                          cdmDatabaseName,
                          oracleTempSchema = cdmDatabaseSchema,
                          cohortDatabaseSchema = cdmDatabaseSchema,
                          cohortTable = "cohort",
                          outcomeDatabaseSchema = cdmDatabaseSchema,
                          outcomeTable = "cohort",
                          cdmVersion = 5,
                          outputFolder = "./PlpOutput",
                          modelAnalysisList,
                          cohortIds,
                          cohortNames,
                          outcomeIds,
                          outcomeNames,
                          washoutPeriod = 0,
                          maxSampleSize = NULL,
                          minCovariateFraction = 0,
                          normalizeData = T,
                          testSplit = "person",
                          testFraction = 0.25,
                          splitSeed = NULL,
                          nfold = 3,
                          verbosity = "INFO") {
  
  # start log:
  clearLoggerType("Multple PLP Log")
  if(!dir.exists(outputFolder)){dir.create(outputFolder,recursive=T)}
  logFileName = paste0(outputFolder,'/plplog.txt')
  logger <- OhdsiRTools::createLogger(name = "Multple PLP Log",
                                      threshold = verbosity,
                                      appenders = list(OhdsiRTools::createFileAppender(layout = OhdsiRTools::layoutParallel,
                                                                                       fileName = logFileName)))
  OhdsiRTools::registerLogger(logger)
  
  if (missing(outcomeIds)){
    stop("Need to specify outcome ids")
  }
  if (missing(cohortIds)){
    stop("Need to specify cohort ids")
  }
  if (missing(connectionDetails)){
    stop("Need to specify connectionDetails")
  }
  if (missing(cdmDatabaseSchema)){
    stop("Need to specify cdmDatabaseSchema")
  }
  if (missing(cdmDatabaseName)){
    stop("Need to specify cdmDatabaseName - a shareable name for the database")
  }
  if (missing(modelAnalysisList)){
    stop("Need to specify modelAnalysisList")
  }
# check input types
  plpDataSettings <- list(connectionDetails = connectionDetails,
                          cdmDatabaseSchema = cdmDatabaseSchema,
                          oracleTempSchema = oracleTempSchema, 
                          cohortDatabaseSchema = cohortDatabaseSchema,
                          cohortTable = cohortTable,
                          outcomeDatabaseSchema = outcomeDatabaseSchema,
                          outcomeTable = outcomeTable,
                          cdmVersion = cdmVersion,
                          firstExposureOnly = F,
                          washoutPeriod = washoutPeriod,
                          sampleSize = maxSampleSize
                          )
  
  runPlpSettings <- list(minCovariateFraction = minCovariateFraction,
                           normalizeData = normalizeData,
                           testSplit = testSplit,
                           testFraction = testFraction,
                           splitSeed = splitSeed,
                           nfold = nfold,
                           verbosity = verbosity )

  if (!dir.exists(outputFolder)){
    dir.create(outputFolder)
    }
  if (!dir.exists(file.path(outputFolder,'Validation'))){
    dir.create(file.path(outputFolder,'Validation'), recursive = T)
  }
  
  OhdsiRTools::logTrace(paste0('Creating reference table'))
  referenceTable <- tryCatch({createPlpReferenceTable(modelAnalysisList,
                                         cohortIds,
                                         outcomeIds,
                                         outputFolder, cdmDatabaseName)},
                             error = function(cont){OhdsiRTools::logTrace(paste0('Creating reference table error:', cont)); stop()})
  if(!missing(cohortNames)){
    if(!is.null(cohortNames))
      if(length(cohortNames)!=length(cohortIds)){
        stop('cohortNames entered but different length to cohortIds')
      }
      cnames <- data.frame(cohortId=cohortIds, cohortName=cohortNames)
      referenceTable <- merge(referenceTable, cnames, by='cohortId', all.x=T)
  }
  if(!missing(outcomeNames)){
    if(!is.null(outcomeNames))
      if(length(outcomeNames)!=length(outcomeIds)){
        stop('outcomeNames entered but different length to outcomeIds')
      }
    onames <- data.frame(outcomeId=outcomeIds, outcomeName=outcomeNames)
    referenceTable <- merge(referenceTable, onames, by='outcomeId', all.x=T)
  }
  
  if(!file.exists(file.path(outputFolder,'settings.csv'))){
    OhdsiRTools::logTrace(paste0('Writing settings csv to ',file.path(outputFolder,'settings.csv') ))
    write.csv(referenceTable,
              file.path(outputFolder,'settings.csv'), 
              row.names = F )
  }
  
  for(i in 1:nrow(referenceTable)){
    
    plpDataFolder <- referenceTable$plpDataFolder[i]
    if(!dir.exists(plpDataFolder)){
      OhdsiRTools::logTrace(paste0('Running setting ', i ))
      
      oind <- referenceTable$cohortId==referenceTable$cohortId[i] & 
              referenceTable$covariateSettingId==referenceTable$covariateSettingId[i]
      outcomeIds <- unique(referenceTable$outcomeId[oind])
          
      plpDataSettings$cohortId <- referenceTable$cohortId[i]
      plpDataSettings$outcomeIds <- outcomeIds 
      plpDataSettings$covariateSettings <- modelAnalysisList$covariateSettings[[referenceTable$covariateSettingId[i]]]

      #defaultTopicModel <- noteCovariateExtraction::loadDefaultTopicModel(noteConceptId = 44814637)
      #noteCovSet<-noteCovariateExtraction::createTopicFromNoteSettings(useTopicFromNote = TRUE,
                                                              # noteConceptId = c(44814637),
                                                              # useDictionary=TRUE,
                                                              # targetLanguage = c('KOR','ENG'),
                                                              # limitedMedicalTermOnlyLanguage = c('KOR','ENG'),
                                                              # nGram = 1L,
                                                              # buildTopicModeling= FALSE,
                                                              # buildTopidModelMinFrac = 0.01,
                                                              # existingTopicModel = defaultTopicModel,
                                                              # useTextToVec = FALSE,
                                                              # useTopicModeling=FALSE,
                                                              # numberOfTopics=10L,
                                                              # optimalTopicValue =TRUE,
                                                              # useGloVe = FALSE,
                                                              # latentDimensionForGlove = 100L,
                                                              # useAutoencoder=FALSE,
                                                              # latentDimensionForAutoEncoder = 100L,
                                                              # sampleSize=-1)
      #plpDataSettings$covariateSettings <- list ( modelAnalysisList$covariateSettings[[referenceTable$covariateSettingId[i]]], noteCovSet)
        
      plpData <- tryCatch(do.call(getPlpData, plpDataSettings),
               finally= OhdsiRTools::logTrace('Done plpData.'),
               error= function(cond){OhdsiRTools::logTrace(paste0('Error with getPlpData:',cond));return(NULL)})
  
      if(!is.null(plpData)){
        OhdsiRTools::logTrace(paste0('Saving data in setting ', i ))
        savePlpData(plpData, referenceTable$plpDataFolder[i])
      }
    } else{
      OhdsiRTools::logTrace(paste0('Loading data in setting ', i ))
      plpData <- loadPlpData(referenceTable$plpDataFolder[i])
    }
    
    if(!file.exists(referenceTable$studyPop[i])){
      OhdsiRTools::logTrace(paste0('Setting population settings for setting ', i ))
      # get pop and save to referenceTable$popFile
      popSettings <- modelAnalysisList$populationSettings[[referenceTable$populationSettingId[i]]]
      popSettings$outcomeId <- referenceTable$outcomeId[i] 
      popSettings$plpData <- plpData
      population <- tryCatch(do.call(createStudyPopulation, popSettings),
               finally= OhdsiRTools::logTrace('Done pop.'), 
               error= function(cond){OhdsiRTools::logTrace(paste0('Error with pop:',cond));return(NULL)})
      if(!is.null(population)){
        OhdsiRTools::logTrace(paste0('Saving population for setting ', i ))
        saveRDS(population, referenceTable$studyPop[i])
      }
    } else{
      OhdsiRTools::logTrace(paste0('Loading population for setting', i ))
      population <- readRDS(referenceTable$studyPop[i])
    }
    
    plpResultFolder = file.path(referenceTable$plpResultFolder[i],'plpResult')
    if(!dir.exists(plpResultFolder)){
      OhdsiRTools::logTrace(paste0('Running runPlp for setting ', i ))
      dir.create(referenceTable$plpResultFolder[i], recursive = T)
      # runPlp and save result to referenceTable$plpResultFolder
      runPlpSettings$modelSettings <- modelAnalysisList$models[[referenceTable$modelSettingId[i]]]
      runPlpSettings$plpData <- plpData
      runPlpSettings$population <- population
      runPlpSettings$saveDirectory <- gsub(paste0('/Analysis_',referenceTable$analysisId[i]),'',referenceTable$plpResultFolder[i])
      runPlpSettings$analysisId <- paste0('Analysis_',referenceTable$analysisId[i])
      runPlpSettings$savePlpData <- F
      runPlpSettings$savePlpResult <- T
      runPlpSettings$savePlpPlots <- F
      runPlpSettings$saveEvaluation <- F
      result <- tryCatch(do.call(runPlp, runPlpSettings),
                             finally= OhdsiRTools::logTrace('Done runPlp.'), 
                             error= function(cond){OhdsiRTools::logTrace(paste0('Error with runPlp:',cond));return(NULL)})
    }
    
  }
  return(referenceTable)
}