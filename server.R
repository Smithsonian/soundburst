library(shiny)
library(tools)
library(devtools)
library(stringr)
library(shinyFiles)
library("aws.s3")
library(shinyTree)
library(seewave)
library(tuneR)
library(httr)
library(shinyBS)
library(audio)
library(sound)
library(stringr)
source("createDirectoryTree.r")
source("playSound.r")
library("fftw")
library(lubridate)

# Removing previously loaded global environment, if any
rm(list=ls())

# Global variables
mainDir <<- NULL
clipCount <<- 0
newName <- NULL
depPath <<- NULL
autoProjectCSVLoad <<- FALSE
newSequenceBool <<- FALSE
autoDepCSVLoad <<- FALSE
annData <<- list()
readSequenceBool <<- FALSE
annotationListDrop <<- list()
currAnnListGlobal <<- list()
annotationListWav <<- vector()
annotationListCsv <<- vector()
annotationListCsvProject <<- vector()
progressValue <<- reactiveValues()
progressValue$one <<- 0
projectFileCountGlobal <<- 0
alreadyAnnotated <<- FALSE
alreadyAnnotatedCount <<- 0
dropSubmitClicked <<- FALSE
firstLoaded <<- TRUE
progressBarExists <<- FALSE


# This is used to connect correctly with AWS
set_config( config( ssl_verifypeer = 0L ) )

options(shiny.trace=TRUE)
options(shiny.maxRequestSize=70*1024^2)
volumes <- getVolumes()
paused <<- FALSE

getOS <- function() {
  if (.Platform$OS.type == "windows") { 
    setWavPlayer("afplay")
    mainDir <<- "C:/"
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    setWavPlayer("afplay")
    mainDir <<- "~"
    "mac" 
  } else if (.Platform$OS.type == "unix") {
    setWavPlayer("aplay")
    mainDir <<- "~"
    "unix"
  } else {
    stop("Unknown OS")
  }
}


progressBar <- function(value = 0, label = FALSE, color = "aqua", size = NULL,
                        striped = FALSE, active = FALSE, vertical = FALSE) {
  stopifnot(is.numeric(value))
  if (value < 0 || value > 100)
    stop("'value' should be in the range from 0 to 100.", call. = FALSE)
  if (!(color %in% shinydashboard:::validColors || color %in% shinydashboard:::validStatuses))
    stop("'color' should be a valid status or color.", call. = FALSE)
  if (!is.null(size))
    size <- match.arg(size, c("sm", "xs", "xxs"))
  text_value <- paste0(value, "%")
  style <- htmltools::css(width = text_value)
  tags$div(
    class = "progress",
    class = if (!is.null(size)) paste0("progress-", size),
    tags$div(
      class = "progress-bar",
      class = paste0("progress-bar-", color),
      style = style,
      role = "progressbar",
      `aria-valuenow` = value,
      `aria-valuemin` = 0,
      `aria-valuemax` = 100,
      tags$span(class = if (!label) "sr-only", text_value)
    )
  )
}

progressGroup <- function(text, value, min = 0, max = value, color = "aqua") {
  stopifnot(is.character(text))
  stopifnot(is.numeric(value))
  if (value < min || value > max)
    stop(sprintf("'value' should be in the range from %d to %d.", min, max), call. = FALSE)
  tags$div(
    class = "progress-group",
    tags$span(class = "progress-text", text),
    tags$span(class = "progress-number", sprintf("%d / %d", value, max)),
    progressBar(round(value / max * 100), color = color, size = "sm")
  )
}

shinyServer(function(input, output, session) {
  
  # HTML("nav",div(id="navbar-title-file-name", "Hello"))
  
  projectName <<- NULL
  spectroFromTime <<- 0
  spectroToTime <<- 0
  deploymentCSVDataTable <<- NULL
  dirPath <<- NULL
  
  shinyjs::onclick("left-column-title", toggleProjectSelect())
  # shinyjs::onclick("species-file-upload", togglecsvFileUploadButton())
  shinyjs::onclick("enter-project-info-label", toggleProjectInfoDisplay())
  shinyjs::onclick("right-column-title", toggledeploymentInfoContainer())
  shinyjs::onclick("completedDepContainer", toggleCompletedDeployment())
  shinyjs::onclick("select-dep-container", toggleDeploymentSelectDisplay())
  shinyjs::onclick("show-tree", toggleTree())
  shinyjs::hide("progressOne")
  shinyjs::hide("tree")
  shinyjs::hide("directorypath")
  shinyjs::hide("deployment")
  shinyjs::hide(id = "playButton",anim = FALSE)
  shinyjs::hide(id = "playButtonClipZoom", anim = FALSE)
  shinyjs::onclick("aws-upload-button", resetAwsCount())
  shinyjs::disable("aws-upload-button")
  shinyjs::onevent('mouseenter', "csvFile", showCSVModal())
  shinyjs::onevent('mouseleave', "csvFile", hideCSVModal())
  
  # shinyjs::onclick("sF-selectButton", toggleAfterProjectSelect())
  
  showCSVModal = function() {
    shinyjs::show("csv-info-modal-container")
  }
  
  hideCSVModal = function() {
    shinyjs::hide("csv-info-modal-container")
  }
  
  toggleProjectSelect = function() {
    shinyjs::toggle("directory", anim = TRUE)
    shinyjs::toggleClass("left-column-title", "open-accordian")
    shinyjs::toggleClass("left-column-title", "closed-accordian")
  }
  
  toggleProjectInfoDisplay = function() {
    shinyjs::toggle("project-info-container", anim = TRUE)
    shinyjs::toggleClass("enter-project-info-label", "open-accordian")
    shinyjs::toggleClass("enter-project-info-label", "closed-accordian")
  }
  
  toggleDeploymentSelectDisplay = function() {
    shinyjs::toggle("deployment", anim = TRUE)
    shinyjs::toggleClass("select-dep-container", "open-accordian")
    shinyjs::toggleClass("select-dep-container", "closed-accordian")
  }
  
  togglecsvFileUploadButton = function() {
    shinyjs::toggle("csvFile", anim = TRUE)
    shinyjs::toggleClass("species-file-upload", "open-accordian")
    shinyjs::toggleClass("species-file-upload", "closed-accordian")
  }
  
  toggledeploymentInfoContainer = function() {
    shinyjs::toggle("species-sidebox-container", anim = TRUE)
    shinyjs::toggleClass("right-column-title", "open-accordian")
    shinyjs::toggleClass("right-column-title", "closed-accordian")
  }
  
  toggleCompletedDeployment = function() {
    shinyjs::toggle("annotationDrop", anim = TRUE)
    shinyjs::toggleClass("completedDepContainer", "open-accordian")
    shinyjs::toggleClass("completedDepContainer", "closed-accordian")
  }
  
  #################################
  ##### This function runs right after the selection of a project is done
  ##### Mainly toggles classes and set the current directory
  ################################
  toggleAfterProjectSelect = function (){
    shinyjs::hide("directory", anim = TRUE)
    shinyjs::addClass("left-column-title", "completed-step")
    shinyjs::show("directorypath")
    shinyjs::show("project-info-container")
    shinyjs::toggleClass("left-column-title", "open-accordian")
    shinyjs::toggleClass("left-column-title", "closed-accordian")
    shinyjs::toggleClass("enter-project-info-label", "open-accordian")
    shinyjs::toggleClass("enter-project-info-label", "closed-accordian")
    dirPath <<- parseDirPath(roots=c(home=mainDir), input$directory)
    # Had to check if dirPath was not of length 0 otherwise shiny would return an error
    # if(length(dirPath) != 0) {
    #   setwd(normalizePath(dirPath))
    # }
  }
  
  toggleAfterProjectCsvLoaded = function() {
    toggleProjectInfoDisplay()
    shinyjs::addClass("enter-project-info-label", "completed-step")
    toggleDeploymentSelectDisplay()
  }
  
  toggleAfterDeploymentCsvLoaded = function() {
    shinyjs::hide("deployment", anim = TRUE)
    shinyjs::removeClass("select-dep-container", "open-accordian")
    shinyjs::addClass("select-dep-container", "closed-accordian")
    shinyjs::addClass("right-column-title", "completed-step")
    shinyjs::removeClass("right-column-title", "open-accordian")
    shinyjs::addClass("right-column-title", "closed-accordian")
  }
  
  toggleAfterDeploymentSelect = function (){
    shinyjs::hide("deployment", anim = TRUE)
    shinyjs::addClass("select-dep-container", "completed-step")
    shinyjs::show("species-sidebox-container")
    shinyjs::removeClass("select-dep-container", "open-accordian")
    shinyjs::addClass("select-dep-container", "closed-accordian")
    shinyjs::addClass("right-column-title", "open-accordian")
    shinyjs::removeClass("right-column-title", "closed-accordian")
    shinyjs::addClass("show-tree", "open-accordian")
    shinyjs::removeClass("show-tree", "closed-accordian")
    shinyjs::show("tree", anim = TRUE)
  }
  
  toggleTree = function() {
    shinyjs::toggleClass("show-tree", "open-accordian")
    shinyjs::toggleClass("show-tree", "closed-accordian")
    shinyjs::toggle("directorypath", anim = TRUE)
    shinyjs::toggle("tree", anim = TRUE)
  }
  
  getOS()
  test <- shinyDirChoose(input, 'directory', updateFreq=60000, session=session, root=c(home=mainDir), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  
  observeEvent(input$directory, {
    toggleAfterProjectSelect()
    # Removing any .wav files that were copied in the /www folder for sound play
    wavToRemove <-list.files(paste0(getwd(), "/www"), pattern='.wav', full.names = TRUE)
    unlink(wavToRemove)
    getOS()
    dirPath <<- parseDirPath(roots=c(home=mainDir), input$directory)
    # Get folder name -> which is also the project name
    projectName <<- gsub("^.*\\/", "", dirPath)
    if(file.exists(paste0(dirPath, "/Project_", projectName, ".csv"))) { # CHANGE
      readProjectCSV(dirPath, projectName)
      createProjectTree()
      return()
    }
    # Updating the value of the project name input value
    updateTextInput(session, inputId = "projectName", label = NULL, value = projectName)
    if(length(dirPath)) {
      createProjectTree()
    }
  })
  
  createProjectTree = function () {
    shinyjs::show("progressOne")
    shinyjs::addClass("directory", "active-button")
    shinyjs::show("aws-upload-button")
    deploymentSelect <- shinyDirChoose(input, 'deployment', updateFreq=60000, session=session, root=c(home=normalizePath(dirPath)), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  }
  
  observeEvent(input$deployment, {
    currAnnList <- list()
    currAnnListGlobal <<- currAnnList
    updateSelectizeInput(session, "annotationDrop", label = "Select an annotation", choices =  currAnnList)
    depPath <<- parseDirPath(root=c(home=normalizePath(dirPath)), input$deployment)
    firstTime <<- FALSE
    if(!is.null(depPath)) {
      shinyjs::show("progressOne")
      files <- list.files(depPath, all.files=F, recursive=T, include.dirs=T)
      sapply(files,FUN=function(eachPath){
        if (substrRight(eachPath,4) == ".wav") {
          if (regexpr('_.*__', eachPath)[1] != -1) {
            fileName <- paste0(depPath,"/",eachPath)
            fileType <- substrRight(eachPath,4)
            removeType <- gsub(".*__","",eachPath)
            addProjectName <- paste0(projectName,"_", removeType)
            # timeStringLength <- regexpr('_.*__', eachPath)
            # matchedString <- timeStringLength + attr(timeStringLength, "match.length")-1
            # fileTime <- substr(eachPath, timeStringLength+1, matchedString-1)
            # fileNameCountRemoved <- gsub(fileTime,"_",eachPath, fixed = TRUE)
            # updatedFileName1 <- sub("_", "",fileNameCountRemoved, fixed = TRUE)
            # updatedWavFileName <- sub("_", "",updatedFileName1, fixed = TRUE)
            updatedWavFilePath <- paste0(depPath,"/",addProjectName)
            file.rename(fileName,updatedWavFilePath)
            firstTime <<- TRUE
          }
        }
      })
      # create_directory_tree(depPath)
      # load("www/dir_tree.Rdata")
      # output$tree <- renderTree(tree, quoted = FALSE)
      shinyjs::addClass("deployment", "active-button")
      deploymentName <<- gsub("^.*\\/", "", depPath)
      toggleAfterDeploymentSelect()
      findFileInfo(firstTime)
      fileDate <- gsub(" ", "-",minTimeVar, fixed = TRUE)
      fileDate <- gsub(":", "-",fileDate, fixed = TRUE)
      depFileName <- paste0(projectName,"_",deploymentName)
      depFilePath <<- paste0(depPath,"/", depFileName, ".csv")
      updateTextInput(session, inputId = "name", label = NULL, value = deploymentName)
      if(file.exists(paste0(depPath,"/", depFileName, ".csv"))) { # CHANGE
        readDeploymentCSV(depPath, depFilePath)
        findFileCount()
        return()
      }
    }
  })
  
  ###########################
  ##### Listener for file selection in "Select Sequence"
  ###########################
  # observeEvent(unlist(get_selected(input$tree)), {
  #   # Plot main spectrogram
  #   shinyjs::show("loadingContainer1")
  #   if (is.null(unlist(get_selected(input$tree))))
  #   {
  #     return()
  #   }
  #   else {
  #     spectroFromTime <<- 0
  #     # Root path of the selected file
  #     path <- getPath(get_selected(input$tree, "names"))
  #     # Full file path
  #     currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
  #     fileType <- substrRight(currDir,4)
  #     if (fileType != ".wav") {
  #       return()
  #     } else {
  #       # Reading the sound file
  #       sound <- readWave(currDir)
  #       # Duration of the sound file
  #       durationMain <<- seewave::duration(sound)
  #       # Storing the start and end time of the wave file in seconds
  #       l <- length(sound)
  #       sr <- sound@samp.rate
  #       soundDuration <- round(l/sr,2)
  #       # TODO Maybe make a function out of this? Might make the code cleaner
  #       newSequenceBool <<- TRUE
  #       if (soundDuration > 59) {
  #         minuteDuration <- round(soundDuration/60)
  #         shinyjs::html("time-box-label", paste0("This file is ", minuteDuration, " minutes long. <br> Would you like to increment the display?"))
  #         shinyjs::show("time-box-container", anim = TRUE)
  #         
  #         # Listener for "Select a Sequence"
  #         observeEvent(input$spectroTimeSubmit, {
  #           # Getting the increment amount
  #           incrementAmount <<- as.numeric(input$spectroEndTime) * 60
  #           spectroToTime <<- incrementAmount
  #           if (file.exists(depFilePath)) {
  #             shinyjs::addClass("loadingContainer1", "loader")
  #             readSequenceCSV(unlist(get_selected(input$tree)))
  #             df <- species()
  #             itemsType <<- c('Select Species',as.character(df[[1]]))
  #             itemsSpecies <<- c('Select Type',as.character(df[[3]]))
  #             updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies)
  #             updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType)
  #           }
  #           file.copy(currDir, paste0(getwd(), "/www"))
  #           shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
  #           renderSpectro(sound)
  #           if (soundDuration > incrementAmount) {
  #             shinyjs::show("spectro-increment-container")
  #             shinyjs::show("next-spectro-increment")
  #           }
  #           shinyjs::show("playButton",anim = FALSE)
  #         })
  #         observeEvent(input$noTimeSubmission,{
  #           if (file.exists(depFilePath)) {
  #             sound <- readWave(paste0(depPath, "/", unlist(get_selected(input$tree))))
  #             soundLength <- seewave::duration(sound)
  #             spectroToTime <<- soundLength
  #             shinyjs::addClass("loadingContainer1", "loader")
  #             df <- species()
  #             itemsType <<- c('Select Species',as.character(df[[1]]))
  #             itemsSpecies <<- c('Select Type',as.character(df[[3]]))
  #             updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies)
  #             updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType)
  #             readSequenceCSV(unlist(get_selected(input$tree)))  
  #           }
  #           spectroToTime <<- soundDuration
  #           renderSpectro(sound)
  #           shinyjs::show("playButton",anim = FALSE)
  #           file.copy(currDir, paste0(getwd(), "/www"))
  #           shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
  #           shinyjs::removeClass("loadingContainer1", "loader")
  #         })
  #       } 
  #       else {
  #         spectroToTime <<- soundDuration
  #         renderSpectro(sound)
  #         shinyjs::show("playButton",anim = FALSE)
  #       }
  #       shinyjs::show("content-id")
  #       if(!is.null(newName)) {
  #         shinyjs::html("titleHeader",newName)
  #       }
  #       else {
  #         shinyjs::html("titleHeader",unlist(get_selected(input$tree)))
  #       }
  #     }
  #   }
  # }, autoDestroy = FALSE)
  
  renderSpectro = function (sound){
    output$spectrogram <- renderPlot({
      # Path to the sound file
      currDir <- paste0(depPath, "/", unlist(get_selected(input$tree)))
      anottationCount <<- 0
      spectro(sound, osc = FALSE, scale = FALSE, tlim = c(spectroFromTime,spectroToTime), cont=TRUE)
      shinyjs::show("complete-deployment")
      shinyjs::removeClass("loadingContainer1", "loader")
    }, res = 72, execOnResize = T)
  }
  
  shinyjs::onclick("previous-spectro-increment", showPreviousSpectroIncrement())
  shinyjs::onclick("next-spectro-increment", showNextSpectroIncrement())
  
  showPreviousSpectroIncrement = function() {
    path <- getPath(get_selected(input$tree, "names"))
    if(!is.null(newName)) {
      currDir <- paste0(depPath, "/", path, newName)
    }
    else {
      currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
    }
    sound <- readWave(currDir)
    l <- length(sound@stereo)
    sr <- sound@samp.rate
    soundDuration <- round(l/sr,2)
    spectroToTime <<- spectroToTime - incrementAmount
    spectroFromTime <<- spectroFromTime - incrementAmount
    renderSpectro(sound)
    readSequenceCSV(unlist(get_selected(input$tree)))
    shinyjs::show("next-spectro-increment")
    if (spectroFromTime == 0) {
      shinyjs::hide("previous-spectro-increment")
    }
    print('clicked')
  }
  
  showNextSpectroIncrement = function() {
    path <- getPath(get_selected(input$tree, "names"))
    if(!is.null(newName)) {
      currDir <- paste0(depPath, "/", path, newName)
    }
    else {
      currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
    }
    sound <- readWave(currDir)
    l <- length(sound@stereo)
    sr <- sound@samp.rate
    shinyjs::show("previous-spectro-increment")
    durationWav <- seewave::duration(sound)
    spectroToTime <<- spectroToTime + incrementAmount
    if(spectroToTime > durationWav) {
      spectroToTime <<- durationWav
      if((spectroFromTime + incrementAmount) < durationWav) {
        spectroFromTime <<- spectroFromTime + incrementAmount
      }
    } 
    else {
      spectroFromTime <<- spectroFromTime + incrementAmount
    }
    renderSpectro(sound)
    readSequenceCSV(unlist(get_selected(input$tree)))
    shinyjs::show("previous-spectro-increment")
    if (spectroToTime >= durationWav) {
      shinyjs::hide("next-spectro-increment")
    }
  }
  
  ##############################
  ###### Function that plays the sound after clicking the play button
  ###### Arg    start       Start time of the sound
  ###### Arg    end         End time of the sound
  ###### Arg    chartType   String to know which play button has been pressed (as we have 3 in the app)
  ##############################
  playSound = function (start, end, chartType){
    currTime <- Sys.time()
    if(paused)
    {
      paused <<- FALSE
      resume(audioSound)
      if(chartType == "spectro")
      {
        shinyjs::show(id = "pauseButton",anim = TRUE)
        shinyjs::hide(id = "playButton",anim = FALSE)
      }
      else if(chartType == "spectroClipZoom")
      {
        shinyjs::show(id = "pauseButtonClipZoom",anim = TRUE)
        shinyjs::hide(id = "playButtonClipZoom",anim = FALSE)
      }
      else {
        shinyjs::show(id = "pauseButtonClip",anim = TRUE)
      }
    }
    else {
      path <- getPath(get_selected(input$tree, "names"))
      if(!is.null(newName)) {
        currDir <- paste0(depPath, "/", path, newName)
      }
      else {
        currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
      }
      wave <- readWave(currDir, from = start, to = end, unit = "seconds")
      sound <- audioSample(wave@stereo, wave@samp.rate, wave@bit)
      if(chartType == "spectro")
      {
        spectroEnd <- currTime + end
        shinyjs::show(id = "pauseButton",anim = TRUE)
        shinyjs::hide(id = "playButton",anim = FALSE)
      }
      else if(chartType == "spectroClipZoom")
      {
        shinyjs::show(id = "pauseButtonClipZoom",anim = TRUE)
        shinyjs::hide(id = "playButtonClipZoom",anim = FALSE)
      }
      else {
        shinyjs::show(id = "pauseButtonClip",anim = TRUE)
      }
      audioSound <<- audio::play(sound)
      audioSound
    }
  }
  
  pauseSound = function (chartType) {
    paused <<- TRUE
    pause(audioSound)
    if(chartType == "spectro")
    {
      shinyjs::hide(id = "pauseButton", anim = TRUE)
      shinyjs::show(id = "playButton", anim = FALSE)
    }
    else if (chartType == "spectroClipZoom") {
      shinyjs::hide(id = "pauseButtonClipZoom", anim = TRUE)
      shinyjs::show(id = "playButtonClipZoom", anim = FALSE)
    }
    else {
      shinyjs::hide(id = "pauseButtonClip", anim = TRUE)
      shinyjs::show(id = "playButtonClip", anim = FALSE)
    }
  }
  
  getPath = function(folderList) {
    one <- attr(folderList[[1L]], "ancestry", TRUE)
    path <- paste(one, collapse = "/")
    path <- paste0(path, "/")
    return(path)
  }
  
  # LOAD IN SPECIES DROPDOWN
  shinyFileChoose(input, 'csvFile', updateFreq=60000, session=session, roots=c(home='~'), restrictions=system.file(package='base'))
  shinyjs::onclick("csvFile",csvFileSubmitClick())
  
  csvFileSubmitClick = function(){
    csvSumbit <- TRUE
  }
  
  species <- reactive({
    if (is.null(input$csvFile)) {
      read.csv("www/species-short.csv", header = TRUE)
    } else {
      req(input$csvFile)
      infile <- parseFilePaths(roots=c(home='~'),input$csvFile)
      correctPath <- file.path(infile$datapath)
      print(correctPath)
      read.csv(correctPath, header = TRUE)
    }
  })
  
  output$speciesType <- renderUI({
    
    df <- species()
    
    if (is.null(df)) return(NULL)
    
    itemsSpecies <<- c('Select Type',as.character(df[[3]]))
    selectInput("typeDropdown", "Type*",itemsSpecies)
  })
  
  output$commonName <- renderUI({
    
    df <- species()
    
    
    if (is.null(df)) return(NULL)
    
    itemsType <<- c('Select Species',as.character(df[[1]]))
    selectInput("speciesDropdown", "Species*",itemsType)
  })
  
  formulaText <- reactive({
    paste(input$speciesDropdown)
  })
  #
  output$speciesName <- renderText({
    if (length(formulaText())==0) {
      ''
    } else if (formulaText()=='Select Species'){
      ''
    } else {
      formulaText()
    }
  })
  
  observeEvent(input$plot_brush$xmin, {
    if(input$plot_brush$xmax != 0)
    {
      shinyjs::hide("time-box-container", anim = TRUE)
      spectroClipMin <<- round(input$plot_brush$xmin, digits = 2)
      spectroClipMax <<- round(input$plot_brush$xmax, digits = 2)
      spectroClipYMin <<- input$plot_brush$ymin
      spectroClipYMax <<- input$plot_brush$ymax
      renderSpectroClip(NULL, spectroClipMin, spectroClipMax, spectroClipYMin, spectroClipYMax, FALSE)
    }
  });
  
  # Function to create the spectro clip after user has brushed the main plot
  renderSpectroClip = function(sound, xMinLocal, xMaxLocal, yMinLocal, yMaxLocal, readSequenceBool)
  {
    output$spectroClip <- renderPlot({
      path <- getPath(get_selected(input$tree, "names"))
      if(!is.null(newName)) {
        currDir <- paste0(depPath, "/", path, newName)
      }
      else {
        currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
      }
      sound <- readWave(currDir)
      
      if(!is.null(xMaxLocal)) {
        frequencyDF <- get_frequency(currDir, 0, xMaxLocal - xMinLocal, yMinLocal, yMaxLocal)
        # Calculating the regression line
        regressionLine <- lm(formula = frequencyDF$y ~ frequencyDF$x)
        lineSlope <<- regressionLine$coefficients[["frequencyDF$x"]]
        if(readSequenceBool)
        {
          spectro(sound, osc = FALSE, scale = FALSE, tlim = c(xMinLocal,xMaxLocal), flim = c(yMinLocal, yMaxLocal))
          abline(lm(formula = frequencyDF$y ~ frequencyDF$x), col = "red", lty = 1, lwd = 2)
          readSequenceBool <<- FALSE
          return()
        }
        spectro(sound, f = sound@samp.rate, osc = FALSE, scale = FALSE, tlim = c(floor(xMinLocal),ceiling(xMaxLocal)), flim = c(yMinLocal, yMaxLocal))
        abline(lm(formula = frequencyDF$y ~ frequencyDF$x), col = "red", lty = 1, lwd = 1)
        # Getting the duration of the clipped graph
        durationSmall <<- round(xMaxLocal - xMinLocal, digits = 1)
        shinyjs::show("clipInfo-container")
        shinyjs::show("playButtonClip",anim = FALSE)
        # Creating a temp wav sound from xMinLocal to xMaxLocal
        temp <- extractWave(sound, from = xMinLocal, to = xMaxLocal, xunit = "time")
        # Writing it to a .wav file
        writeWave(temp, paste0(getwd(), "/www/temp.wav"))
        # Creating an audio tag holding that temp.wav file to be played
        shinyjs::html(id = "playButtonClip", paste0(html = '<audio src="temp.wav" type="audio/wav" controls></audio>'))
      }
      xmin <<- xMinLocal
      xmax <<- xMaxLocal
      frequencyDF <- get_frequency(currDir, xMinLocal, xMaxLocal, yMinLocal, yMaxLocal);
      # Storing some variables used for calculating the abline
      ablineY <<- frequencyDF$y
      ablineX <<- frequencyDF$x
      # Storing the frequencies globaly
      maxFreq <<- round(max(frequencyDF$y, na.rm = T), digits = 2)
      minFreq <<- round(min(frequencyDF$y, na.rm = T), digits = 2)
      meanFreq <<- round(mean(frequencyDF$y, na.rm = T), digits = 2)
      bandwidth <<- round((maxFreq - minFreq), digits = 2)
      showSpeciesDropdown(xMinLocal, xMaxLocal, maxFreq, minFreq, meanFreq, bandwidth, lineSlope)
    })
  }
  
  get_frequency <- function(currDir, xMinLocal, xMaxLocal, yMinLocal, yMaxLocal) {
    freqSound <- readWave(currDir, from = xMinLocal, to = xMaxLocal, units = c("seconds"))
    filteredSound <- ffilter(freqSound, from = yMinLocal * 1000, to = yMaxLocal * 1000, output = "Wave", fftw = T)
    finalFreq <- dfreq(filteredSound, fftw = T, clip = 0.11, plot = F)
    df <- as.data.frame(finalFreq)
  }
  
  showSpeciesDropdown = function (xmin, xmax, maxFreq, minFreq, meanFreq, bandwidth, lineSlope){
    shinyjs::show("clip-species-dropdown")
    if(!is.null(xmax)) {
      shinyjs::html("timeMin", paste0("Start Time ",round(xmin, digits = 2)))
      shinyjs::html("timeMax", paste0("End Time ",round(xmax, digits = 2)))
      shinyjs::html("maxFreq", paste0("Max ",round(maxFreq, digits = 2)))
      shinyjs::html("minFreq", paste0("Min ",round(minFreq, digits = 2)))
      shinyjs::html("meanFreq", paste0("Mean ",round(meanFreq, digits = 2)))
      shinyjs::html("bandwidth", paste0("Bandwidth ",round(bandwidth, digits = 2)))
      shinyjs::html("slope", paste0("Slope ",round(lineSlope, digits = 4)))
    } else {
      shinyjs::html("timeMin", c("Start Time ",paste(0)))
      shinyjs::html("timeMax", c("End Time ",paste(0)))
      shinyjs::html("maxFreq", c("Max ",paste(0)))
      shinyjs::html("minFreq", c("Min ",paste(0)))
      shinyjs::html("meanFreq", c("Mean ",paste(0)))
      shinyjs::html("bandwidth", c("Bandwidth ",paste(0)))
      shinyjs::html("slope", c("Slope ",paste(0)))
    }
  }
  
  shinyjs::onclick("close-species-drop",shinyjs::hide("clip-species-dropdown"))
  shinyjs::hide("clip-species-dropdown")
  
  siteFields <- c("name", "lat", "lon", "recId", "siteNotes")
  # Getting user input data from the deployment info
  formDataSite <- reactive({
    data <- sapply(siteFields, function(x) input[[x]])
    data
  })
  
  # NOT CURRENTLY CALLED
  createCSVFilePath = function(){
    if(!is.null(newName)) {
      fileName <- sub(".wav", "", newName)
      return(fileName)
    } else {
      fileFullName <- unlist(get_selected(input$tree))
      fileName <- sub(".wav", "", fileFullName)
      return(fileName)
    }
  }
  
  ######################################
  ####### Observe event for the site information at the bottom right of the app
  ######################################
  observeEvent(input$deploymentInfo, {
    # Getting the site data
    data <- formDataSite()
    files <- list.files(depPath, all.files=F, recursive=T, include.dirs=T)
    sapply(files,FUN=function(eachPath){
      if (substrRight(eachPath,4) == ".wav") {
        if (!grepl(paste0("_",data[[4]],"_"),eachPath)){
          fileName <- paste0(depPath,"/",eachPath)
          removeProjectName <- gsub(paste0(projectName),"",eachPath)
          addProjectNameAndDeployment <- paste0(projectName,"_",data[[4]], removeProjectName)
          updatedWavFilePath <- paste0(depPath,"/",addProjectNameAndDeployment)
          file.rename(fileName,updatedWavFilePath)
        }
      }
    })
    create_directory_tree(depPath)
    load("www/dir_tree.Rdata")
    output$tree <- renderTree(tree, quoted = FALSE)
    observeEvent(unlist(get_selected(input$tree)), {
      shinyjs::hide("next-spectro-increment")
      shinyjs::hide("spectro-increment-container")
      # Plot main spectrogram
      shinyjs::show("loadingContainer1")
      if (is.null(unlist(get_selected(input$tree))))
      {
        return()
      }
      else {
        spectroFromTime <<- 0
        # Root path of the selected file
        path <- getPath(get_selected(input$tree, "names"))
        # Full file path
        currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
        fileType <- substrRight(currDir,4)
        if (fileType != ".wav") {
          return()
        } else {
          # Reading the sound file
          sound <- readWave(currDir)
          # Duration of the sound file
          durationMain <<- seewave::duration(sound)
          # Storing the start and end time of the wave file in seconds
          l <- length(sound)
          sr <- sound@samp.rate
          soundDuration <- round(l/sr,2)
          # TODO Maybe make a function out of this? Might make the code cleaner
          newSequenceBool <<- TRUE
          if (soundDuration > 59) {
            minuteDuration <- round(soundDuration/60)
            shinyjs::html("time-box-label", paste0("This file is ", minuteDuration, " minutes long. <br> Would you like to increment the display?"))
            shinyjs::show("time-box-container", anim = TRUE)
            
            # Listener for "Select a Sequence"
            observeEvent(input$spectroTimeSubmit, {
              # Getting the increment amount
              incrementAmount <<- as.numeric(input$spectroEndTime) * 60
              spectroToTime <<- incrementAmount
              if (file.exists(depFilePath)) {
                shinyjs::addClass("loadingContainer1", "loader")
                readSequenceCSV(unlist(get_selected(input$tree)))
                df <- species()
                itemsType <<- c('Select Species',as.character(df[[1]]))
                itemsSpecies <<- c('Select Type',as.character(df[[3]]))
                updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies)
                updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType)
              }
              file.copy(currDir, paste0(getwd(), "/www"))
              shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
              renderSpectro(sound)
              if (soundDuration > incrementAmount) {
                shinyjs::show("spectro-increment-container")
                shinyjs::show("next-spectro-increment")
              }
              shinyjs::show("playButton",anim = FALSE)
            })
            observeEvent(input$noTimeSubmission,{
              if (file.exists(depFilePath)) {
                sound <- readWave(paste0(depPath, "/", unlist(get_selected(input$tree))))
                soundLength <- seewave::duration(sound)
                spectroToTime <<- soundLength
                shinyjs::addClass("loadingContainer1", "loader")
                df <- species()
                itemsType <<- c('Select Species',as.character(df[[1]]))
                itemsSpecies <<- c('Select Type',as.character(df[[3]]))
                updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies)
                updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType)
                readSequenceCSV(unlist(get_selected(input$tree)))  
              }
              shinyjs::hide("spectro-increment-container")
              shinyjs::hide("next-spectro-increment")
              spectroToTime <<- soundDuration
              renderSpectro(sound)
              shinyjs::show("playButton",anim = FALSE)
              file.copy(currDir, paste0(getwd(), "/www"))
              shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
              shinyjs::removeClass("loadingContainer1", "loader")
            })
          } 
          else {
            spectroToTime <<- soundDuration
            renderSpectro(sound)
            shinyjs::show("playButton",anim = FALSE)
          }
          shinyjs::show("content-id")
          if(!is.null(newName)) {
            shinyjs::html("titleHeader",newName)
          }
          else {
            shinyjs::html("titleHeader",unlist(get_selected(input$tree)))
          }
        }
      }
    }, autoDestroy = FALSE)
    if (!is.null(input$tree)){
      # Sys.sleep(2)
      js$fixTree()
    }
    if (data[[1]] != input$deployment[[1]][2][[1]]) {
      newDepPath <- gsub(input$deployment[[1]][2][[1]],data[[1]],depPath)
      file.rename(depPath,newDepPath)
      depFilePath <<- gsub(input$deployment[[1]][2][[1]],data[[1]],depFilePath)
      # paste0(depPath,"/", depFileName, ".csv")
      depPath <<- newDepPath
    }
    # Getting file list
    files <- list.files(depPath, all.files=F, recursive=T, include.dirs=T)
    if (data[[1]] == "") {
      shinyjs::show("dep-name-warning")
    } else if (data[[4]] == "") {
      shinyjs::show("dep-name-warning")
    } else if (data[[2]] == "") {
      shinyjs::show("dep-name-warning")
    } else if (data[[3]] == "") {
      shinyjs::show("dep-name-warning")
    } else {
      shinyjs::hide("dep-name-warning")
      shinyjs::hide("recid-name-warning")
      data <- c(data, as.character(minTimeVar))
      # Adding the min and max time variables to the data
      names(data)[6] <- "start_time_date"
      data <- c(data, as.character(maxTimeVar))
      names(data)[7] <- "end_time_date"
      # Link to the location of the LAT/LON entered by the user, saved into the CSV
      googleMapsLink <- paste0("https://www.google.com/maps/@", data[[2]], ",", data[[3]], ",13z")
      dataArray <- c(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]],data[[6]],data[[7]], googleMapsLink)
      dataMatrix <- matrix(dataArray,ncol = length(dataArray), byrow = TRUE)
      colnames(dataMatrix) <- c("Name", "Lat", "Lon", "Record ID", "Site Notes", "Start", "End", "Google Maps")
      siteDataTable <- as.table(dataMatrix)
      deploymentCSVDataTable <<- siteDataTable
      # If we have multiple clips on a given spectro, give a new column name to each clip
      clipCount <<- 0
      # Reformating user input
      fileDate <- gsub(" ", "-",data[[6]], fixed = TRUE)
      fileDate <- gsub(":", "-",fileDate, fixed = TRUE)
      # Creating a new filename out of the metadata
      newFileName <<- paste0(projectName,"_",data[[1]])
      # Checking for file duplicate
      fileNameDuplicate <- 0
      # Checking for file duplicates within that folder
      for (i in 1:length(files)) {
        if (files[i] == paste0(newFileName,".csv")) {
          fileNameDuplicate <- as.numeric(fileNameDuplicate) + 1
        }
      }
      # Checking for file duplication, alert if any; otherwise create the file
      if (fileNameDuplicate == 0 || !autoDepCSVLoad) { # VERIFIY
        writeDeploymentCSV(siteDataTable)
      }
      else if(!autoDepCSVLoad) {
        shinyjs::show("file-name-warning-container")
      }
    }
  })
  
  projectFields <- c("projectName", "projectNotes")
  
  formDataProject <- reactive({
    data <- sapply(projectFields, function(x) input[[x]])
    data
  })
  
  #####################################
  ###### Project info submit button listener
  #####################################
  observeEvent(input$projectInfo, {
    data <- formDataProject()
    if (data[[1]] == "") {
      shinyjs::show("proj-name-warning")
    } else {
      shinyjs::hide("proj-name-warning")
      projectInfo(data[[1]], data[[2]])
      shinyjs::hide("directory", anim = TRUE)
      shinyjs::addClass("projectInfo", "active-button")
      shinyjs::hide("project-info-container", anim = TRUE)
      shinyjs::addClass("enter-project-info-label", "completed-step")
      shinyjs::toggleClass("enter-project-info-label", "open-accordian")
      shinyjs::toggleClass("enter-project-info-label", "closed-accordian")
      shinyjs::show("deployment", anim = TRUE)
      shinyjs::addClass("select-dep-container", "open-accordian")
    }
  })
  
  projectInfo <- function(projectNameArg, projectNotesArg) {
    data <- formDataProject()
    dataArray <- c(projectNameArg,projectNotesArg)
    dataMatrix <- matrix(dataArray,ncol = 2, byrow = TRUE)
    colnames(dataMatrix) <- c("Project Name", "Project Notes")
    projectData <<- as.table(dataMatrix)
    oldProjectName <- projectName
    projectName <<- projectNameArg
    newDirPath <- gsub(oldProjectName,projectName,dirPath)
    file.rename(dirPath,newDirPath)
    dirPath <<- newDirPath
    write.csv(projectData, paste0(dirPath,"/",paste0('Project_', projectName,'.csv')), row.names = FALSE)
    if (length(annotationListCsv) != 0) {
      annotationListCsvProject <<- c(annotationListCsv, normalizePath(paste0(dirPath,"/",paste0('Project_', projectName,'.csv'))))
    }
  }
  
  speciesFields <- c("speciesDropdown", "typeDropdown", "annotNotes")
  
  formDataSpecies <- reactive({
    data <- sapply(speciesFields, function(x) input[[x]])
    data
  })
  
  # Adding the clip metadata to the spectrogram metadata
  observeEvent(input$speciesDropSubmit, {
    dropSubmitClicked <<- TRUE
    fileFullName <- unlist(get_selected(input$tree))
    if (is.null(deploymentCSVDataTable) ) {
      if(!autoDepCSVLoad) {
        shinyjs::show("site-info-warning-container") 
      }
    }
    else {
      shinyjs::enable("aws-upload-button")
      dataSet <- formDataSpecies()
      shinyjs::hide("site-info-warning-container")
      if (dataSet[[1]] == "Select Species") {
        shinyjs::show("type-name-warning")
      } 
      else if (dataSet[[2]] == "Select Type") {
        shinyjs::show("type-name-warning")
      } 
      else {
        if (clipCount == 0) {
          dataArray <- c(fileFullName, clipCount, xmin, xmax, durationSmall, dataSet[[2]], dataSet[[1]], maxFreq, minFreq, meanFreq, bandwidth, lineSlope, dataSet[[3]], spectroClipYMin, spectroClipYMax)
          dataMatrix <- matrix(dataArray,ncol = 15, byrow = TRUE)
          colnames(dataMatrix) <- c("File Name", "Annotation#","Time Min (s)", "Time Max (s)", "Duration", "Type", "Species", "Max Freq", "Min Freq", "Mean Freq", "Bandwidth", "Annotation Slope", "Annotation Notes", "yMin", "yMax")
          dataTable <- as.table(dataMatrix)
          deploymentCSVDataTable <<- cbind(deploymentCSVDataTable, dataTable)
        } 
        else {
          dataArray <- c(deploymentCSVDataTable[1,1], deploymentCSVDataTable[1,2], deploymentCSVDataTable[1,3], deploymentCSVDataTable[1,4], deploymentCSVDataTable[1,5], deploymentCSVDataTable[1,6], deploymentCSVDataTable[1,7], deploymentCSVDataTable[1,8], fileFullName, clipCount, spectroClipMin, spectroClipMax, durationSmall, dataSet[[2]], dataSet[[1]], maxFreq, minFreq, meanFreq, bandwidth, lineSlope, dataSet[[3]], spectroClipYMin, spectroClipYMax)
          deploymentCSVDataTable <<- rbind(deploymentCSVDataTable, dataArray)
        }
        if(newSequenceBool)
        {
          # increaseStatusBar()
          newSequenceBool <<- FALSE
        }
        clipCount <<- clipCount + 1
        # Creating the path with the file name
        filePathFull <- paste0(depPath,"/",fileFullName)
        # Adding the file to the list of annotated files for later zipping and S3 upload
        annotationListWav <<- c(annotationListWav, normalizePath(filePathFull))
        # If the deployment csv already exists, add metadata to the existing csv
        if (autoDepCSVLoad) {
          write.csv(deploymentCSVDataTable,depFilePath, row.names = FALSE)
          annotationListCsv <<- normalizePath(depFilePath)
        } else {
          write.csv(deploymentCSVDataTable, normalizePath(paste0(depPath,"/",paste0(newFileName,'.csv'))), row.names = FALSE)
          annotationListCsv <<- normalizePath(paste0(depPath,"/",paste0(newFileName,'.csv')))
        }
        shinyjs::addClass('completedDepContainer', "open-accordian")
        shinyjs::removeClass('completedDepContainer', "closed-accordian")
        shinyjs::show("annotationDrop")
        
        annotationList <- c(paste0(dataSet[[1]], " at " , xmin))
        annotationListDrop <<- c(annotationListDrop, annotationList)
        
        # Updating the global annotations list
        currAnnListGlobal <<- c(currAnnListGlobal, annotationList)
        updateSelectizeInput(session, "annotationDrop", label = "Select an annotation", choices =  currAnnListGlobal, selected = tail(currAnnListGlobal, 1))
        
        if(progressBarExists) {
          progressBarExists <<- TRUE
          # Create some REACTIVE VALUES
          awsProgressValue <<- reactiveValues()
          awsProgressValue$one <<- 0
          # Creating the progress bar for AWS upload
          output$awsProgress <- renderUI({
            progressGroup(text = "Status",  value = awsProgressValue$one,   min = 0, max = 3, color = "green")
          })
        } else {
          updateProgressBar(getProgressBarValue() + 1)
        }
      }
    }
  })
  
  observeEvent(input$annotationDrop, {
    # Checking that we actually have an element in the dropdown
    if(input$annotationDrop != "")
    {        
      tryCatch({
        annData <- read.csv(depFilePath)[ ,10:23]
      }, error=function(e) {
        print("Error with the CSV file. Error #1")
      })
      annCount <<- length(annData[[1]])
      sound <- readWave(paste0(depPath, "/", unlist(get_selected(input$tree))))
      currentSelectedMin <- trimws(head(strsplit(input$annotationDrop, split=" at ")[[1]],2)[2], which = "both")
      minLast <- tail(annData[[2]], 1)
      # If current selection is last element in dropdown
      if(str_detect(input$annotationDrop, as.character(tail(annData[[6]], 1))) && as.character(currentSelectedMin) == as.character(minLast))
      {
        # Getting the last sequence from the deployment metadata
        minLast <- tail(annData[[2]], 1)
        maxLast <- tail(annData[[3]], 1)
        maxFreqLast <- tail(annData[[7]], 1)
        minFreqLast <- tail(annData[[8]], 1)
        meanFreqLast <- tail(annData[[9]], 1)
        bandwidthLast <- tail(annData[[10]], 1)
        slopeLast <- tail(annData[[11]], 1)
        annLast <- tail(annData[[12]], 1)
        csvYMin <- tail(annData[[13]], 1)
        csvYMax <- tail(annData[[14]], 1)
        
        shinyjs::html("timeMin", paste0("Start Time ",round(minLast, digits = 2)))
        shinyjs::html("timeMax", paste0("End Time ",round(maxLast, digits = 2)))
        shinyjs::html("maxFreq", paste0("Max ",maxFreqLast))
        shinyjs::html("minFreq", paste0("Min ",minFreqLast))
        shinyjs::html("meanFreq", paste0("Mean ",meanFreqLast))
        shinyjs::html("bandwidth", paste0("Bandwidth ",bandwidthLast))
        shinyjs::html("slope", paste0("Slope ",slopeLast))
        if (!dropSubmitClicked) {
          alreadyAnnotated <<- TRUE
        }
        updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies, selected = as.character(tail(annData[[5]], 1)))
        filteredSpecies <- filterSpecies(as.character(tail(annData[[5]], 1)), annCount)
        updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  as.character(filteredSpecies$CommonName), selected = as.character(tail(annData[[6]], 1)))
        updateTextAreaInput(session, 'annotNotes', value = annLast)

        # Creating a temp wav sound from xmin to xmax
        temp <- extractWave(sound, from = minLast, to = maxLast, xunit = "time")
        writeWave(temp, paste0(getwd(), "/www/temp.wav"))
        renderSpectroClip(sound, minLast, maxLast, csvYMin, csvYMax, TRUE)
        
        # Creating an audio tag holding that temp.wav file to be played
        shinyjs::show("playButtonClip",anim = FALSE)
        shinyjs::html(id = "playButtonClip", paste0(html = '<audio src="temp.wav" type="audio/wav" controls></audio>'))
        shinyjs::removeClass("loadingContainer1", "loader")
      } else {
        currentSelectedMin <- trimws(head(strsplit(input$annotationDrop,split=" at ")[[1]],2)[2], which = "both")
        currentSelectedSpecies <- trimws(head(strsplit(input$annotationDrop,split=" at ")[[1]],2)[1], which = "both")
        df <- as.data.frame(annData)
        selectedAnn <- df[which(df$Time.Min..s. == currentSelectedMin & df$Species == currentSelectedSpecies), ]
        
        minCurr <- selectedAnn$Time.Min..s.
        maxCurr <- selectedAnn$Time.Max..s.
        maxFreqCurr <- selectedAnn$Max.Freq
        minFreqCurr <- selectedAnn$Min
        meanFreqCurr <- selectedAnn$Mean.Freq
        bandwidthCurr <- selectedAnn$Bandwidth
        lineSlopeCurr <- selectedAnn$Annotation.Slope
        typeCurr <- selectedAnn$Type
        speciesCurr <<- selectedAnn$Species
        annNotes <- selectedAnn$Annotation.Notes
        csvYMin <- selectedAnn$yMin
        csvYMax <- selectedAnn$yMax
        renderSpectroClip(sound, minCurr, maxCurr, csvYMin, csvYMax, TRUE)
        
        shinyjs::html("timeMin", paste0("Start Time ",round(minCurr, digits = 2)))
        shinyjs::html("timeMax", paste0("End Time ",round(maxCurr, digits = 2)))
        shinyjs::html("maxFreq", paste0("Max ", maxFreqCurr))
        shinyjs::html("minFreq", paste0("Min ", minFreqCurr))
        shinyjs::html("meanFreq", paste0("Mean ", meanFreqCurr))
        shinyjs::html("bandwidth", paste0("Bandwidth ", bandwidthCurr))
        shinyjs::html("slope", paste0("Slope ", lineSlopeCurr))
        alreadyAnnotated <<- TRUE
        updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies, selected = as.character(typeCurr))
        filteredSpecies <- filterSpecies(as.character(typeCurr), annCount)
        updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices = as.character(filteredSpecies$CommonName), selected = as.character(currentSelectedSpecies))
        updateTextAreaInput(session, 'annotNotes', value = annNotes)
        
        # Creating a temp wav sound from xmin to xmax
        temp <- extractWave(sound, from = minCurr, to = maxCurr, xunit = "time")
        # Writing it to a .wav file
        writeWave(temp, paste0(getwd(), "/www/temp.wav"))
        # Creating an audio tag holding that temp.wav file to be played
        shinyjs::show("playButtonClip",anim = FALSE)
        shinyjs::html(id = "playButtonClip", paste0(html = '<audio src="temp.wav" type="audio/wav" controls></audio>'))
      }
      
    }
  })
  
  # Filter species in dropdown depending on the species' family
  filterSpecies = function(typeCurr, count) {
    if(is.null(typeCurr) || length(typeCurr) == 0 || typeCurr != "Select Type")
    {
      speciesList <- species()
      speciesDF <- as.data.frame(speciesList)
      filteredSpecies <- speciesDF[which(speciesDF$Type == typeCurr), ] 
    }
  }
  
  # Listener for animal Type selection dropdown
  # On change, it refreshes the species list to the its type
  observeEvent(input$typeDropdown, {
    if(alreadyAnnotated || alreadyAnnotatedCount < 2) {
      skipDropdownRefresh <<- TRUE
      if (nrow(deploymentCSVDataTable) > 0) {
        for (i in 1:nrow(deploymentCSVDataTable)) {
          if (ncol(deploymentCSVDataTable) > 8 && deploymentCSVDataTable[i,9] == unlist(get_selected(input$tree))){
            skipDropdownRefresh <<- FALSE
          }
        }
      }
      
      if (!alreadyAnnotated && alreadyAnnotatedCount == 1 && skipDropdownRefresh) {
        filteredSpecies <- filterSpecies(input$typeDropdown, annCount)
        currentSelectedSpecies <- trimws(head(strsplit(input$annotationDrop,split = " at ")[[1]],2)[1], which = "both")
        updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  filteredSpecies$CommonName)
      }
      alreadyAnnotated <<- FALSE
      alreadyAnnotatedCount <<- alreadyAnnotatedCount + 1
      dropSubmitClicked <<- FALSE
    } else if (!alreadyAnnotated || alreadyAnnotatedCount >= 2){
      alreadyAnnotated <<- FALSE
      alreadyAnnotatedCount <<- alreadyAnnotatedCount + 1
      filteredSpecies <- filterSpecies(input$typeDropdown, annCount)
      currentSelectedSpecies <- trimws(head(strsplit(input$annotationDrop,split = " at ")[[1]],2)[1], which = "both")
      updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  filteredSpecies$CommonName)
      dropSubmitClicked <<- FALSE
    }
  })
  
  ################################
  ######## Modal UI for AWS
  ################################
  observeEvent(input$awsUploadModal, {
    awsAccessKey <- input$awsAccessKey
    awsSecretKey <- input$awsSecretKey
    awsBucket <- input$awsBucket
    if(awsBucket == "" || awsSecretKey == "" || awsAccessKey == "" ) {
      shinyjs::show("awsEmptyFieldsContainer")
    }
    else {
      shinyjs::hide("awsEmptyFieldsContainer")
      Sys.setenv("AWS_ACCESS_KEY_ID" = input$awsAccessKey, "AWS_SECRET_ACCESS_KEY" = input$awsSecretKey)
      if(bucket_exists(awsBucket)) {
        # Copy files to temp folder for zipping
        tempDir <- tempdir()
        csvDir <- paste0(tempDir, "/", projectName)
        wavDir <- paste0(csvDir, "/deployment")
        # If directory already exists, remove any previous files
        if(dir.exists(csvDir)) {
          unlink(csvDir, recursive = TRUE)
        }
        data <- formDataProject()
        projectInfo(data[[1]], data[[2]])
        dir.create(csvDir)
        dir.create(wavDir)
        file.copy(annotationListCsv, wavDir)
        file.copy(annotationListCsvProject[length(annotationListCsvProject)], csvDir)
        file.copy(annotationListWav, wavDir)
        incrementAwsCount()
        # Zip folder
        oldwd <- getwd()
        setwd(tempDir)
        # Getting the site data
        data <- formDataSite()
        # Reformating user input
        fileDate <- gsub(" ", "-",as.character(minTimeVar), fixed = TRUE)
        fileDate <- gsub(":", "-",fileDate, fixed = TRUE)
        # Creating a new filename out of the metadata
        newFileName <<- paste0(projectName,"_",data[[1]])
        zipName <- sub('_([^_]*)$', '', newFileName)
        currDate <- format(Sys.time(), "%Y%m%d")
        fullZipName <- paste0("/", zipName, "_", currDate)
        zip(normalizePath(paste0(dirPath, fullZipName, ".zip")), paste0(projectName, "/"))
        incrementAwsCount()
        setwd(oldwd)
        # Upload to AWS
        awsUpload <- put_object(file = normalizePath(paste0(dirPath, fullZipName, ".zip")), bucket = awsBucket)
        incrementAwsCount()
        # Resetting annotationListWav to 0
        annotationListWav <<- vector();
        annotationListCsv <<- vector();
      } else {
        output$warningBucket <- renderUI({
          tagList(
            HTML("Error: The bucket that you have entered does not exist. Please select another bucket or create one.")
          )
        })
      }
    }
  })
  
  awsUpload <- function() {
    observeEvent(awsUpload, {
      if(awsUpload[1] == TRUE)
      {
        incrementAwsCount();
      }
    })
  }
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
  
  findFileCount = function() {
    projectFileCount <- 0
    files <- list.files(depPath, all.files=F, recursive=T, include.dirs=T)
    for (i in 1:length(files)) {
      if (substrRight(files[i],4) == ".wav") {
        projectFileCount <- projectFileCount +1
        projectFileCountGlobal <<- projectFileCountGlobal + 1
      }
    }
    
    # Render UI output
    output$progressOne <- renderUI({
      progressGroup(text = "Status", value = progressValue$one, min = 0, max = projectFileCount, color = "aqua")
    })
  }
  
  #################################
  ###### Function to increase the count of the status bar
  #################################
  increaseStatusBar = function () {
    progressValue$one <<- progressValue$one + 1
    if (progressValue$one == projectFileCountGlobal) {
      shinyjs::hide("tree", anim = TRUE)
      shinyjs::addClass("show-tree", "closed-accordian")
      shinyjs::addClass("show-tree", "completed-step")
      shinyjs::removeClass("show-tree", "open-accordian")
    } else {
      
    }
  }
  
  resetStatusBarCount <- function () {
    progressValue$one <<- 0
  }
  
  incrementAwsCount = function () {
    awsProgressValue$one <<- awsProgressValue$one + 1
  }
  
  resetAwsCount = function()
  {
    awsProgressValue$one <<- 0
  }
  
  
  ###############################
  #### If there is a CSV with the same name as the project folder
  #### Read it and fill in the input values in "Enter Project Info"
  ##############################
  readProjectCSV = function(projectDir, projectName) {
    if(!is.null(projectName))
    {
      projectCSV <- read.csv(paste0(projectDir, "/Project_", projectName, ".csv"))
      updateTextInput(session, inputId = "projectName", label = NULL, value = projectCSV$Project.Name[[1]])
      shinyjs::html("projectNotes", projectCSV$Project.Notes[[1]])
      toggleAfterProjectCsvLoaded()
    }
  }
  
  readDeploymentCSV <- function(depPath, depFilePath) {
    if(!is.null(depPath))
    {
      shinyjs::hide("species-sidebox-container", anim = TRUE)
      shinyjs::toggleClass("select-dep-container", "open-accordian")
      shinyjs::toggleClass("select-dep-container", "closed-accordian")
      resetStatusBarCount()
      csvFileName <<- gsub("^.*\\/", "", depPath)
      deploymentCSV <- read.csv(depFilePath)
      createDataVarFromCSV(deploymentCSV)
      updateTextInput(session, inputId = "name", label = NULL, value = deploymentCSV$Name[[1]])
      updateTextInput(session, inputId = "lat", label = NULL, value = as.character(deploymentCSV$Lat[[1]]))
      updateTextInput(session, inputId = "lon", label = NULL, value = as.character(deploymentCSV$Lon[[1]]))
      updateTextInput(session, inputId = "recId", label = NULL, value = as.character(deploymentCSV$Record.ID[[1]]))
      shinyjs::html("siteNotes", deploymentCSV$Site.Notes[[1]])
      countAnnDone <- getStatusBarCount()
      updateProgressBar(countAnnDone, TRUE)
      toggleAfterDeploymentCsvLoaded()
      autoDepCSVLoad <<- TRUE
    }
  }
  
  # Update the progress bar with the passed argument (should be a number)
  updateProgressBar <- function(countAnnDone, deploymentMeta = FALSE) {
    # Only update the progress bar if we have an empty sequence
    # The progress bar should only update once for each sequence.
    if(length(input$annotationDrop) == 0 || deploymentMeta || input$annotationDrop == "") {
      progressValue$one <- as.numeric(countAnnDone)
    } 
  }
  
  # Returns the current progress bar status/value
  getProgressBarValue <- function() {
    progressValue$one
  }
  
  createDataVarFromCSV = function (deploymentCSV) {
    csvLength <- length(deploymentCSV$Name)
    if (csvLength > 0) {
      for(i in 1:csvLength) {
        if (length(as.character(deploymentCSV$Annotation.[[1]])) > 0) {
          dataArray <- c(as.character(deploymentCSV$Name[[i]]),as.character(deploymentCSV$Lat[[i]]),as.character(deploymentCSV$Lon[[i]]),as.character(deploymentCSV$Record.ID[[i]]), as.character(deploymentCSV$Site.Notes[[i]]), as.character(deploymentCSV$Start[[i]]),as.character(deploymentCSV$End[[i]]),as.character(deploymentCSV$Google.Maps[[i]]),as.character(deploymentCSV$File.Name[[i]]),as.character(deploymentCSV$Annotation.[[i]]),as.character(deploymentCSV$Time.Min..s.[[i]]),as.character(deploymentCSV$Time.Max..s.[[i]]),as.character(deploymentCSV$Duration[[i]]),as.character(deploymentCSV$Type[[i]]),as.character(deploymentCSV$Species[[i]]),as.character(deploymentCSV$Max.Freq[[i]]),as.character(deploymentCSV$Min.Freq[[i]]),as.character(deploymentCSV$Mean.Freq[[i]]),as.character(deploymentCSV$Bandwidth[[i]]), as.character(deploymentCSV$Annotation.Slope[[i]]), as.character(deploymentCSV$Annotation.Notes[[i]]), as.character(deploymentCSV$yMin[[i]]), as.character(deploymentCSV$yMax[[i]]))
          dataMatrix <- matrix(dataArray,ncol = 23, byrow = TRUE)
          colnames(dataMatrix) <- c("Name", "Lat", "Lon", "Record ID", "Site Notes", "Start", "End", "Google Maps", "File Name", "Annotation#","Time Min (s)", "Time Max (s)", "Duration", "Type", "Species", "Max Freq", "Min Freq", "Mean Freq", "Bandwidth", "Annotation Slope","Annotation Notes", "yMin", "yMax")
          dataTable <- as.table(dataMatrix)
          if(is.null(deploymentCSVDataTable)) {
            deploymentCSVDataTable <<- rbind(dataTable)
          } else {
            deploymentCSVDataTable <<- rbind(deploymentCSVDataTable, dataArray)
          }
          clipCount <<- clipCount + 1
        } else {
          dataArray <- c(as.character(deploymentCSV$Name[[i]]),as.character(deploymentCSV$Lat[[i]]),as.character(deploymentCSV$Lon[[i]]),as.character(deploymentCSV$Record.ID[[i]]), as.character(deploymentCSV$Site.Notes[[i]]), as.character(deploymentCSV$Start[[i]]),as.character(deploymentCSV$End[[i]]),as.character(deploymentCSV$Google.Maps[[i]]))
          dataMatrix <- matrix(dataArray,ncol = 8, byrow = TRUE)
          colnames(dataMatrix) <- c("Name", "Lat", "Lon", "Record ID", "Site Notes", "Start", "End", "Google Maps")
          dataTable <- as.table(dataMatrix)
          if(is.null(deploymentCSVDataTable)) {
            deploymentCSVDataTable <<- rbind(dataTable)
          } else {
            deploymentCSVDataTable <<- rbind(deploymentCSVDataTable, dataArray)
          }
        }
      }
    }
    create_directory_tree(depPath)
    load("www/dir_tree.Rdata")
    output$tree <- renderTree(tree, quoted = FALSE)
    observeEvent(unlist(get_selected(input$tree)), {
      # Plot main spectrogram
      shinyjs::show("loadingContainer1")
      if (is.null(unlist(get_selected(input$tree))))
      {
        return()
      }
      else {
        spectroFromTime <<- 0
        # Root path of the selected file
        path <- getPath(get_selected(input$tree, "names"))
        # Full file path
        currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
        fileType <- substrRight(currDir,4)
        if (fileType != ".wav") {
          return()
        } else {
          # Reading the sound file
          sound <- readWave(currDir)
          # Duration of the sound file
          durationMain <<- seewave::duration(sound)
          # Storing the start and end time of the wave file in seconds
          l <- length(sound)
          sr <- sound@samp.rate
          soundDuration <- round(l/sr,2)
          # TODO Maybe make a function out of this? Might make the code cleaner
          newSequenceBool <<- TRUE
          if (soundDuration > 59) {
            minuteDuration <- round(soundDuration/60)
            shinyjs::html("time-box-label", paste0("This file is ", minuteDuration, " minutes long. <br> Would you like to increment the display?"))
            shinyjs::show("time-box-container", anim = TRUE)

            # Listener for "Select a Sequence"
            observeEvent(input$spectroTimeSubmit, {
              # Getting the increment amount
              incrementAmount <<- as.numeric(input$spectroEndTime) * 60
              spectroToTime <<- incrementAmount
              if (file.exists(depFilePath)) {
                shinyjs::addClass("loadingContainer1", "loader")
                readSequenceCSV(unlist(get_selected(input$tree)))
                df <- species()
                itemsType <<- c('Select Species',as.character(df[[1]]))
                itemsSpecies <<- c('Select Type',as.character(df[[3]]))
                updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies)
                updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType)
              }
              file.copy(currDir, paste0(getwd(), "/www"))
              shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
              renderSpectro(sound)
              if (soundDuration > incrementAmount) {
                shinyjs::show("spectro-increment-container")
                shinyjs::show("next-spectro-increment")
              }
              shinyjs::show("playButton",anim = FALSE)
            })
            observeEvent(input$noTimeSubmission,{
              shinyjs::hide("spectro-increment-container")
              shinyjs::hide("next-spectro-increment")
              if (file.exists(depFilePath)) {
                sound <- readWave(paste0(depPath, "/", unlist(get_selected(input$tree))))
                soundLength <- seewave::duration(sound)
                spectroToTime <<- soundLength
                shinyjs::addClass("loadingContainer1", "loader")
                df <- species()
                itemsType <<- c('Select Species',as.character(df[[1]]))
                itemsSpecies <<- c('Select Type',as.character(df[[3]]))
                updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies)
                updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType)
                readSequenceCSV(unlist(get_selected(input$tree)))
              }
              spectroToTime <<- soundDuration
              renderSpectro(sound)
              shinyjs::show("playButton",anim = FALSE)
              file.copy(currDir, paste0(getwd(), "/www"))
              shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
              shinyjs::removeClass("loadingContainer1", "loader")
            })
          }
          else {
            spectroToTime <<- soundDuration
            renderSpectro(sound)
            shinyjs::show("playButton",anim = FALSE)
          }
          shinyjs::show("content-id")
          if(!is.null(newName)) {
            shinyjs::html("titleHeader",newName)
          }
          else {
            shinyjs::html("titleHeader",unlist(get_selected(input$tree)))
          }
        }
      }
    }, autoDestroy = FALSE)
    if (!is.null(input$tree)){
      # Sys.sleep(2)
      js$fixTree()
    }
  #   load("www/dir_tree.Rdata")
  #   output$tree <- renderTree(tree, quoted = FALSE)
  # }
    # load("www/dir_tree.Rdata")
    # output$tree <- renderTree(tree, quoted = FALSE)
    # observeEvent(input$tree[1], {
    #   print('stop me')
    # }, autoDestroy = FALSE)
    # if (!is.null(input$tree)){
    #   # Sys.sleep(2)
    #   js$fixTree()
    # }
  }
  
  writeDeploymentCSV <- function(siteDataTable) {
    shinyjs::hide("file-name-warning-container")
    
    count <- 0
    if(!autoDepCSVLoad)
    {
      write.csv(siteDataTable, paste0(depPath,"/",paste0(newFileName,'.csv')), row.names = FALSE)
    }
    
    if(!is.null(newName)) {
      shinyjs::html("titleHeader",newName)
    }
    else {
      shinyjs::html("titleHeader",unlist(get_selected(input$tree)))
    }
    shinyjs::addClass("deploymentInfo", "active-button")
    shinyjs::hide("species-sidebox-container")
    shinyjs::addClass("right-column-title", "completed-step")
    shinyjs::toggleClass("right-column-title", "open-accordian")
    shinyjs::toggleClass("right-column-title", "closed-accordian")
  }
  
  readSequenceCSV <- function(wavFileName)
  { 
    shinyjs::enable("aws-upload-button")
    toggleCompletedDeployment()
    annDataFull <- read.csv(depFilePath)
    # Check if file is empty
    if("File.Name" %in% colnames(annDataFull)){
      tryCatch({
        annData <- annDataFull[ ,9:23]
      }, error=function(e) {
        print("Error with the CSV file. Error #1")
      })
      # currentSelectedMin <- trimws(head(strsplit(input$annotationDrop,split=" at ")[[1]],2)[2], which = "both")
      # currentSelectedSpecies <- trimws(head(strsplit(input$annotationDrop,split=" at ")[[1]],2)[1], which = "both")
      df <- as.data.frame(annData)
      # Filtering by currently selected sequence
      selectedWav <- df[which(df$File.Name == wavFileName), ]
      # Filtering the data frame to get only the data that is within the plot's view
      selectedWav <- selectedWav[which(selectedWav$Time.Min..s. >= spectroFromTime & selectedWav$Time.Max..s. <= spectroToTime), ]
      # Ordering the data frame
      selectedWav <- selectedWav[order(selectedWav$Time.Min..s.), ]
      # If there are no annotations for that sequence
      if(length(selectedWav$Annotation.) == 0)
      {
        # No annotations, resetting currAnnList
        currAnnList <- list()
        currAnnListGlobal <<- currAnnList
        updateSelectizeInput(session, "annotationDrop", label = "Select an annotation", choices =  currAnnList)
        return()
      }
      currAnnSize <- length(selectedWav$Annotation.)
      clipCount <<- length(selectedWav$Annotation.)
      currAnnList <- list()
      for(i in 1:currAnnSize) {
        currSpeciesList <- selectedWav$Species[i]
        currMinList <- selectedWav$Time.Min..s.[i]
        currList <- paste0(currSpeciesList, " at ", currMinList)
        currAnnList <- c(currAnnList, currList)
      }
      currAnnListGlobal <<- currAnnList
      minLast <- tail(selectedWav[[3]], 1)
      maxLast <- tail(selectedWav[[4]], 1)
      maxFreqLast <- tail(selectedWav[[8]], 1)
      minFreqLast <- tail(selectedWav[[9]], 1)
      meanFreqLast <- tail(selectedWav[[10]], 1)
      bandwidthLast <- tail(selectedWav[[11]], 1)
      slopeLast <- tail(selectedWav[[12]], 1)
      csvYMin <- tail(selectedWav[[14]], 1)
      csvYMax <- tail(selectedWav[[15]], 1)
      
      typeLast <- tail(selectedWav[[5]], 1)
      speciesLast <- tail(selectedWav[[6]], 1)
      sound <- readWave(paste0(depPath, "/", wavFileName))
      readSequenceBool <- TRUE
      renderSpectroClip(sound, minLast, maxLast, csvYMin, csvYMax, readSequenceBool)
      # Creating a temp wav sound from xmin to xmax
      temp <- extractWave(sound, from = minLast, to = maxLast, xunit = "time")
      # Writing it to a .wav file
      writeWave(temp, paste0(getwd(), "/www/temp.wav"))
      # Creating an audio tag holding that temp.wav file to be played
      shinyjs::show("playButtonClip",anim = FALSE)
      shinyjs::html(id = "playButtonClip", paste0(html = '<audio src="temp.wav" type="audio/wav" controls></audio>'))
      df <- species()
      if (is.null(df)) return(NULL)
      itemsType <<- c('Select Species',as.character(df[[1]]))
      itemsSpecies <<- c('Select Type',as.character(df[[3]]))
      
      shinyjs::show(id = "spectroClip", anim = FALSE)
      shinyjs::show(id = "clipInfo-container", anim = FALSE)
      updateSelectizeInput(session, "annotationDrop", label = "Select an annotation", choices =  currAnnList, selected = tail(currAnnList, 1))
      shinyjs::html("timeMin", paste0("Start Time ",round(minLast, digits = 2)))
      shinyjs::html("timeMax", paste0("End Time ",round(maxLast, digits = 2)))
      shinyjs::html("maxFreq", paste0("Max ", maxFreqLast))
      shinyjs::html("minFreq", paste0("Min ", minFreqLast))
      shinyjs::html("meanFreq", paste0("Mean ", meanFreqLast))
      shinyjs::html("bandwidth", paste0("Bandwidth ", bandwidthLast))
      shinyjs::html("slope", paste0("Slope ", slopeLast))
      updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies, selected = as.character(typeLast))
      updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType, selected = as.character(speciesLast))
    }
    else { # Otherwise just return since nothing to read
      return()
    }
  }
  
  getStatusBarCount <- function() {
    csv <- read.csv(depFilePath)
    if("Annotation." %in% colnames(csv)){
      # Couting the unique different sequences that have been already annotated
      countAnnDone <- length(unique(csv[ ,9]))
    }
  }
  
  findFileInfo = function(firstTime) {
    files <- list.files(depPath, all.files=F, recursive=T, include.dirs=T)
    filesArray <<- 0
    dateArray <<- 0
    for (i in 1:length(files)) {
      if (substrRight(files[i],4) == ".wav") {
        fileName <- paste0(depPath,"/",files[i])
        wavFile <- substr(fileName, attr(regexpr('.*/', fileName),"match.length")+1, nchar(fileName))
        timeStringLength <- regexpr('_.*_', wavFile)
        matchedString <- timeStringLength + attr(timeStringLength, "match.length")-1
        fileTime <- ''
        if(firstTime) {
          fileTime <- substr(wavFile, timeStringLength+1, matchedString-1)
        } else {
          string <- substr(wavFile, timeStringLength+1, matchedString-1)
          fileTime <- gsub(".*_", "", string)
        }
        dateArray <<- c(dateArray, fileTime)
        filesArray <<- c(filesArray, fileName)
      }
    }
    findMaxAndMinFileDates(dateArray)
  }
  
  findMaxAndMinFileTimes = function (filesArray){
    timeArray <<- NULL
    for (i in 2:length(filesArray)) {
      timeArray <<- c(timeArray, file.info(filesArray[i])[[4]])
    }
    minTimeVar <<- as.POSIXct(min(timeArray), origin="1970-01-01")
    maxTimeVar <<- as.POSIXct(max(timeArray), origin="1970-01-01")
    output$minTime <- renderPrint({cat(as.character(minTimeVar))})
    output$maxTime <- renderPrint({cat(as.character(maxTimeVar))})
  }
  
  findMaxAndMinFileDates = function (dateArray){
    timeArray <<- dateArray[rev(order(as.Date(ymd(dateArray), format = "%m-%d-%Y")))]
    # minTimeVar <<- as.POSIXct(min(timeArray), origin="1970-01-01")
    # maxTimeVar <<- as.POSIXct(max(timeArray), origin="1970-01-01")
    minTimeVar <<- as.Date(ymd(timeArray[length(timeArray)]))
    maxTimeVar <<- as.Date(ymd(timeArray[2]))
    output$minTime <- renderPrint({cat(as.character(minTimeVar))})
    output$maxTime <- renderPrint({cat(as.character(maxTimeVar))})
  }
})