#sound <- readWave("/Users/tgirgin/1/1/deployment/BLAN01_2015-05-16_073100_EDT.wav")
#test <- meanspec(sound, f = sound@samp.rate)
#ts <- spectru(test)

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
# On a linux distribution, this will have to be changed to a media player that is installed. E.g., setWavPlayer("aplay")
setWavPlayer("aplay")


# Some global variables
mainDir <<- NULL
clipCount <<- 0
newName <- NULL
autoCSVLoad <<- FALSE
readSequenceBool <<- FALSE
annotationListDrop <<- list()
annotationListWav <<- vector()
annotationListCsv <<- vector()
annotationListCsvProject <<- vector()
# Create some REACTIVE VALUES
progressValue <<- reactiveValues()
progressValue$one <<- 0
projectFileCountGlobal <<- 0

# This is used to connect correctly with AWS
set_config( config( ssl_verifypeer = 0L ) )

options(shiny.trace=TRUE)
options(shiny.maxRequestSize=70*1024^2)
volumes <- getVolumes()
paused <<- FALSE

getOS <- function() {
  if (.Platform$OS.type == "windows") { 
    mainDir <<- "C:/"
    "win"
  } else if (Sys.info()["sysname"] == "Darwin") {
    mainDir <<- "~"
    "mac" 
  } else if (.Platform$OS.type == "unix") { 
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
  siteDF <<- NULL
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
  shinyjs::hide(id = "playButton",anim = FALSE)
  shinyjs::hide(id = "playButtonClipZoom", anim = FALSE)
  shinyjs::onclick("aws-upload-button", resetAwsCount())
  shinyjs::disable("aws-upload-button")
  shinyjs::onevent('mouseenter', "csvFile", showCSVModal())
  shinyjs::onevent('mouseleave', "csvFile", hideCSVModal())
  
  shinyjs::onclick("sF-selectButton", toggleAfterProjectSelect())
  
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
    dirPath <<- parseDirPath(roots=c(home='~'), input$directory)
    # Had to check if dirPath was not of length 0 otherwise shiny would return an error
    if(length(dirPath) != 0) {
      setwd(normalizePath(dirPath))
    }
  }
  
  toggleAfterProjectCsvLoaded = function() {
    toggleProjectInfoDisplay()
    shinyjs::addClass("enter-project-info-label", "completed-step")
    toggleDeploymentSelectDisplay()
  }
  
  toggleAfterDeploymentCsvLoaded = function() {
    toggleDeploymentSelectDisplay()
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
    # folders <- list.dirs(dirPath, full.names = F, recursive = TRUE)
    if(length(dirPath)) {
      createProjectTree()
    }
  })
  
  createProjectTree = function () {
    shinyjs::show("progressOne")
    # create_directory_tree(dirPath)
    # load("www/dir_tree.Rdata")
    # output$tree <- renderTree(tree, quoted = FALSE)
    shinyjs::addClass("directory", "active-button")
    shinyjs::show("aws-upload-button")
    deploymentSelect <- shinyDirChoose(input, 'deployment', updateFreq=60000, session=session, root=c(home=normalizePath(dirPath)), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  }
  
  observeEvent(input$deployment, {
    depPath <<- parseDirPath(root=c(home=normalizePath(dirPath)), input$deployment)
    if(length(dirPath)) {
      # folders <- list.dirs(depPath, full.names = F, recursive = TRUE)
      shinyjs::show("progressOne")
      create_directory_tree(depPath)
      load("www/dir_tree.Rdata")
      output$tree <- renderTree(tree, quoted = FALSE)
      shinyjs::addClass("deployment", "active-button")
      deploymentName <<- gsub("^.*\\/", "", depPath)
      toggleAfterDeploymentSelect()
      findFileInfo()
      fileDate <- gsub(" ", "-",minTimeVar, fixed = TRUE)
      fileDate <- gsub(":", "-",fileDate, fixed = TRUE)
      depFileName <- paste0(projectName,"_",deploymentName,"_",fileDate)
      depFilePath <<- paste0(depPath,"/", depFileName, ".csv")
      updateTextInput(session, inputId = "name", label = NULL, value = deploymentName)
      if(file.exists(paste0(depPath,"/", depFileName, ".csv"))) { # CHANGE
        readDeploymentCSV(depPath, depFilePath)
        findFileCount()
        return()
      }
    }
  })
  
  # Creating onclick event for each play and pause button
  shinyjs::onclick("playButton", onPlay("spectro"))
  shinyjs::onclick("pauseButton", pauseSound("spectro"))
  shinyjs::onclick("playButtonClip", onPlay("spectroClip"))
  shinyjs::onclick("pauseButtonClip", pauseSound("spectroClip"))
  shinyjs::onclick("playButtonClipZoom", onPlay("spectroClipZoom"))
  shinyjs::onclick("pauseButtonClipZoom", pauseSound("spectroClipZoom"))
  
  
  
  # This is the function that actually calls the play sound function, as it was impossible to pass
  # in arguments in the above shinyjs function call.
  onPlay = function(chartType) {
    if(chartType == "spectro")
    {
      playSound(spectroFromTime, spectroToTime, "spectro")
    }
    else if(chartType == "spectroClipZoom")
    {
      playSound(xminZoom, xmaxZoom, "spectroClipZoom")
    }
    else {
      playSound(xmin, xmax, "spectroClip")
    }
  }
  
  # This is the function that actually calls the pause sound function, as it was impossible to pass
  # in arguments in the above shinyjs function call.
  onPause = function(chartType) {
    if(chartType == "spectro")
    {
      pauseSound("spectro")
    }
    else if(chartType == "spectroClipZoom")
    {
      pauseSound("spectroClipZoom")
    }
    else {
      pauseSound("spectroClip")
    }
  }
  
  ###########################
  ##### Listener for file selection in "Select Sequence"
  ###########################
  observeEvent(unlist(get_selected(input$tree)), {
    # Plot main spectrogram
    shinyjs::show("loadingContainer1")
    if (is.null(unlist(get_selected(input$tree))))
    {
      return()
    }
    else {
      # Saving project information
      # annotationListCsv <<- c(annotationListCsv, normalizePath(paste0(dirPath,"/", file_path_sans_ext(unlist(get_selected(input$tree)))),'.csv'))
      # Resetting listCompleted
      listCompleted <<- list()
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
        waveDate <<- as.character(as.POSIXct(file.info(currDir)[[4]], origin="1970-01-01", format = "%m/%d/%y"), "%m/%d/%y")
        waveStartTime <<- as.character(as.POSIXct(file.info(currDir)[[4]], origin="1970-01-01", format = "%H:%M:%S %p"), format = "%H:%M:%S %p")
        waveEndTime <<- as.character(as.POSIXct((file.info(currDir)[[4]] + durationMain), origin="1970-01-01", format = "%H:%M:%S %p"), format = "%H:%M:%S %p")
        l <- length(sound)
        sr <- sound@samp.rate
        soundDuration <- round(l/sr,2)
        # TODO Maybe make a function out of this? Might make the code cleaner
        if (soundDuration > 59) {
          minuteDuration <- round(soundDuration/60)
          shinyjs::html("time-box-label", paste0("This file is ", minuteDuration, " minutes long. Would you like to increment the display?"))
          shinyjs::show("time-box-container", anim = TRUE)
          
          # Listener for "Select a Sequence"
          observeEvent(input$spectroTimeSubmit, {
            # incrementAmount <<- (as.numeric(input$spectroEndTime)*60)
            file.copy(currDir, paste0(getwd(), "/www"))
            shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
            incrementAmount <<- (soundDuration/as.numeric(input$spectroEndTime))
            spectroToTime <<- incrementAmount
            renderSpectro(sound)
            if (soundDuration > incrementAmount) {
              shinyjs::show("spectro-increment-container")
              shinyjs::show("next-spectro-increment")
            }
            shinyjs::show("playButton",anim = FALSE)
          })
          observeEvent(input$noTimeSubmission,{
            if (file.exists(depFilePath)) {
              readSequenceCSV(unlist(get_selected(input$tree)))  
            }
            spectroToTime <<- soundDuration
            renderSpectro(sound)
            shinyjs::show("playButton",anim = FALSE)
            file.copy(currDir, paste0(getwd(), "/www"))
            shinyjs::html(id = "playButton", paste0(html = '<audio controls preload="auto"><source src="', unlist(get_selected(input$tree)), '" type="audio/wav"></audio>'))
          })
        } else {
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
  })
  
  renderSpectro = function (sound){
    output$spectrogram <- renderPlot({
      shinyjs::hide("time-box-container", anim = TRUE)
      anottationCount <<- 0
      spectro(sound, osc = TRUE, scale = FALSE, tlim = c(spectroFromTime,spectroToTime))
      shinyjs::show("complete-deployment")
      
      # shinyjs::onclick("complete-deployment", increaseStatusBar())
      # spectroFromTime <<- spectroToTime
    })
  }
  
  # renderSpectroClip = function(sound, xmin, xmax){
  #   output$spectroClip <- renderPlot({
  #     # shinyjs::hide("time-box-container", anim = TRUE)
  #     # anottationCount <<- 0
  #     spectro(sound, osc = TRUE, scale = FALSE, tlim = c(xmin,xmax))
  #     # shinyjs::show("complete-deployment")
  #     
  #     # shinyjs::onclick("complete-deployment", increaseStatusBar())
  #     # spectroFromTime <<- spectroToTime
  #   })
  # }
  
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
    soundDuration <- round(l/sr,2)
    shinyjs::show("previous-spectro-increment")
    spectroToTime <<- spectroToTime + incrementAmount
    spectroFromTime <<- spectroFromTime + incrementAmount
    # if (spectroToTime >soundDuration) {
    #   spectroToTime <<- soundDuration
    # }
    renderSpectro(sound)
    print('clicked')
    shinyjs::show("previous-spectro-increment")
    if (spectroToTime >= soundDuration) {
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
      # Use  from = 1, to = 5, units = "seconds" when playing from a certain time
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
      # test <- wait(audioSound)
      print("t")
      # pause(a)
      # shinyjs::onclick("pauseButton",pause(a))
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
    # names <- get_selected(input$tree, "names")
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
      testCSV <- read.csv("www/species-short.csv", header = TRUE)
      read.csv("www/species-short.csv", header = TRUE)
    } else {
      req(input$csvFile)
      infile <- parseFilePaths(roots=c(home='~'),input$csvFile)
      correctPath <- file.path(infile$datapath)
      print(correctPath)
      read.csv(correctPath, header = TRUE)
    }
  })
  
  output$commonName <- renderUI({
    
    df <- species()
    
    
    if (is.null(df)) return(NULL)
    
    itemsType <<- c('Select Species',as.character(df[[1]]))
    selectInput("speciesDropdown", "Species*",itemsType)
  })
  
  output$speciesType <- renderUI({
    
    df <- species()
    
    if (is.null(df)) return(NULL)
    
    itemsSpecies <<- c('Select Type',as.character(df[[3]]))
    selectInput("typeDropdown", "Type*",itemsSpecies)
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
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    species()
  })
  # 
  #   # This creates the oscillo clips after brush
  #   output$spectroZoomClip <- renderPlot({
  #     path <- getPath(get_selected(input$tree, "names"))
  #     if(!is.null(newName)) {
  #       currDir <- paste0(depPath, "/", path, newName)
  #     }
  #     else {
  #       currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
  #     }
  #     sound <- readWave(currDir)
  #     if(!is.null(input$plotZoom$xmax)) {
  # 
  #       spectro(sound, scale = FALSE, osc = FALSE, tlim = c(input$plotZoom$xmin,input$plotZoom$xmax), flim = c(input$plotZoom$ymin,input$plotZoom$ymax))
  # 
  #       # Min and max values for the spectropClipZoom
  #       xminZoom <<- input$plotZoom$xmin
  #       xmaxZoom <<- input$plotZoom$xmax
  # 
  #       # Min and max values for the spectroClip, not the spectroClipZoom
  #       xmin <- input$plot_brush$xmin
  #       xmax <- input$plot_brush$xmax
  #       # shinyjs::html('remove',tags$div(class = "close-clip", "hello There"))
  #       shinyjs::onclick("spectroClip",showSpeciesDropdown(xmin, xmax))
  #       shinyjs::show("playButtonClipZoom",anim = FALSE)
  #     }
  #   })
  # 
  
  observeEvent(input$plot_brush$xmin, {
    renderSpectroClip(NULL, input$plot_brush$xmin, input$plot_brush$xmax, FALSE)
  });
  
  # This creates the oscillo after brush
  renderSpectroClip = function(sound, xmin, xmax, readSequenceBool)
  {
    output$spectroClip <- renderPlot({
      if(readSequenceBool)
      {
        spectro(sound, osc = TRUE, scale = FALSE, tlim = c(xmin,xmax))
        readSequenceBool <<- FALSE
        return()
      }
      path <- getPath(get_selected(input$tree, "names"))
      if(!is.null(newName)) {
        currDir <- paste0(depPath, "/", path, newName)
      }
      else {
        currDir <- paste0(depPath, "/", path, unlist(get_selected(input$tree)))
      }
      sound <- readWave(currDir)
      
      # shinyjs::show("spectro-clip-container")
      if(!is.null(xmax)) {
        spectro(sound, f = sound@samp.rate, osc = TRUE, scale = FALSE, tlim = c(floor(as.integer(xmin)),floor(as.integer(xmax))))
        # oscillo(sound, from=input$plot_brush$xmin, to=input$plot_brush$xmax)
        # shinyjs::show("spectroClip")
        # xmin <<- input$plot_brush$xmin
        # xmax <<- input$plot_brush$xmax
        # Getting the duration of the clipped graph
        durationSmall <<- round(xmax - xmin, digits = 1)
        shinyjs::show("clipInfo-container")
        shinyjs::show("playButtonClip",anim = FALSE)
        # Creating a temp wav sound from xmin to xmax
        temp <- extractWave(sound, from = xmin, to = xmax, xunit = "time")
        # Writing it to a .wav file
        writeWave(temp, paste0(getwd(), "/www/temp.wav"))
        # Creating an audio tag holding that temp.wav file to be played
        shinyjs::html(id = "playButtonClip", paste0(html = '<audio src="temp.wav" type="audio/wav" controls></audio>'))
      } else {
        # browser()
      }
      showSpeciesDropdown(xmin, xmax)
    })
  }

  
  showSpeciesDropdown = function (xmin, xmax){
    shinyjs::show("clip-species-dropdown")
    if(!is.null(xmax)) {
      updateTextInput(session, "timeMin",label = paste("Time Start: "), value = paste(round(xmin,digits=1)))
      updateTextInput(session, "timeMax",label = paste("Time End: "), value = paste(round(xmax,digits=1)))
      shinyjs::disable("timeMin")
      shinyjs::disable("timeMax")
    } else {
      updateTextInput(session, "timeMin",label = paste("Time Start: "), value = paste(0))
      updateTextInput(session, "timeMax",label = paste("Time End: "), value = paste(0))
      shinyjs::disable("timeMin")
      shinyjs::disable("timeMax")
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
    # Getting file list
    files <- list.files(depPath, all.files=F, recursive=T, include.dirs=T)
    # Getting the site data
    data <- formDataSite()
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
      # dataArray <- c(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]],data[[6]],data[[7]], fileFullName, waveStartTime, waveEndTime, waveDate, googleMapsLink)
      dataArray <- c(data[[1]],data[[2]],data[[3]],data[[4]],data[[5]],data[[6]],data[[7]], googleMapsLink)
      dataMatrix <- matrix(dataArray,ncol = length(dataArray), byrow = TRUE)
      # colnames(dataMatrix) <- c("Name", "Lat", "Lon", "Record ID", "Site Notes", "Start", "End", "File Name", "Wave Start", "Wave End", "Wave Date", "Google Maps")
      colnames(dataMatrix) <- c("Name", "Lat", "Lon", "Record ID", "Site Notes", "Start", "End", "Google Maps")
      siteDataTable <- as.table(dataMatrix)
      siteDF <<- siteDataTable
      # If we have multiple clips on a given spectro, give a new column name to each clip
      clipCount <<- 0
      # Reformating user input
      fileDate <- gsub(" ", "-",data[[6]], fixed = TRUE)
      fileDate <- gsub(":", "-",fileDate, fixed = TRUE)
      # Creating a new filename out of the metadata
      newFileName <<- paste0(projectName,"_",data[[1]],"_",fileDate)
      # Creating the new file path
      newFullFilePath <- paste0(depPath,"/",newFileName)
      # Checking for file duplicate
      fileNameDuplicate <- 0
      # Checking for file duplicates within that folder
      for (i in 1:length(files)) {
        if (files[i] == paste0(newFileName,".csv")) {
          fileNameDuplicate <- as.numeric(fileNameDuplicate) + 1
        }
      }
      # Checking for file duplication, alert if any; otherwise create the file
      if (fileNameDuplicate == 0 || autoCSVLoad) {
        writeDeploymentCSV(siteDataTable)
      }
      else if(!autoCSVLoad) {
        shinyjs::show("file-name-warning-container")
      }
    }
  })
  # 
  # deploymentInfo = function (dataArray) {
  #   
  # }
  
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
    print(data)
    oldProjectName <- projectName
    projectName <<- projectNameArg
    newDirPath <- gsub(oldProjectName,projectName,dirPath)
    file.rename(dirPath,newDirPath)
    dirPath <<- newDirPath
    write.csv(projectData, paste0(dirPath,"/",paste0('Project_', projectName,'.csv')), row.names = FALSE)
    annotationListCsvProject <<- c(annotationListCsv, normalizePath(paste0(dirPath,"/",paste0('Project_', projectName,'.csv'))))
  }
  
  speciesFields <- c("timeMin", "timeMax", "speciesDropdown", "typeDropdown", "annotNotes")
  
  formDataSpecies <- reactive({
    data <- sapply(speciesFields, function(x) input[[x]])
    data
  })
  
  # Adding the clip metadata to the spectrogram metadata
  # ClipCount -> If we have multiple clips on a given spectro, give a new column name to each clip
  observeEvent(input$speciesDropSubmit, {
    # fileFullName <- unlist(get_selected(input$tree))
    if (is.null(siteDF) ) {
      if(!autoCSVLoad) {
        shinyjs::show("site-info-warning-container") 
      }
      else {
        shinyjs::enable("aws-upload-button")
        dataSet <- formDataSpecies()
        shinyjs::hide("site-info-warning-container")
        if (dataSet[[3]] == "Select Species") {
          shinyjs::show("type-name-warning")
        } 
        else if (dataSet[[4]] == "Select Type") {
          shinyjs::show("type-name-warning")
        } 
        else {
          if (clipCount == 0) {
            dataArray <- c(fileFullName,clipCount,dataSet[[1]],dataSet[[2]], durationSmall, dataSet[[4]],dataSet[[3]],dataSet[[5]])
            dataMatrix <- matrix(dataArray,ncol = 8, byrow = TRUE)
            colnames(dataMatrix) <- c("File Name", "Annotation#","Time Min (s)", "Time Max (s)", "Duration", "Type", "Species", "Annotation Notes")
            dataTable <- as.table(dataMatrix)
            siteDF <<- cbind(siteDF, dataTable)
          } 
          else {
            dataArray <- c(siteDF[1,1],siteDF[1,2],siteDF[1,3],siteDF[1,4],siteDF[1,5],siteDF[1,6],siteDF[1,7],siteDF[1,8],fileFullName, clipCount,dataSet[[1]],dataSet[[2]],durationSmall, dataSet[[4]],dataSet[[3]],dataSet[[5]])
            siteDF <<- rbind(siteDF, dataArray)
          }
          increaseStatusBar()
          clipCount <<- clipCount + 1
          # Creating the path with the file name
          filePathFull <- paste0(depPath,"/",fileFullName)
          # Adding the file to the list of annotated files for later zipping and S3 upload
          annotationListWav <<- c(annotationListWav, normalizePath(filePathFull))
          write.csv(siteDF, paste0(depPath,"/",paste0(newFileName,'.csv')), row.names = FALSE)
          annotationListCsv <<- c(annotationListCsv, normalizePath(paste0(depPath,"/",paste0(newFileName,'.csv'))))
          shinyjs::addClass('completedDepContainer', "open-accordian")
          shinyjs::removeClass('completedDepContainer', "closed-accordian")
          shinyjs::show("annotationDrop")
          
          annotationList <- c(paste0(dataSet[[3]], " at " , dataSet[[1]]))
          annotationListDrop <<- c(annotationListDrop, annotationList)
          
          updateSelectizeInput(session, "annotationDrop", label = "Select an annotation", choices =  annotationListDrop, selected = tail(annotationListDrop, 1))
          
          # Create some REACTIVE VALUES
          awsProgressValue <<- reactiveValues()
          awsProgressValue$one <<- 0
          # Creating the progress bar for AWS upload
          output$awsProgress <- renderUI({
            progressGroup(text = "Status",    value = awsProgressValue$one,   min = 0, max = 3, color = "green")
          })
        }
      }
    }
  })
  
  observeEvent(input$annotationDrop, {
    # Checking that we actually have an element in the dropdown
    if(input$annotationDrop != "")
    {        
      annData <- read.csv(depFilePath)[ ,10:16]
      sound <- readWave(paste0(depPath, "/", unlist(get_selected(input$tree))))
      # If current selection is last element in dropdown
      if(str_detect(input$annotationDrop, as.character(tail(annData[[6]], 1))))
      {
        annCount <- length(annData[[1]])
        annLast <- tail(annData, 1)
        minLast <- tail(annData[[2]], 1)
        maxLast <- tail(annData[[3]], 1)
        updateTextInput(session, "timeMin",label = paste("Time Start: "), value = minLast)
        updateTextInput(session, "timeMax",label = paste("Time End: "), value = maxLast)
        updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies, selected = tail(annData[[5]], 1))
        updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType, selected = tail(annData[[6]], 1))
        # Creating a temp wav sound from xmin to xmax
        temp <- extractWave(sound, from = minLast, to = maxLast, xunit = "time")
        # Writing it to a .wav file
        writeWave(temp, paste0(getwd(), "/www/temp.wav"))
        # Creating an audio tag holding that temp.wav file to be played
        shinyjs::show("playButtonClip",anim = FALSE)
        shinyjs::html(id = "playButtonClip", paste0(html = '<audio src="temp.wav" type="audio/wav" controls></audio>'))
      } else {
        currentSelectedMin <- trimws(head(strsplit(input$annotationDrop,split="at")[[1]],2)[2], which = "both")
        currentSelectedSpecies <- trimws(head(strsplit(input$annotationDrop,split="at")[[1]],2)[1], which = "both")
        df <- as.data.frame(annData)
        selectedAnn <- df[which(df$Time.Min..s. == currentSelectedMin & df$Species == currentSelectedSpecies), ]
        annCurr <- selectedAnn$Annotatiion.
        minCurr <- selectedAnn$Time.Min..s.
        maxCurr <- selectedAnn$Time.Max..s.
        typeCurr <- selectedAnn$Type
        speciesCurr <- selectedAnn$Species
        renderSpectroClip(sound, minCurr, maxCurr, TRUE)
        updateTextInput(session, "timeMin",label = paste("Time Start: "), value = as.character(minCurr))
        updateTextInput(session, "timeMax",label = paste("Time End: "), value = as.character(maxCurr))
        updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies, selected = as.character(typeCurr))
        updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType, selected = as.character(speciesCurr))
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
  
  ################################
  ######## Modal UI
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
        if(dir.exists(csvDir)) {
          unlink(csvDir, recursive = TRUE)
        }
        dir.create(csvDir)
        dir.create(wavDir)
        file.copy(annotationListCsv, wavDir)
        file.copy(annotationListCsvProject, csvDir)
        file.copy(annotationListWav, wavDir)
        incrementAwsCount()
        dirToZip <- csvDir
        # Zip folder
        oldwd <- getwd()
        setwd(tempDir)
        zipName <- sub('_([^_]*)$', '', newFileName)
        currDate <- format(Sys.time(), "%Y%m%d")
        fullZipName <- paste0("/", zipName, "_", currDate)
        zip(normalizePath(paste0(dirPath, fullZipName, ".zip")), paste0(projectName, "/"))
        incrementAwsCount()
        setwd(oldwd)
        # Upload to AWS
        awsUpload <- put_object(file = normalizePath(paste0(dirPath, fullZipName, ".zip")), bucket = awsBucket)
        
        observeEvent(awsUpload, {
          if(awsUpload[1] == TRUE)
          {
            incrementAwsCount();
          }
        })
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
  
  removeAnnotationFromCSV <- function(annotationNumber) {
    browser()
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
    # Create some REACTIVE VALUES
    progressValue <<- reactiveValues()
    progressValue$one <<- 0
    # Render UI output
    output$progressOne <- renderUI({
      progressGroup(text = "Status",    value = progressValue$one,   min = 0, max = projectFileCount, color = "aqua")
    })
  }
  
  #################################
  ###### Function to increase the count of the status bar
  #################################
  increaseStatusBar = function () {
    # shinyjs::hide("content-id")
    progressValue$one <<- progressValue$one + 1
    if (progressValue$one == projectFileCountGlobal) {
      shinyjs::hide("tree", anim = TRUE)
      shinyjs::addClass("show-tree", "closed-accordian")
      shinyjs::addClass("show-tree", "completed-step")
      shinyjs::removeClass("show-tree", "open-accordian")
    } else {
      
    }
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
      # projectInfo(projectCSV$Project.Name[[1]], projectCSV$Project.Notes[[1]])
    }
  }
  
  readDeploymentCSV <- function(depPath, depFilePath) {
    shinyjs::hide("species-sidebox-container", anim = TRUE)
    shinyjs::toggleClass("select-dep-container", "open-accordian")
    shinyjs::toggleClass("select-dep-container", "closed-accordian")
    deploymentCSV <- read.csv(paste0(depFilePath))
    updateTextInput(session, inputId = "name", label = NULL, value = deploymentCSV$Name[[1]])
    updateTextInput(session, inputId = "lat", label = NULL, value = as.character(deploymentCSV$Lat[[1]]))
    updateTextInput(session, inputId = "lon", label = NULL, value = as.character(deploymentCSV$Lon[[1]]))
    updateTextInput(session, inputId = "recId", label = NULL, value = as.character(deploymentCSV$Record.ID[[1]]))
    shinyjs::html("siteNotes", deploymentCSV$Site.Notes[[1]])
    toggleAfterDeploymentCsvLoaded()
    autoCSVLoad <<- TRUE
    # deploymentInfo(projectCSV$Project.Name[[1]], projectCSV$Site.Notes[[1]])
  }
  
  writeDeploymentCSV <- function(siteDataTable) {
    shinyjs::hide("file-name-warning-container")
    
    count <- 0
    if(!autoCSVLoad)
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
    annData <- read.csv(depFilePath)
    # Check if we have annotation files
    if("File.Name" %in% colnames(annData)){
      annData <- annData[ ,9:16]
      currentSelectedMin <- trimws(head(strsplit(input$annotationDrop,split="at")[[1]],2)[2], which = "both")
      currentSelectedSpecies <- trimws(head(strsplit(input$annotationDrop,split="at")[[1]],2)[1], which = "both")
      df <- as.data.frame(annData)
      selectedWav <- df[which(df$File.Name == wavFileName), ]
      # If there are no annotations for that sequence
      if(length(selectedWav$Annotation.) == 0)
      {
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
      minLast <- tail(selectedWav[[3]], 1)
      maxLast <- tail(selectedWav[[4]], 1)
      typeLast <- tail(selectedWav[[5]], 1)
      speciesLast <- tail(selectedWav[[6]], 1)
      sound <- readWave(paste0(depPath, "/", wavFileName))
      readSequenceBool <- TRUE
      renderSpectroClip(sound, minLast, maxLast, readSequenceBool)
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
      updateSelectizeInput(session, "annotationDrop", label = "Select an annotation", choices =  currAnnList, selected = tail(annotationListDrop, 1))
      updateTextInput(session, "timeMin",label = paste("Time Start: "), value = as.character(minLast))
      updateTextInput(session, "timeMax",label = paste("Time End: "), value = as.character(maxLast))
      updateSelectizeInput(session, "typeDropdown", label = "Type*", choices =  itemsSpecies, selected = as.character(typeLast))
      updateSelectizeInput(session, "speciesDropdown", label = "Species*", choices =  itemsType, selected = as.character(speciesLast))
    }
    else { # Otherwise just return since nothing to read
      return()
    }
  }
  
  findFileInfo = function() {
    files <- list.files(depPath, all.files=F, recursive=T, include.dirs=T)
    filesArray <<- 0
    for (i in 1:length(files)) {
      if (substrRight(files[i],4) == ".wav") {
        fileName <- paste0(depPath,"/",files[i])
        filesArray <<- c(filesArray, fileName)
      }
    }
    findMaxAndMinFileTimes(filesArray)
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
  
})