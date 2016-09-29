# install.packages("shiny")
# install.packages("devtools")
# install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
# install.packages('shinyFiles')
# install_github("trestletech/shinyTree")
# install.packages("sound")
# install.packages("audio")

library(shiny)
library(tools)
library(devtools)
library(stringr)
library(shinyFiles)
# library("aws.s3")
library(shinyTree)
library(seewave)
library(tuneR)
# install_github("trestletech/shinyStore")
# load_all('~/dev/emammal-soundburst/soundBurst/R')
# load_all('~/dev/emammal-soundBurst/soundBurst/R')
library(audio)
# setWavPlayer('"/Applications/QuickTime\ Player"')
setWavPlayer("afplay")
library(sound)
source("createDirectoryTree.r")
source("playSound.r")

clipCount <<- 0
newName <- NULL

options(shiny.trace=TRUE)
options(shiny.maxRequestSize=70*1024^2) 
volumes <- getVolumes()
paused <<- FALSE 

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

  shinyjs::onclick("left-column-title", toggleProjectSelect())
  # shinyjs::onclick("species-file-upload", togglecsvFileUploadButton())
  shinyjs::onclick("enter-project-info-label", toggleProjectInfoDisplay())
  shinyjs::onclick("right-column-title", toggleSiteInfoContainer())
  shinyjs::onclick("completedDepContainer", toggleCompletedDeployment())
  # shinyjs::hide("csvFile")
  shinyjs::onclick("show-tree", toggleTree())
  shinyjs::hide("pauseButton")
  shinyjs::hide("pauseButtonClip")
  shinyjs::hide("pauseButtonClipZoom")
  shinyjs::hide("clipInfo-container")
  # shinyjs::hide("spectroZoomClip")
  shinyjs::hide("project-info-container")
  shinyjs::hide("species-sidebox-container")
  shinyjs::hide("complete-deployment")
  shinyjs::hide("progressOne")
  # shinyjs::hide("spectro-clip-container")
  shinyjs::hide("time-box-container")
  shinyjs::hide("spectro-increment-container")
  shinyjs::hide("previous-spectro-increment")
  shinyjs::hide("tree")
  shinyjs::hide("directorypath")
  shinyjs::hide(id = "playButton",anim = FALSE)
  shinyjs::hide(id = "playButtonClip",anim = FALSE)
  shinyjs::hide(id = "playButtonClipZoom", anim = FALSE)
  shinyjs::hide("file-name-warning-container")
  shinyjs::hide("site-info-warning-container")
  shinyjs::hide("aws-upload-button")
  shinyjs::disable("aws-upload-button")

  shinyjs::onclick("sF-selectButton", toggleAfterProjectSelect())
  
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
  
  togglecsvFileUploadButton = function() {
    shinyjs::toggle("csvFile", anim = TRUE)
    shinyjs::toggleClass("species-file-upload", "open-accordian")
    shinyjs::toggleClass("species-file-upload", "closed-accordian")
  }
  
  toggleSiteInfoContainer = function() {
    shinyjs::toggle("species-sidebox-container", anim = TRUE)
    shinyjs::toggleClass("right-column-title", "open-accordian")
    shinyjs::toggleClass("right-column-title", "closed-accordian")
  }
  
  toggleCompletedDeployment = function() {
    shinyjs::toggle("listCompleted", anim = TRUE)
    shinyjs::toggleClass("completedDepContainer", "open-accordian")
    shinyjs::toggleClass("completedDepContainer", "closed-accordian")
  }
  
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
  }
  
  toggleTree = function() {
    shinyjs::toggleClass("show-tree", "open-accordian")
    shinyjs::toggleClass("show-tree", "closed-accordian")
    shinyjs::toggle("directorypath", anim = TRUE)
    shinyjs::toggle("tree", anim = TRUE)
  }
  
  test <- shinyDirChoose(input, 'directory', updateFreq=60000, session=session, roots=c(home='~'), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  output$directorypath <- renderPrint({
    dirPath <<- parseDirPath(roots=c(home='~'), input$directory)
    folders <- list.dirs(dirPath, full.names = F, recursive = TRUE)
    
    if(!(is.null(dirPath))) {
      shinyjs::show("progressOne")
      create_directory_tree(dirPath)
      load("www/dir_tree.Rdata")
      output$tree <- renderTree(tree, quoted = FALSE)
      shinyjs::addClass("directory", "active-button")
      shinyjs::show("aws-upload-button")
    }
    findFileCount()
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
  
  observeEvent(unlist(get_selected(input$tree)), {
    # Plot main spectrogram
    shinyjs::show("loadingContainer1")
    if (is.null(unlist(get_selected(input$tree))))
    {
      return()
    } 
    else {
      listCompleted <<- list()
      path <- getPath(get_selected(input$tree, "names"))
      currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
      sound <- readWave(currDir)
      l <- length(sound@left)
      sr <- sound@samp.rate
      soundDuration <- round(l/sr,2)
      if (soundDuration > 59) {
        shinyjs::show("time-box-container", anim = TRUE)
          observeEvent(input$spectroTimeSubmit, {
          incrementAmount <<- (soundDuration / as.numeric(input$spectroEndTime))
          spectroToTime <<- incrementAmount
          renderSpectro(sound)
          if (soundDuration > incrementAmount) {
            shinyjs::show("spectro-increment-container")
          }
          shinyjs::show("playButton",anim = FALSE)
          shinyjs::show("species-sidebox-container")
        }) 
          observeEvent(input$noTimeSubmission,{
            spectroToTime <<- soundDuration
            renderSpectro(sound)
            shinyjs::show("playButton",anim = FALSE)
            shinyjs::show("species-sidebox-container")
          })
      } else {
        spectroToTime <<- soundDuration
        renderSpectro(sound)
        shinyjs::show("playButton",anim = FALSE)
        shinyjs::show("species-sidebox-container")
      }
      shinyjs::show("content-id")
      shinyjs::hide("tree", anim = TRUE)
      shinyjs::addClass("show-tree", "closed-accordian")
      shinyjs::addClass("show-tree", "completed-step")
      shinyjs::removeClass("show-tree", "open-accordian")
      shinyjs::addClass("right-column-title", "open-accordian")
      shinyjs::removeClass("right-column-title", "closed-accordian")
      if(!is.null(newName)) {
        shinyjs::html("titleHeader",newName)
      }
      else {
        shinyjs::html("titleHeader",unlist(get_selected(input$tree)))
      }
    }
  })
  
  renderSpectro = function (sound){
    output$spectrogram <- renderPlot({
      shinyjs::hide("time-box-container", anim = TRUE)
      anottationCount <<- 0
      # shinyjs::html("right-column-title",createCSVFilePath())
      spectro(sound, osc = TRUE, scale = FALSE, tlim = c(spectroFromTime,spectroToTime))
      shinyjs::removeClass("right-column-title", "completed-step")
      shinyjs::show("species-sidebox-container", anim = TRUE)
      findFileInfo()
      shinyjs::show("complete-deployment")
      shinyjs::onclick("complete-deployment", increaseStatusBar())
      # spectroFromTime <<- spectroToTime
    })
  }
  
  shinyjs::onclick("previous-spectro-increment", showPreviousSpectroIncrement())
  shinyjs::onclick("next-spectro-increment", showNextSpectroIncrement())
  
  showPreviousSpectroIncrement = function() {
    path <- getPath(get_selected(input$tree, "names"))
    if(!is.null(newName)) {
      currDir <- paste0(dirPath, "/", path, newName)
    }
    else {
      currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    }
    sound <- readWave(currDir)
    l <- length(sound@left)
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
      currDir <- paste0(dirPath, "/", path, newName)
    }
    else {
      currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    }
    sound <- readWave(currDir)
    l <- length(sound@left)
    sr <- sound@samp.rate
    soundDuration <- round(l/sr,2)
    shinyjs::show("previous-spectro-increment")
    spectroToTime <<- spectroToTime + incrementAmount
    spectroFromTime <<- spectroFromTime + incrementAmount
    renderSpectro(sound)
    print('clicked')
    shinyjs::show("previous-spectro-increment")
    if (spectroToTime >= soundDuration) {
      shinyjs::hide("next-spectro-increment")
    }
  }

  playSound = function (start, end, chartType){
    if(paused)
    {
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
        shinyjs::hide(id = "playButtonClip",anim = FALSE)
      }
    } 
    else {
      path <- getPath(get_selected(input$tree, "names"))
      if(!is.null(newName)) {
        currDir <- paste0(dirPath, "/", path, newName)
      }
      else {
        currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
      }
      # Use  from = 1, to = 5, units = "seconds" when playing from a certain time
      wave <- readWave(currDir, from = start, to = end, unit = "seconds")
      sound <- audioSample(wave@left, wave@samp.rate, wave@bit)
      
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
        shinyjs::hide(id = "playButtonClip",anim = FALSE)
      }
      audioSound <<- audio::play(sound)
      audioSound
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

    itemsSpecies <- c('Select Species',as.character(df[[1]]))
    selectInput("speciesDropdown", "Species:",itemsSpecies)
  })
  
  output$speciesType <- renderUI({

      df <- species()

    if (is.null(df)) return(NULL)
    
    itemsSpecies <- c('Select Type',as.character(df[[3]]))
    selectInput("typeDropdown", "Type:",itemsSpecies)
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
  
  # This creates the oscillo clips after brush
  output$spectroZoomClip <- renderPlot({
    path <- getPath(get_selected(input$tree, "names"))         
    if(!is.null(newName)) {
      currDir <- paste0(dirPath, "/", path, newName)
    }
    else {
      currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    }
    sound <- readWave(currDir)
    if(!is.null(input$plotZoom$xmax)) {
      
      spectro(sound, scale = FALSE, osc = FALSE, tlim = c(input$plotZoom$xmin,input$plotZoom$xmax), flim = c(input$plotZoom$ymin,input$plotZoom$ymax))
      
      # Min and max values for the spectropClipZoom
      xminZoom <<- input$plotZoom$xmin
      xmaxZoom <<- input$plotZoom$xmax
      
      # Min and max values for the spectroClip, not the spectroClipZoom
      xmin <- input$plot_brush$xmin
      xmax <- input$plot_brush$xmax
      # shinyjs::html('remove',tags$div(class = "close-clip", "hello There"))
      shinyjs::onclick("spectroClip",showSpeciesDropdown(xmin, xmax)) 
      shinyjs::show("playButtonClipZoom",anim = FALSE)
    }
  })
  
  # This creates the oscillo clips after brush
  output$spectroClip <- renderPlot({
    path <- getPath(get_selected(input$tree, "names"))
    if(!is.null(newName)) {
      currDir <- paste0(dirPath, "/", path, newName)
    }
    else {
      currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    }
    sound <- readWave(currDir)
    # shinyjs::show("spectro-clip-container")
    if(!is.null(input$plot_brush$xmax)) {
      spectro(sound, scale = FALSE, osc = FALSE, tlim = c(input$plot_brush$xmin,input$plot_brush$xmax))
      # oscillo(sound, from=input$plot_brush$xmin, to=input$plot_brush$xmax)
      # shinyjs::show("spectroClip")
      xmin <<- input$plot_brush$xmin
      xmax <<- input$plot_brush$xmax
      shinyjs::show("clipInfo-container")
      shinyjs::show("playButtonClip",anim = FALSE)
    }
    showSpeciesDropdown(xmin, xmax)
  })
  
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
  
  observeEvent(input$siteInfo, {
    # Getting file list
    files <- list.files(dirPath, all.files=F, recursive=T, include.dirs=T)
    # fileFullName <- unlist(get_selected(input$tree))
    # fileName <- sub(".wav", "", fileFullName)
    # Getting the file name
    fileFullName <- unlist(get_selected(input$tree))
    # Creating the path with the file name
    filePathFull <- paste0(dirPath,"/",fileFullName)
    # Getting the site data
    data <- formDataSite()
    # Adding the min and max time variables to the data
    data <- c(data, as.character(minTimeVar))
    names(data)[6] <- "start_time_date"
    data <- c(data, as.character(maxTimeVar))
    names(data)[7] <- "end_time_date"
    siteDF <<- data
    # If we have multiple clips on a given spectro, give a new column name to each clip
    clipCount <<- 0
    # Reformating user input
    fileDate <- gsub(" ", "-",data[[6]], fixed = TRUE)
    fileDate <- gsub(":", "-",fileDate, fixed = TRUE)
    # Creating a new filename out of the metadata
    newFileName <- paste0(projectName,"_",data[[1]],"_",fileDate)
    # shinyjs::html("right-column-title",newFileName)
    # Creating the new file path
    newFullFilePath <- paste0(dirPath,"/",newFileName)

    # Checking for file duplicate
    fileNameDuplicate <- 0

    # Checking for file duplicates within that folder
    for (i in 1:length(files)) {
      if (files[i] == paste0(newFileName,".wav")) {
        fileNameDuplicate <- as.numeric(fileNameDuplicate) + 1
      }
    }

    # Checking for file duplication, alert if any; otherwise create the file
    if (fileNameDuplicate == 0) {
      shinyjs::hide("file-name-warning-container")
      # Update tree
      load("www/dir_tree.Rdata")
      count <- 0
      for (name in names(tree)) {
        count = count + 1
        if(name == unlist(get_selected(input$tree)))
        {
          print(count)
          names(tree)[count] <- paste0(newFileName, ".wav")
          newName <<- paste0(newFileName, ".wav")
        }
      }
      # output$tree <- renderTree(tree, quoted = FALSE)

      file.rename(filePathFull, paste0(newFullFilePath,".wav"))
      write.csv(data, paste0(dirPath,"/",paste0(newFileName,'.csv')))
      if(!is.null(newName)) {
        shinyjs::html("titleHeader",newName)
      }
      else {
        shinyjs::html("titleHeader",unlist(get_selected(input$tree)))
      }
      shinyjs::addClass("siteInfo", "active-button")
      shinyjs::hide("species-sidebox-container")
      shinyjs::addClass("right-column-title", "completed-step")
      shinyjs::toggleClass("right-column-title", "open-accordian")
      shinyjs::toggleClass("right-column-title", "closed-accordian")
    }
    else {
      shinyjs::show("file-name-warning-container")
    }

  })
  
  projectFields <- c("projectName", "projectNotes")
  
  formDataProject <- reactive({
    data <- sapply(projectFields, function(x) input[[x]])
    data
  })
  
  observeEvent(input$projectInfo, {
    print(paste0(dirPath,"/",'projectInfo.csv'))
    data <- formDataProject()
    print(data)
    projectName <<- data[[1]]
    write.csv(data, paste0(dirPath,"/",'projectInfo.csv'))
    # shinyjs::hide("csvFile", anim = TRUE)
    shinyjs::hide("directory", anim = TRUE)
    shinyjs::addClass("projectInfo", "active-button")
    shinyjs::hide("project-info-container", anim = TRUE)
    shinyjs::addClass("enter-project-info-label", "completed-step")
    shinyjs::toggleClass("enter-project-info-label", "open-accordian")
    shinyjs::toggleClass("enter-project-info-label", "closed-accordian")
    shinyjs::addClass("show-tree", "open-accordian")
    shinyjs::removeClass("show-tree", "closed-accordian")
    shinyjs::show("tree", anim = TRUE)
  })
  
  speciesFields <- c("timeMin", "timeMax", "speciesDropdown", "typeDropdown", "annotNotes")
  
  formDataSpecies <- reactive({
    data <- sapply(speciesFields, function(x) input[[x]])
    data
  })
  
  # Adding the clip metadata to the spectrogram metadata
  # ClipCount -> If we have multiple clips on a given spectro, give a new column name to each clip
  observeEvent(input$speciesDropSubmit, {
    if (is.null(siteDF)) {
      shinyjs::show("site-info-warning-container")
    } else {
      shinyjs::hide("site-info-warning-container")
      clipCount <<- clipCount + 1
      dataSet <- formDataSpecies()
      names(dataSet)[1] <- paste0(names(dataSet)[1],clipCount) # timeMin
      names(dataSet)[2] <- paste0(names(dataSet)[2],clipCount) # timeMax
      names(dataSet)[3] <- paste0(names(dataSet)[3],clipCount) # spciesInput
      names(dataSet)[4] <- paste0(names(dataSet)[4],clipCount) # typeInput
      names(dataSet)[5] <- paste0(names(dataSet)[5],clipCount) # annotNotes
      formattedData <- c(siteDF, dataSet)
      siteDF <<- formattedData
      write.csv(siteDF, paste0(dirPath,"/",paste0(createCSVFilePath(),'.csv')))
      shinyjs::addClass('completedDepContainer', "open-accordian")
      shinyjs::show("listCompleted")
      
      # Creating the element that will old the name of the completed annotation
      listEl <- as.character(paste0(tags$div(class="annotations",id=clipCount, tags$span(paste0(dataSet[[4]], " at " , dataSet[[1]])),tags$div(class='removeAnn'))))
      # Storing the element in a list that gets reset every time a new deployment is selected
      listCompleted <<- c(listCompleted, listEl)
      # Converting that list to a tagList
      finalCompleted <- tagList(listCompleted)
      # Display the list of tag in the UI
      shinyjs::html('listCompleted', finalCompleted)
      tags$head(tags$script(src="removeAnnotation.js"))
    }
  })
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  findFileCount = function() {
    projectFileCount <- 0
    projectFileCountGlobal <<- 0
    files <- list.files(dirPath, all.files=F, recursive=T, include.dirs=T)
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

  increaseStatusBar = function () {
    shinyjs::hide("content-id")
    progressValue$one <<- progressValue$one + 1
    if (progressValue$one == projectFileCountGlobal) {
      shinyjs::enable("aws-upload-button")
      shinyjs::addClass(".active-aws-button")
    } else {
      shinyjs::show("tree", anim = TRUE)
      shinyjs::removeClass("show-tree", "closed-accordian")
      shinyjs::removeClass("right-column-title", "completed-step")
      shinyjs::removeClass("show-tree", "completed-step")
      shinyjs::addClass("show-tree", "open-accordian")
    }
  }
  
  findFileInfo = function() {
    files <- list.files(dirPath, all.files=F, recursive=T, include.dirs=T)
    filesArray <<- 0
    for (i in 1:length(files)) {
      if (substrRight(files[i],4) == ".wav") {
        fileName <- paste0(dirPath,"/",files[i])
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

