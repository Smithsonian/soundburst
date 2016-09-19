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
# install_github("trestletech/shinyStore")
# load_all('~/dev/emammal-soundburst/soundBurst/R')
load_all('~/dev/emammal-soundBurst/soundBurst/R')
library(audio)
# setWavPlayer('"/Applications/QuickTime\ Player"')
setWavPlayer("afplay")
library(sound)
source("createDirectoryTree.r")
source("playSound.r")



# play sound tags$audio(src = "audio.wav", type = "audio/wav", autoplay = NA, controls = NA)

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
  shinyjs::onclick("left-column-title", toggleProjectSelect())
  shinyjs::onclick("species-file-upload", togglecsvFileUploadButton())
  shinyjs::onclick("enter-project-info-label", toggleProjectInfoDisplay())
  shinyjs::onclick("right-column-title", toggleSiteInfoContainer())
  shinyjs::hide("csvFile")
  shinyjs::onclick("show-tree", toggleTree())
  shinyjs::hide("pauseButton")
  shinyjs::hide("project-info-container")
  shinyjs::hide("site-info-container")
  shinyjs::hide("complete-deployment")
  shinyjs::hide("status-bar-container")
  # shinyjs::hide("spectroClip")
  shinyjs::hide("time-box-container")
  shinyjs::hide("spectro-increment-container")
  shinyjs::hide("previous-spectro-increment")
  shinyjs::hide("tree")
  shinyjs::hide("directorypath")
  shinyjs::hide(id = "playButton",anim = FALSE)
  
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
    shinyjs::toggle("site-info-container", anim = TRUE)
    shinyjs::toggleClass("right-column-title", "open-accordian")
    shinyjs::toggleClass("right-column-title", "closed-accordian")
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
  
  projectName <<- NULL
  spectroFromTime <<- 0
  
  test <- shinyDirChoose(input, 'directory', updateFreq=60000, session=session, roots=c(home='~'), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  output$directorypath <- renderPrint({
    dirPath <<- parseDirPath(roots=c(home='~'), input$directory)
    folders <- list.dirs(dirPath, full.names = F, recursive = TRUE)
    
    if(!(is.null(dirPath))) {
      shinyjs::show("status-bar-container")
      create_directory_tree(dirPath)
      load("www/dir_tree.Rdata")
      output$tree <- renderTree(tree, quoted = FALSE)
      shinyjs::addClass("directory", "active-button")
    }
    findFileCount()
  })
  
  shinyjs::onclick("playButton", playSound())
  shinyjs::onclick("pauseButton", pauseSound())
  
  observe({
    # Plot main spectrogram
    if (is.null(unlist(get_selected(input$tree))))
    {
      return()
    } 
    else {
      path <- getPath(get_selected(input$tree, "names"))
      currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
      sound <- readWave(currDir)
      l <- length(sound@left)
      sr <- sound@samp.rate
      soundDuration <- round(l/sr,2)
      if (soundDuration > 59) {
        shinyjs::show("time-box-container")
          observeEvent(input$spectroTimeSubmit, {
          incrementAmount <<- as.numeric(input$spectroEndTime) * 60
          spectroToTime <<- incrementAmount
          renderSpectro(sound)
          if (soundDuration > incrementAmount) {
            shinyjs::show("spectro-increment-container")
          }
        }) 
      } else {
        spectroToTime <<- soundDuration
        renderSpectro(sound)
      }
      shinyjs::show("playButton",anim = FALSE)
      shinyjs::show("site-info-container")
      shinyjs::toggleClass("right-column-title", "open-accordian")
      shinyjs::toggleClass("right-column-title", "closed-accordian")
    }
  })
  
  renderSpectro = function (sound){
    output$spectrogram <- renderPlot({
      shinyjs::hide("time-box-container")
      anottationCount <<- 0
      # shinyjs::html("right-column-title",createCSVFilePath())
      oscillo(sound, from=spectroFromTime, to=spectroToTime)
      shinyjs::removeClass("right-column-title", "completed-step")
      shinyjs::show("site-info-container")
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
    currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    sound <- readWave(currDir)
    spectroToTime <<- spectroToTime - incrementAmount
    spectroFromTime <<- spectroFromTime - incrementAmount
    renderSpectro(sound)
    if (spectroFromTime == 0) {
      shinyjs::hide("previous-spectro-increment")
    }
    print('clicked')
  }
  
  showNextSpectroIncrement = function() {
    path <- getPath(get_selected(input$tree, "names"))
    currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    sound <- readWave(currDir)
    shinyjs::show("previous-spectro-increment")
    spectroToTime <<- spectroToTime + incrementAmount
    spectroFromTime <<- spectroFromTime + incrementAmount
    renderSpectro(sound)
    print('clicked')
  }

  playSound = function (){
    if(paused)
    {
      resume(audioSound)
      shinyjs::show(id = "pauseButton",anim = TRUE)
      shinyjs::hide(id = "playButton",anim = FALSE)
    } 
    else {
      path <- getPath(get_selected(input$tree, "names"))
      currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
      # Use  from = 1, to = 5, units = "seconds" when playing from a certain time
      wave <- readWave(currDir)
      sound <- audioSample(wave@left, wave@samp.rate, wave@bit)
      
      shinyjs::show(id = "pauseButton",anim = TRUE)
      shinyjs::hide(id = "playButton",anim = FALSE)
      audioSound <<- audio::play(sound)
      audioSound
      # pause(a)
      # shinyjs::onclick("pauseButton",pause(a))
    }
  }
  
  pauseSound = function () {
    paused <<- TRUE
    pause(audioSound)
    shinyjs::hide(id = "pauseButton", anim = TRUE)
    shinyjs::show(id = "playButton", anim = FALSE)
  }

  getPath = function(folderList) {
    # names <- get_selected(input$tree, "names")
    one <- attr(folderList[[1L]], "ancestry", TRUE)
    path <- paste(one, collapse = "/")
    path <- paste0(path, "/")
    return(path)
  }
  

  
  # get_audio_tag<-function(filename){
  #   tags$audio(src = filename, type ="audio/wav", controls = NA)
  # }
  # 
  # output$audiotag<-renderUI(get_audio_tag("tempwav.wav")) #starting wave file
  
  # LOAD IN SPECIES DROPDOWN
  
  shinyFileChoose(input, 'csvFile', updateFreq=60000, session=session, roots=c(home='~'), restrictions=system.file(package='base'))
  shinyjs::onclick("csvFile",shinyjs::addClass("csvFile", "active-button"))
  filedata <- reactive({
    req(input$csvFile)
    infile <- parseFilePaths(roots=c(home='~'),input$csvFile)
    correctPath <- file.path(infile$datapath)
    print(correctPath)
    read.csv(correctPath, header = TRUE)
  })
  
  output$commonName <- renderUI({
    
      df <<-filedata() 

    if (is.null(df)) return(NULL)

    items <- c('Select Species',as.character(df[[1]]))
    selectInput("speciesDropdown", "Species:",items)
  })
  
  output$speciesType <- renderUI({
    df <-filedata()

    if (is.null(df)) return(NULL)
    
    items <- c('Select Type',as.character(df[[3]]))
    selectInput("typeDropdown", "Type:",items)
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
    filedata()
  })
  
  # This creates the oscillo clips after brush
  output$spectroClip <- renderPlot({
    path <- getPath(get_selected(input$tree, "names"))
    currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    sound <- readWave(currDir)
    if(!is.null(input$plot_brush$xmax)) {
      oscillo(sound, from=input$plot_brush$xmin, to=input$plot_brush$xmax)
      # shinyjs::show("spectroClip")
      xmin <- input$plot_brush$xmin
      xmax <- input$plot_brush$xmax
      shinyjs::onclick("spectroClip",showSpeciesDropdown(xmin, xmax)) 
    }
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
  shinyjs::onclick("close-time-box",shinyjs::hide("time-box-container"))
  shinyjs::hide("clip-species-dropdown")
  
  siteFields <- c("name", "lat", "lon", "recId", "siteNotes")
  
  formDataSite <- reactive({
    data <- sapply(siteFields, function(x) input[[x]])
    data
  })
  
  createCSVFilePath = function(){
    fileFullName <- unlist(get_selected(input$tree))
    fileName <- sub(".wav", "", fileFullName)
    fileName
  }
  
  observeEvent(input$siteInfo, {
    # fileFullName <- unlist(get_selected(input$tree))
    # fileName <- sub(".wav", "", fileFullName)
    fileFullName <- unlist(get_selected(input$tree))
    filePathFull <- paste0(dirPath,"/",fileFullName)
    print(paste0(dirPath,"/",paste0(createCSVFilePath(),'.csv')))
    data <- formDataSite()
    data <- c(data, as.character(minTimeVar))
    names(data)[6] <- "start_time_date"
    data <- c(data, as.character(maxTimeVar))
    names(data)[7] <- "end_time_date"
    print(data)
    siteDF <<- data
    clipCount <<- 0
    fileDate <- gsub(" ", "-",data[[6]], fixed = TRUE)
    fileDate <- gsub(":", "-",fileDate, fixed = TRUE)
    newFileName <- paste0(projectName,"_",data[[1]],"_",fileDate)
    # shinyjs::html("right-column-title",newFileName)
    newFullFilePath <- paste0(dirPath,"/",newFileName)
    
    # Update tree
    load("www/dir_tree.Rdata")
    count <- 0
    for (name in names(tree)) {
      count = count + 1
      if(name == unlist(get_selected(input$tree)))
      {
        print(count)
        names(tree)[count] <- paste0(newFileName, ".wav")
      }
    }
    output$tree <- renderTree(tree, quoted = FALSE)
    
    file.rename(filePathFull, paste0(newFullFilePath,".wav"))
    write.csv(data, paste0(dirPath,"/",paste0(newFileName,'.csv')))
    shinyjs::addClass("siteInfo", "active-button")
    shinyjs::hide("site-info-container")
    shinyjs::addClass("right-column-title", "completed-step")
    shinyjs::toggleClass("right-column-title", "open-accordian")
    shinyjs::toggleClass("right-column-title", "closed-accordian")
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
    shinyjs::hide("csvFile", anim = TRUE)
    shinyjs::hide("directory", anim = TRUE)
    shinyjs::addClass("projectInfo", "active-button")
    shinyjs::hide("project-info-container")
    shinyjs::addClass("enter-project-info-label", "completed-step")
    shinyjs::toggleClass("enter-project-info-label", "open-accordian")
    shinyjs::toggleClass("enter-project-info-label", "closed-accordian")
    shinyjs::toggleClass("show-tree", "open-accordian")
    shinyjs::toggleClass("show-tree", "closed-accordian")
    shinyjs::show("tree")
  })
  
  speciesFields <- c("timeMin", "timeMax", "speciesDropdown", "typeDropdown", "annotNotes")
  
  formDataSpecies <- reactive({
    data <- sapply(speciesFields, function(x) input[[x]])
    data
  })
  
  observeEvent(input$speciesDropSubmit, {
    clipCount <<- clipCount + 1
    dataSet <- formDataSpecies()
    names(dataSet)[1] <- paste0(names(dataSet)[1],clipCount)
    names(dataSet)[2] <- paste0(names(dataSet)[2],clipCount)
    names(dataSet)[3] <- paste0(names(dataSet)[3],clipCount)
    names(dataSet)[4] <- paste0(names(dataSet)[4],clipCount)
    names(dataSet)[5] <- paste0(names(dataSet)[5],clipCount)
    formattedData <- c(siteDF, dataSet)
    siteDF <<- formattedData
    # print(formattedData)
    write.csv(siteDF, paste0(dirPath,"/",paste0(createCSVFilePath(),'.csv')))
  })
  
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }

  findFileCount = function() {
    projectFileCount <- 0
    projectStatusCount <<- 0
    files <- list.files(dirPath, all.files=F, recursive=T, include.dirs=T)
    for (i in 1:length(files)) {
      if (substrRight(files[i],4) == ".wav") {
        projectFileCount <- projectFileCount +1
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
    progressValue$one <<- progressValue$one + 1
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

