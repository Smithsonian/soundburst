# install.packages("shiny")
# install.packages("devtools")
# install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
# install.packages('shinyFiles')
# install_github("trestletech/shinyTree")
# install.packages("sound")
# install.packages("audio")

# fileInput max upload size is 30mb
library(shiny)
library(tools)
library(devtools)
library(stringr)
library(shinyFiles)
# library("aws.s3")
library(shinyTree)
# install_github("trestletech/shinyStore")
load_all('~/dev/emammal-soundburst/soundBurst/R')
library(audio)
# setWavPlayer('"/Applications/QuickTime\ Player"')
setWavPlayer("afplay")
library(sound)


# play sound tags$audio(src = "audio.wav", type = "audio/wav", autoplay = NA, controls = NA)

options(shiny.trace=TRUE)
options(shiny.maxRequestSize=70*1024^2) 
volumes <- getVolumes()

getPath = function(folderList) {
  # names <- get_selected(input$tree, "names")
  one <- attr(folderList[[1L]], "ancestry", TRUE)
  path <- paste(one, collapse = "/")
  path <- paste0(path, "/")
  return(path)
}

create_directory_tree = function(root) {
  tree <- list()
  files <- list.files(root, all.files=F, recursive=T, include.dirs=T)
  print(root)

  walk_directory = function(tree, path) {
    fp <- file.path(root, path)
    is_dir <- file.info(fp)$isdir
    folders <- str_split(path, "/")[[1]]
    if (is_dir) {
      txt <- paste("tree", paste("$'", folders, "'", sep="", collapse=""), " = numeric(0)", sep="")
    } else {
      txt <- paste("tree", paste("$'", folders, "'", sep="", collapse=""), " = structure('', sticon='file')", sep="")
    }
    eval(parse(text = txt))
    return(tree)
  }
  
  for (i in 1:length(files)) {
    tree = walk_directory(tree, files[i])
  }
  save(tree, file="www/dir_tree.Rdata")
}

shinyServer(function(input, output, session) {
  shinyjs::onclick("remove",shinyjs::toggle(id = "tree", anim = TRUE))
  shinyjs::hide("pauseButton")
  shinyjs::hide("project-info-container")
  shinyjs::hide("site-info-container")
  shinyjs::hide("submit-site-complete-container")
  shinyjs::hide("status-bar-container")
  
  projectName <<- NULL
  
  test <- shinyDirChoose(input, 'directory', updateFreq=60000, session=session, roots=c(home='~'), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  output$directorypath <- renderPrint({
    dirPath <<- parseDirPath(roots=c(home='~'), input$directory)
    folders <- list.dirs(dirPath, full.names = F, recursive = TRUE)
    
    if(!(is.null(dirPath))) {
      create_directory_tree(dirPath)
      load("www/dir_tree.Rdata")
      output$tree <- renderTree(tree, quoted = FALSE)
      shinyjs::addClass("directory", "active-button")
      shinyjs::show("project-info-container")
      shinyjs::show("status-bar-container")
      findFileCount()
    }
  })
  
  observe({
    # Plot main spectrogram
    if (is.null(unlist(get_selected(input$tree))))
    {
      return()
    } 
    else {
      output$spectrogram <- renderPlot({
        path <- getPath(get_selected(input$tree, "names"))
        currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
        sound <- readWave(currDir)
        shinyjs::html("right-column-title",createCSVFilePath())
        oscillo(sound)
        shinyjs::show("site-info-container")
        findFileInfo()
        shinyjs::show("submit-site-complete-container")
        shinyjs::onclick("playButton",playSound())
        shinyjs::onclick("submit-site-complete-container", increaseStatusBar())
      })
      # output$audiotag<-renderUI({
      #   path <- getPath(get_selected(input$tree, "names"))
      #   currDir <- paste0(dirPath, path, unlist(get_selected(input$tree)))
      #   print(currDir)
      #   get_audio_tag(currDir)
      # })
    }
  })
  
  shinyjs::hide("species-sidebox-container")
  shinyjs::addClass("content-id", "content-all-open")
  shinyjs::onclick("show-species-sidebar", toggleRightColumn())
  
  toggleRightColumn = function (){
    shinyjs::toggleClass("show-species-sidebar-container", "move-marker-right")
    shinyjs::toggleClass("show-species-sidebar-container", "position-marker-left")
    shinyjs::toggleClass("content-id", "content-all-open")
    shinyjs::toggle("species-sidebox-container")
  }
  
  playSound = function (){
    path <- getPath(get_selected(input$tree, "names"))
    currDir <- paste0(dirPath, "/", path, unlist(get_selected(input$tree)))
    sound <- readWave(currDir)
    print(sound)
    shinyjs::show(id = "pauseButton",anim = TRUE)
    shinyjs::hide(id = "playButton",anim = FALSE)
    a <- play(currDir)
    a
    # pause(a)
    # shinyjs::onclick("pauseButton",pause(a))
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
    df <-filedata()

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
    oscillo(sound, from=input$plot_brush$xmin, to=input$plot_brush$xmax)
    xmin <- input$plot_brush$xmin
    xmax <- input$plot_brush$xmax
    # shinyjs::html('remove',tags$div(class = "close-clip", "hello There"))
    shinyjs::onclick("spectroClip",showSpeciesDropdown(xmin, xmax))
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
    shinyjs::html("right-column-title",newFileName)
    newFullFilePath <- paste0(dirPath,"/",newFileName)
    file.rename(filePathFull, paste0(newFullFilePath,".wav"))
    write.csv(data, paste0(dirPath,"/",paste0(newFileName,'.csv')))
    shinyjs::addClass("siteInfo", "active-button")
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
    shinyjs::addClass("projectInfo", "active-button")
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
    projectFileCount <<- 0
    projectStatusCount <<- 0
    files <- list.files(dirPath, all.files=F, recursive=T, include.dirs=T)
    for (i in 1:length(files)) {
      if (substrRight(files[i],4) == ".wav") {
        projectFileCount <<- projectFileCount +1
      } 
    }
    output$statusCount <- renderPrint({cat(projectStatusCount,"/",projectFileCount)})
  }
  
  increaseStatusBar = function () {
    projectStatusCount <<- projectStatusCount + 1
    output$statusCount <- renderPrint({cat(projectStatusCount,"/",projectFileCount)})
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

