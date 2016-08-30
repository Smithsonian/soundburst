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
  tree = list()
  files = list.files(root, all.files=F, recursive=T, include.dirs=T)
  print(root)
  
  walk_directory = function(tree, path) {
    fp = file.path(root, path)
    is_dir = file.info(fp)$isdir
    folders = str_split(path, "/")[[1]]
    if (is_dir) {
      txt = paste("tree", paste("$'", folders, "'", sep="", collapse=""), " = numeric(0)", sep="")
    } else {
      txt = paste("tree", paste("$'", folders, "'", sep="", collapse=""), " = structure('', sticon='file')", sep="")
    }
    eval(parse(text = txt))
    return(tree)
  }
  
  for (i in 1:length(files)) tree = walk_directory(tree, files[i])
  save(tree, file="www/dir_tree.Rdata")
}

shinyServer(function(input, output, session) {
  shinyjs::onclick("remove",shinyjs::toggle(id = "tree", anim = TRUE))
  shinyjs::hide("pauseButton")
  shinyjs::hide("project-info-container")
  shinyjs::hide("site-info-container")
  
  test <- shinyDirChoose(input, 'directory', updateFreq=60000, session=session, roots=c(home='~'), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  output$directorypath <- renderPrint({
    dirPath <<- parseDirPath(roots=c(home='~'), input$directory)
    folders <- list.dirs(dirPath, full.names = F, recursive = TRUE)
    
    if(!(is.null(dirPath))) {
      create_directory_tree(dirPath)
      load("www/dir_tree.Rdata")
      output$tree <- renderTree(tree, quoted = FALSE)
      shinyjs::show("project-info-container")
      shinyjs::show("site-info-container")
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
        oscillo(sound)
        shinyjs::onclick("playButton",playSound())
      })
      # output$audiotag<-renderUI({
      #   path <- getPath(get_selected(input$tree, "names"))
      #   currDir <- paste0(dirPath, path, unlist(get_selected(input$tree)))
      #   print(currDir)
      #   get_audio_tag(currDir)
      # })
    }
  })
  
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
  filedata <- reactive({
    print('read')
    req(input$csvFile)
    print('req')
    infile <- parseFilePaths(roots=c(home='~'),input$csvFile)
    correctPath <- file.path(infile$datapath)
    print(correctPath)
    read.csv(correctPath, header = TRUE)
  })
  
  output$commonName <- renderUI({
    print('hello')
    df <-filedata()
    print(df)
    
    if (is.null(df)) return(NULL)

    items <- c('Select Species',as.character(df[[1]]))
    selectInput("speciesDropdown", "Species:",items)
  })
  
  output$speciesType <- renderUI({
    print('hello')
    df <-filedata()
    print(df)
    
    if (is.null(df)) return(NULL)
    
    items <- c('Select Type',as.character(df[[3]]))
    selectInput("speciesDropdown", "Type:",items)
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
    if(xmax) {
      shinyjs::html("time-min",round(xmin,digits=1))
      shinyjs::html("time-max",round(xmax,digits=1)) 
    }
  }
  
  shinyjs::onclick("close-species-drop",shinyjs::hide("clip-species-dropdown"))
  shinyjs::hide("clip-species-dropdown")
  
  siteFields <- c("name", "lat", "lon", "recId", "siteNotes")
  
  formDataSite <- reactive({
    data <- sapply(siteFields, function(x) input[[x]])
    data
  })
  
  observeEvent(input$siteInfo, {
    print(paste0(dirPath,"/",'test.csv'))
    data <- formDataSite()
    print(data)
    write.csv(data, paste0(dirPath,"/",'test.csv'))
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
    write.csv(data, paste0(dirPath,"/",'projectInfo.csv'))
  })

})

