# install.packages("shiny")
# install.packages("devtools")
# install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
# install.packages('shinyFiles')
# install_github("trestletech/shinyTree")

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

#play sound tags$audio(src = "audio.wav", type = "audio/wav", autoplay = NA, controls = NA)

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
  
  test <- shinyDirChoose(input, 'directory', updateFreq=60000, session=session, roots=c(home='~'), restrictions=system.file(package='base'), filetypes=c('', '.wav'))
  output$directorypath <- renderPrint({
    dirPath <<- parseDirPath(roots=c(home='~'), input$directory)
    folders <- list.dirs(dirPath, full.names = F, recursive = TRUE)
    
    if(!(is.null(dirPath))) {
      create_directory_tree(dirPath)
      load("www/dir_tree.Rdata")
      output$tree <- renderTree(tree, quoted = FALSE)
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
        # setWavPlayer("~/Applications/iTunes")
        # getWavPlayer()
        # listen(sound, f=8000)
        # createSpectrogram(getwd(), unlist(get_selected(selectedFile)))
      })
    }
  })
  
  
  # TESTING OUT SPECIES DROPDOWN
  
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
  
  # filedata <- reactive({
  #   infile <- input$datafile
  #   req(infile)
  # print(infile$datapath)
  #   read.csv(infile$datapath, header = TRUE)
  # })
  
  output$toCol <- renderUI({
    print('hello')
    df <-filedata()
    print(df)
    
    if (is.null(df)) return(NULL)
    
    # items=names(df)
    # names(items)=items
    items <- c('Select Species',as.character(df[[1]]))
    selectInput("speciesDropdown", "Species:",items)
  })
  
  formulaText <- reactive({
    paste(input$speciesDropdown)
  })
  # 
  output$speciesName <- renderText({
    formulaText()
  })
  
  #This previews the CSV data file
  output$filetable <- renderTable({
    filedata()
  })

})