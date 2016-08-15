# install.packages("shiny")
# install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"))
# install.packages("devtools")
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

options(shiny.trace=TRUE)
options(shiny.maxRequestSize=70*1024^2) 
volumes <- getVolumes()

shinyServer(function(input, output, session) {
  
  shinyDirChoose(input, 'directory', roots=volumes, session=session, restrictions=system.file(package='base'))
  output$directorypath <- renderPrint({parseDirPath(volumes, input$directory)})
  output$textDisplay <- renderText({
    # false means file has been uploaded
    if (is.null(input$userData))
      return()
    else
      dir <- dirname(input$userData$datapath)
      setwd(dir)
      toUnZip <- paste0(dir, "/", input$userData$name)
      file.copy(input$userData$datapath, paste0(toUnZip), overwrite = TRUE)
      unzip(toUnZip, overwrite=TRUE, exdir=file_path_sans_ext(input$userData$name))
      folders <- list.dirs(dir, full.names = F, recursive = TRUE)
      files <- as.array(list.files(dir, full.names = FALSE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE))
      # folders
      
      # df <- data.frame(
      #   filename = sapply(files,
      #     function(fl) paste0("data.tree","/",fl)
      #   ),
      #   file.info(paste(dir, files, sep = "/")),
      #   stringsAsFactors = FALSE
      # )
      
      # testtest = list(
      #   rootFolder1 = list(file1 = "", file2 = "", file3=""),
      #   rootFolder2 = list(
      #     
      #     subfolder1 = list(file1 = "", file2 = "", file3=""),
      #     subfolder2 = list(
      #       subSubFolder1 = list(file1 = "", file2 = "", file3="")
      #     )
      #   
      # ))
      create_directory_tree(dir)
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
      
      
      output$tree <- renderTree(create_directory_tree(dir), quoted = FALSE)
        
      # if(nchar(folders[1]) != 0)
      # {
        # output$menu <- renderMenu({
        #     menuItem("Menu item",tabName = 'menuTwoTwo',  icon = icon("folder"),
        #       collapsible =
        #         menuSubItem('Sub-Item Three', tabName = 'subItemThree', icon = icon('users')),
        #         menuSubItem('Sub-Item Four', tabName = 'subItemFour')
        #     )
        # })
      # }
      
      # test <- list.dirs(path = paste0(input$userData$datapath, "/", input$userData$name), full.names = TRUE, recursive = TRUE)
      # fileStructure <- as.Node(df, pathName = "filename")
      
      
  })
})