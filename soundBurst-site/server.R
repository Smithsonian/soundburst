# fileInput max upload size is 30mb
library(shiny)
library(tools)
library("aws.s3")
library(shinyTree)
library(data.tree)
library(shinyFiles)

options(shiny.trace=TRUE)
options(shiny.maxRequestSize=70*1024^2) 


shinyServer(function(input, output) {
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
      folders
      files <- as.array(list.files(dir, full.names = FALSE, recursive = TRUE, include.dirs = TRUE, no.. = TRUE))
      
      # df <- data.frame(
      #   filename = sapply(files,
      #     function(fl) paste0("data.tree","/",fl)
      #   ),
      #   file.info(paste(dir, files, sep = "/")),
      #   stringsAsFactors = FALSE
      # )
      
      testtest = quote(list(
        rootFolder1 = "",
        rootFolder2 = list(
          subfolder1 = list(file1 = "", file2 = "", file3=""),
          subfolder2 = list(
            subSubFolder1 = list(file1 = "", file2 = "", file3="")
          )
        )
      ))
      
      output$tree <- renderTree(testtest, quoted = TRUE)
        
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