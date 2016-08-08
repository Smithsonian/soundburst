# fileInput max upload size is 30mb
library(shiny)
library(shinyFiles)
library("aws.s3")
library(tools)

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
      paste0(input$userData$datapath, "/", input$userData$name)
      folders <- list.dirs(dir, full.names = FALSE)
      
      # if(nchar(folders[1]) != 0)
      # {
        output$menu <- renderMenu({
            menuItem("Menu item",tabName = 'menuTwo',  icon = icon("folder"),
              collapsible =
                menuSubItem('Sub-Item Three', tabName = 'subItemThree', icon = icon('users')),
                menuSubItem('Sub-Item Four', tabName = 'subItemFour')
            )
        })
      # }
      
      # test <- list.dirs(path = paste0(input$userData$datapath, "/", input$userData$name), full.names = TRUE, recursive = TRUE)
      folders
  })
})