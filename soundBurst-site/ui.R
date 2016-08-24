# install.packages("shinydashboard")
# install_github("trestletech/shinyTree")
# install.packages("shinyjs")

## ui.R ##
library(shinydashboard)
library(shinyTree)
library(shinyjs)
library(shinyFiles)

dashboardPage(
  dashboardHeader(title = "SoundBurst App"),
  dashboardSidebar(
    includeCSS("sidebar.css"),
    column(12,
      shinyDirButton('directory', 'Folder select', 'Please select a folder'),
      useShinyjs(),
      div(id = "remove", "Remove Files"),
      sidebarMenuOutput("menu"),
      shinyTree("tree")
    )
  ),
  dashboardBody(
    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE)),
    verbatimTextOutput('directorypath'),
    textOutput('speciesName'),
    column(width = 3, id = "species-sidebox",
           box(width = NULL, status = "warning",
               shinyFilesButton('csvFile', 'File select', 'Please select a file', FALSE),
               # fileInput('datafile', 'Upload Species CSV',
               #           accept=c('text/csv', 'text/comma-separated-values,text/plain')),
               #These column selectors are dynamically created when the file is loaded
               uiOutput("toCol")
               # ,
               # tableOutput("filetable")
               # uiOutput("routeSelect")
           )
    ),
    useShinyjs(),
    div(id = "next-one", "Move to next clip"),
    column(width = 4,
           plotOutput("spectroClip")
    )
  )
)