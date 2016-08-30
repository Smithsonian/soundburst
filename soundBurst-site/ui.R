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
    includeCSS("main.css"),
    # uiOutput("audiotag"),
    div(id = "playButton"),
    div(id = "pauseButton"),
    column(width = 2, id = "species-sidebox",
           div(id = "right-column-title", "Site Information"),
           textInput("name", "Name:", "Name"),
           textInput("lat", "Lat:", "Latitude"),
           textInput("lon", "Lon:", "Longitude"),
           textInput("recId", "RecId:", "RecId"),
           actionButton("submit", "Submit"),
           div(id = "right-column-title", "Upload Species File"),
           box(width = NULL, status = "warning",
               shinyFilesButton('csvFile', 'File select', 'Please select a file', FALSE)
           )
    ),
    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE)),
    verbatimTextOutput('directorypath'),
    # textOutput('speciesName'),
    useShinyjs(),
    column(width = 4, id = "spectro-clip-container",
           plotOutput("spectroClip"),
           column(width = 10, id = "clip-species-dropdown",
                  box(width = NULL, id = "species-dropdown-box",status = "warning",
                      div(id = "close-species-drop", "X"),
                      div(id = "time-min-container", "Time Start: ",span(id = "time-min")),
                      div(id = "time-max-container", "Time End:",span(id = "time-max")),
                      #These column selectors are dynamically created when the file is loaded
                      uiOutput("commonName"),
                      uiOutput("speciesType")
                  )
           )
    )
  )
)