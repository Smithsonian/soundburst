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
    includeCSS("main.css"),
    column(12,
      div(id = "status-bar-container",
          div(id = "status-bar", "Status"),
          textOutput("statusCount")),
      div(id = "left-column-title", "Select a Project"),
      shinyDirButton('directory', class = 'inactive-button', 'Folder select', 'Please select a folder'),
      useShinyjs(),
      div(id = "remove", "Remove Files"),
      div(id = "project-info-container", 
          div(id = "left-column-title", "Enter Project Info:"),
          textInput("projectName", " Name:", "Project Name"),
          HTML('<label>Project Notes:</label>'),
          HTML('<textarea id="projectNotes" rows="3" cols="40">Project Notes</textarea>'),
          # textInput("projectNotes", "Project Notes:", "Project Notes"),
          actionButton("projectInfo", class = "inactive-button", "Submit")
          ),
      sidebarMenuOutput("menu"),
      verbatimTextOutput('directorypath'),
      shinyTree("tree")
    )
  ),
  dashboardBody(id = "content-id",
    includeCSS("main.css"),
    # uiOutput("audiotag"),
    div(id = "right-column-container",
    div(id = "show-species-sidebar-container", class = "move-marker-right", 
    div(id = "show-species-sidebar","")),
    div(id = "species-sidebox-container",
    column(width = 2, id = "species-sidebox",
           div(id = "right-column-title", "Select a Site"),
           div(id = "site-info-container", 
               textInput("name", "Name:", "Name"),
               textInput("lat", "Lat:", "Latitude"),
               textInput("lon", "Lon:", "Longitude"),
               textInput("recId", "RecId:", "RecId"),
               HTML('<label>Site Notes:</label>'),
               HTML('<textarea id="siteNotes" rows="3" cols="40">Site Notes</textarea>'),
               HTML('<label>Start date/time: </label>'),
               textOutput("minTime"),
               HTML('<label>End date/time: </label>'),
               textOutput("maxTime"),
               # textInput("siteNotes", "Site Notes:", "Notes"),
               actionButton("siteInfo", class = "inactive-button", "Submit")
               ),
           div(id = "species-select-file-container",
           div(id = "right-column-upload", "Upload Species File"),
           shinyFilesButton('csvFile', class = "inactive-button", 'File select', 'Please select a file', FALSE)
           ),
           div(id = "submit-site-complete-container", 
               div(id = "submit-site-text", "When you have submited data for all clips on a given site, click below and move on to the next"),
               div(id = "submit-site-complete", "Finish Site"))
    ))),
    div(id = "playButton"),
    div(id = "pauseButton"),
    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE)),
    # textOutput('speciesName'),
    useShinyjs(),
    column(width = 4, id = "spectro-clip-container",
           plotOutput("spectroClip"),
           column(width = 10, id = "clip-species-dropdown",
                  box(width = NULL, id = "species-dropdown-box",status = "warning",
                      div(id = "close-species-drop", "X"),
                      # div(id = "time-min-container", "Time Start: ",span(id = "timeMin", 1)),
                      # div(id = "time-max-container", "Time End:",span(id = "timeMax", 2)),
                      textInput("timeMin", "Time Start:", 1),
                      textInput("timeMax", "Time End:", 2),
                      #These column selectors are dynamically created when the file is loaded
                      uiOutput("commonName"),
                      uiOutput("speciesType"),
                      actionButton("speciesDropSubmit", "Submit")
                  )
           )
    )
  )
)