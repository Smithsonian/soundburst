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
              uiOutput(outputId = "progressOne")
          ),
      div(id = "left-column-title", class = "open-accordian", "Select a Project"),
      shinyDirButton('directory', class = 'inactive-button', 'Folder select', 'Please select a folder'),
      div(id = "species-select-file-container",
          div(id = "species-file-upload", class = "completed-step closed-accordian", "Load New Species CSV?"),
          shinyFilesButton('csvFile', class = "inactive-button", 'File select', 'Please select a file', FALSE)
      ),
      useShinyjs(),
      div(id = "enter-project-info-label", class = "closed-accordian", "Enter Project Info"),
      div(id = "project-info-container", 
          textInput("projectName", " Name:", "Project Name"),
          HTML('<label>Project Notes:</label>'),
          HTML('<textarea id="projectNotes" rows="3" cols="40">Project Notes</textarea>'),
          # textInput("projectNotes", "Project Notes:", "Project Notes"),
          actionButton("projectInfo", class = "inactive-button", "Submit")
          ),
      div(id = "show-tree", class = "closed-accordian", "Select a Deployment"),
      shinyTree("tree"),
      div(id = "right-column-title", class = "closed-accordian", "Enter Deployment Info"),
      div(id = "species-sidebox-container",
          column(width = 2, id = "species-sidebox",
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
                 div(id = "complete-deployment", "Complete Deployment")
                 # div(id = "submit-site-complete-container", 
                 #     div(id = "submit-site-text", "When you have submited data for all clips on a given site, click below and move on to the next"),
                 #     div(id = "submit-site-complete", "Finish Site"))
          )),
      verbatimTextOutput('directorypath')
    )
  ),
  dashboardBody(id = "content-id",
    includeCSS("main.css"),
    # uiOutput("audiotag"),
    div(id = "playButton"),
    div(id = "pauseButton"),
    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE)),
    div(id = "time-box-container",
        div(id = "secondary-time-box-container",
          div(id = "close-time-box", "X"),
          HTML('<label>Add Time to Increment Display (mins)</label>'),
          textInput("spectroEndTime", "Mins to Increment", 1),
          actionButton("spectroTimeSubmit", "Submit")
      )
    ),
    div(id = "spectro-increment-container",
          div(id = "previous-spectro-increment", "Previous Increment"),
          div(id = "next-spectro-increment", "Next Increment")
        ),
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
                      HTML('<label>Site Notes:</label>'),
                      HTML('<textarea id="annotNotes" rows="3" cols="40">Annotation Notes</textarea>'),
                      actionButton("speciesDropSubmit", "Submit")
                  )
           )
    )
  )
)