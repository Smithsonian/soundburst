# install.packages("shinydashboard")
# install_github("trestletech/shinyTree")
# install.packages("shinyjs")

## ui.R ##
library(shinydashboard)
library(shinyTree)
library(shinyjs)
library(shinyFiles)
# source("customHeader.r")

# species <- read.csv("www/species-short.csv", header = TRUE)
# itemsSpecies <<- c('Select Type',as.character(species$CommonName))
# speciesType <<- c('Select Type',as.character(species$Type))

dashboardPage(
  dashboardHeader(title = "SoundBurst App"),
  dashboardSidebar(
    includeCSS("sidebar.css"),
    includeCSS("main.css"),
    column(12, id = "sidebar-column",
      div(id = "status-bar-container",
              uiOutput(outputId = "progressOne")
          ),
      div(id = "left-column-title", class = "open-accordian", "Select a Project"),
      shinyDirButton('directory', class = 'inactive-button', 'Folder select', 'Please select a folder'),
      useShinyjs(),
      div(id = "enter-project-info-label", class = "closed-accordian", "Enter Project Info"),
      div(id = "project-info-container",
          div(id = "species-select-file-container",
              div(id = "species-file-upload", "Load New Species CSV?"),
              shinyFilesButton('csvFile', class = "inactive-button", 'File select', 'Please select a file', FALSE)
          ), 
          textInput("projectName", " Name:", "Project Name"),
          HTML('<label>Project Notes:</label>'),
          HTML('<textarea id="projectNotes" rows="3" cols="40">Project Notes</textarea>'),
          # textInput("projectNotes", "Project Notes:", "Project Notes"),
          actionButton("projectInfo", class = "inactive-button", "Submit")
          ),
      div(id = "show-tree", class = "closed-accordian", "Select a Deployment"),
      shinyTree("tree"),
      div(id = "right-column-title", class = "closed-accordian", "Enter Deployment Info"),
      div(id = "file-name-warning-container",
          div(id = "file-name-warning", "Attention: A file with the same name already exists. Please enter a new Deployment Name and resubmit.")
      ),
      div(id = "time-box-container",
          div(id = "secondary-time-box-container",
              HTML('<label>This file exceeds 2 total minutes, would you like to increment the display?</label>'),
              textInput("spectroEndTime", "How many times would you like to split the file?", 1),
              actionButton("spectroTimeSubmit", "Submit"),
              actionButton("noTimeSubmission", "Do not Increment")
          )
      ),
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
    uiOutput("audiotag"),
    div(id = "spectro-increment-container",
        div(id = "previous-spectro-increment"),
        div(id = "next-spectro-increment")
    ),
    div(id = "playButton"),
    div(id = "pauseButton"),
    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE, delay = 500, opacity = 0.45, stroke = "#FFD265", fill="#EEEE00")),
    useShinyjs(),
    fluidRow(
      column(width = 4, id = "oscillo-clip-container",
        plotOutput("spectroClip", brush = brushOpts(id = "plotZoom", direction = "xy", delay = 500, opacity = 0.45, stroke = "#FFD265", fill="#EEEE00"))
      ),
      column(width = 4, id = "spectro-clip-container",
        plotOutput("spectroZoomClip")
      ),
      column(width = 4, id = "clipInfo-container",
             div(id = "site-info-warning-container",
                 div(id = "site-info-warning", "Attention: Please ensure you have submitted information for this Deployment on the left prior to submitting annotation information")
             ),
             textInput("timeMin", "Time Start:", 1),
             textInput("timeMax", "Time End:  ", 2),
             # selectizeInput(
             #   'speciesInput', 'Select the species that you heard', choices = itemsSpecies,
             #   options = list(
             #     placeholder = 'Please select a species below',
             #     onInitialize = I('function() { this.setValue(""); }')
             #   )
             # ),
             # selectizeInput(
             #   'typeInput', 'Select the type of species that you heard', choices = speciesType,
             #   options = list(
             #     placeholder = 'Please select a type below',
             #     onInitialize = I('function() { this.setValue(""); }')
             #   )
             # ),
             #These column selectors are dynamically created when the file is loaded
             uiOutput("commonName"),
             uiOutput("speciesType"),
             HTML('<label>Site Notes:</label>'),
             HTML('<textarea id="annotNotes" rows="3" cols="40">Annotation Notes</textarea>'),
             actionButton("speciesDropSubmit", "Submit")
      )
  ),
  tags$head(tags$script(src="multiClip.js"))
)
)