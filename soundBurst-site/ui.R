# install.packages("shinydashboard")
# install_github("trestletech/shinyTree")
# install.packages("shinyjs")

## ui.R ##
library(shinydashboard)
library(shinyTree)
library(shinyjs)
library(shinyFiles)

species <- read.csv("data/species-short.csv", header = TRUE)
itemsSpecies <<- c('Select Type',as.character(species$CommonName))

dashboardPage(
  dashboardHeader(title = "SoundBurst App"),
  dashboardSidebar(
    includeCSS("sidebar.css"),
    includeCSS("main.css"),
    column(12,
      div(id = "status-bar-container",
              uiOutput(outputId = "progressOne")
          ),
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
    uiOutput("audiotag"),
    div(id = "playButton"),
    div(id = "pauseButton"),
    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE, delay = 500, opacity = 0.45, stroke = "#FFD265", fill="#EEEE00")),
    # textOutput('speciesName'),
    useShinyjs(),
    fluidRow(
      column(width = 6, id = "oscillo-clip-container",
        plotOutput("spectroClip", brush = brushOpts(id = "plotZoom", direction = "xy", delay = 500, opacity = 0.45, stroke = "#FFD265", fill="#EEEE00")),
        column(width = 10, id = "clip-species-dropdown",
          box(width = NULL, id = "species-dropdown-box",status = "warning",
            div(id = "close-species-drop", "X"),
            # div(id = "time-min-container", "Time Start: ",span(id = "timeMin", 1)),
            # div(id = "time-max-container", "Time End:",span(id = "timeMax", 2)),
            textInput("timeMin", "Time Start:", 1),
            textInput("timeMax", "Time End:  ", 2),
            selectizeInput(
              'species', 'Select the species that you heard', choices = itemsSpecies,
              options = list(
                placeholder = 'Please select a species below',
                onInitialize = I('function() { this.setValue(""); }')
              )
            ),
            #These column selectors are dynamically created when the file is loaded
            uiOutput("commonName"),
            uiOutput("speciesType"),
            HTML('<label>Site Notes:</label>'),
            HTML('<textarea id="annotNotes" rows="3" cols="40">Annotation Notes</textarea>'),
            actionButton("speciesDropSubmit", "Submit")
          )
        )
      ),
      column(width = 6, id = "spectro-clip-container",
        plotOutput("spectroZoomClip")
      )
  )
  )
)