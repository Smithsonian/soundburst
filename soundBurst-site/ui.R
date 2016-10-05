# install.packages("shinydashboard")
# install_github("trestletech/shinyTree")
# install.packages("shinyjs")

## ui.R ##
library(shinydashboard)
library(shinyTree)
library(shinyjs)
library(shinyFiles)
library(shinyBS)
# source("customHeader.r")

# species <- read.csv("www/species-short.csv", header = TRUE)
# itemsSpecies <<- c('Select Type',as.character(species$CommonName))
# speciesType <<- c('Select Type',as.character(species$Type))

jscode <- "
  shinyjs.addClickListener = function(params) {
    var defaultParams = {
      id : null
    };
    params = shinyjs.getParams(params, defaultParams);
    $('#params.id').click(function() {
      $('#spectrogram_brush').clone(true).prop('id', count + 1).prop('class', 'completedBrush').css({'background-color':'green'}).appendTo('#spectrogram')
    });
  }
"

dashboardPage(
  dashboardHeader(title = "SoundBurst App"),
  dashboardSidebar(
    includeCSS("sidebar.css"),
    includeCSS("main.css"),
    column(12, id = "sidebar-column",
      div(id = "status-bar-container",
              uiOutput(outputId = "progressOne")
          ),
      div(id = "aws-upload-container",
          actionButton("aws-upload-button", class="inactive-aws-button", "Upload to AWS")),
      div(id = "project-container",
          div(id = "left-column-title", class = "open-accordian unfinished-step", "Select Project"),
          shinyDirButton('directory', class = 'inactive-button', 'Folder select', 'Please select a folder')
          ),
      useShinyjs(),
      div(id = "full-project-info-container",
          div(id = "enter-project-info-label", class = "closed-accordian unfinished-step", "Enter Project Info"),
          div(id = "project-info-container",
              div(id = "species-select-file-container",
                  div(id = "species-file-upload", "Load New Species CSV?"),
                  shinyFilesButton('csvFile', 'File select', 'Please select a file', FALSE)
              ),
              div(id = "proj-name-warning", "Attention: Please make sure this field is filled out."),
              textInput("projectName", " Name*"),
              HTML('<label>Project Notes</label>'),
              HTML('<textarea id="projectNotes" rows="3" cols="40" placeholder = "Project Notes"></textarea>'),
              # textInput("projectNotes", "Project Notes", "Project Notes"),
              actionButton("projectInfo", class = "inactive-button", "Submit")
          )
          ),
      div(id = "deployment-select-container",
          div(id = "select-dep-container", class = "closed-accordian unfinished-step", "Select a Deployment"),
          shinyDirButton('deployment', class = 'inactive-button', 'Folder select', 'Please select a folder')
      ),
      div(id = "full-project-info-container",
          div(id = "right-column-title", class = "closed-accordian unfinished-step", "Enter Deployment Info"),
          div(id = "species-sidebox-container",
              div(id = "species-sidebox",
                  div(id = "site-info-container",
                      div(id = "dep-name-warning", "Attention: Please make sure this field is filled out."),
                      textInput("name", "Name*", placeholder = "Name"),
                      textInput("lat", "Lat", placeholder = "Latitude"),
                      textInput("lon", "Lon", placeholder = "Longitude"),
                      div(id = "recid-name-warning", "Attention: Please make sure this field is filled out."),
                      textInput("recId", "Recorder ID*", placeholder = "RecId"),
                      HTML('<label>Site Notes</label>'),
                      HTML('<textarea id="siteNotes" rows="3" cols="40" placeholder="Enter Site Notes.."></textarea>'),
                      HTML('<label>Start date/time </label>'),
                      textOutput("minTime"),
                      HTML('<label>End date/time </label>'),
                      textOutput("maxTime"),
                      # textInput("siteNotes", "Site Notes", "Notes"),
                      div(id = "file-name-warning-container",
                          div(id = "file-name-warning", "Attention: A file with the same name already exists. Please enter a new Deployment Name and resubmit.")
                      ),
                      actionButton("deploymentInfo", class = "inactive-button", "Submit")
                  )
                  # div(id = "submit-site-complete-container",
                  #     div(id = "submit-site-text", "When you have submited data for all clips on a given site, click below and move on to the next"),
                  #     div(id = "submit-site-complete", "Finish Site"))
              ))
      ),
      div(id = "full-tree-container",
          div(id = "show-tree", class = "closed-accordian unfinished-step", "Select a Sequence"),
          shinyTree("tree"),
          div(id = "time-box-container",
              div(id = "secondary-time-box-container",
                  HTML('<label>This file exceeds 2 total minutes, would you like to increment the display?</label>'),
                  textInput("spectroEndTime", "How many times would you like to split the file?", placeholder = ""),
                  actionButton("spectroTimeSubmit", "Increment"),
                  actionButton("noTimeSubmission", "Do not Increment")
              )
          )
          ),
      div(id = "completed-container",
          div(id = "completedDepContainer", class = "closed-accordian unfinished-step", "View Annotations"),
          div(id = "listCompleted")
      ),
      # div(id = "complete-deployment", "Complete Deployment"),
      verbatimTextOutput('directorypath'),
      verbatimTextOutput('deploymentpath')
    )
  ),
  dashboardBody(id = "content-id",
    includeCSS("main.css"),
    uiOutput("audiotag"),
    bsModal(id = "awsModal", title = "Upload to AWS", trigger = "aws-upload-button", 
            textInput(inputId = "awsAccessKey", label = "Access key", value = NULL, placeholder = "Your AWS access key"),
            textInput(inputId = "awsSecretKey", label = "Secret key", value = NULL, placeholder = "Your AWS secret key"),
            textInput(inputId = "awsBucket", label = "AWS bucket", value = NULL, placeholder = "Your AWS bucket"),
            actionButton("awsUploadModal", "Upload to AWS"),
            div(id = "awsEmptyFieldsContainer",
                div(id = "awsEmptyFields", "All fields must be filled!")
            )
      ),
    bsModal(id = "warningBucket", trigger = "", title = "Error", uiOutput("warningBucket")),
    div(id = "playButton"),
    div(id = "pauseButton"),
    div(id = "mainPlotContainer",
      # img(src = "ajax-loader.gif", id = "plotSpinner"),
      hidden(tags$div(id = "loadingContainer1", HTML("<i id='loadingMain' class='fa fa-spinner fa-spin' style='font-size:128px;'></i>"))),
      plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE, delay = 500, opacity = 0.45, stroke = "#FFD265", fill="#EEEE00"))
    ),
    div(id = "spectro-increment-container",
      div(id = "previous-spectro-increment"),
      div(id = "next-spectro-increment")
    ),
    useShinyjs(),
    fluidRow(
      column(width = 4, id = "oscillo-clip-container",
        div(id = "playButtonClip"),
        div(id = "pauseButtonClip"),
        plotOutput("spectroClip", brush = brushOpts(id = "plotZoom", direction = "xy", delay = 500, opacity = 0.45, stroke = "#FFD265", fill="#EEEE00"))
      ),
      column(width = 4, id = "spectro-clip-container",
        div(id = "playButtonClipZoom"),
        div(id = "pauseButtonClipZoom"),
        plotOutput("spectroZoomClip")
      ),
      column(width = 4, id = "clipInfo-container",
             div(id = "site-info-warning-container",
                 div(id = "site-info-warning", "Attention: Please ensure you have submitted information for this Deployment on the left prior to submitting annotation information")
             ),
             textInput("timeMin", "Time Start", 1),
             textInput("timeMax", "Time End", 2),
             #These column selectors are dynamically created when the file is loaded
             div(id = "type-name-warning", "Attention: Please make sure all required fields (*) are filled out."),
             uiOutput("speciesType"),
             uiOutput("commonName"),
             HTML('<label>Site Notes</label>'),
             HTML('<textarea id="annotNotes" rows="3" cols="40" placeholder = "Annotation Notes"></textarea>'),
             actionButton("speciesDropSubmit", "Submit")
      )
  ),
  tags$head(tags$script(src="multiClip.js"))
)
)
