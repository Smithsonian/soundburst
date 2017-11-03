# * Copyright 2015-2016 Smithsonian Institution.
# *
# * Licensed under the Apache License, Version 2.0 (the "License"); you may not
# * use this file except in compliance with the License.You may obtain a copy of
# * the License at: http://www.apache.org/licenses/
# *
#   * This software and accompanying documentation is supplied without
# * warranty of any kind. The copyright holder and the Smithsonian Institution:
# * (1) expressly disclaim any warranties, express or implied, including but not
# * limited to any implied warranties of merchantability, fitness for a
# * particular purpose, title or non-infringement; (2) do not assume any legal
# * liability or responsibility for the accuracy, completeness, or usefulness of
# * the software; (3) do not represent that use of the software would not
# * infringe privately owned rights; (4) do not warrant that the software
# * is error-free or will be maintained, supported, updated or enhanced;
# * (5) will not be liable for any indirect, incidental, consequential special
# * or punitive damages of any kind or nature, including but not limited to lost
# * profits or loss of data, on any basis arising from contract, tort or
# * otherwise, even if any of the parties has been warned of the possibility of
# * such loss or damage.
# *
#   * This distribution includes several third-party libraries, each with their own
# * license terms. For a complete copy of all copyright and license terms, including
# * those of third-party libraries, please see the product release notes.
# *


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

jsCode <- "shinyjs.fixTree = function(){setTimeout(function(){ Shiny.unbindAll();Shiny.bindAll();console.log('bind & unbind');}, 1000)}"

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
               hidden(actionButton("aws-upload-button", class="inactive-aws-button", "Upload to AWS"))),
           div(id = "project-container",
               div(id = "left-column-title", class = "open-accordian unfinished-step", 
                   span(class="dropdown-span", "Select Project")),
               shinyDirButton('directory', class = 'inactive-button', 'Folder select', 'Please select a folder')
           ),
           useShinyjs(),
           extendShinyjs(text = jsCode),
           div(id = "full-project-info-container",
               div(id = "enter-project-info-label", class = "closed-accordian unfinished-step", 
                   span(class="dropdown-span", "Enter Project Info")),
               hidden(div(id = "project-info-container",
                          div(id = "species-file-upload", "Load New Species CSV?"),
                          shinyFilesButton('csvFile', 'File select', 'Please select a file', FALSE),
                          div(id = "species-select-file-container",
                              hidden(div(id = "csv-info-modal-container",
                                         div(id = "csv-info-modal", "A CSV with a list of Species has already been loaded. However, you may upload a new CSV, as long as it is in the following format."),
                                         div(id = "csv-layout-guide", "A column must be included that contains the TYPE of animal. This column must be called 'Type'. Another column must include the specific SPECIES. This column must be titled 'CommonName'.")
                              ))
                          ),
                          hidden(div(id = "proj-name-warning", "Attention: Please make sure this field is filled out.")),
                          textInput("projectName", " Name*"),
                          HTML('<label>Project Notes</label>'),
                          HTML('<textarea id="projectNotes" rows="3" cols="40" placeholder = "Project Notes"></textarea>'),
                          # textInput("projectNotes", "Project Notes", "Project Notes"),
                          actionButton("projectInfo", class = "inactive-button", "Submit")
               ))
           ),
           div(id = "deployment-select-container",
               div(id = "select-dep-container", class = "closed-accordian unfinished-step", 
                   span(class="dropdown-span", "Select a Deployment")),
               shinyDirButton('deployment', class = 'inactive-button', 'Folder select', 'Please select a folder')
           ),
           div(id = "full-project-info-container",
               div(id = "right-column-title", class = "closed-accordian unfinished-step", 
                   span(class="dropdown-span", "Enter Deployment Info")),
               hidden(div(id = "species-sidebox-container",
                          div(id = "species-sidebox",
                              div(id = "site-info-container",
                                  hidden(div(id = "dep-name-warning", "Attention: Please make sure all required fields are filled out.")),
                                  textInput("name", "Name*", placeholder = "Name"),
                                  textInput("lat", "Lat*", placeholder = "Latitude"),
                                  textInput("lon", "Lon*", placeholder = "Longitude"),
                                  textInput("recId", "Recorder ID*", placeholder = "RecId"),
                                  HTML('<label>Deployment Notes</label>'),
                                  HTML('<textarea id="siteNotes" rows="3" cols="40" placeholder="Enter Deployment Notes.."></textarea>'),
                                  HTML('<label>Start date </label>'),
                                  textOutput("minTime"),
                                  HTML('<label>End date </label>'),
                                  textOutput("maxTime"),
                                  # textInput("siteNotes", "Site Notes", "Notes"),
                                  hidden(div(id = "file-name-warning-container",
                                             div(id = "file-name-warning", "Attention: A file with the same name already exists. Please enter a new Deployment Name and resubmit.")
                                  )),
                                  actionButton("deploymentInfo", class = "inactive-button", "Submit")
                              )
                              # div(id = "submit-site-complete-container",
                              #     div(id = "submit-site-text", "When you have submited data for all clips on a given site, click below and move on to the next"),
                              #     div(id = "submit-site-complete", "Finish Site"))
                          )))
           ),
           div(id = "full-tree-container",
               div(id = "show-tree", class = "closed-accordian unfinished-step", 
                   span(class="dropdown-span", "Select a Sequence")),
               shinyTree("tree"),
               hidden(div(id = "time-box-container",
                          div(id = "secondary-time-box-container",
                              HTML('<label id="time-box-label"> </label>'),
                              hidden(div(id = "increment-value-warning", "Attention: Please make sure this field contains a value between 0 and 1.")),
                              textInput("spectroEndTime", label = NULL, placeholder = "Please enter a number between 0 and 1 if Increment"),
                              actionButton("spectroTimeSubmit", "Increment"),
                              actionButton("noTimeSubmission", "Do not Increment")
                          )
               ))
           ),
           div(id = "completed-container",
               div(id = "completedDepContainer", class = "closed-accordian unfinished-step", 
                   span(class="dropdown-span", "View Annotations")),
               hidden(selectizeInput("annotationDrop", "Select an annotation", choices = NULL))
               # div(id = "listCompleted")
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
                        div(id = "uploadStatusContainer",
                            uiOutput(outputId = "awsProgress")
                        ),
                        hidden(div(id = "awsEmptyFieldsContainer",
                                   div(id = "awsEmptyFields", "All fields must be filled!")
                        ))
                ),
                bsModal(id = "warningBucket", trigger = "", title = "Error", uiOutput("warningBucket")),
                div(id = "playButton"),
                # tags$audio(src = "sound.mp3", type = "audio/mp3", autoplay = NA, controls = T),
                hidden(div(id = "pauseButton")),
                div(id = "mainPlotContainer",
                    # img(src = "ajax-loader.gif", id = "plotSpinner"),
                    hidden(tags$div(id = "loadingContainer1", HTML("<i id='loadingMain' class='fa fa-spinner fa-spin' style='font-size:128px;'></i>"))),
                    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "xy", resetOnNew = TRUE, delay = 500, opacity = 0.45, stroke = "#FFD265", fill="#EEEE00"))
                ),
                div(id = "spectro-increment-container",
                    hidden(div(id = "previous-spectro-increment")),
                    hidden(div(id = "next-spectro-increment"))
                ),
                useShinyjs(),
                extendShinyjs(text = jsCode),
                fluidRow(
                  column(width = 8, id = "oscillo-clip-container",
                         hidden(div(id = "playButtonClip")),
                         hidden(div(id = "pauseButtonClip")),
                         plotOutput("spectroClip")
                  ),
                  # column(width = 4, id = "spectro-clip-container",
                  #   div(id = "playButtonClipZoom"),
                  #   div(id = "pauseButtonClipZoom"),
                  #   plotOutput("spectroZoomClip")
                  # ),
                  hidden(column(width = 3, id = "clipInfo-container",
                                hidden(div(id = "site-info-warning-container",
                                           div(id = "site-info-warning", "Attention: Please ensure you have submitted information for this Deployment on the left prior to submitting annotation information")
                                )),
                                # textInput("timeMin", "Time Start", 1),
                                # textInput("timeMax", "Time End", 2),
                                div(id="detail-info-container",
                                    div(id="time-container",
                                        HTML('<div id="timeMin"></div>'),
                                        HTML('<div id="timeMax"></div>')
                                    ),
                                    div(id="freq-title", "Frequency"),
                                    div(id="freq-container",
                                        HTML('<div id="maxFreq"></div>'),
                                        HTML('<div id="minFreq"></div>'),
                                        HTML('<div id="meanFreq"></div>')
                                    ),
                                    div(id="freq-container",
                                        HTML('<div id="bandwidth"></div>'),
                                        HTML('<div id="slope"></div>')
                                    )
                                  ),
                                # textInput("maxFreq", "Max Frequency", 1),
                                # textInput("minFreq", "Min Frequency", 1),
                                # textInput("meanFreq", "Mean Frequency", 1),
                                # textInput("bandwidth", "Bandwidth", 1),
                                #These column selectors are dynamically created when the file is loaded
                                hidden(div(id = "type-name-warning", "Attention: Please make sure all required fields (*) are filled out.")),
                                uiOutput("speciesType"),
                                uiOutput("commonName"),
                                HTML('<label>Site Notes</label>'),
                                HTML('<textarea id="annotNotes" rows="3" cols="40" placeholder = "Annotation Notes"></textarea>'),
                                actionButton("speciesDropSubmit", "Submit")
                  ))
                ),
                # Absolute need to load jquery before multiClip.js otherwise document is never ready
                # tags$head(tags$script(src="shared/jquery.min.js")),
                tags$head(tags$script(src="multiClip.js"))
  )
)
