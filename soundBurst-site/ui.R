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
      shinyTree("tree")
    )
  ),
  dashboardBody(
    plotOutput("spectrogram", brush = brushOpts(id = "plot_brush", direction = "x", resetOnNew = TRUE)),
    verbatimTextOutput('directorypath')
  )
)