## ui.R ##
library(shinydashboard)
library(shinyFiles)
library(shinyTree)
library(shinyjs)

dashboardPage(
  dashboardHeader(title = "SoundBurst App"),
  dashboardSidebar(
    includeCSS("sidebar.css"),
    column(12,
      fileInput("userData", "Upload file:", accept="application/x-compressed"),
      helpText("Default max. file size is 70MB"),
      shinyTree("tree")
    )
  ),
  dashboardBody(
    verbatimTextOutput("textDisplay")
  )
)