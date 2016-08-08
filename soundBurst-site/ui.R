## ui.R ##
library(shinydashboard)
library(shinyFiles)
library(shinyjs)


dashboardPage(
  dashboardHeader(title = "SoundBurst App"),
  dashboardSidebar(
    includeCSS("sidebar.css"),
    column(12,
      fileInput("userData", "Upload file:", accept="application/x-compressed"),
      helpText("Default max. file size is 70MB"),
      menuItem('Menu Two', tabName = 'menuTwo', icon = icon('folder'),
        collapsible =
          menuSubItem('Sub-Item Three', tabName = 'subItemThree', icon = icon('users')),
          menuSubItem('Sub-Item Four', tabName = 'subItemFour')
      )
    )
  ),
  dashboardBody(
    verbatimTextOutput("textDisplay")
  )
)