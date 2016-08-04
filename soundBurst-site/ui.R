## ui.R ##
library(shinydashboard)
library(shinyjs)

dashboardPage(
  dashboardHeader(title = "SoundBurst App"),
  dashboardSidebar(
    # 
    fileInput("file", "Upload file:", multiple = TRUE),
    
    menuItem('Menu Two', tabName = 'menuTwo', icon = icon('users'),
      collapsible =
        menuSubItem('Sub-Item Three', tabName = 'subItemThree'), icon = icon('users'),
      menuSubItem('Sub-Item Four', tabName = 'subItemFour')
      )
    ),
  dashboardBody()
)