
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
options(shiny.trace=TRUE)

# fileInput max upload size is 30mb
options(shiny.maxRequestSize=30*1024^2) 

shinyServer(function(input, output) {

  output$distPlot <- renderPlot({

    browser()
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)

    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')

  })

})
