# fileInput max upload size is 30mb
library(shiny)
options(shiny.trace=TRUE)
options(shiny.maxRequestSize=70*1024^2) 

shinyServer(function(input, output) {
  output$textDisplay <- renderText({
    if (is.null(input$userData))
      return()
    else
      input$userData$datapath
  })
})