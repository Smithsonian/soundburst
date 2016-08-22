library("devtools")
library(roxygen2)

library("seewave")
library("tuneR")
# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

cars <- c(1, 3, 6, 4, 9)

# Graph the cars vector with all defaults
test2 <- plot(cars)

createSpectrogram <- function(soundFileName, wd) {
  currDir <- paste0(wd, "/", soundFileName)
  sound <- readWave(currDir)
  test <- oscillo(sound, zoom=T)
  actualSound <- listen(sound,f=4000)
}
