# install.packages("devtools")
# install.packages("roxygen2")
# install.packages("seewave", repos="http://cran.at.r-project.org/")
# install.packages(c("fftw","tuneR","rgl","rpanel"), repos="http://cran.at.r-project.org/")

library("devtools")
library(roxygen2)
library(seewave)
library(tuneR)

showSpectro <- function(wavFile) {
  oscillo(wavFile,f=22050)
}