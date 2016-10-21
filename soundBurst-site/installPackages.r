install.packages("devtools")
# This loads the devtools library for github installs
library(devtools)
install.packages("shiny")
install.packages('shinyFiles')
install.packages("sound")
install.packages("audio")
install.packages("httr")
install.packages("shinyBS")
# If you are on a Linux distribution, please run this line below, otherwise skip that line
install.packages(c("fftw","tuneR","rgl","rpanel"), repos="http://cran.at.r-project.org/")
install.packages("seewave")
install.packages("stringr")
install.packages("shinydashboard")
install_github("trestletech/shinyTree")
install.packages("shinyjs")
# If using windows, use this line to install S3 package
install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"), INSTALL_opts = "--no-multiarch")
# Otherwise use line below
install_github("cloudyr/aws.s3")
install.packages("rlist")
