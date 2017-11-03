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
install.packages("lubridate")
install_github("cloudyr/aws.s3")
# If you are having trouble installing the AWS S3 package, please run the line below.
# install.packages("aws.s3", repos = c("cloudyr" = "http://cloudyr.github.io/drat"), INSTALL_opts = "--no-multiarch")
