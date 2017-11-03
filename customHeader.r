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

customHeader = function (..., title = NULL, centerTitle = NULL, titleWidth = NULL, disable = FALSE, 
          .list = NULL) 
{
  items <- c(list(...), .list)
  lapply(items, tagAssert, type = "li", class = "dropdown")
  titleWidth <- validateCssUnit(titleWidth)
  custom_css <- NULL
  if (!is.null(titleWidth)) {
    custom_css <- tags$head(tags$style(HTML(gsub("_WIDTH_", 
                                                 titleWidth, fixed = TRUE, "\n      @media (min-width: 768px) {\n        .main-header > .navbar {\n          margin-left: _WIDTH_;\n        }\n        .main-header .logo {\n          width: _WIDTH_;\n        }\n      }\n    "))))
  }
  tags$header(class = "main-header", custom_css, style = if (disable) 
    "display: none;", span(class = "logo", title), tags$nav(class = "navbar navbar-static-top", span(class = "logo", centerTitle),
                                                            role = "navigation", span(shiny::icon("bars"), style = "display:none;"), 
                                                            a(href = "#", class = "sidebar-toggle", `data-toggle` = "offcanvas", 
                                                              role = "button", span(class = "sr-only", "Toggle navigation")), 
                                                            div(class = "navbar-custom-menu", tags$ul(class = "nav navbar-nav", 
                                                                                                      items))))
}