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