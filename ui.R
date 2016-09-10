# -------------------------------------
# UI for the NBA simulation app
#
# By: alberto.sanchez.rodelgo@gmail.com
# -------------------------------------

library(shinythemes)
library(shinyBS)
# use javascript
library(shinyjs)
library(V8)
# load functions and global data
source("global_utils.R", local = TRUE)

tagList(
  shinyjs::useShinyjs(),
  #includeCSS("css/shinytcmn.css"),
  
  navbarPage(#tcmn_logo(), id = "tcmn-logo", 
    title = NULL,
    windowTitle = "ShinyNBA", collapsible = TRUE, 
    inverse = FALSE, position = "fixed-top",
    theme = shinythemes::shinytheme("flatly"),
    tabPanel(title = "Prediction",
             h3("Here goes prediction"),
             fluidPage(
               column(12, h4("Regular Season Standings"),
                      h5("Predicted regular season standings"),
                      h6(
                        a("How this works: ", 
                          "http://www.basketball-reference.com")),
                      br(),
                      selectInput('regSeasonDay', 'Select a day:', choices=datesRange,selectize=FALSE), 
                      br()
               ),
               tabsetPanel(
                 tabPanel("Games",
                          source(file.path("ui_files", "ui_regSeasonPredGames.R"), local = TRUE)$value
                 ),
                 tabPanel("Standings",
                          source(file.path("ui_files", "ui_regSeasonPredStandings.R"), local = TRUE)$value
                 ),
#                  tabPanel("Playoffs",
#                           source(file.path("ui_files", "ui_playoffs.R"), local = TRUE)$value
#                  ),
                 tabPanel("Rosters",
                          source(file.path("ui_files", "ui_rosters.R"), local = TRUE)$value
                 )
               )
          )
      )
    )
)