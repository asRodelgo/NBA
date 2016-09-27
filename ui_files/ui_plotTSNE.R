# plot TSNE UI ----------

fluidPage(
  fluidRow(splitLayout(cellWidths = rep("18%", 5),
              selectInput('colSeason', 'Season:', choices=c("All",sort(unique(data_tsne_sample$Season))),selectize=FALSE),
              selectInput('colTeam', 'Team:', choices=c("All",sort(unique(data_tsne_sample$Tm))),selectize=FALSE),
              selectInput('colPlayer', 'Player:', choices=c("All",sort(unique(data_tsne_sample$Player))),selectize=FALSE),
              selectInput('colSkill', 'Skill:', choices=c("All",names(data_tsne_sample)[6:ncol(data_tsne_sample)]),selectize=FALSE),
              selectInput('colAge', 'Age:', choices=c("All",sort(unique(data_tsne_sample$Age))),selectize=FALSE)
          )
  ),        
  br(),
  div(style = "position:relative",plotOutput('plotTSNEdensities',height="150px")),
  br(),
  fluidRow(
    splitLayout(cellWidths = c("80%","20%"),
      div(
       style = "position:relative",
       plotOutput('plotTSNE', 
                  hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce"),
                  click = clickOpts("plot_click"),
                  brush = brushOpts("plot_brush", delay = 100, delayType = "debounce")),
       uiOutput("hover_info")
      ),
      div(style = "position:relative",
            plotOutput('plotRadarBrushed')
      )#,
      #div(style = "position:relative",
      #    plotOutput('plotRadarBrushed_Def')
      #)
    )
  ),
  div(style = "position:relative",
      DT::dataTableOutput('tableBrushed')
  )

)